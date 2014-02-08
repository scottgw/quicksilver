#include <algorithm>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <vector>
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

typedef Value* pq_node;
typedef std::set<pq_node> pq_nodes;

namespace
{

  struct LiftSync : public FunctionPass
  {
    static char ID;
    LiftSync() : FunctionPass(ID) {}
    
    Function* priv_sync_func;
    int removed_count;

    virtual bool doInitialization(Module &M)
    {
      priv_sync_func = M.getFunction("priv_queue_sync");
      removed_count = 0;
      return false;
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const
    {
      AU.addRequiredTransitive<AliasAnalysis>();
      AU.addPreserved<AliasAnalysis>();
    }

    struct block_sync_info 
    {
      pq_nodes synced;
      pq_nodes asynced;
    };

    virtual bool
    runOnFunction(Function &F)
    {
      std::map<BasicBlock*, pq_nodes> final_block_map =
	sync_end_cfg_pass (F);

      trim_syncs (F, final_block_map);

      return removed_count > 0;
    }

    // Calculate the nodes which are synchronized in the all predecessors
    // of `block'.
    pq_nodes
    common_syncs (BasicBlock* block,
		  std::map<BasicBlock*, pq_nodes> &block_map)
    {
      pq_nodes accum;
      bool first = true;

      for (pred_iterator PI = pred_begin (block), E = pred_end (block);
	   PI != E;
	   ++PI)
	{
	  BasicBlock *pred = *PI;

	  if (first)
	    {
	      accum = block_map [pred];
	      first = false;
	    }
	  else
	    {
	      pq_nodes intersect;
	      pq_nodes synced = block_map [pred];
	      std::set_intersection (synced.begin(), synced.end(),
				     accum.begin(), accum.end(),
				     std::inserter (intersect,
						    intersect.begin()));
	      accum = intersect;
	    }
	}

      return accum;
    }

    std::map<BasicBlock*, pq_nodes>
    sync_end_cfg_pass (Function &F)
    {
      std::set <BasicBlock*> changed_blocks;
      std::map<BasicBlock*, pq_nodes> cfg_block_map;

      for (auto &block : F)
	{
	  changed_blocks.insert (&block);
	  cfg_block_map [&block] = pq_nodes();
	}

      while (!changed_blocks.empty())
	{
	  // Remove a basic block from the blocks left to process.
	  BasicBlock *block = *changed_blocks.begin ();
	  changed_blocks.erase (block);

	  // Find all common nodes that are synced at the end of
	  // the predecessors
	  pq_nodes common = common_syncs (block, cfg_block_map);

	  pq_nodes synced_new = update_synced (common, block);

	  // If we added/removed some sync_nodes, then update the map and
	  // add our successors to the changed set.
	  if (synced_new != cfg_block_map [block])
	    {
	      cfg_block_map [block] = synced_new;
	      for (auto it = succ_begin (block), end = succ_end (block);
		   it != end;
		   ++it)
		{
		  changed_blocks.insert (*it);
		}
	    }
	}

      return cfg_block_map;
    }

    void
    trim_syncs (Function &F, std::map<BasicBlock*, pq_nodes> &block_map)
    {
      for (auto &block : F)
    	{
    	  trim_syncs_for_block (block_map, &block);
    	}
    }

    // returns true if all predecessors of the given block have q in their
    // sync-end (i.e., q is synced up by the end of all predecessors).
    bool
    predecessor_has_sync (std::map<BasicBlock*, pq_nodes> &block_map,
			  BasicBlock *BB, pq_node q)
    {
      for (pred_iterator PI = pred_begin (BB), E = pred_end (BB);
    	   PI != E;
    	   ++PI)
    	{
    	  BasicBlock *pred = *PI;

    	  if (!block_map [pred].count (q))
    	    {
    	      return false;
    	    }
    	}

      return pred_begin (BB) != pred_end (BB);
    }

    void
    trim_syncs_for_block (std::map<BasicBlock*, pq_nodes> &block_map,
			  BasicBlock *block)
    {
      AliasAnalysis &aa = getAnalysis<AliasAnalysis>();
      pq_nodes synced = common_syncs (block, block_map);

      for (auto it = block->begin(), end = block->end();
	   it != end;
	   ++it)
    	{
    	  pq_node q;
	  auto inst = &(*it);

    	  if ((q = is_sync (inst)))
    	    {
	      auto pred =
		[&q, &aa](pq_node n)
		{
		  return aa.alias (n, q) == AliasAnalysis::MustAlias;
		};

	      bool already_in_synced =
		std::any_of (synced.begin(), synced.end(), pred);

	      if (already_in_synced)
		{
		  removed_count++;
		  it = block->getInstList().erase (it);
		}

	      synced.insert (q);
    	    }
    	  else if ((q = is_bound (inst)))
    	    {
	      pq_nodes mod_synced;
	      auto pred =
		[&] (const pq_node n)
		{
		  auto res = aa.alias (q, n);
		  
		  printf ("alias analysis: %d\n", res);

		  return res == AliasAnalysis::NoAlias;
		};

	      std::copy_if (synced.begin(), synced.end(),
			    std::inserter (mod_synced, mod_synced.begin()),
			    pred);

	      synced = mod_synced;
	      printf ("updated synced size: %d\n", synced.size());
    	    }
    	}
    }

    pq_nodes
    update_synced (pq_nodes &start_synced, BasicBlock *block)
    {
      AliasAnalysis &aa = getAnalysis<AliasAnalysis>();

      pq_nodes synced = start_synced;

      for (auto &inst : *block)
	{
	  pq_node q;

	  if ((q = is_sync (&inst)))
	    {
	      synced.insert (q);
	    }
	  else if ((q = is_bound (&inst)))
	    {
	      pq_nodes mod_synced;
	      auto pred =
		[&] (const pq_node n)
		{
		  return aa.alias (q, n) == AliasAnalysis::NoAlias;
		};

	      std::copy_if (synced.begin(), synced.end(),
			    std::inserter (mod_synced, mod_synced.begin()),
			    pred);

	      synced = mod_synced;
	    }
	}

      return synced;
    }

    void block_coalesce_sync(BasicBlock *block, pq_node n)
    {
      bool first = true;

      for (BasicBlock::iterator
             II = block->begin(),
             IIE = block->end();
           II != IIE;
           ++II)
        {
          pq_node m = is_sync(&(*II));
          if (m && m == n)
            {
              if (first)
                {
                  first = false;
                }
              else
                {
                  // changed = true;
                  II = block->getInstList().erase(II);
                }
            }
          else
            {
              // FIXME: add code to detect routine and unlock here and 
              // reset 'first' to true
            }
        }

    }

  private:
    pq_node is_sync(Instruction *inst)
    {
      return first_arg_call(inst, "priv_queue_sync");
    }

    pq_node is_bound(Instruction *inst)
    {
      pq_node q;
      if ((q = first_arg_call(inst, "priv_queue_set_in_body"))) {}
      else if ((q = first_arg_call(inst, "priv_queue_lock"))) {}
      else if ((q = first_arg_call(inst, "priv_queue_unlock"))) {}
      else
        {
          q = first_arg_call(inst, "priv_queue_routine");
        }
      
      return q;
    }

    pq_node first_arg_call(Instruction *inst, StringRef str)
    {
      if (CallInst *CI = dyn_cast_or_null<CallInst>(inst))
        {
          Function *fun = CI->getCalledFunction();
          if (fun && fun->getName() == str)
            {
              return CI->getOperand(0);
            }
          else
            {
              return 0;
            }
        }
      return 0;
    }
  };
}

char LiftSync::ID = 0;
static RegisterPass<LiftSync> X("lift-sync", "Lift Qs sync operations", false, false);

#include <algorithm>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <vector>
#include "llvm/Pass.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CFG.h" // required for a scc_iterator<Function*>
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

typedef Value* pq_node;
typedef std::set<pq_node> pq_nodes;

template <class A1, class A2, class R>
R
easy_union (A1 a1, A2 a2)
{
  R result;
  std::set_union (a1.begin(), a1.end(), a2.begin(), a2.end(),
		  std::inserter (result, result.begin()));
}


template <class A1, class A2, class R>
R
easy_intersection (A1 a1, A2 a2)
{
  R result;
  std::set_intersection (a1.begin(), a1.end(), a2.begin(), a2.end(),
			 std::inserter (result, result.begin()));
}


template <class A1, class A2, class R>
R
easy_difference (A1 a1, A2 a2)
{
  R result;
  std::set_difference (a1.begin(), a1.end(), a2.begin(), a2.end(),
		       std::inserter (result, result.begin()));
}

namespace
{

  struct LiftSync : public FunctionPass
  {
    static char ID;
    LiftSync() : FunctionPass(ID) {}
    
    Function* priv_sync_func;
    bool changed;

    virtual bool doInitialization(Module &M)
    {
      priv_sync_func = M.getFunction("priv_queue_sync");
      changed = false;
      return false;
    }

    struct block_sync_info 
    {
      pq_nodes synced;
      pq_nodes asynced;
    };


    virtual bool
    runOnFunction(Function &F)
    {
      std::map<BasicBlock*, block_sync_info> simple_block_map =
	sync_end_basic_pass (F);
      std::map<BasicBlock*, block_sync_info> final_block_map =
	sync_end_cfg_pass (F, simple_block_map);

      trim_syncs (F, final_block_map);

      return changed;
    }

    // Calculate the nodes which are synchronized in the all predecessors
    // of `block'.
    pq_nodes
    common_syncs (BasicBlock* block,
		  std::map<BasicBlock*, block_sync_info> &block_map)
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
	      accum = block_map [pred].synced;
	      first = false;
	    }
	  else
	    {
	      pq_nodes intersect;
	      pq_nodes synced = block_map [pred].synced;
	      std::set_intersection (synced.begin(), synced.end(),
				     accum.begin(), accum.end(),
				     std::inserter (intersect,
						    intersect.begin()));
	      accum = intersect;
	    }
	}

      return accum;
    }

    std::map<BasicBlock*, block_sync_info>
    sync_end_cfg_pass (Function &F,
		       std::map<BasicBlock*, block_sync_info> &block_map)
    {
      std::set <BasicBlock*> changed_blocks;
      std::map<BasicBlock*, block_sync_info> cfg_block_map = block_map;

      for (auto &block : F)
	{
	  changed_blocks.insert (&block);
	}

      while (!changed_blocks.empty())
	{
	  // Remove a basic block from the blocks left to process.
	  BasicBlock *block = *changed_blocks.begin ();
	  changed_blocks.erase (block);

	  block_sync_info block_info = cfg_block_map [block];
	  pq_nodes end_synced = block_info.synced;

	  // Find all common nodes that are synced at the end of
	  // the predecessors
	  pq_nodes common = common_syncs (block, cfg_block_map);

	  // Add the nodes that we sync.
	  pq_nodes sync_union;
	  std::set_union (end_synced.begin(), end_synced.end(),
			  common.begin(), common.end(),
			  std::inserter (sync_union, sync_union.begin()));

	  // Remove the nodes that we async
	  pq_nodes remaining;
	  std::set_difference (sync_union.begin(), sync_union.end(),
			       block_info.asynced.begin(),
			       block_info.asynced.end(),
			       std::inserter (remaining, remaining.begin()));

	  // If we added/removed some sync_nodes, then update the map and
	  // add our successors to the changed set.
	  if (remaining != block_info.synced)
	    {
	      cfg_block_map [block].synced = remaining;
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
    trim_syncs (Function &F,
		std::map<BasicBlock*, block_sync_info> &block_map)
    {
      for (auto &block : F)
    	{
    	  trim_syncs_for_block (block_map, &block);
    	}
    }

    // returns true if all predecessors of the given block have q in their
    // sync-end (i.e., q is synced up by the end of all predecessors).
    bool
    predecessor_has_sync (std::map<BasicBlock*, block_sync_info> &block_map,
			  BasicBlock *BB, pq_node q)
    {
      for (pred_iterator PI = pred_begin (BB), E = pred_end (BB);
    	   PI != E;
    	   ++PI)
    	{
    	  BasicBlock *pred = *PI;

    	  if (!block_map [pred].synced.count (q))
    	    {
    	      return false;
    	    }
    	}

      return true;
    }

    void
    trim_syncs_for_block (std::map<BasicBlock*, block_sync_info> &block_map,
			  BasicBlock *BB)
    {
      std::set<pq_node> has_bound;

      for (auto &inst : *BB)
    	{
    	  pq_node q;

    	  // The candidate node, `q', must be a sync node, all the predecessors
    	  // must have it, and it cannot yet be invalidated within this block
    	  // by having a bound node encountered for it.
	  //
	  // We can run a separate pass to coalesce the sync operations
	  // within blocks.
    	  if ((q = is_sync (&inst)) &&
    	      predecessor_has_sync (block_map, BB, q) &&
    	      !has_bound.count (q))
    	    {
	      changed = true;
    	      inst.removeFromParent();
    	    }
    	  else if ((q = is_bound (&inst)))
    	    {
    	      has_bound.insert (q);
    	    }
    	}
    }

    std::map<BasicBlock*, block_sync_info>
    sync_end_basic_pass (Function &F)
    {
      std::map<BasicBlock*, block_sync_info> block_map;

      for (auto &block : F)
    	{
    	  block_map[&block] = sync_end_for_block (&block);
    	}

      return block_map;
    }

    block_sync_info
    sync_end_for_block (BasicBlock *BB)
    {
      block_sync_info info;

      for (auto &inst : *BB)
	{
	  pq_node q;

	  if ((q = is_sync (&inst)))
	    {
	      info.synced.insert (q);
	      info.asynced.erase (q);
	    }
	  else if ((q = is_bound (&inst)))
	    {
	      info.synced.erase (q);
	      info.asynced.insert (q);
	    }
	}

      return info;
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
                  changed = true;
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
      if (CallInst *CI = dyn_cast<CallInst>(inst))
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

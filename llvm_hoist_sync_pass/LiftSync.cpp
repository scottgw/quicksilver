#include <algorithm>
#include <map>
#include <set>
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
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

    int removed_count;

    virtual bool doInitialization(Module &M)
    {
      removed_count = 0;
      return false;
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const
    {
      AU.addRequiredTransitive<AliasAnalysis>();
      AU.addPreserved<AliasAnalysis>();
    }

    virtual bool
    runOnFunction(Function &F)
    {
      std::map<BasicBlock*, pq_nodes> final_block_map = sync_end_cfg_pass (F);

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

    pq_nodes
    clear_bound (pq_node q, pq_nodes &synced)
    {
      AliasAnalysis &aa = getAnalysis<AliasAnalysis>();
      pq_nodes mod_synced;
      auto pred =
	[&] (const pq_node n)
	{
	  return aa.alias (q, n) == AliasAnalysis::NoAlias;
	};

      std::copy_if (synced.begin(), synced.end(),
		    std::inserter (mod_synced, mod_synced.begin()),
		    pred);

      return mod_synced;
    }

    pq_nodes
    clear_synced_for_call (CallInst *call, pq_nodes &synced)
    {
      Function *func = call->getCalledFunction();

      if (func != 0 &&
          (func->hasFnAttribute (Attribute::ReadOnly) ||
           func->hasFnAttribute (Attribute::ReadNone)))
	{
	  return synced;
	}
      else
	{
	  return pq_nodes();
	}
    }

    bool
    already_synced (pq_node q, pq_nodes &synced)
    {
      AliasAnalysis &aa = getAnalysis<AliasAnalysis>();
      auto pred =
	[&q, &aa](pq_node n)
	{
	  return aa.alias (n, q) == AliasAnalysis::MustAlias;
	};

      return std::any_of (synced.begin(), synced.end(), pred);
    }

    void
    trim_syncs_for_block (std::map<BasicBlock*, pq_nodes> &block_map,
			  BasicBlock *block)
    {
      pq_nodes synced = common_syncs (block, block_map);

      for (auto it = block->begin(), end = block->end();
	   it != end;
	   ++it)
    	{
    	  pq_node q;
	  auto inst = &(*it);

    	  if ((q = is_sync (inst)))
    	    {

	      if (already_synced (q, synced))
		{
		  removed_count++;
		  it = block->getInstList().erase (it);
		}
              else
                {
                  // If it's not already synced (i.e., this is the first sync),
                  // skip to the next instruction because we don't want to
                  // clear the synced list for it (we know it won't affect the
                  // synced-ness).
                  ++it;
                }

	      synced.insert (q);
    	    }
    	  else if ((q = is_bound (inst)))
    	    {
	      synced = clear_bound (q, synced);
    	    }
	  else if (CallInst *call = dyn_cast<CallInst>(inst))
	    {
	      synced = clear_synced_for_call (call, synced);
	    }
    	}
    }

    pq_nodes
    update_synced (pq_nodes &start_synced, BasicBlock *block)
    {
      pq_nodes synced = start_synced;

      for (auto it = block->begin(), end = block->end();
	   it != end;
	   ++it)
	{
	  pq_node q;
          auto inst = &(*it);

	  if ((q = is_sync (inst)))
	    {
	      synced.insert (q);

              // Skip the next instruction because we don't want to
              // clear the synced list for it (we know it won't
              // affect the synced-ness).
              ++it;
	    }
	  else if ((q = is_bound (inst)))
	    {
	      synced = clear_bound (q, synced);
	    }
	  else if (CallInst *call = dyn_cast<CallInst>(inst))
	    {
	      synced = clear_synced_for_call (call, synced);
	    }
	}

      return synced;
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

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

    virtual bool doInitialization(Module &M)
    {
      priv_sync_func = M.getFunction("priv_queue_sync");
      return false;
    }

    virtual bool runOnFunction(Function &F)
    {
      // We work backwards through the strongly connected components
      // because we want to pull the syncs up the graph.
      for (scc_iterator<Function *>
             I = scc_begin(&F),
             IE = scc_end(&F);
           I != IE;
           ++I)
        {
          std::vector<BasicBlock*> &component = *I;
          pq_nodes* free_syncs = 0;

          // Find intersection of all free sync operations among all
          // basic blocks in the SCC.
          // 
          // Also coalesce those not in the intersection as much as we can,
          // in case there is some very long straight-line code that may
          // benefit from this.
          for (std::vector<BasicBlock*>::iterator
                 BBI = component.begin(),
                 BBIE = component.end();
               BBI != BBIE;
               ++BBI)
            {
              pq_nodes bound_nodes;
              pq_nodes *ns = block_gather_syncs(*BBI, bound_nodes);
              if (free_syncs)
                {
                  free_syncs = pq_intersection(*free_syncs, *ns);
                  delete ns;
                }
              else
                {
                  free_syncs = ns;
                }

              for (pq_nodes::iterator
                     ni = bound_nodes.begin(),
                     nie = bound_nodes.end();
                   ni != nie;
                   ++ni)
                {
                  block_coalesce_sync(*BBI, *ni);
                }
            }

          // Go through all blocks in the SCC and lift those syncs
          // in the intersection.
          for (std::vector<BasicBlock*>::iterator
                 BBI = component.begin(),
                 BBIE = component.end();
               BBI != BBIE;
               ++BBI)
            {
              for (pq_nodes::iterator
                     ni = free_syncs->begin(),
                     nie = free_syncs->end();
                   ni != nie;
                   ++ni)
                {
                  block_lift_sync(F, component, *BBI, *free_syncs, *ni);
                }
            }

          if(free_syncs)
            {
              delete free_syncs;
            }
        }

      return true; // FIXME: Don't always indicate that something changed.
    }

    pq_nodes* pq_intersection(pq_nodes &s1, pq_nodes &s2)
    {
      pq_nodes *result = new pq_nodes();
      std::set_intersection(s1.begin(), s1.end(),
                            s2.begin(), s2.end(),
                            std::inserter(*result, result->end()));

      return result;
    }

    // This will gather sync operations on a particular queue
    // together within a basic block. If there was no point that forces
    // the sync (such as lock or async_routine operations) then that private
    // queue is returned in the vector and no sync operations on that queue are
    // in the argument basic block: all are removed.
    //
    // The purpose of the vector is to indicate that these sync operations have
    // to be inserted somewhere else (outside of the loop body for example).
    //
    // Post:  all of the nodes in the result set are not in any 'routine' or
    // 'set_in_body' routines.
    pq_nodes* block_gather_syncs(BasicBlock *block,
                                   pq_nodes &bound_nodes)
    {
      pq_nodes syncs;
      pq_nodes sync_barriers;

      for (BasicBlock::reverse_iterator
             II = block->rbegin(),
             IIE = block->rend();
           II != IIE;
           ++II)
        {
          if (pq_node q = is_sync(&(*II)))
            {
              syncs.insert(q);
            }
          else if (pq_node q = is_body_or_routine(&(*II)))
            {
              sync_barriers.insert(q);
            }
        }

      pq_nodes* free_nodes = new pq_nodes();

      std::set_difference(syncs.begin(), syncs.end(),
                          sync_barriers.begin(), sync_barriers.end(),
                          std::inserter(*free_nodes, free_nodes->end()));

      std::set_difference(syncs.begin(), syncs.end(),
                          free_nodes->begin(), free_nodes->end(),
                          std::inserter(bound_nodes, bound_nodes.end()));

      assert (free_nodes);
      return free_nodes;
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
              errs() << "Found\n";
              if (first)
                {
                  first = false;
                }
              else
                {
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

    // Lift the sync out of the block. This involves two things:
    //   1) removing the sync on the the node 'n' from the basic block.
    //   2) inserting the sync into the incoming edges to this block
    //      from OUTSIDE the SCC.
    void block_lift_sync(Function &F,
                         std::vector<BasicBlock*> &scc,
                         BasicBlock *block,
                         pq_nodes &ns,
                         pq_node n)
    {
      // Remove the sync instructions that operate on the queues
      // in the diff_syncs set.
      for (BasicBlock::iterator
             II = block->begin(),
             IIE = block->end();
           II != IIE;
           ++II)
        {
          pq_node m = is_sync(&(*II));
          if (m && m == n)
            {
              II = block->getInstList().erase(II);
            }
        }

      // Examine incoming edges to the first node and see if they coem
      // from outside the component. If they do, insert a sync node
      // there.
      BasicBlock* pred_block = new_incoming_sync_block(F, ns);
      // FIXME: Hook up the block to the incoming edges to this block
      // if they come from outside the SCC.
    }

    // Createa a new basic block that syncs all the nodes in `ns'.
    BasicBlock* new_incoming_sync_block(Function &F, pq_nodes &ns)
    {
      BasicBlock* block = BasicBlock::Create(F.getContext(),
                                             "LiftedSyncBlock",
                                             &F);

      Value *currProc = &(*(F.arg_begin()));

      BasicBlock::InstListType &insts = block->getInstList();
      for (pq_nodes::iterator
             ni = ns.begin(),
             nie = ns.end();
           ni != nie;
           ++ni)
        {
          std::vector<Value*> args(2);
          args[0] = *ni;
          args[1] = currProc;

          CallInst::Create(priv_sync_func, args, "", block);
        }

      return block;
    }

    pq_node is_sync(Instruction *inst)
    {
      return first_arg_call(inst, "priv_queue_sync");
    }

    pq_node is_body_or_routine(Instruction *inst)
    {
      if (pq_node q = first_arg_call(inst, "priv_queue_set_in_body"))
        {
          return q;
        }
      else
        {
          return first_arg_call(inst, "priv_queue_routine");
        }
    }

  private:
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

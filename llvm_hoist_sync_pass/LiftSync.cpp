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

namespace
{

  struct LiftSync : public FunctionPass
  {
    static char ID;
    LiftSync() : FunctionPass(ID) {}
    
    Function* priv_sync_func;
    bool full_lift;
    bool coalesce;

    virtual bool doInitialization(Module &M)
    {
      priv_sync_func = M.getFunction("priv_queue_sync");
      return false;
    }

    virtual bool runOnFunction(Function &F)
    {
      full_lift = false;
      coalesce = false;
      // We work backwards through the strongly connected components
      // because we want to pull the syncs up the graph.
      for (scc_iterator<Function *>
             I = scc_begin(&F),
             IE = scc_end(&F);
           I != IE;
           ++I)
        {
          std::vector<BasicBlock*> &component = *I;
          pq_nodes all_syncs = get_all_syncs(component);

          // Find all sync operations that aren't bound in any part of
          // the component.
          for (std::vector<BasicBlock*>::iterator
                 BBI = component.begin(),
                 BBIE = component.end();
               BBI != BBIE;
               ++BBI)
            {
              pq_nodes bound_nodes = block_gather_bound(*BBI);

              all_syncs = pq_difference(all_syncs, bound_nodes);

              // Coalesce the bound nodes as much as possible within the block.
              // This may help some straight-line code.
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
          // that should be lifted outside the SCC.
          for (std::vector<BasicBlock*>::iterator
                 BBI = component.begin(),
                 BBIE = component.end();
               BBI != BBIE;
               ++BBI)
            {
              for (pq_nodes::iterator
                     ni = all_syncs.begin(),
                     nie = all_syncs.end();
                   ni != nie;
                   ++ni)
                {
                  block_lift_sync(F, component, *BBI, all_syncs, *ni);
                }
            }
        }

      if (full_lift || coalesce)
        {
          if (full_lift)
            {
              errs() << "full lift\n";
            }

          if (coalesce)
            {
              errs() << "coalesce\n";
            }
          F.viewCFG();
        }
      return full_lift || coalesce;
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
                  coalesce = true;
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
              full_lift = true;
              II = block->getInstList().erase(II);
            }
        }

      // Examine incoming edges to the first node and see if they coem
      // from outside the component. If they do, insert a sync node
      // there.

      for (pred_iterator
             pi = pred_begin(block),
             pie = pred_end(block);
           pi != pie;
           ++pi)
        {
          // For every predecessor that's not in this component
          if (count(scc.begin(), scc.end(), *pi) == 0)
            {
              full_lift = true;
              BasicBlock* new_pred = SplitEdge(*pi, block, this);
              fill_split (F, new_pred, ns);
            }
        }
    }

    void fill_split(Function &F, BasicBlock *new_block, pq_nodes &ns)
    {

      Value *currProc = &(*(F.arg_begin()));

      for (pq_nodes::iterator
             ni = ns.begin(),
             nie = ns.end();
           ni != nie;
           ++ni)
        {
          std::vector<Value*> args(2);
          args[0] = *ni;
          args[1] = currProc;

          CallInst::Create(priv_sync_func, args, "", &new_block->back());
        }      
    }


  private:
    pq_nodes pq_difference(pq_nodes &s1, pq_nodes &s2)
    {
      pq_nodes result;

      std::set_difference(s1.begin(), s1.end(),
                          s2.begin(), s2.end(),
                          std::inserter(result, result.end()));

      return result;
    }

    pq_nodes get_all_syncs(std::vector<BasicBlock*> &component)
    {
      pq_nodes syncs;
      for (std::vector<BasicBlock*>::const_iterator
             BBI = component.begin(),
             BBIE = component.end();
           BBI != BBIE;
           ++BBI)
        {
          BasicBlock* block = *BBI;
          for (BasicBlock::iterator
                 II = block->begin(),
                 IIE = block->end();
               II != IIE;
               ++II)
            {
              if (pq_node q = is_sync(&(*II)))
                {
                  syncs.insert(q);
                }
            }
        }
      return syncs;
    }

    pq_nodes block_gather_bound(BasicBlock *block)
    {
      pq_nodes bound;

      for (BasicBlock::iterator
             II = block->begin(),
             IIE = block->end();
           II != IIE;
           ++II)
        {
          if (pq_node q = is_bound(&(*II)))
            {
              bound.insert(q);
            }
        }

      return bound;
    }

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

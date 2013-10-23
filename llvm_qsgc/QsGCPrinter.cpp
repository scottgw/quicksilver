#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/CodeGen/GCMetadataPrinter.h"
#include "llvm/Support/Compiler.h"
#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/Support/Compiler.h"

using namespace llvm;

namespace {
  class LLVM_LIBRARY_VISIBILITY QsGC : public GCStrategy {
  public:
    QsGC()
      {
        InitRoots = true;
        NeededSafePoints =
          1 << GC::PostCall
          | 1 << GC::Return
          | 1 << GC::Return
          | 1 << GC::Loop;
        UsesMetadata = true;
      }
  };

  GCRegistry::Add<QsGC>
  X("qsgc", "My bespoke garbage collector.");
}

namespace {
  class LLVM_LIBRARY_VISIBILITY QsGCPrinter : public GCMetadataPrinter {
  public:
    virtual void beginAssembly(AsmPrinter &AP);

    virtual void finishAssembly(AsmPrinter &AP);
  };

  GCMetadataPrinterRegistry::Add<QsGCPrinter>
  Y("qsgc", "QS garbage collector.");
}

std::string stack_map_top_str = "__gc_stack_map_top";
std::string stack_map_func_str = "stack_map_count_addr";

/* Hackish way to emit the stack function. It would be more portable
   to learn the LLVM way of generating platform dependent assembly.
*/
void emitStackMapFunc(AsmPrinter &AP)
{
  MCContext &MCCtx = AP.OutContext;
  MCStreamer &OS = AP.OutStreamer;
  StringRef stackFuncStr(stack_map_func_str);
  MCSymbol* stackFuncSym = MCCtx.GetOrCreateSymbol(stackFuncStr);

  OS.SwitchSection(AP.getObjFileLowering().getTextSection());

  OS.EmitRawText(StringRef("\t.globl " + stack_map_func_str));
  OS.EmitRawText(StringRef("\t.align 16, 0x90"));
  OS.EmitRawText(StringRef("\t.type " + stack_map_func_str +",@function"));
  OS.EmitLabel(stackFuncSym);
  OS.EmitCFIStartProc();
  OS.EmitRawText(StringRef("\tleal " + stack_map_top_str + ",  %eax"));
  OS.EmitRawText(StringRef("\tret"));

  {
    MCSymbol* tempSym = MCCtx.CreateTempSymbol();
    OS.EmitLabel(tempSym);
    OS.EmitELFSize
      (stackFuncSym,
       MCBinaryExpr::CreateSub
       (MCSymbolRefExpr::Create (tempSym, MCCtx),
        MCSymbolRefExpr::Create (stackFuncSym, MCCtx),
        MCCtx));

  }
  OS.EmitCFIEndProc();

}

void QsGCPrinter::beginAssembly(AsmPrinter &AP) {
  // Nothing to do.
}

void QsGCPrinter::finishAssembly(AsmPrinter &AP) {
  MCContext &MCCtx = AP.OutContext;
  MCStreamer &OS = AP.OutStreamer;
  unsigned IntPtrSize = AP.TM.getDataLayout()->getPointerSize();

  emitStackMapFunc(AP);

  // Put this in the data section.
  OS.SwitchSection(AP.getObjFileLowering().getDataSection());


  {
    StringRef stackMapStr(stack_map_top_str);
    MCSymbol* stackSym = MCCtx.GetOrCreateSymbol(stackMapStr);
    OS.EmitLabel(stackSym);
    AP.EmitInt32(std::distance(begin(), end()));
  }

  // For each function...
  for (iterator FI = begin(), FE = end(); FI != FE; ++FI) {
    GCFunctionInfo &MD = **FI;

    // A compact GC layout. Emit this data structure:
    //
    // struct {
    //   int32_t PointCount;
    //   int32_t StackFrameSize; // in words
    //   int32_t StackArity;
    //   int32_t LiveCount;
    //   void *SafePointAddress[PointCount];
    //   int32_t LiveOffsets[LiveCount];
    // } __gcmap_<FUNCTIONNAME>;

    // Align to address width.
    AP.EmitAlignment(IntPtrSize == 4 ? 2 : 3);

    // Emit PointCount.
    OS.AddComment("safe point count");
    AP.EmitInt32(MD.size());

    // Stack information never change in safe points! Only print info from the
    // first call-site.
    GCFunctionInfo::iterator PI = MD.begin();

    // Emit the stack frame size.
    OS.AddComment("stack frame size (in words)");
    AP.EmitInt32(MD.getFrameSize() / IntPtrSize);

    // Emit stack arity, i.e. the number of stacked arguments.
    unsigned RegisteredArgs = IntPtrSize == 4 ? 5 : 6;
    unsigned StackArity = MD.getFunction().arg_size() > RegisteredArgs ?
                          MD.getFunction().arg_size() - RegisteredArgs : 0;
    OS.AddComment("stack arity");
    AP.EmitInt32(StackArity);

    // Emit the number of live roots in the function.
    OS.AddComment("live root count");
    AP.EmitInt32(MD.live_size(PI));

    // And each safe point...
    for (GCFunctionInfo::iterator PI = MD.begin(),
                                  PE = MD.end(); PI != PE; ++PI) {
      // Emit the address of the safe point.
      OS.AddComment("safe point address");
      MCSymbol *Label = PI->Label;
      AP.EmitLabelPlusOffset(Label/*Hi*/, 0/*Offset*/, 8/*Size*/);
    }

    // And for each live root...
    for (GCFunctionInfo::live_iterator LI = MD.live_begin(PI),
                                       LE = MD.live_end(PI);
                                       LI != LE; ++LI) {
      // Emit live root's offset within the stack frame.
      OS.AddComment("stack index (offset / wordsize)");
      AP.EmitInt32(LI->StackOffset);
    }
  }
}

; RUN: opt -load=/home/scott/src/llvm-3.4/Release+Asserts/lib/libLLVMQs.so -O3 -lift-sync < %s | llvm-dis -o - | FileCheck %s

%struct.priv_queue_ = type { i32 }

declare void @priv_queue_sync(%struct.priv_queue_*) #1
declare void @priv_queue_routine(%struct.priv_queue_*) #1
declare zeroext i1 @pred(%struct.priv_queue_*) readonly #1

; CHECK: @f
; CHECK: @priv_queue_sync
; CHECK-NOT: @priv_queue_sync
; CHECK: ret

; Function Attrs: nounwind uwtable
define i32 @f() #0 {
  %p_ = alloca %struct.priv_queue_, align 4
  %q_ = alloca %struct.priv_queue_, align 4
  %1 = bitcast %struct.priv_queue_* %p_ to i8*
  %2 = bitcast %struct.priv_queue_* %q_ to i8*

  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  %3 = call zeroext i1 @pred(%struct.priv_queue_* %p_)
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  call void @priv_queue_routine(%struct.priv_queue_* %q_)
  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  br label %6

; <label>:5                                       ; preds = %0
  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  br label %6

; <label>:6                                       ; preds = %5, %4
  %i.0 = phi i32 [ 1, %4 ], [ 2, %5 ]
  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  ret i32 %i.0
}

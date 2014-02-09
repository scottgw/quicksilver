; RUN: opt -load=../LLVMQs.so -O3 -lift-sync < %s | llvm-dis -o - | FileCheck %s

%struct.priv_queue_ = type { i32 }

declare void @priv_queue_sync(%struct.priv_queue_*) #1
declare void @priv_queue_routine(%struct.priv_queue_*) #1
declare zeroext i1 @pred(%struct.priv_queue_*) readonly #1

; CHECK: @f
; CHECK: @priv_queue_sync
; CHECK: @priv_queue_routine
; CHECK: @priv_queue_sync
; CHECK-NOT: @priv_queue_sync
; CHECK: ret

; Function Attrs: nounwind uwtable
define i32 @f() #0 {
  %p_ = alloca %struct.priv_queue_, align 4
  %q_ = bitcast %struct.priv_queue_* %p_ to %struct.priv_queue_*

  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  %b = call zeroext i1 @pred(%struct.priv_queue_* %p_)
  br i1 %b, label %1, label %2

; <label>:1                                       ; preds = %0
  call void @priv_queue_routine(%struct.priv_queue_* %q_)
  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  br label %3

; <label>:2                                       ; preds = %0
  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  br label %3

; <label>:3                                       ; preds = %1, %2
  %i.0 = phi i32 [ 1, %1 ], [ 2, %2 ]
  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  ret i32 %i.0
}

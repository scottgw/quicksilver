; RUN: opt -load=../LLVMQs.so -O3 -lift-sync < %s | llvm-dis -o - | FileCheck %s

declare void @priv_queue_sync(%struct.priv_queue_*) #1
declare zeroext i1 @pred(%struct.priv_queue_*) readonly #1

%struct.priv_queue_ = type { i32 }

; CHECK: @f
; CHECK: @priv_queue_sync
; CHECK-NOT: @priv_queue_sync
; CHECK: ret

; Function Attrs: nounwind uwtable
define i32 @f(%struct.priv_queue_* %p) #0 {
  call void @priv_queue_sync(%struct.priv_queue_* %p)
  %1 = call zeroext i1 @pred(%struct.priv_queue_* %p)
  br i1 %1, label %2, label %3

; <label>:2                                       ; preds = %0
  call void @priv_queue_sync(%struct.priv_queue_* %p)
  br label %4

; <label>:3                                       ; preds = %0
  call void @priv_queue_sync(%struct.priv_queue_* %p)
  br label %4

; <label>:4                                       ; preds = %3, %2
  %i.0 = phi i32 [ 1, %2 ], [ 2, %3 ]
  call void @priv_queue_sync(%struct.priv_queue_* %p)
  ret i32 %i.0
}

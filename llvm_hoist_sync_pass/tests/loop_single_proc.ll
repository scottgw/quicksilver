; RUN: opt -load=/home/scott/src/llvm-3.4/Release+Asserts/lib/libLLVMQs.so -O3 -lift-sync < %s | llvm-dis -o - | FileCheck %s

%struct.priv_queue_ = type { i32 }

declare void @priv_queue_sync(%struct.priv_queue_*) #1

;; We want to check that there is only one sync left in the code, i.e., 
;; that it is lifted out of the loop and removed from the loop body. 
;; This test may be a little brittle as it relies on -O3 peeling a single
;; sync body out of the loop body initially.

; CHECK: @f
; CHECK: @priv_queue_sync
; CHECK-NOT: @priv_queue_sync
; CHECK: ret

define i32 @f(%struct.priv_queue_* %p) #0 {
  br label %1

; <label>:1                                       ; preds = %5, %0
  %i = phi i32 [ 0, %0 ], [ %6, %5 ]
  %s = phi i32 [ 1, %0 ], [ %4, %5 ]
  %2 = icmp slt i32 %i, 200000
  br i1 %2, label %3, label %7

; <label>:3                                       ; preds = %1
  %4 = add nsw i32 %s, %s
  call void @priv_queue_sync(%struct.priv_queue_* %p)
  br label %5

; <label>:5                                       ; preds = %3
  %6 = add nsw i32 %i, 1
  br label %1

; <label>:7                                       ; preds = %1
  call void @priv_queue_sync(%struct.priv_queue_* %p)
  ret i32 %s
}

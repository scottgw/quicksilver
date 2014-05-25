; RUN: opt -load=../libLLVMQs.so -O3 -lift-sync < %s | llvm-dis -o - | FileCheck %s

%struct.priv_queue_ = type { i32 }

declare void @priv_queue_sync(%struct.priv_queue_*) #1
declare void @priv_queue_routine(%struct.priv_queue_*) #1
declare zeroext i1 @pred(%struct.priv_queue_*) readonly #1

; CHECK: @f
; CHECK: @priv_queue_sync
; CHECK-NOT: @priv_queue_sync
; CHECK: ret

; Function Attrs: nounwind uwtable
define i1 @f() #0 {
  %p_ = alloca %struct.priv_queue_, align 4
  %q_ = bitcast %struct.priv_queue_* %p_ to %struct.priv_queue_*

  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  %b1 = call zeroext i1 @pred(%struct.priv_queue_* %p_)

  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  %b2 = call zeroext i1 @pred(%struct.priv_queue_* %p_)

  call void @priv_queue_sync(%struct.priv_queue_* %p_)
  %b3 = call zeroext i1 @pred(%struct.priv_queue_* %p_)

  ret i1 %b3
}

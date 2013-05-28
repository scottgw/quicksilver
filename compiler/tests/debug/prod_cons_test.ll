; ModuleID = 'prod_cons_test.obc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.string = type { i64, i8* }
%struct.sockaddr_in = type { i16, i16, %struct.in_addr, [8 x i8] }
%struct.in_addr = type { i32 }
%struct.sockaddr = type { i16, [14 x i8] }
%procStructType = type opaque
%syncdataStructType = type opaque
%privqStructType = type opaque
%closureStructType = type opaque
%clostypeStructType = type opaque
%notifierStructType = type opaque
%Socket = type { i64 }
%Socket_Buffer = type { %Socket*, %struct.string* }
%File = type { i64 }
%Data = type { i64 }
%Worker = type { %separate_wrapper*, i64 }
%separate_wrapper = type <{ %procStructType*, i8* }>

@_global = weak global [1 x i8] zeroinitializer
@"Could not create socket\0A_global" = weak global [25 x i8] c"Could not create socket\0A\00"
@"Could not bind socket\0A_global" = weak global [23 x i8] c"Could not bind socket\0A\00"
@"Error in accept\0A_global" = weak global [17 x i8] c"Error in accept\0A\00"
@"File not found_global" = weak global [15 x i8] c"File not found\00"
@"Worker done \0A_global" = weak global [14 x i8] c"Worker done \0A\00"

define void @qs_init() nounwind uwtable readnone {
  ret void
}

define noalias i8* @qs_malloc(i64 %sz) nounwind uwtable {
  %1 = tail call noalias i8* @malloc(i64 %sz) nounwind
  ret i8* %1
}

declare noalias i8* @malloc(i64) nounwind

define void @exit_with(i64 %i) noreturn nounwind uwtable {
  %1 = trunc i64 %i to i32
  tail call void @exit(i32 %1) noreturn nounwind
  unreachable
}

declare void @exit(i32) noreturn nounwind

define void @array_make(i8* nocapture %proc, i8*** nocapture %array, i64 %size) nounwind uwtable {
  %1 = shl i64 %size, 3
  %2 = tail call noalias i8* @malloc(i64 %1) nounwind
  %3 = bitcast i8* %2 to i8**
  store i8** %3, i8*** %array, align 8
  ret void
}

define i8* @array_item(i8* nocapture %proc, i8*** nocapture %array, i64 %i) nounwind uwtable readonly {
  %1 = load i8*** %array, align 8
  %2 = getelementptr inbounds i8** %1, i64 %i
  %3 = load i8** %2, align 8
  ret i8* %3
}

define void @array_put(i8* nocapture %proc, i8*** nocapture %array, i64 %i, i8* %elem) nounwind uwtable {
  %1 = load i8*** %array, align 8
  %2 = getelementptr inbounds i8** %1, i64 %i
  store i8* %elem, i8** %2, align 8
  ret void
}

define noalias i8* @new_pointer_8(i64 %n) nounwind uwtable {
  %1 = tail call noalias i8* @malloc(i64 %n) nounwind
  ret i8* %1
}

define void @pointer_8_put(i8* nocapture %p, i64 %i, i8 signext %c) nounwind uwtable {
  %1 = getelementptr inbounds i8* %p, i64 %i
  store i8 %c, i8* %1, align 1
  ret void
}

define signext i8 @pointer_8_get(i8* nocapture %p, i64 %i) nounwind uwtable readonly {
  %1 = getelementptr inbounds i8* %p, i64 %i
  %2 = load i8* %1, align 1
  ret i8 %2
}

define signext i8 @int8_to_char(i8 signext %i) nounwind uwtable readnone {
  ret i8 %i
}

define signext i8 @char_to_int8(i8 signext %c) nounwind uwtable readnone {
  ret i8 %c
}

define i64 @int8_to_int(i8 signext %i) nounwind uwtable readnone {
  %1 = sext i8 %i to i64
  ret i64 %1
}

define signext i8 @int_to_int8(i64 %i) nounwind uwtable readnone {
  %1 = trunc i64 %i to i8
  ret i8 %1
}

define void @print(%struct.string* nocapture %str) nounwind uwtable {
  %1 = getelementptr inbounds %struct.string* %str, i64 0, i32 1
  %2 = load i8** %1, align 8
  %3 = getelementptr inbounds %struct.string* %str, i64 0, i32 0
  %4 = load i64* %3, align 8
  %5 = tail call i64 @write(i32 1, i8* %2, i64 %4) nounwind
  ret void
}

declare i64 @write(i32, i8* nocapture, i64)

define i32 @fd_close(i32 %fd) nounwind uwtable {
  %1 = tail call i32 @close(i32 %fd) nounwind
  ret i32 %1
}

declare i32 @close(i32)

define i32 @open_read(%struct.string* nocapture %str) nounwind uwtable {
  %1 = getelementptr inbounds %struct.string* %str, i64 0, i32 1
  %2 = load i8** %1, align 8
  %3 = tail call i32 (i8*, i32, ...)* @open(i8* %2, i32 0) nounwind
  ret i32 %3
}

declare i32 @open(i8* nocapture, i32, ...)

define i64 @fd_read(i32 %fd, i8* nocapture %buf, i64 %size) nounwind uwtable {
  %1 = tail call i64 @read(i32 %fd, i8* %buf, i64 %size) nounwind
  ret i64 %1
}

declare i64 @read(i32, i8* nocapture, i64)

define i32 @new_tcp_socket() nounwind uwtable {
  %enable = alloca i32, align 4
  %1 = call i32 @socket(i32 2, i32 1, i32 0) nounwind
  store i32 1, i32* %enable, align 4
  %2 = bitcast i32* %enable to i8*
  %3 = call i32 @setsockopt(i32 %1, i32 1, i32 2, i8* %2, i32 4) nounwind
  ret i32 %1
}

declare i32 @socket(i32, i32, i32) nounwind

declare i32 @setsockopt(i32, i32, i32, i8*, i32) nounwind

define i32 @socket_bind(i32 %socketfd, i32 %port) nounwind uwtable {
  %local_addr = alloca %struct.sockaddr_in, align 4
  %1 = getelementptr inbounds %struct.sockaddr_in* %local_addr, i64 0, i32 0
  store i16 2, i16* %1, align 4
  %2 = getelementptr inbounds %struct.sockaddr_in* %local_addr, i64 0, i32 2, i32 0
  store i32 0, i32* %2, align 4
  %3 = trunc i32 %port to i16
  %4 = call zeroext i16 @htons(i16 zeroext %3) nounwind readnone
  %5 = getelementptr inbounds %struct.sockaddr_in* %local_addr, i64 0, i32 1
  store i16 %4, i16* %5, align 2
  %6 = bitcast %struct.sockaddr_in* %local_addr to %struct.sockaddr*
  %7 = call i32 @bind(i32 %socketfd, %struct.sockaddr* %6, i32 16) nounwind
  ret i32 %7
}

declare zeroext i16 @htons(i16 zeroext) nounwind readnone

declare i32 @bind(i32, %struct.sockaddr*, i32) nounwind

define i32 @socket_listen(i32 %socketfd, i32 %backlog_size) nounwind uwtable {
  %1 = tail call i32 @listen(i32 %socketfd, i32 %backlog_size) nounwind
  ret i32 %1
}

declare i32 @listen(i32, i32) nounwind

define i32 @socket_accept(i32 %socketfd) nounwind uwtable {
  %1 = tail call i32 @accept(i32 %socketfd, %struct.sockaddr* null, i32* null) nounwind
  ret i32 %1
}

declare i32 @accept(i32, %struct.sockaddr*, i32*)

define i32 @socket_recv(i32 %socketfd, %struct.string* nocapture %str) nounwind uwtable {
  %1 = getelementptr inbounds %struct.string* %str, i64 0, i32 1
  %2 = load i8** %1, align 8
  %3 = getelementptr inbounds %struct.string* %str, i64 0, i32 0
  %4 = load i64* %3, align 8
  %5 = tail call i64 @recv(i32 %socketfd, i8* %2, i64 %4, i32 0) nounwind
  store i64 %5, i64* %3, align 8
  %6 = trunc i64 %5 to i32
  ret i32 %6
}

declare i64 @recv(i32, i8*, i64, i32)

define i32 @socket_send(i32 %socketfd, i8* %buf, i32 %len) nounwind uwtable {
  %1 = sext i32 %len to i64
  %2 = tail call i64 @send(i32 %socketfd, i8* %buf, i64 %1, i32 0) nounwind
  %3 = trunc i64 %2 to i32
  ret i32 %3
}

declare i64 @send(i32, i8*, i64, i32)

define void @__Pointer_8_put(%procStructType* nocapture, i8* nocapture, i64, i8) nounwind {
putStartB:
  %4 = getelementptr inbounds i8* %1, i64 %2
  store i8 %3, i8* %4, align 1
  ret void
}

define i8 @__Pointer_8_char_item(%procStructType* nocapture, i8* nocapture, i64) nounwind readonly {
char_itemStartB:
  %3 = getelementptr inbounds i8* %1, i64 %2
  %4 = load i8* %3, align 1
  ret i8 %4
}

declare %procStructType* @proc_new_from_other(%procStructType*)

declare %procStructType* @proc_new_root(%syncdataStructType*, void (%procStructType*)*)

declare %privqStructType* @proc_get_queue(%procStructType*, %procStructType*)

declare void @proc_deref_priv_queues(%procStructType*)

declare void @proc_shutdown(%procStructType*, %procStructType*)

declare void @proc_wait_for_available(%procStructType*, %procStructType*)

declare %closureStructType* @closure_new(i8*, %clostypeStructType*, i64, i8****, %clostypeStructType***)

declare %clostypeStructType* @closure_pointer_type()

declare %clostypeStructType* @closure_sint_type()

declare %clostypeStructType* @closure_void_type()

declare void @priv_queue_lock(%privqStructType*, %procStructType*)

declare void @priv_queue_unlock(%privqStructType*, %procStructType*)

declare void @priv_queue_routine(%privqStructType*, %closureStructType*, %procStructType*)

declare void @priv_queue_function(%privqStructType*, %closureStructType*, i8*, %procStructType*)

declare void @create_executors(%syncdataStructType*, i64)

declare void @join_executors()

declare %notifierStructType* @notifier_spawn(%syncdataStructType*)

declare void @notifier_join(%notifierStructType*)

declare %syncdataStructType* @sync_data_new(i64)

declare void @sync_data_free(%syncdataStructType*)

define void @__Prelude_print(%struct.string* nocapture) nounwind {
printStartB:
  %1 = getelementptr inbounds %struct.string* %0, i64 0, i32 1
  %2 = load i8** %1, align 8
  %3 = getelementptr inbounds %struct.string* %0, i64 0, i32 0
  %4 = load i64* %3, align 8
  %5 = tail call i64 @write(i32 1, i8* %2, i64 %4) nounwind
  ret void
}

define void @__Prelude_exit_with(i64) noreturn nounwind {
exit_withStartB:
  tail call void @exit_with(i64 %0)
  unreachable
}

define i64 @__Prelude_open_read(%struct.string* nocapture) nounwind {
open_readStartB:
  %1 = getelementptr inbounds %struct.string* %0, i64 0, i32 1
  %2 = load i8** %1, align 8
  %3 = tail call i32 (i8*, i32, ...)* @open(i8* %2, i32 0) nounwind
  %4 = zext i32 %3 to i64
  ret i64 %4
}

define void @__Prelude_fd_close(i64) nounwind {
fd_closeStartB:
  %1 = trunc i64 %0 to i32
  %2 = tail call i32 @close(i32 %1) nounwind
  ret void
}

define i64 @__Prelude_fd_read(i64, i8* nocapture, i64) nounwind {
fd_readStartB:
  %3 = trunc i64 %0 to i32
  %4 = tail call i64 @read(i32 %3, i8* %1, i64 %2) nounwind
  ret i64 %4
}

define i8 @__Prelude_int8_to_char(i8) nounwind readnone {
int8_to_charStartB:
  ret i8 %0
}

define i8 @__Prelude_char_to_int8(i8) nounwind readnone {
char_to_int8StartB:
  ret i8 %0
}

define i64 @__Prelude_int8_to_int(i8) nounwind readnone {
int8_to_intStartB:
  %1 = sext i8 %0 to i64
  ret i64 %1
}

define i8 @__Prelude_int_to_int8(i64) nounwind readnone {
int_to_int8StartB:
  %1 = trunc i64 %0 to i8
  ret i8 %1
}

define noalias %struct.string* @__Prelude_int_to_str(%procStructType* nocapture, i64) nounwind {
int_to_strStartB:
  %2 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %2 to %struct.string*
  %gep.i = bitcast i8* %2 to i64*
  store i64 0, i64* %gep.i, align 8
  %gep1.i = getelementptr i8* %2, i64 8
  %3 = bitcast i8* %gep1.i to i8**
  store i8* getelementptr inbounds ([1 x i8]* @_global, i64 0, i64 0), i8** %3, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop condition.backedge", %int_to_strStartB
  %str.011 = phi %struct.string* [ %"casting char ptr to typed ptr", %int_to_strStartB ], [ %"casting char ptr to typed ptr.i", %"loop condition.backedge" ]
  %rest.010 = phi i64 [ %1, %int_to_strStartB ], [ %"genBinOp generated operation2", %"loop condition.backedge" ]
  %"genBinOp generated operation1" = srem i64 %rest.010, 10
  %4 = trunc i64 %"genBinOp generated operation1" to i8
  %"genBinOp generated operation2" = sdiv i64 %rest.010, 10
  %"genBinOp generated operation3" = add i8 %4, 48
  %5 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i" = bitcast i8* %5 to %struct.string*
  %gep.i8 = getelementptr %struct.string* %str.011, i64 0, i32 0
  %6 = load i64* %gep.i8, align 8
  %"genBinOp generated operation.i" = add i64 %6, 1
  %gep.i.i = bitcast i8* %5 to i64*
  store i64 %"genBinOp generated operation.i", i64* %gep.i.i, align 8
  %gep1.i.i = getelementptr i8* %5, i64 8
  %7 = bitcast i8* %gep1.i.i to i8**
  %"genBinOp generated operation.i.i" = add i64 %6, 2
  %8 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i") nounwind
  store i8* %8, i8** %7, align 8
  %9 = getelementptr inbounds i8* %8, i64 %"genBinOp generated operation.i"
  store i8 0, i8* %9, align 1
  store i8 %"genBinOp generated operation3", i8* %8, align 1
  %"genBinOp generated operation320.i" = icmp slt i64 %6, 1
  br i1 %"genBinOp generated operation320.i", label %"loop condition.backedge", label %"loop body.lr.ph.i"

"loop condition.backedge":                        ; preds = %"loop body.i", %"loop body"
  %rest.010.off = add i64 %rest.010, 9
  %10 = icmp ult i64 %rest.010.off, 19
  br i1 %10, label %"after loop", label %"loop body"

"loop body.lr.ph.i":                              ; preds = %"loop body"
  %gep.i18.i = getelementptr %struct.string* %str.011, i64 0, i32 1
  %.pre.i = load i8** %gep.i18.i, align 8
  br label %"loop body.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.021.i = phi i64 [ 1, %"loop body.lr.ph.i" ], [ %"genBinOp generated operation4.i", %"loop body.i" ]
  %"genBinOp generated operation.i19.i" = add i64 %i.021.i, -1
  %11 = getelementptr inbounds i8* %.pre.i, i64 %"genBinOp generated operation.i19.i"
  %12 = load i8* %11, align 1
  %13 = getelementptr inbounds i8* %8, i64 %i.021.i
  store i8 %12, i8* %13, align 1
  %"genBinOp generated operation4.i" = add i64 %i.021.i, 1
  %"genBinOp generated operation3.i" = icmp sgt i64 %"genBinOp generated operation4.i", %6
  br i1 %"genBinOp generated operation3.i", label %"loop condition.backedge", label %"loop body.i"

"after loop":                                     ; preds = %"loop condition.backedge"
  ret %struct.string* %"casting char ptr to typed ptr.i"
}

define noalias i8* @__Pointer_new_pointer_8(i64) nounwind {
new_pointer_8StartB:
  %1 = tail call noalias i8* @malloc(i64 %0) nounwind
  ret i8* %1
}

define void @__Pointer_pointer_8_put(i8* nocapture, i64, i8) nounwind {
pointer_8_putStartB:
  %3 = getelementptr inbounds i8* %0, i64 %1
  store i8 %2, i8* %3, align 1
  ret void
}

define i8 @__Pointer_pointer_8_get(i8* nocapture, i64) nounwind readonly {
pointer_8_getStartB:
  %2 = getelementptr inbounds i8* %0, i64 %1
  %3 = load i8* %2, align 1
  ret i8 %3
}

define void @__String_make(%procStructType* nocapture, %struct.string* nocapture, i64) nounwind {
makeStartB:
  %gep = getelementptr %struct.string* %1, i64 0, i32 0
  store i64 %2, i64* %gep, align 8
  %gep1 = getelementptr %struct.string* %1, i64 0, i32 1
  %"genBinOp generated operation" = add i64 %2, 1
  %3 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation") nounwind
  store i8* %3, i8** %gep1, align 8
  %4 = getelementptr inbounds i8* %3, i64 %2
  store i8 0, i8* %4, align 1
  ret void
}

define void @__String_make_with_pointer(%procStructType* nocapture, %struct.string* nocapture, i64, i8*) nounwind {
make_with_pointerStartB:
  %gep = getelementptr %struct.string* %1, i64 0, i32 0
  store i64 %2, i64* %gep, align 8
  %gep1 = getelementptr %struct.string* %1, i64 0, i32 1
  store i8* %3, i8** %gep1, align 8
  ret void
}

define i8 @__String_item(%procStructType* nocapture, %struct.string* nocapture, i64) nounwind readonly {
itemStartB:
  %gep = getelementptr %struct.string* %1, i64 0, i32 1
  %3 = load i8** %gep, align 8
  %"genBinOp generated operation" = add i64 %2, -1
  %4 = getelementptr inbounds i8* %3, i64 %"genBinOp generated operation"
  %5 = load i8* %4, align 1
  ret i8 %5
}

define i1 @__String_starts_with(%procStructType* nocapture, %struct.string* nocapture, %struct.string* nocapture) nounwind readonly {
starts_withStartB:
  %gep = getelementptr %struct.string* %2, i64 0, i32 0
  %3 = load i64* %gep, align 8
  %gep1 = getelementptr %struct.string* %1, i64 0, i32 0
  %4 = load i64* %gep1, align 8
  %"genBinOp generated operation" = icmp sgt i64 %3, %4
  br i1 %"genBinOp generated operation", label %merge, label %"loop condition.preheader"

"loop condition.preheader":                       ; preds = %starts_withStartB
  %"genBinOp generated operation312" = icmp slt i64 %3, 1
  br i1 %"genBinOp generated operation312", label %merge, label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %"loop condition.preheader"
  %gep.i = getelementptr %struct.string* %1, i64 0, i32 1
  %5 = load i8** %gep.i, align 8
  %gep.i10 = getelementptr %struct.string* %2, i64 0, i32 1
  %6 = load i8** %gep.i10, align 8
  br label %"loop body"

merge:                                            ; preds = %"loop condition.after loop_crit_edge", %"loop condition.preheader", %starts_withStartB
  %Result.0 = phi i1 [ false, %starts_withStartB ], [ %phitmp, %"loop condition.after loop_crit_edge" ], [ true, %"loop condition.preheader" ]
  ret i1 %Result.0

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.013 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation5", %"loop body" ]
  %"genBinOp generated operation.i" = add i64 %i.013, -1
  %7 = getelementptr inbounds i8* %5, i64 %"genBinOp generated operation.i"
  %8 = load i8* %7, align 1
  %9 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation.i"
  %10 = load i8* %9, align 1
  %"equality check" = icmp ne i8 %8, %10
  %"genBinOp generated operation5" = add i64 %i.013, 1
  %"genBinOp generated operation3" = icmp sgt i64 %"genBinOp generated operation5", %3
  %"genBinOp generated operation4" = or i1 %"genBinOp generated operation3", %"equality check"
  br i1 %"genBinOp generated operation4", label %"loop condition.after loop_crit_edge", label %"loop body"

"loop condition.after loop_crit_edge":            ; preds = %"loop body"
  %phitmp = xor i1 %"equality check", true
  br label %merge
}

define i1 @__String_equals(%procStructType* nocapture, %struct.string* nocapture, %struct.string* nocapture) nounwind readonly {
equalsStartB:
  %gep = getelementptr %struct.string* %2, i64 0, i32 0
  %3 = load i64* %gep, align 8
  %gep1 = getelementptr %struct.string* %1, i64 0, i32 0
  %4 = load i64* %gep1, align 8
  %"equality check" = icmp eq i64 %3, %4
  br i1 %"equality check", label %"loop condition.preheader", label %merge

"loop condition.preheader":                       ; preds = %equalsStartB
  %"genBinOp generated operation12" = icmp slt i64 %3, 1
  br i1 %"genBinOp generated operation12", label %merge, label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %"loop condition.preheader"
  %gep.i = getelementptr %struct.string* %1, i64 0, i32 1
  %5 = load i8** %gep.i, align 8
  %gep.i10 = getelementptr %struct.string* %2, i64 0, i32 1
  %6 = load i8** %gep.i10, align 8
  br label %"loop body"

merge:                                            ; preds = %"loop condition.after loop_crit_edge", %"loop condition.preheader", %equalsStartB
  %Result.0 = phi i1 [ false, %equalsStartB ], [ %phitmp, %"loop condition.after loop_crit_edge" ], [ true, %"loop condition.preheader" ]
  ret i1 %Result.0

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.013 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation5", %"loop body" ]
  %"genBinOp generated operation.i" = add i64 %i.013, -1
  %7 = getelementptr inbounds i8* %5, i64 %"genBinOp generated operation.i"
  %8 = load i8* %7, align 1
  %9 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation.i"
  %10 = load i8* %9, align 1
  %"equality check4" = icmp ne i8 %8, %10
  %"genBinOp generated operation5" = add i64 %i.013, 1
  %"genBinOp generated operation" = icmp sgt i64 %"genBinOp generated operation5", %3
  %"genBinOp generated operation3" = or i1 %"genBinOp generated operation", %"equality check4"
  br i1 %"genBinOp generated operation3", label %"loop condition.after loop_crit_edge", label %"loop body"

"loop condition.after loop_crit_edge":            ; preds = %"loop body"
  %phitmp = xor i1 %"equality check4", true
  br label %merge
}

define i64 @__String_find(%procStructType* nocapture, %struct.string* nocapture, i8) nounwind readonly {
findStartB:
  %gep = getelementptr %struct.string* %1, i64 0, i32 0
  %3 = load i64* %gep, align 8
  %"genBinOp generated operation11" = icmp slt i64 %3, 1
  br i1 %"genBinOp generated operation11", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %findStartB
  %gep.i = getelementptr %struct.string* %1, i64 0, i32 1
  %4 = load i8** %gep.i, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.012 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation2", %"loop body" ]
  %"genBinOp generated operation.i" = add i64 %i.012, -1
  %5 = getelementptr inbounds i8* %4, i64 %"genBinOp generated operation.i"
  %6 = load i8* %5, align 1
  %"equality check" = icmp eq i8 %6, %2
  %"genBinOp generated operation2" = add i64 %i.012, 1
  %"genBinOp generated operation" = icmp sgt i64 %"genBinOp generated operation2", %3
  %"genBinOp generated operation1" = or i1 %"genBinOp generated operation", %"equality check"
  br i1 %"genBinOp generated operation1", label %"loop condition.after loop_crit_edge", label %"loop body"

"loop condition.after loop_crit_edge":            ; preds = %"loop body"
  %phitmp = select i1 %"equality check", i64 %i.012, i64 -1
  br label %"after loop"

"after loop":                                     ; preds = %"loop condition.after loop_crit_edge", %findStartB
  %found.0.lcssa = phi i64 [ %phitmp, %"loop condition.after loop_crit_edge" ], [ -1, %findStartB ]
  ret i64 %found.0.lcssa
}

define noalias %struct.string* @__String_append(%procStructType* nocapture, %struct.string* nocapture, %struct.string* nocapture) nounwind {
appendStartB:
  %3 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %3 to %struct.string*
  %gep = getelementptr %struct.string* %1, i64 0, i32 0
  %4 = load i64* %gep, align 8
  %gep1 = getelementptr %struct.string* %2, i64 0, i32 0
  %5 = load i64* %gep1, align 8
  %"genBinOp generated operation" = add i64 %5, %4
  %gep.i = bitcast i8* %3 to i64*
  store i64 %"genBinOp generated operation", i64* %gep.i, align 8
  %gep1.i = getelementptr i8* %3, i64 8
  %6 = bitcast i8* %gep1.i to i8**
  %"genBinOp generated operation.i" = add i64 %"genBinOp generated operation", 1
  %7 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %7, i8** %6, align 8
  %8 = getelementptr inbounds i8* %7, i64 %"genBinOp generated operation"
  store i8 0, i8* %8, align 1
  %"genBinOp generated operation430" = icmp slt i64 %4, 1
  br i1 %"genBinOp generated operation430", label %"loop condition7.preheader", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %appendStartB
  %gep.i23 = getelementptr %struct.string* %1, i64 0, i32 1
  %.pre32 = load i8** %gep.i23, align 8
  br label %"loop body"

"loop condition7.preheader":                      ; preds = %"loop body", %appendStartB
  %i.0.lcssa = phi i64 [ 1, %appendStartB ], [ %"genBinOp generated operation6", %"loop body" ]
  %"genBinOp generated operation1328" = icmp sgt i64 %i.0.lcssa, %"genBinOp generated operation"
  br i1 %"genBinOp generated operation1328", label %"after loop9", label %"loop body8.lr.ph"

"loop body8.lr.ph":                               ; preds = %"loop condition7.preheader"
  %gep.i25 = getelementptr %struct.string* %2, i64 0, i32 1
  %.pre = load i8** %gep.i25, align 8
  br label %"loop body8"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.031 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation6", %"loop body" ]
  %"genBinOp generated operation5" = add i64 %i.031, -1
  %9 = getelementptr inbounds i8* %.pre32, i64 %"genBinOp generated operation5"
  %10 = load i8* %9, align 1
  %11 = getelementptr inbounds i8* %7, i64 %"genBinOp generated operation5"
  store i8 %10, i8* %11, align 1
  %"genBinOp generated operation6" = add i64 %i.031, 1
  %"genBinOp generated operation4" = icmp sgt i64 %"genBinOp generated operation6", %4
  br i1 %"genBinOp generated operation4", label %"loop condition7.preheader", label %"loop body"

"loop body8":                                     ; preds = %"loop body8", %"loop body8.lr.ph"
  %i.129 = phi i64 [ %i.0.lcssa, %"loop body8.lr.ph" ], [ %"genBinOp generated operation17", %"loop body8" ]
  %"genBinOp generated operation14" = add i64 %i.129, -1
  %"genBinOp generated operation.i26" = sub i64 %"genBinOp generated operation14", %4
  %12 = getelementptr inbounds i8* %.pre, i64 %"genBinOp generated operation.i26"
  %13 = load i8* %12, align 1
  %14 = getelementptr inbounds i8* %7, i64 %"genBinOp generated operation14"
  store i8 %13, i8* %14, align 1
  %"genBinOp generated operation17" = add i64 %i.129, 1
  %"genBinOp generated operation13" = icmp sgt i64 %"genBinOp generated operation17", %"genBinOp generated operation"
  br i1 %"genBinOp generated operation13", label %"after loop9", label %"loop body8"

"after loop9":                                    ; preds = %"loop body8", %"loop condition7.preheader"
  ret %struct.string* %"casting char ptr to typed ptr"
}

define noalias %struct.string* @__String_prepend_char(%procStructType* nocapture, %struct.string* nocapture, i8) nounwind {
prepend_charStartB:
  %3 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %3 to %struct.string*
  %gep = getelementptr %struct.string* %1, i64 0, i32 0
  %4 = load i64* %gep, align 8
  %"genBinOp generated operation" = add i64 %4, 1
  %gep.i = bitcast i8* %3 to i64*
  store i64 %"genBinOp generated operation", i64* %gep.i, align 8
  %gep1.i = getelementptr i8* %3, i64 8
  %5 = bitcast i8* %gep1.i to i8**
  %"genBinOp generated operation.i" = add i64 %4, 2
  %6 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %6, i8** %5, align 8
  %7 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation"
  store i8 0, i8* %7, align 1
  store i8 %2, i8* %6, align 1
  %"genBinOp generated operation320" = icmp slt i64 %4, 1
  br i1 %"genBinOp generated operation320", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %prepend_charStartB
  %gep.i18 = getelementptr %struct.string* %1, i64 0, i32 1
  %.pre = load i8** %gep.i18, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.021 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation4", %"loop body" ]
  %"genBinOp generated operation.i19" = add i64 %i.021, -1
  %8 = getelementptr inbounds i8* %.pre, i64 %"genBinOp generated operation.i19"
  %9 = load i8* %8, align 1
  %10 = getelementptr inbounds i8* %6, i64 %i.021
  store i8 %9, i8* %10, align 1
  %"genBinOp generated operation4" = add i64 %i.021, 1
  %"genBinOp generated operation3" = icmp sgt i64 %"genBinOp generated operation4", %4
  br i1 %"genBinOp generated operation3", label %"after loop", label %"loop body"

"after loop":                                     ; preds = %"loop body", %prepend_charStartB
  ret %struct.string* %"casting char ptr to typed ptr"
}

define noalias %struct.string* @__String_append_char(%procStructType* nocapture, %struct.string* nocapture, i8) nounwind {
append_charStartB:
  %3 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %3 to %struct.string*
  %gep = getelementptr %struct.string* %1, i64 0, i32 0
  %4 = load i64* %gep, align 8
  %"genBinOp generated operation" = add i64 %4, 1
  %gep.i = bitcast i8* %3 to i64*
  store i64 %"genBinOp generated operation", i64* %gep.i, align 8
  %gep1.i = getelementptr i8* %3, i64 8
  %5 = bitcast i8* %gep1.i to i8**
  %"genBinOp generated operation.i" = add i64 %4, 2
  %6 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %6, i8** %5, align 8
  %7 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation"
  store i8 0, i8* %7, align 1
  %"genBinOp generated operation316" = icmp slt i64 %4, 1
  br i1 %"genBinOp generated operation316", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %append_charStartB
  %gep.i14 = getelementptr %struct.string* %1, i64 0, i32 1
  %.pre = load i8** %gep.i14, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %"genBinOp generated operation618" = phi i64 [ 0, %"loop body.lr.ph" ], [ %i.017, %"loop body" ]
  %i.017 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation5", %"loop body" ]
  %8 = getelementptr inbounds i8* %.pre, i64 %"genBinOp generated operation618"
  %9 = load i8* %8, align 1
  %10 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation618"
  store i8 %9, i8* %10, align 1
  %"genBinOp generated operation5" = add i64 %i.017, 1
  %"genBinOp generated operation3" = icmp sgt i64 %"genBinOp generated operation5", %4
  br i1 %"genBinOp generated operation3", label %"after loop", label %"loop body"

"after loop":                                     ; preds = %"loop body", %append_charStartB
  %"genBinOp generated operation6.lcssa" = phi i64 [ 0, %append_charStartB ], [ %i.017, %"loop body" ]
  %11 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation6.lcssa"
  store i8 %2, i8* %11, align 1
  ret %struct.string* %"casting char ptr to typed ptr"
}

define noalias %struct.string* @__String_substring(%procStructType* nocapture, %struct.string* nocapture, i64, i64) nounwind {
substringStartB:
  %4 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %4 to %struct.string*
  %"genBinOp generated operation" = sub i64 %3, %2
  %gep.i = bitcast i8* %4 to i64*
  store i64 %"genBinOp generated operation", i64* %gep.i, align 8
  %gep1.i = getelementptr i8* %4, i64 8
  %5 = bitcast i8* %gep1.i to i8**
  %"genBinOp generated operation.i" = add i64 %"genBinOp generated operation", 1
  %6 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %6, i8** %5, align 8
  %7 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation"
  store i8 0, i8* %7, align 1
  %"genBinOp generated operation113" = icmp slt i64 %2, %3
  br i1 %"genBinOp generated operation113", label %"loop body.lr.ph", label %"after loop"

"loop body.lr.ph":                                ; preds = %substringStartB
  %gep.i11 = getelementptr %struct.string* %1, i64 0, i32 1
  %.pre = load i8** %gep.i11, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.014 = phi i64 [ %2, %"loop body.lr.ph" ], [ %"genBinOp generated operation3", %"loop body" ]
  %"genBinOp generated operation2" = sub i64 %i.014, %2
  %"genBinOp generated operation.i12" = add i64 %i.014, -1
  %8 = getelementptr inbounds i8* %.pre, i64 %"genBinOp generated operation.i12"
  %9 = load i8* %8, align 1
  %10 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation2"
  store i8 %9, i8* %10, align 1
  %"genBinOp generated operation3" = add i64 %i.014, 1
  %exitcond = icmp eq i64 %"genBinOp generated operation3", %3
  br i1 %exitcond, label %"after loop", label %"loop body"

"after loop":                                     ; preds = %"loop body", %substringStartB
  ret %struct.string* %"casting char ptr to typed ptr"
}

define i64 @__Socket_Util_new_tcp_socket() nounwind {
new_tcp_socketStartB:
  %enable.i = alloca i32, align 4
  %0 = bitcast i32* %enable.i to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %0) nounwind
  %1 = call i32 @socket(i32 2, i32 1, i32 0) nounwind
  store i32 1, i32* %enable.i, align 4
  %2 = call i32 @setsockopt(i32 %1, i32 1, i32 2, i8* %0, i32 4) nounwind
  call void @llvm.lifetime.end(i64 -1, i8* %0) nounwind
  %3 = zext i32 %1 to i64
  ret i64 %3
}

define i64 @__Socket_Util_socket_bind(i64, i64) nounwind {
socket_bindStartB:
  %local_addr.i = alloca %struct.sockaddr_in, align 4
  %2 = trunc i64 %0 to i32
  %3 = bitcast %struct.sockaddr_in* %local_addr.i to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %3) nounwind
  %4 = getelementptr inbounds %struct.sockaddr_in* %local_addr.i, i64 0, i32 0
  store i16 2, i16* %4, align 4
  %5 = getelementptr inbounds %struct.sockaddr_in* %local_addr.i, i64 0, i32 2, i32 0
  store i32 0, i32* %5, align 4
  %6 = trunc i64 %1 to i16
  %7 = call zeroext i16 @htons(i16 zeroext %6) nounwind readnone
  %8 = getelementptr inbounds %struct.sockaddr_in* %local_addr.i, i64 0, i32 1
  store i16 %7, i16* %8, align 2
  %9 = bitcast %struct.sockaddr_in* %local_addr.i to %struct.sockaddr*
  %10 = call i32 @bind(i32 %2, %struct.sockaddr* %9, i32 16) nounwind
  call void @llvm.lifetime.end(i64 -1, i8* %3) nounwind
  %11 = zext i32 %10 to i64
  ret i64 %11
}

define i64 @__Socket_Util_socket_listen(i64, i64) nounwind {
socket_listenStartB:
  %2 = trunc i64 %0 to i32
  %3 = trunc i64 %1 to i32
  %4 = tail call i32 @listen(i32 %2, i32 %3) nounwind
  %5 = zext i32 %4 to i64
  ret i64 %5
}

define i64 @__Socket_Util_socket_accept(i64) nounwind {
socket_acceptStartB:
  %1 = trunc i64 %0 to i32
  %2 = tail call i32 @accept(i32 %1, %struct.sockaddr* null, i32* null) nounwind
  %3 = zext i32 %2 to i64
  ret i64 %3
}

define i64 @__Socket_Util_socket_recv(i64, %struct.string* nocapture) nounwind {
socket_recvStartB:
  %2 = trunc i64 %0 to i32
  %3 = getelementptr inbounds %struct.string* %1, i64 0, i32 1
  %4 = load i8** %3, align 8
  %5 = getelementptr inbounds %struct.string* %1, i64 0, i32 0
  %6 = load i64* %5, align 8
  %7 = tail call i64 @recv(i32 %2, i8* %4, i64 %6, i32 0) nounwind
  store i64 %7, i64* %5, align 8
  %8 = and i64 %7, 4294967295
  ret i64 %8
}

define i64 @__Socket_Util_socket_send(i64, i8*, i64) nounwind {
socket_sendStartB:
  %3 = trunc i64 %0 to i32
  %sext = shl i64 %2, 32
  %4 = ashr exact i64 %sext, 32
  %5 = tail call i64 @send(i32 %3, i8* %1, i64 %4, i32 0) nounwind
  %6 = and i64 %5, 4294967295
  ret i64 %6
}

define void @__Socket_make_fd(%procStructType* nocapture, %Socket* nocapture, i64) nounwind {
make_fdStartB:
  %gep = getelementptr %Socket* %1, i64 0, i32 0
  store i64 %2, i64* %gep, align 8
  ret void
}

define void @__Socket_make_server(%procStructType* nocapture, %Socket* nocapture, i64) nounwind {
merge:
  %local_addr.i.i = alloca %struct.sockaddr_in, align 4
  %enable.i.i = alloca i32, align 4
  %gep = getelementptr %Socket* %1, i64 0, i32 0
  %3 = bitcast i32* %enable.i.i to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %3) nounwind
  %4 = call i32 @socket(i32 2, i32 1, i32 0) nounwind
  store i32 1, i32* %enable.i.i, align 4
  %5 = call i32 @setsockopt(i32 %4, i32 1, i32 2, i8* %3, i32 4) nounwind
  call void @llvm.lifetime.end(i64 -1, i8* %3) nounwind
  %6 = zext i32 %4 to i64
  store i64 %6, i64* %gep, align 8
  %7 = bitcast %struct.sockaddr_in* %local_addr.i.i to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %7) nounwind
  %8 = getelementptr inbounds %struct.sockaddr_in* %local_addr.i.i, i64 0, i32 0
  store i16 2, i16* %8, align 4
  %9 = getelementptr inbounds %struct.sockaddr_in* %local_addr.i.i, i64 0, i32 2, i32 0
  store i32 0, i32* %9, align 4
  %10 = trunc i64 %2 to i16
  %11 = call zeroext i16 @htons(i16 zeroext %10) nounwind readnone
  %12 = getelementptr inbounds %struct.sockaddr_in* %local_addr.i.i, i64 0, i32 1
  store i16 %11, i16* %12, align 2
  %13 = bitcast %struct.sockaddr_in* %local_addr.i.i to %struct.sockaddr*
  %14 = call i32 @bind(i32 %4, %struct.sockaddr* %13, i32 16) nounwind
  call void @llvm.lifetime.end(i64 -1, i8* %7) nounwind
  %"equality check4" = icmp eq i32 %14, 0
  br i1 %"equality check4", label %merge7, label %then5

then5:                                            ; preds = %merge
  %15 = call i64 @write(i32 1, i8* getelementptr inbounds ([23 x i8]* @"Could not bind socket\0A_global", i64 0, i64 0), i64 22) nounwind
  call void @__Prelude_exit_with(i64 1)
  unreachable

merge7:                                           ; preds = %merge
  ret void
}

define void @__Socket_listen(%procStructType* nocapture, %Socket* nocapture, i64) nounwind {
listenStartB:
  %gep = getelementptr %Socket* %1, i64 0, i32 0
  %3 = load i64* %gep, align 8
  %4 = trunc i64 %3 to i32
  %5 = trunc i64 %2 to i32
  %6 = tail call i32 @listen(i32 %4, i32 %5) nounwind
  %"equality check" = icmp eq i32 %6, 0
  br i1 %"equality check", label %merge, label %then

then:                                             ; preds = %listenStartB
  tail call void @__Prelude_exit_with(i64 1)
  unreachable

merge:                                            ; preds = %listenStartB
  ret void
}

define noalias %Socket* @__Socket_accept(%procStructType* nocapture, %Socket* nocapture) nounwind {
merge:
  %gep = getelementptr %Socket* %1, i64 0, i32 0
  %2 = load i64* %gep, align 8
  %3 = trunc i64 %2 to i32
  %4 = tail call i32 @accept(i32 %3, %struct.sockaddr* null, i32* null) nounwind
  %5 = zext i32 %4 to i64
  %6 = tail call noalias i8* @malloc(i64 8) nounwind
  %"casting char ptr to typed ptr1" = bitcast i8* %6 to %Socket*
  %gep.i8 = bitcast i8* %6 to i64*
  store i64 %5, i64* %gep.i8, align 8
  ret %Socket* %"casting char ptr to typed ptr1"
}

define noalias %struct.string* @__Socket_recv(%procStructType* nocapture, %Socket* nocapture) nounwind {
recvStartB:
  %2 = tail call noalias i8* @malloc(i64 16) nounwind
  %gep.i = bitcast i8* %2 to i64*
  %gep1.i = getelementptr i8* %2, i64 8
  %3 = bitcast i8* %gep1.i to i8**
  %4 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %4, i8** %3, align 8
  %5 = getelementptr inbounds i8* %4, i64 1024
  store i8 0, i8* %5, align 1
  %gep = getelementptr %Socket* %1, i64 0, i32 0
  %6 = load i64* %gep, align 8
  %7 = trunc i64 %6 to i32
  %8 = tail call i64 @recv(i32 %7, i8* %4, i64 1024, i32 0) nounwind
  store i64 %8, i64* %gep.i, align 8
  switch i64 %8, label %else [
    i64 -1, label %merge
    i64 0, label %merge
  ]

else:                                             ; preds = %recvStartB
  %"casting char ptr to typed ptr" = bitcast i8* %2 to %struct.string*
  br label %merge

merge:                                            ; preds = %else, %recvStartB, %recvStartB
  %Result.0 = phi %struct.string* [ %"casting char ptr to typed ptr", %else ], [ null, %recvStartB ], [ null, %recvStartB ]
  ret %struct.string* %Result.0
}

define i64 @__Socket_send(%procStructType* nocapture, %Socket* nocapture, %struct.string* nocapture) nounwind {
sendStartB:
  %gep = getelementptr %Socket* %1, i64 0, i32 0
  %3 = load i64* %gep, align 8
  %gep1 = getelementptr %struct.string* %2, i64 0, i32 1
  %4 = load i8** %gep1, align 8
  %gep2 = getelementptr %struct.string* %2, i64 0, i32 0
  %5 = load i64* %gep2, align 8
  %6 = trunc i64 %3 to i32
  %sext.i = shl i64 %5, 32
  %7 = ashr exact i64 %sext.i, 32
  %8 = tail call i64 @send(i32 %6, i8* %4, i64 %7, i32 0) nounwind
  %9 = and i64 %8, 4294967295
  ret i64 %9
}

define void @__Socket_send_all(%procStructType* nocapture, %Socket* nocapture, %struct.string* nocapture) nounwind {
send_allStartB:
  %gep5 = getelementptr %struct.string* %2, i64 0, i32 0
  %3 = load i64* %gep5, align 8
  %"genBinOp generated operation6" = icmp slt i64 %3, 1
  br i1 %"genBinOp generated operation6", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %send_allStartB
  %gep.i = getelementptr %Socket* %1, i64 0, i32 0
  br label %"loop body"

"loop body":                                      ; preds = %"loop condition.backedge", %"loop body.lr.ph"
  %4 = phi i64 [ %3, %"loop body.lr.ph" ], [ %"genBinOp generated operation.i", %"loop condition.backedge" ]
  %gep8 = phi i64* [ %gep5, %"loop body.lr.ph" ], [ %gep.i.i, %"loop condition.backedge" ]
  %str.07 = phi %struct.string* [ %2, %"loop body.lr.ph" ], [ %"casting char ptr to typed ptr.i", %"loop condition.backedge" ]
  %5 = load i64* %gep.i, align 8
  %gep1.i = getelementptr %struct.string* %str.07, i64 0, i32 1
  %6 = load i8** %gep1.i, align 8
  %7 = trunc i64 %5 to i32
  %sext.i.i = shl i64 %4, 32
  %8 = ashr exact i64 %sext.i.i, 32
  %9 = tail call i64 @send(i32 %7, i8* %6, i64 %8, i32 0) nounwind
  %10 = and i64 %9, 4294967295
  %"genBinOp generated operation1" = add i64 %10, 1
  %11 = load i64* %gep8, align 8
  %"genBinOp generated operation3" = add i64 %11, 1
  %12 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i" = bitcast i8* %12 to %struct.string*
  %"genBinOp generated operation.i" = sub i64 %"genBinOp generated operation3", %"genBinOp generated operation1"
  %gep.i.i = bitcast i8* %12 to i64*
  store i64 %"genBinOp generated operation.i", i64* %gep.i.i, align 8
  %gep1.i.i = getelementptr i8* %12, i64 8
  %13 = bitcast i8* %gep1.i.i to i8**
  %"genBinOp generated operation.i.i" = add i64 %"genBinOp generated operation.i", 1
  %14 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i") nounwind
  store i8* %14, i8** %13, align 8
  %15 = getelementptr inbounds i8* %14, i64 %"genBinOp generated operation.i"
  store i8 0, i8* %15, align 1
  %"genBinOp generated operation113.i" = icmp slt i64 %"genBinOp generated operation1", %"genBinOp generated operation3"
  br i1 %"genBinOp generated operation113.i", label %"loop body.lr.ph.i", label %"loop condition.backedge"

"loop condition.backedge":                        ; preds = %"loop body.i", %"loop body"
  %"genBinOp generated operation" = icmp slt i64 %"genBinOp generated operation.i", 1
  br i1 %"genBinOp generated operation", label %"after loop", label %"loop body"

"loop body.lr.ph.i":                              ; preds = %"loop body"
  %.pre.i = load i8** %gep1.i, align 8
  br label %"loop body.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.014.i = phi i64 [ %"genBinOp generated operation1", %"loop body.lr.ph.i" ], [ %"genBinOp generated operation3.i", %"loop body.i" ]
  %"genBinOp generated operation2.i" = sub i64 %i.014.i, %"genBinOp generated operation1"
  %"genBinOp generated operation.i12.i" = add i64 %i.014.i, -1
  %16 = getelementptr inbounds i8* %.pre.i, i64 %"genBinOp generated operation.i12.i"
  %17 = load i8* %16, align 1
  %18 = getelementptr inbounds i8* %14, i64 %"genBinOp generated operation2.i"
  store i8 %17, i8* %18, align 1
  %"genBinOp generated operation3.i" = add i64 %i.014.i, 1
  %exitcond.i = icmp eq i64 %"genBinOp generated operation3.i", %"genBinOp generated operation3"
  br i1 %exitcond.i, label %"loop condition.backedge", label %"loop body.i"

"after loop":                                     ; preds = %"loop condition.backedge", %send_allStartB
  ret void
}

define void @__Socket_close(%procStructType* nocapture, %Socket* nocapture) nounwind {
closeStartB:
  %gep = getelementptr %Socket* %1, i64 0, i32 0
  %2 = load i64* %gep, align 8
  %3 = trunc i64 %2 to i32
  %4 = tail call i32 @close(i32 %3) nounwind
  ret void
}

define void @__Socket_Buffer_make(%procStructType* nocapture, %Socket_Buffer* nocapture, %Socket*) nounwind {
makeStartB:
  %gep = getelementptr %Socket_Buffer* %1, i64 0, i32 0
  store %Socket* %2, %Socket** %gep, align 8
  %gep1 = getelementptr %Socket_Buffer* %1, i64 0, i32 1
  %3 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %3 to %struct.string*
  %gep.i = bitcast i8* %3 to i64*
  store i64 0, i64* %gep.i, align 8
  %gep1.i = getelementptr i8* %3, i64 8
  %4 = bitcast i8* %gep1.i to i8**
  store i8* getelementptr inbounds ([1 x i8]* @_global, i64 0, i64 0), i8** %4, align 8
  store %struct.string* %"casting char ptr to typed ptr", %struct.string** %gep1, align 8
  ret void
}

define %struct.string* @__Socket_Buffer_read_line(%procStructType* nocapture, %Socket_Buffer* nocapture) nounwind {
read_lineStartB:
  %gep = getelementptr %Socket_Buffer* %1, i64 0, i32 1
  %2 = load %struct.string** %gep, align 8
  %"equality check.i" = icmp eq %struct.string* %2, null
  br i1 %"equality check.i", label %then, label %else.i

else.i:                                           ; preds = %read_lineStartB
  %gep.i.i = getelementptr %struct.string* %2, i64 0, i32 0
  %3 = load i64* %gep.i.i, align 8
  %"genBinOp generated operation11.i.i" = icmp slt i64 %3, 1
  br i1 %"genBinOp generated operation11.i.i", label %__String_find.exit33.i, label %"loop body.lr.ph.i.i"

"loop body.lr.ph.i.i":                            ; preds = %else.i
  %gep.i.i.i = getelementptr %struct.string* %2, i64 0, i32 1
  %4 = load i8** %gep.i.i.i, align 8
  br label %"loop body.i.i"

"loop body.i.i":                                  ; preds = %"loop body.i.i", %"loop body.lr.ph.i.i"
  %i.012.i.i = phi i64 [ 1, %"loop body.lr.ph.i.i" ], [ %"genBinOp generated operation2.i.i", %"loop body.i.i" ]
  %"genBinOp generated operation.i.i.i" = add i64 %i.012.i.i, -1
  %5 = getelementptr inbounds i8* %4, i64 %"genBinOp generated operation.i.i.i"
  %6 = load i8* %5, align 1
  %"equality check.i.i" = icmp eq i8 %6, 13
  %"genBinOp generated operation2.i.i" = add i64 %i.012.i.i, 1
  %"genBinOp generated operation.i.i" = icmp sgt i64 %"genBinOp generated operation2.i.i", %3
  %"genBinOp generated operation1.i.i" = or i1 %"genBinOp generated operation.i.i", %"equality check.i.i"
  br i1 %"genBinOp generated operation1.i.i", label %"loop body.i29.i", label %"loop body.i.i"

"loop body.i29.i":                                ; preds = %"loop body.i29.i", %"loop body.i.i"
  %i.012.i23.i = phi i64 [ %"genBinOp generated operation2.i26.i", %"loop body.i29.i" ], [ 1, %"loop body.i.i" ]
  %"genBinOp generated operation.i.i24.i" = add i64 %i.012.i23.i, -1
  %7 = getelementptr inbounds i8* %4, i64 %"genBinOp generated operation.i.i24.i"
  %8 = load i8* %7, align 1
  %"equality check.i25.i" = icmp eq i8 %8, 10
  %"genBinOp generated operation2.i26.i" = add i64 %i.012.i23.i, 1
  %"genBinOp generated operation.i27.i" = icmp sgt i64 %"genBinOp generated operation2.i26.i", %3
  %"genBinOp generated operation1.i28.i" = or i1 %"genBinOp generated operation.i27.i", %"equality check.i25.i"
  br i1 %"genBinOp generated operation1.i28.i", label %"loop condition.after loop_crit_edge.i31.i", label %"loop body.i29.i"

"loop condition.after loop_crit_edge.i31.i":      ; preds = %"loop body.i29.i"
  %phitmp.i.i = select i1 %"equality check.i.i", i64 %i.012.i.i, i64 -1
  %phitmp.i30.i = select i1 %"equality check.i25.i", i64 %i.012.i23.i, i64 -1
  br label %__String_find.exit33.i

__String_find.exit33.i:                           ; preds = %"loop condition.after loop_crit_edge.i31.i", %else.i
  %found.0.lcssa.i35.i = phi i64 [ %phitmp.i.i, %"loop condition.after loop_crit_edge.i31.i" ], [ -1, %else.i ]
  %found.0.lcssa.i32.i = phi i64 [ %phitmp.i30.i, %"loop condition.after loop_crit_edge.i31.i" ], [ -1, %else.i ]
  %"genBinOp generated operation.i" = add i64 %found.0.lcssa.i35.i, 1
  %"equality check1.i" = icmp eq i64 %found.0.lcssa.i32.i, %"genBinOp generated operation.i"
  br i1 %"equality check1.i", label %then2.i, label %elseIf25.i

then2.i:                                          ; preds = %__String_find.exit33.i
  %"genBinOp generated operation33.i" = add i64 %found.0.lcssa.i32.i, 1
  br label %__Socket_Buffer_find_cutoff.exit

elseIf9.i:                                        ; preds = %elseIf13.i
  %"genBinOp generated operation11.i" = icmp slt i64 %found.0.lcssa.i32.i, %found.0.lcssa.i35.i
  %"genBinOp generated operation12.i" = add i64 %found.0.lcssa.i32.i, 1
  %"genBinOp generated operation12..i" = select i1 %"genBinOp generated operation11.i", i64 %"genBinOp generated operation12.i", i64 1
  br label %__Socket_Buffer_find_cutoff.exit

elseIf13.i:                                       ; preds = %elseIf17.i
  %"genBinOp generated operation15.i" = icmp slt i64 %found.0.lcssa.i35.i, %found.0.lcssa.i32.i
  br i1 %"genBinOp generated operation15.i", label %__Socket_Buffer_find_cutoff.exit, label %elseIf9.i

elseIf17.i:                                       ; preds = %elseIf25.i
  %"equality check20.i" = icmp eq i64 %found.0.lcssa.i35.i, -1
  %"equality check22.i" = icmp ne i64 %found.0.lcssa.i32.i, -1
  %"genBinOp generated operation23.i" = and i1 %"equality check20.i", %"equality check22.i"
  br i1 %"genBinOp generated operation23.i", label %elseIfThen18.i, label %elseIf13.i

elseIfThen18.i:                                   ; preds = %elseIf17.i
  %"genBinOp generated operation24.i" = add i64 %found.0.lcssa.i32.i, 1
  br label %__Socket_Buffer_find_cutoff.exit

elseIf25.i:                                       ; preds = %__String_find.exit33.i
  %"equality check28.i" = icmp eq i64 %found.0.lcssa.i32.i, -1
  %"equality check30.i" = icmp ne i64 %found.0.lcssa.i35.i, -1
  %"genBinOp generated operation31.i" = and i1 %"equality check28.i", %"equality check30.i"
  br i1 %"genBinOp generated operation31.i", label %__Socket_Buffer_find_cutoff.exit, label %elseIf17.i

__Socket_Buffer_find_cutoff.exit:                 ; preds = %elseIf25.i, %elseIfThen18.i, %elseIf13.i, %elseIf9.i, %then2.i
  %9 = phi i64 [ %"genBinOp generated operation12..i", %elseIf9.i ], [ %"genBinOp generated operation33.i", %then2.i ], [ %"genBinOp generated operation24.i", %elseIfThen18.i ], [ %"genBinOp generated operation.i", %elseIf13.i ], [ %"genBinOp generated operation.i", %elseIf25.i ]
  %"equality check" = icmp eq i64 %9, 1
  br i1 %"equality check", label %then, label %else

then:                                             ; preds = %__Socket_Buffer_find_cutoff.exit, %read_lineStartB
  %gep1 = getelementptr %Socket_Buffer* %1, i64 0, i32 0
  %10 = load %Socket** %gep1, align 8
  %11 = tail call noalias i8* @malloc(i64 16) nounwind
  %gep.i.i40 = bitcast i8* %11 to i64*
  %gep1.i.i41 = getelementptr i8* %11, i64 8
  %12 = bitcast i8* %gep1.i.i41 to i8**
  %13 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %13, i8** %12, align 8
  %14 = getelementptr inbounds i8* %13, i64 1024
  store i8 0, i8* %14, align 1
  %gep.i = getelementptr %Socket* %10, i64 0, i32 0
  %15 = load i64* %gep.i, align 8
  %16 = trunc i64 %15 to i32
  %17 = tail call i64 @recv(i32 %16, i8* %13, i64 1024, i32 0) nounwind
  store i64 %17, i64* %gep.i.i40, align 8
  switch i64 %17, label %"loop condition.preheader" [
    i64 -1, label %then23
    i64 0, label %then23
  ]

"loop condition.preheader":                       ; preds = %then
  %"equality check7203" = icmp eq i8* %11, null
  br i1 %"equality check7203", label %then23, label %"loop body.lr.ph"

else:                                             ; preds = %__Socket_Buffer_find_cutoff.exit
  %18 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i46" = bitcast i8* %18 to %struct.string*
  %"genBinOp generated operation.i47" = add i64 %9, -1
  %gep.i.i48 = bitcast i8* %18 to i64*
  store i64 %"genBinOp generated operation.i47", i64* %gep.i.i48, align 8
  %gep1.i.i49 = getelementptr i8* %18, i64 8
  %19 = bitcast i8* %gep1.i.i49 to i8**
  %20 = tail call noalias i8* @malloc(i64 %9) nounwind
  store i8* %20, i8** %19, align 8
  %21 = getelementptr inbounds i8* %20, i64 %"genBinOp generated operation.i47"
  store i8 0, i8* %21, align 1
  %"genBinOp generated operation113.i50" = icmp sgt i64 %9, 1
  br i1 %"genBinOp generated operation113.i50", label %"loop body.lr.ph.i53", label %__String_substring.exit60

"loop body.lr.ph.i53":                            ; preds = %else
  %gep.i11.i51 = getelementptr %struct.string* %2, i64 0, i32 1
  %.pre.i52 = load i8** %gep.i11.i51, align 8
  br label %"loop body.i59"

"loop body.i59":                                  ; preds = %"loop body.i59", %"loop body.lr.ph.i53"
  %i.014.i54 = phi i64 [ 1, %"loop body.lr.ph.i53" ], [ %"genBinOp generated operation3.i57", %"loop body.i59" ]
  %"genBinOp generated operation2.i55" = add i64 %i.014.i54, -1
  %22 = getelementptr inbounds i8* %.pre.i52, i64 %"genBinOp generated operation2.i55"
  %23 = load i8* %22, align 1
  %24 = getelementptr inbounds i8* %20, i64 %"genBinOp generated operation2.i55"
  store i8 %23, i8* %24, align 1
  %"genBinOp generated operation3.i57" = add i64 %i.014.i54, 1
  %exitcond.i58 = icmp eq i64 %"genBinOp generated operation3.i57", %9
  br i1 %exitcond.i58, label %__String_substring.exit60, label %"loop body.i59"

__String_substring.exit60:                        ; preds = %"loop body.i59", %else
  %25 = load i64* %gep.i.i, align 8
  %"genBinOp generated operation34" = add i64 %25, 1
  %26 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i61" = bitcast i8* %26 to %struct.string*
  %"genBinOp generated operation.i62" = sub i64 %"genBinOp generated operation34", %9
  %gep.i.i63 = bitcast i8* %26 to i64*
  store i64 %"genBinOp generated operation.i62", i64* %gep.i.i63, align 8
  %gep1.i.i64 = getelementptr i8* %26, i64 8
  %27 = bitcast i8* %gep1.i.i64 to i8**
  %"genBinOp generated operation.i.i65" = add i64 %"genBinOp generated operation.i62", 1
  %28 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i65") nounwind
  store i8* %28, i8** %27, align 8
  %29 = getelementptr inbounds i8* %28, i64 %"genBinOp generated operation.i62"
  store i8 0, i8* %29, align 1
  %"genBinOp generated operation113.i66" = icmp slt i64 %9, %"genBinOp generated operation34"
  br i1 %"genBinOp generated operation113.i66", label %"loop body.lr.ph.i69", label %__String_substring.exit76

"loop body.lr.ph.i69":                            ; preds = %__String_substring.exit60
  %gep.i11.i67 = getelementptr %struct.string* %2, i64 0, i32 1
  %.pre.i68 = load i8** %gep.i11.i67, align 8
  br label %"loop body.i75"

"loop body.i75":                                  ; preds = %"loop body.i75", %"loop body.lr.ph.i69"
  %i.014.i70 = phi i64 [ %9, %"loop body.lr.ph.i69" ], [ %"genBinOp generated operation3.i73", %"loop body.i75" ]
  %"genBinOp generated operation2.i71" = sub i64 %i.014.i70, %9
  %"genBinOp generated operation.i12.i72" = add i64 %i.014.i70, -1
  %30 = getelementptr inbounds i8* %.pre.i68, i64 %"genBinOp generated operation.i12.i72"
  %31 = load i8* %30, align 1
  %32 = getelementptr inbounds i8* %28, i64 %"genBinOp generated operation2.i71"
  store i8 %31, i8* %32, align 1
  %"genBinOp generated operation3.i73" = add i64 %i.014.i70, 1
  %exitcond.i74 = icmp eq i64 %"genBinOp generated operation3.i73", %"genBinOp generated operation34"
  br i1 %exitcond.i74, label %__String_substring.exit76, label %"loop body.i75"

__String_substring.exit76:                        ; preds = %"loop body.i75", %__String_substring.exit60
  store %struct.string* %"casting char ptr to typed ptr.i61", %struct.string** %gep, align 8
  br label %merge

merge:                                            ; preds = %then23, %__String_substring.exit, %"after loop", %__String_substring.exit76
  %Result.0 = phi %struct.string* [ %86, %then23 ], [ %Result.1198, %"after loop" ], [ %"casting char ptr to typed ptr.i46", %__String_substring.exit76 ], [ %"casting char ptr to typed ptr.i159", %__String_substring.exit ]
  ret %struct.string* %Result.0

else.i80:                                         ; preds = %__String_substring.exit, %"loop body.lr.ph"
  %Result.1198 = phi %struct.string* [ null, %"loop body.lr.ph" ], [ %"casting char ptr to typed ptr.i159", %__String_substring.exit ]
  %33 = load i64* %gep.i.i78, align 8
  %"genBinOp generated operation11.i.i79" = icmp slt i64 %33, 1
  br i1 %"genBinOp generated operation11.i.i79", label %__String_find.exit33.i105, label %"loop body.lr.ph.i.i82"

"loop body.lr.ph.i.i82":                          ; preds = %else.i80
  %34 = load i8** %61, align 8
  br label %"loop body.i.i89"

"loop body.i.i89":                                ; preds = %"loop body.i.i89", %"loop body.lr.ph.i.i82"
  %i.012.i.i83 = phi i64 [ 1, %"loop body.lr.ph.i.i82" ], [ %"genBinOp generated operation2.i.i86", %"loop body.i.i89" ]
  %"genBinOp generated operation.i.i.i84" = add i64 %i.012.i.i83, -1
  %35 = getelementptr inbounds i8* %34, i64 %"genBinOp generated operation.i.i.i84"
  %36 = load i8* %35, align 1
  %"equality check.i.i85" = icmp eq i8 %36, 13
  %"genBinOp generated operation2.i.i86" = add i64 %i.012.i.i83, 1
  %"genBinOp generated operation.i.i87" = icmp sgt i64 %"genBinOp generated operation2.i.i86", %33
  %"genBinOp generated operation1.i.i88" = or i1 %"genBinOp generated operation.i.i87", %"equality check.i.i85"
  br i1 %"genBinOp generated operation1.i.i88", label %"loop body.i29.i98", label %"loop body.i.i89"

"loop body.i29.i98":                              ; preds = %"loop body.i29.i98", %"loop body.i.i89"
  %i.012.i23.i92 = phi i64 [ %"genBinOp generated operation2.i26.i95", %"loop body.i29.i98" ], [ 1, %"loop body.i.i89" ]
  %"genBinOp generated operation.i.i24.i93" = add i64 %i.012.i23.i92, -1
  %37 = getelementptr inbounds i8* %34, i64 %"genBinOp generated operation.i.i24.i93"
  %38 = load i8* %37, align 1
  %"equality check.i25.i94" = icmp eq i8 %38, 10
  %"genBinOp generated operation2.i26.i95" = add i64 %i.012.i23.i92, 1
  %"genBinOp generated operation.i27.i96" = icmp sgt i64 %"genBinOp generated operation2.i26.i95", %33
  %"genBinOp generated operation1.i28.i97" = or i1 %"genBinOp generated operation.i27.i96", %"equality check.i25.i94"
  br i1 %"genBinOp generated operation1.i28.i97", label %"loop condition.after loop_crit_edge.i31.i100", label %"loop body.i29.i98"

"loop condition.after loop_crit_edge.i31.i100":   ; preds = %"loop body.i29.i98"
  %phitmp.i.i90 = select i1 %"equality check.i.i85", i64 %i.012.i.i83, i64 -1
  %phitmp.i30.i99 = select i1 %"equality check.i25.i94", i64 %i.012.i23.i92, i64 -1
  br label %__String_find.exit33.i105

__String_find.exit33.i105:                        ; preds = %"loop condition.after loop_crit_edge.i31.i100", %else.i80
  %found.0.lcssa.i35.i101 = phi i64 [ %phitmp.i.i90, %"loop condition.after loop_crit_edge.i31.i100" ], [ -1, %else.i80 ]
  %found.0.lcssa.i32.i102 = phi i64 [ %phitmp.i30.i99, %"loop condition.after loop_crit_edge.i31.i100" ], [ -1, %else.i80 ]
  %"genBinOp generated operation.i103" = add i64 %found.0.lcssa.i35.i101, 1
  %"equality check1.i104" = icmp eq i64 %found.0.lcssa.i32.i102, %"genBinOp generated operation.i103"
  br i1 %"equality check1.i104", label %then2.i109, label %elseIf25.i125

then2.i109:                                       ; preds = %__String_find.exit33.i105
  %"genBinOp generated operation33.i108" = add i64 %found.0.lcssa.i32.i102, 1
  br label %__Socket_Buffer_find_cutoff.exit126

elseIf9.i113:                                     ; preds = %elseIf13.i115
  %"genBinOp generated operation11.i110" = icmp slt i64 %found.0.lcssa.i32.i102, %found.0.lcssa.i35.i101
  %"genBinOp generated operation12.i111" = add i64 %found.0.lcssa.i32.i102, 1
  %"genBinOp generated operation12..i112" = select i1 %"genBinOp generated operation11.i110", i64 %"genBinOp generated operation12.i111", i64 1
  br label %__Socket_Buffer_find_cutoff.exit126

elseIf13.i115:                                    ; preds = %elseIf17.i119
  %"genBinOp generated operation15.i114" = icmp slt i64 %found.0.lcssa.i35.i101, %found.0.lcssa.i32.i102
  br i1 %"genBinOp generated operation15.i114", label %__Socket_Buffer_find_cutoff.exit126, label %elseIf9.i113

elseIf17.i119:                                    ; preds = %elseIf25.i125
  %"equality check20.i116" = icmp eq i64 %found.0.lcssa.i35.i101, -1
  %"equality check22.i117" = icmp ne i64 %found.0.lcssa.i32.i102, -1
  %"genBinOp generated operation23.i118" = and i1 %"equality check20.i116", %"equality check22.i117"
  br i1 %"genBinOp generated operation23.i118", label %elseIfThen18.i121, label %elseIf13.i115

elseIfThen18.i121:                                ; preds = %elseIf17.i119
  %"genBinOp generated operation24.i120" = add i64 %found.0.lcssa.i32.i102, 1
  br label %__Socket_Buffer_find_cutoff.exit126

elseIf25.i125:                                    ; preds = %__String_find.exit33.i105
  %"equality check28.i122" = icmp eq i64 %found.0.lcssa.i32.i102, -1
  %"equality check30.i123" = icmp ne i64 %found.0.lcssa.i35.i101, -1
  %"genBinOp generated operation31.i124" = and i1 %"equality check28.i122", %"equality check30.i123"
  br i1 %"genBinOp generated operation31.i124", label %__Socket_Buffer_find_cutoff.exit126, label %elseIf17.i119

__Socket_Buffer_find_cutoff.exit126:              ; preds = %elseIf25.i125, %elseIfThen18.i121, %elseIf13.i115, %elseIf9.i113, %then2.i109
  %39 = phi i64 [ %"genBinOp generated operation12..i112", %elseIf9.i113 ], [ %"genBinOp generated operation33.i108", %then2.i109 ], [ %"genBinOp generated operation24.i120", %elseIfThen18.i121 ], [ %"genBinOp generated operation.i103", %elseIf13.i115 ], [ %"genBinOp generated operation.i103", %elseIf25.i125 ]
  %"equality check8" = icmp eq i64 %39, 1
  %40 = load %struct.string** %gep, align 8
  br i1 %"equality check8", label %then9, label %else10

"after loop":                                     ; preds = %"loop condition.outer.backedge"
  br i1 %"equality check7", label %then23, label %merge

then9:                                            ; preds = %__Socket_Buffer_find_cutoff.exit126
  %41 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i127" = bitcast i8* %41 to %struct.string*
  %gep.i128 = getelementptr %struct.string* %40, i64 0, i32 0
  %42 = load i64* %gep.i128, align 8
  %43 = load i64* %gep.i.i78, align 8
  %"genBinOp generated operation.i129" = add i64 %43, %42
  %gep.i.i130 = bitcast i8* %41 to i64*
  store i64 %"genBinOp generated operation.i129", i64* %gep.i.i130, align 8
  %gep1.i.i131 = getelementptr i8* %41, i64 8
  %44 = bitcast i8* %gep1.i.i131 to i8**
  %"genBinOp generated operation.i.i132" = add i64 %"genBinOp generated operation.i129", 1
  %45 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i132") nounwind
  store i8* %45, i8** %44, align 8
  %46 = getelementptr inbounds i8* %45, i64 %"genBinOp generated operation.i129"
  store i8 0, i8* %46, align 1
  %"genBinOp generated operation430.i" = icmp slt i64 %42, 1
  br i1 %"genBinOp generated operation430.i", label %"loop condition7.preheader.i", label %"loop body.lr.ph.i133"

"loop body.lr.ph.i133":                           ; preds = %then9
  %gep.i23.i = getelementptr %struct.string* %40, i64 0, i32 1
  %.pre32.i = load i8** %gep.i23.i, align 8
  br label %"loop body.i135"

"loop condition7.preheader.i":                    ; preds = %"loop body.i135", %then9
  %i.0.lcssa.i = phi i64 [ 1, %then9 ], [ %"genBinOp generated operation6.i", %"loop body.i135" ]
  %"genBinOp generated operation1328.i" = icmp sgt i64 %i.0.lcssa.i, %"genBinOp generated operation.i129"
  br i1 %"genBinOp generated operation1328.i", label %__String_append.exit, label %"loop body8.lr.ph.i"

"loop body8.lr.ph.i":                             ; preds = %"loop condition7.preheader.i"
  %.pre.i134 = load i8** %61, align 8
  br label %"loop body8.i"

"loop body.i135":                                 ; preds = %"loop body.i135", %"loop body.lr.ph.i133"
  %i.031.i = phi i64 [ 1, %"loop body.lr.ph.i133" ], [ %"genBinOp generated operation6.i", %"loop body.i135" ]
  %"genBinOp generated operation5.i" = add i64 %i.031.i, -1
  %47 = getelementptr inbounds i8* %.pre32.i, i64 %"genBinOp generated operation5.i"
  %48 = load i8* %47, align 1
  %49 = getelementptr inbounds i8* %45, i64 %"genBinOp generated operation5.i"
  store i8 %48, i8* %49, align 1
  %"genBinOp generated operation6.i" = add i64 %i.031.i, 1
  %"genBinOp generated operation4.i" = icmp sgt i64 %"genBinOp generated operation6.i", %42
  br i1 %"genBinOp generated operation4.i", label %"loop condition7.preheader.i", label %"loop body.i135"

"loop body8.i":                                   ; preds = %"loop body8.i", %"loop body8.lr.ph.i"
  %i.129.i = phi i64 [ %i.0.lcssa.i, %"loop body8.lr.ph.i" ], [ %"genBinOp generated operation17.i", %"loop body8.i" ]
  %"genBinOp generated operation14.i" = add i64 %i.129.i, -1
  %"genBinOp generated operation.i26.i" = sub i64 %"genBinOp generated operation14.i", %42
  %50 = getelementptr inbounds i8* %.pre.i134, i64 %"genBinOp generated operation.i26.i"
  %51 = load i8* %50, align 1
  %52 = getelementptr inbounds i8* %45, i64 %"genBinOp generated operation14.i"
  store i8 %51, i8* %52, align 1
  %"genBinOp generated operation17.i" = add i64 %i.129.i, 1
  %"genBinOp generated operation13.i" = icmp sgt i64 %"genBinOp generated operation17.i", %"genBinOp generated operation.i129"
  br i1 %"genBinOp generated operation13.i", label %__String_append.exit, label %"loop body8.i"

__String_append.exit:                             ; preds = %"loop body8.i", %"loop condition7.preheader.i"
  store %struct.string* %"casting char ptr to typed ptr.i127", %struct.string** %gep, align 8
  %53 = load %Socket** %gep1, align 8
  %54 = tail call noalias i8* @malloc(i64 16) nounwind
  %gep.i.i136 = bitcast i8* %54 to i64*
  %gep1.i.i137 = getelementptr i8* %54, i64 8
  %55 = bitcast i8* %gep1.i.i137 to i8**
  %56 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %56, i8** %55, align 8
  %57 = getelementptr inbounds i8* %56, i64 1024
  store i8 0, i8* %57, align 1
  %gep.i138 = getelementptr %Socket* %53, i64 0, i32 0
  %58 = load i64* %gep.i138, align 8
  %59 = trunc i64 %58 to i32
  %60 = tail call i64 @recv(i32 %59, i8* %56, i64 1024, i32 0) nounwind
  store i64 %60, i64* %gep.i.i136, align 8
  switch i64 %60, label %"loop condition.outer.backedge" [
    i64 -1, label %then23
    i64 0, label %then23
  ]

"loop condition.outer.backedge":                  ; preds = %__String_append.exit
  %"equality check3196" = icmp ne %struct.string* %Result.1198, null
  %"equality check7" = icmp eq i8* %54, null
  %"genBinOp generated operation197" = or i1 %"equality check3196", %"equality check7"
  br i1 %"genBinOp generated operation197", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %"loop condition.outer.backedge", %"loop condition.preheader"
  %new_str.0.ph190204.in = phi i8* [ %54, %"loop condition.outer.backedge" ], [ %11, %"loop condition.preheader" ]
  %gep.i.i78 = bitcast i8* %new_str.0.ph190204.in to i64*
  %gep.i11.i149 = getelementptr i8* %new_str.0.ph190204.in, i64 8
  %61 = bitcast i8* %gep.i11.i149 to i8**
  br label %else.i80

else10:                                           ; preds = %__Socket_Buffer_find_cutoff.exit126
  %"genBinOp generated operation.i145" = add i64 %39, -1
  %62 = tail call noalias i8* @malloc(i64 %39) nounwind
  %63 = getelementptr inbounds i8* %62, i64 %"genBinOp generated operation.i145"
  store i8 0, i8* %63, align 1
  %"genBinOp generated operation113.i148" = icmp sgt i64 %39, 1
  br i1 %"genBinOp generated operation113.i148", label %"loop body.lr.ph.i151", label %__String_substring.exit158

"loop body.lr.ph.i151":                           ; preds = %else10
  %.pre.i150 = load i8** %61, align 8
  br label %"loop body.i157"

"loop body.i157":                                 ; preds = %"loop body.i157", %"loop body.lr.ph.i151"
  %i.014.i152 = phi i64 [ 1, %"loop body.lr.ph.i151" ], [ %"genBinOp generated operation3.i155", %"loop body.i157" ]
  %"genBinOp generated operation2.i153" = add i64 %i.014.i152, -1
  %64 = getelementptr inbounds i8* %.pre.i150, i64 %"genBinOp generated operation2.i153"
  %65 = load i8* %64, align 1
  %66 = getelementptr inbounds i8* %62, i64 %"genBinOp generated operation2.i153"
  store i8 %65, i8* %66, align 1
  %"genBinOp generated operation3.i155" = add i64 %i.014.i152, 1
  %exitcond.i156 = icmp eq i64 %"genBinOp generated operation3.i155", %39
  br i1 %exitcond.i156, label %__String_substring.exit158, label %"loop body.i157"

__String_substring.exit158:                       ; preds = %"loop body.i157", %else10
  %67 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i159" = bitcast i8* %67 to %struct.string*
  %gep.i160 = getelementptr %struct.string* %40, i64 0, i32 0
  %68 = load i64* %gep.i160, align 8
  %"genBinOp generated operation.i162" = add i64 %"genBinOp generated operation.i145", %68
  %gep.i.i163 = bitcast i8* %67 to i64*
  store i64 %"genBinOp generated operation.i162", i64* %gep.i.i163, align 8
  %gep1.i.i164 = getelementptr i8* %67, i64 8
  %69 = bitcast i8* %gep1.i.i164 to i8**
  %"genBinOp generated operation.i.i165" = add i64 %39, %68
  %70 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i165") nounwind
  store i8* %70, i8** %69, align 8
  %71 = getelementptr inbounds i8* %70, i64 %"genBinOp generated operation.i162"
  store i8 0, i8* %71, align 1
  %"genBinOp generated operation430.i166" = icmp slt i64 %68, 1
  br i1 %"genBinOp generated operation430.i166", label %"loop condition7.preheader.i172", label %"loop body.lr.ph.i169"

"loop body.lr.ph.i169":                           ; preds = %__String_substring.exit158
  %gep.i23.i167 = getelementptr %struct.string* %40, i64 0, i32 1
  %.pre32.i168 = load i8** %gep.i23.i167, align 8
  br label %"loop body.i180"

"loop condition7.preheader.i172":                 ; preds = %"loop body.i180", %__String_substring.exit158
  %i.0.lcssa.i170 = phi i64 [ 1, %__String_substring.exit158 ], [ %"genBinOp generated operation6.i178", %"loop body.i180" ]
  %"genBinOp generated operation1328.i171" = icmp sgt i64 %i.0.lcssa.i170, %"genBinOp generated operation.i162"
  br i1 %"genBinOp generated operation1328.i171", label %__String_append.exit187, label %"loop body8.i186"

"loop body.i180":                                 ; preds = %"loop body.i180", %"loop body.lr.ph.i169"
  %i.031.i176 = phi i64 [ 1, %"loop body.lr.ph.i169" ], [ %"genBinOp generated operation6.i178", %"loop body.i180" ]
  %"genBinOp generated operation5.i177" = add i64 %i.031.i176, -1
  %72 = getelementptr inbounds i8* %.pre32.i168, i64 %"genBinOp generated operation5.i177"
  %73 = load i8* %72, align 1
  %74 = getelementptr inbounds i8* %70, i64 %"genBinOp generated operation5.i177"
  store i8 %73, i8* %74, align 1
  %"genBinOp generated operation6.i178" = add i64 %i.031.i176, 1
  %"genBinOp generated operation4.i179" = icmp sgt i64 %"genBinOp generated operation6.i178", %68
  br i1 %"genBinOp generated operation4.i179", label %"loop condition7.preheader.i172", label %"loop body.i180"

"loop body8.i186":                                ; preds = %"loop body8.i186", %"loop condition7.preheader.i172"
  %i.129.i181 = phi i64 [ %"genBinOp generated operation17.i184", %"loop body8.i186" ], [ %i.0.lcssa.i170, %"loop condition7.preheader.i172" ]
  %"genBinOp generated operation14.i182" = add i64 %i.129.i181, -1
  %"genBinOp generated operation.i26.i183" = sub i64 %"genBinOp generated operation14.i182", %68
  %75 = getelementptr inbounds i8* %62, i64 %"genBinOp generated operation.i26.i183"
  %76 = load i8* %75, align 1
  %77 = getelementptr inbounds i8* %70, i64 %"genBinOp generated operation14.i182"
  store i8 %76, i8* %77, align 1
  %"genBinOp generated operation17.i184" = add i64 %i.129.i181, 1
  %"genBinOp generated operation13.i185" = icmp sgt i64 %"genBinOp generated operation17.i184", %"genBinOp generated operation.i162"
  br i1 %"genBinOp generated operation13.i185", label %__String_append.exit187, label %"loop body8.i186"

__String_append.exit187:                          ; preds = %"loop body8.i186", %"loop condition7.preheader.i172"
  %78 = load i64* %gep.i.i78, align 8
  %"genBinOp generated operation18" = add i64 %78, 1
  %79 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i" = bitcast i8* %79 to %struct.string*
  %"genBinOp generated operation.i37" = sub i64 %"genBinOp generated operation18", %39
  %gep.i.i38 = bitcast i8* %79 to i64*
  store i64 %"genBinOp generated operation.i37", i64* %gep.i.i38, align 8
  %gep1.i.i = getelementptr i8* %79, i64 8
  %80 = bitcast i8* %gep1.i.i to i8**
  %"genBinOp generated operation.i.i39" = add i64 %"genBinOp generated operation.i37", 1
  %81 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i39") nounwind
  store i8* %81, i8** %80, align 8
  %82 = getelementptr inbounds i8* %81, i64 %"genBinOp generated operation.i37"
  store i8 0, i8* %82, align 1
  %"genBinOp generated operation113.i" = icmp slt i64 %39, %"genBinOp generated operation18"
  br i1 %"genBinOp generated operation113.i", label %"loop body.lr.ph.i", label %__String_substring.exit

"loop body.lr.ph.i":                              ; preds = %__String_append.exit187
  %.pre.i = load i8** %61, align 8
  br label %"loop body.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.014.i = phi i64 [ %39, %"loop body.lr.ph.i" ], [ %"genBinOp generated operation3.i", %"loop body.i" ]
  %"genBinOp generated operation2.i" = sub i64 %i.014.i, %39
  %"genBinOp generated operation.i12.i" = add i64 %i.014.i, -1
  %83 = getelementptr inbounds i8* %.pre.i, i64 %"genBinOp generated operation.i12.i"
  %84 = load i8* %83, align 1
  %85 = getelementptr inbounds i8* %81, i64 %"genBinOp generated operation2.i"
  store i8 %84, i8* %85, align 1
  %"genBinOp generated operation3.i" = add i64 %i.014.i, 1
  %exitcond.i = icmp eq i64 %"genBinOp generated operation3.i", %"genBinOp generated operation18"
  br i1 %exitcond.i, label %__String_substring.exit, label %"loop body.i"

__String_substring.exit:                          ; preds = %"loop body.i", %__String_append.exit187
  store %struct.string* %"casting char ptr to typed ptr.i", %struct.string** %gep, align 8
  %"equality check3" = icmp eq i8* %67, null
  br i1 %"equality check3", label %else.i80, label %merge

then23:                                           ; preds = %__String_append.exit, %__String_append.exit, %"after loop", %"loop condition.preheader", %then, %then
  %86 = load %struct.string** %gep, align 8
  store %struct.string* null, %struct.string** %gep, align 8
  br label %merge
}

define i64 @__Socket_Buffer_find_cutoff(%procStructType* nocapture, %Socket_Buffer* nocapture, %struct.string*) nounwind readonly {
find_cutoffStartB:
  %"equality check" = icmp eq %struct.string* %2, null
  br i1 %"equality check", label %merge, label %else

else:                                             ; preds = %find_cutoffStartB
  %gep.i = getelementptr %struct.string* %2, i64 0, i32 0
  %3 = load i64* %gep.i, align 8
  %"genBinOp generated operation11.i" = icmp slt i64 %3, 1
  br i1 %"genBinOp generated operation11.i", label %__String_find.exit33, label %"loop body.lr.ph.i"

"loop body.lr.ph.i":                              ; preds = %else
  %gep.i.i = getelementptr %struct.string* %2, i64 0, i32 1
  %4 = load i8** %gep.i.i, align 8
  br label %"loop body.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.012.i = phi i64 [ 1, %"loop body.lr.ph.i" ], [ %"genBinOp generated operation2.i", %"loop body.i" ]
  %"genBinOp generated operation.i.i" = add i64 %i.012.i, -1
  %5 = getelementptr inbounds i8* %4, i64 %"genBinOp generated operation.i.i"
  %6 = load i8* %5, align 1
  %"equality check.i" = icmp eq i8 %6, 13
  %"genBinOp generated operation2.i" = add i64 %i.012.i, 1
  %"genBinOp generated operation.i" = icmp sgt i64 %"genBinOp generated operation2.i", %3
  %"genBinOp generated operation1.i" = or i1 %"genBinOp generated operation.i", %"equality check.i"
  br i1 %"genBinOp generated operation1.i", label %"loop body.lr.ph.i22", label %"loop body.i"

"loop body.lr.ph.i22":                            ; preds = %"loop body.i"
  %phitmp.i = select i1 %"equality check.i", i64 %i.012.i, i64 -1
  br label %"loop body.i29"

"loop body.i29":                                  ; preds = %"loop body.i29", %"loop body.lr.ph.i22"
  %i.012.i23 = phi i64 [ 1, %"loop body.lr.ph.i22" ], [ %"genBinOp generated operation2.i26", %"loop body.i29" ]
  %"genBinOp generated operation.i.i24" = add i64 %i.012.i23, -1
  %7 = getelementptr inbounds i8* %4, i64 %"genBinOp generated operation.i.i24"
  %8 = load i8* %7, align 1
  %"equality check.i25" = icmp eq i8 %8, 10
  %"genBinOp generated operation2.i26" = add i64 %i.012.i23, 1
  %"genBinOp generated operation.i27" = icmp sgt i64 %"genBinOp generated operation2.i26", %3
  %"genBinOp generated operation1.i28" = or i1 %"genBinOp generated operation.i27", %"equality check.i25"
  br i1 %"genBinOp generated operation1.i28", label %"loop condition.after loop_crit_edge.i31", label %"loop body.i29"

"loop condition.after loop_crit_edge.i31":        ; preds = %"loop body.i29"
  %phitmp.i30 = select i1 %"equality check.i25", i64 %i.012.i23, i64 -1
  br label %__String_find.exit33

__String_find.exit33:                             ; preds = %"loop condition.after loop_crit_edge.i31", %else
  %found.0.lcssa.i35 = phi i64 [ %phitmp.i, %"loop condition.after loop_crit_edge.i31" ], [ -1, %else ]
  %found.0.lcssa.i32 = phi i64 [ %phitmp.i30, %"loop condition.after loop_crit_edge.i31" ], [ -1, %else ]
  %"genBinOp generated operation" = add i64 %found.0.lcssa.i35, 1
  %"equality check1" = icmp eq i64 %found.0.lcssa.i32, %"genBinOp generated operation"
  br i1 %"equality check1", label %then2, label %elseIf25

merge:                                            ; preds = %elseIf25, %elseIfThen18, %elseIf13, %then2, %find_cutoffStartB
  %Result.0 = phi i64 [ 1, %find_cutoffStartB ], [ %"genBinOp generated operation33", %then2 ], [ %"genBinOp generated operation24", %elseIfThen18 ], [ %"genBinOp generated operation", %elseIf13 ], [ %"genBinOp generated operation", %elseIf25 ]
  ret i64 %Result.0

then2:                                            ; preds = %__String_find.exit33
  %"genBinOp generated operation33" = add i64 %found.0.lcssa.i32, 1
  br label %merge

elseIf9:                                          ; preds = %elseIf13
  %"genBinOp generated operation11" = icmp slt i64 %found.0.lcssa.i32, %found.0.lcssa.i35
  %"genBinOp generated operation12" = add i64 %found.0.lcssa.i32, 1
  %"genBinOp generated operation12." = select i1 %"genBinOp generated operation11", i64 %"genBinOp generated operation12", i64 1
  ret i64 %"genBinOp generated operation12."

elseIf13:                                         ; preds = %elseIf17
  %"genBinOp generated operation15" = icmp slt i64 %found.0.lcssa.i35, %found.0.lcssa.i32
  br i1 %"genBinOp generated operation15", label %merge, label %elseIf9

elseIf17:                                         ; preds = %elseIf25
  %"equality check20" = icmp eq i64 %found.0.lcssa.i35, -1
  %"equality check22" = icmp ne i64 %found.0.lcssa.i32, -1
  %"genBinOp generated operation23" = and i1 %"equality check20", %"equality check22"
  br i1 %"genBinOp generated operation23", label %elseIfThen18, label %elseIf13

elseIfThen18:                                     ; preds = %elseIf17
  %"genBinOp generated operation24" = add i64 %found.0.lcssa.i32, 1
  br label %merge

elseIf25:                                         ; preds = %__String_find.exit33
  %"equality check28" = icmp eq i64 %found.0.lcssa.i32, -1
  %"equality check30" = icmp ne i64 %found.0.lcssa.i35, -1
  %"genBinOp generated operation31" = and i1 %"equality check28", %"equality check30"
  br i1 %"genBinOp generated operation31", label %merge, label %elseIf17
}

define void @__File_open_read(%procStructType* nocapture, %File* nocapture, %struct.string* nocapture) nounwind {
merge:
  %gep = getelementptr %File* %1, i64 0, i32 0
  %3 = getelementptr inbounds %struct.string* %2, i64 0, i32 1
  %4 = load i8** %3, align 8
  %5 = tail call i32 (i8*, i32, ...)* @open(i8* %4, i32 0) nounwind
  %6 = zext i32 %5 to i64
  store i64 %6, i64* %gep, align 8
  ret void
}

define noalias %struct.string* @__File_read(%procStructType* nocapture, %File* nocapture, i64) nounwind {
readStartB:
  %3 = tail call noalias i8* @malloc(i64 16) nounwind
  %gep.i = bitcast i8* %3 to i64*
  store i64 %2, i64* %gep.i, align 8
  %gep1.i = getelementptr i8* %3, i64 8
  %4 = bitcast i8* %gep1.i to i8**
  %"genBinOp generated operation.i" = add i64 %2, 1
  %5 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %5, i8** %4, align 8
  %6 = getelementptr inbounds i8* %5, i64 %2
  store i8 0, i8* %6, align 1
  %gep = getelementptr %File* %1, i64 0, i32 0
  %7 = load i64* %gep, align 8
  %8 = trunc i64 %7 to i32
  %9 = tail call i64 @read(i32 %8, i8* %5, i64 %2) nounwind
  %"genBinOp generated operation" = icmp sgt i64 %9, 0
  br i1 %"genBinOp generated operation", label %then, label %merge

then:                                             ; preds = %readStartB
  %"casting char ptr to typed ptr" = bitcast i8* %3 to %struct.string*
  store i64 %9, i64* %gep.i, align 8
  br label %merge

merge:                                            ; preds = %then, %readStartB
  %Result.0 = phi %struct.string* [ %"casting char ptr to typed ptr", %then ], [ null, %readStartB ]
  ret %struct.string* %Result.0
}

define noalias %struct.string* @__File_read_all(%procStructType* nocapture, %File* nocapture) nounwind {
read_allStartB:
  %2 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %2 to %struct.string*
  %gep.i = bitcast i8* %2 to i64*
  store i64 0, i64* %gep.i, align 8
  %gep1.i = getelementptr i8* %2, i64 8
  %3 = bitcast i8* %gep1.i to i8**
  store i8* getelementptr inbounds ([1 x i8]* @_global, i64 0, i64 0), i8** %3, align 8
  %4 = tail call noalias i8* @malloc(i64 16) nounwind
  %gep.i.i11 = bitcast i8* %4 to i64*
  store i64 1024, i64* %gep.i.i11, align 8
  %gep1.i.i12 = getelementptr i8* %4, i64 8
  %5 = bitcast i8* %gep1.i.i12 to i8**
  %6 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %6, i8** %5, align 8
  %7 = getelementptr inbounds i8* %6, i64 1024
  store i8 0, i8* %7, align 1
  %gep.i13 = getelementptr %File* %1, i64 0, i32 0
  %8 = load i64* %gep.i13, align 8
  %9 = trunc i64 %8 to i32
  %10 = tail call i64 @read(i32 %9, i8* %6, i64 1024) nounwind
  %"genBinOp generated operation.i14" = icmp sgt i64 %10, 0
  br i1 %"genBinOp generated operation.i14", label %"loop condition.preheader", label %"after loop"

"loop condition.preheader":                       ; preds = %read_allStartB
  store i64 %10, i64* %gep.i.i11, align 8
  %"equality check25" = icmp eq i8* %4, null
  br i1 %"equality check25", label %"after loop", label %"loop body"

"loop body":                                      ; preds = %"loop condition.backedge", %"loop condition.preheader"
  %Result.027 = phi %struct.string* [ %"casting char ptr to typed ptr.i19", %"loop condition.backedge" ], [ %"casting char ptr to typed ptr", %"loop condition.preheader" ]
  %str.026.in = phi i8* [ %24, %"loop condition.backedge" ], [ %4, %"loop condition.preheader" ]
  %11 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i19" = bitcast i8* %11 to %struct.string*
  %gep.i20 = getelementptr %struct.string* %Result.027, i64 0, i32 0
  %12 = load i64* %gep.i20, align 8
  %gep1.i21 = bitcast i8* %str.026.in to i64*
  %13 = load i64* %gep1.i21, align 8
  %"genBinOp generated operation.i22" = add i64 %13, %12
  %gep.i.i23 = bitcast i8* %11 to i64*
  store i64 %"genBinOp generated operation.i22", i64* %gep.i.i23, align 8
  %gep1.i.i24 = getelementptr i8* %11, i64 8
  %14 = bitcast i8* %gep1.i.i24 to i8**
  %"genBinOp generated operation.i.i" = add i64 %"genBinOp generated operation.i22", 1
  %15 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i") nounwind
  store i8* %15, i8** %14, align 8
  %16 = getelementptr inbounds i8* %15, i64 %"genBinOp generated operation.i22"
  store i8 0, i8* %16, align 1
  %"genBinOp generated operation430.i" = icmp slt i64 %12, 1
  br i1 %"genBinOp generated operation430.i", label %"loop condition7.preheader.i", label %"loop body.lr.ph.i"

"loop body.lr.ph.i":                              ; preds = %"loop body"
  %gep.i23.i = getelementptr %struct.string* %Result.027, i64 0, i32 1
  %.pre32.i = load i8** %gep.i23.i, align 8
  br label %"loop body.i"

"loop condition7.preheader.i":                    ; preds = %"loop body.i", %"loop body"
  %i.0.lcssa.i = phi i64 [ 1, %"loop body" ], [ %"genBinOp generated operation6.i", %"loop body.i" ]
  %"genBinOp generated operation1328.i" = icmp sgt i64 %i.0.lcssa.i, %"genBinOp generated operation.i22"
  br i1 %"genBinOp generated operation1328.i", label %__String_append.exit, label %"loop body8.lr.ph.i"

"loop body8.lr.ph.i":                             ; preds = %"loop condition7.preheader.i"
  %gep.i25.i = getelementptr i8* %str.026.in, i64 8
  %17 = bitcast i8* %gep.i25.i to i8**
  %.pre.i = load i8** %17, align 8
  br label %"loop body8.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.031.i = phi i64 [ 1, %"loop body.lr.ph.i" ], [ %"genBinOp generated operation6.i", %"loop body.i" ]
  %"genBinOp generated operation5.i" = add i64 %i.031.i, -1
  %18 = getelementptr inbounds i8* %.pre32.i, i64 %"genBinOp generated operation5.i"
  %19 = load i8* %18, align 1
  %20 = getelementptr inbounds i8* %15, i64 %"genBinOp generated operation5.i"
  store i8 %19, i8* %20, align 1
  %"genBinOp generated operation6.i" = add i64 %i.031.i, 1
  %"genBinOp generated operation4.i" = icmp sgt i64 %"genBinOp generated operation6.i", %12
  br i1 %"genBinOp generated operation4.i", label %"loop condition7.preheader.i", label %"loop body.i"

"loop body8.i":                                   ; preds = %"loop body8.i", %"loop body8.lr.ph.i"
  %i.129.i = phi i64 [ %i.0.lcssa.i, %"loop body8.lr.ph.i" ], [ %"genBinOp generated operation17.i", %"loop body8.i" ]
  %"genBinOp generated operation14.i" = add i64 %i.129.i, -1
  %"genBinOp generated operation.i26.i" = sub i64 %"genBinOp generated operation14.i", %12
  %21 = getelementptr inbounds i8* %.pre.i, i64 %"genBinOp generated operation.i26.i"
  %22 = load i8* %21, align 1
  %23 = getelementptr inbounds i8* %15, i64 %"genBinOp generated operation14.i"
  store i8 %22, i8* %23, align 1
  %"genBinOp generated operation17.i" = add i64 %i.129.i, 1
  %"genBinOp generated operation13.i" = icmp sgt i64 %"genBinOp generated operation17.i", %"genBinOp generated operation.i22"
  br i1 %"genBinOp generated operation13.i", label %__String_append.exit, label %"loop body8.i"

__String_append.exit:                             ; preds = %"loop body8.i", %"loop condition7.preheader.i"
  %24 = tail call noalias i8* @malloc(i64 16) nounwind
  %gep.i.i = bitcast i8* %24 to i64*
  store i64 1024, i64* %gep.i.i, align 8
  %gep1.i.i = getelementptr i8* %24, i64 8
  %25 = bitcast i8* %gep1.i.i to i8**
  %26 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %26, i8** %25, align 8
  %27 = getelementptr inbounds i8* %26, i64 1024
  store i8 0, i8* %27, align 1
  %28 = load i64* %gep.i13, align 8
  %29 = trunc i64 %28 to i32
  %30 = tail call i64 @read(i32 %29, i8* %26, i64 1024) nounwind
  %"genBinOp generated operation.i" = icmp sgt i64 %30, 0
  br i1 %"genBinOp generated operation.i", label %"loop condition.backedge", label %"after loop"

"loop condition.backedge":                        ; preds = %__String_append.exit
  store i64 %30, i64* %gep.i.i, align 8
  %"equality check" = icmp eq i8* %24, null
  br i1 %"equality check", label %"after loop", label %"loop body"

"after loop":                                     ; preds = %"loop condition.backedge", %__String_append.exit, %"loop condition.preheader", %read_allStartB
  %Result.0.lcssa = phi %struct.string* [ %"casting char ptr to typed ptr", %"loop condition.preheader" ], [ %"casting char ptr to typed ptr.i19", %"loop condition.backedge" ], [ %"casting char ptr to typed ptr", %read_allStartB ], [ %"casting char ptr to typed ptr.i19", %__String_append.exit ]
  ret %struct.string* %Result.0.lcssa
}

define void @__Array_make(%procStructType* nocapture, i8*** nocapture, i64) nounwind {
makeStartB:
  %3 = shl i64 %2, 3
  %4 = tail call noalias i8* @malloc(i64 %3) nounwind
  %5 = bitcast i8* %4 to i8**
  store i8** %5, i8*** %1, align 8
  ret void
}

define i8* @__Array_item(%procStructType* nocapture, i8*** nocapture, i64) nounwind readonly {
itemStartB:
  %3 = load i8*** %1, align 8
  %4 = getelementptr inbounds i8** %3, i64 %2
  %5 = load i8** %4, align 8
  ret i8* %5
}

define void @__Array_put(%procStructType* nocapture, i8*** nocapture, i64, i8*) nounwind {
putStartB:
  %4 = load i8*** %1, align 8
  %5 = getelementptr inbounds i8** %4, i64 %2
  store i8* %3, i8** %5, align 8
  ret void
}

define void @__Prod_Cons_Test_main(%procStructType*) {
mainStartB:
  %1 = call noalias i8* @malloc(i64 16) nounwind
  %2 = call %procStructType* @proc_new_from_other(%procStructType* %0)
  %3 = call noalias i8* @malloc(i64 8) nounwind
  %4 = bitcast i8* %1 to %procStructType**
  store %procStructType* %2, %procStructType** %4, align 8
  %5 = getelementptr i8* %1, i64 8
  %6 = bitcast i8* %5 to i8**
  store i8* %3, i8** %6, align 8
  %7 = call %privqStructType* @proc_get_queue(%procStructType* %0, %procStructType* %2)
  call void @priv_queue_lock(%privqStructType* %7, %procStructType* %0)
  %8 = call %clostypeStructType* @closure_void_type()
  %9 = alloca i8***, align 8
  %10 = alloca %clostypeStructType**, align 8
  %11 = call %closureStructType* @closure_new(i8* bitcast (void (%procStructType*, %Data*)* @__Data_make to i8*), %clostypeStructType* %8, i64 2, i8**** %9, %clostypeStructType*** %10)
  %12 = load %clostypeStructType*** %10, align 8
  %13 = load i8**** %9, align 8
  %14 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %14, %clostypeStructType** %12, align 8
  %15 = call %clostypeStructType* @closure_pointer_type()
  %16 = getelementptr %clostypeStructType** %12, i64 1
  store %clostypeStructType* %15, %clostypeStructType** %16, align 8
  %"arg store bitcast" = bitcast %procStructType* %2 to i8*
  %17 = load i8*** %13, align 8
  store i8* %"arg store bitcast", i8** %17, align 8
  %18 = getelementptr i8*** %13, i64 1
  %19 = load i8*** %18, align 8
  store i8* %3, i8** %19, align 8
  call void @priv_queue_routine(%privqStructType* %7, %closureStructType* %11, %procStructType* %0)
  call void @priv_queue_unlock(%privqStructType* %7, %procStructType* %0)
  %20 = call noalias i8* @malloc(i64 16) nounwind
  %21 = bitcast i8* %20 to i8**
  br label %"loop body"

"loop condition11.preheader":                     ; preds = %"loop body"
  %22 = load i8** %21, align 8
  %23 = bitcast i8* %22 to %procStructType**
  %getProc15 = load %procStructType** %23, align 8
  ; call void @proc_shutdown(%procStructType* %getProc15, %procStructType* %0)
  %24 = getelementptr inbounds i8* %20, i64 8
  %25 = bitcast i8* %24 to i8**
  %26 = load i8** %25, align 8
  %27 = bitcast i8* %26 to %procStructType**
  %getProc15.1 = load %procStructType** %27, align 8
  ; call void @proc_shutdown(%procStructType* %getProc15.1, %procStructType* %0)
  %getProc17 = load %procStructType** %4, align 8
  ; call void @proc_shutdown(%procStructType* %getProc17, %procStructType* %0)
  ; call void @proc_deref_priv_queues(%procStructType* %0)
  ret void

"loop body":                                      ; preds = %"loop body", %mainStartB
  %i.036 = phi i64 [ 0, %mainStartB ], [ %"genBinOp generated operation10", %"loop body" ]
  %28 = call noalias i8* @malloc(i64 16) nounwind
  %29 = call %procStructType* @proc_new_from_other(%procStructType* %0)
  %30 = call noalias i8* @malloc(i64 16) nounwind
  %31 = bitcast i8* %28 to %procStructType**
  store %procStructType* %29, %procStructType** %31, align 8
  %32 = getelementptr i8* %28, i64 8
  %33 = bitcast i8* %32 to i8**
  store i8* %30, i8** %33, align 8
  %34 = call %privqStructType* @proc_get_queue(%procStructType* %0, %procStructType* %29)
  call void @priv_queue_lock(%privqStructType* %34, %procStructType* %0)
  %35 = call %clostypeStructType* @closure_void_type()
  %36 = alloca i8***, align 8
  %37 = alloca %clostypeStructType**, align 8
  %38 = call %closureStructType* @closure_new(i8* bitcast (void (%procStructType*, %Worker*, i64, %separate_wrapper*)* @__Worker_make to i8*), %clostypeStructType* %35, i64 4, i8**** %36, %clostypeStructType*** %37)
  %39 = load %clostypeStructType*** %37, align 8
  %40 = load i8**** %36, align 8
  %41 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %41, %clostypeStructType** %39, align 8
  %42 = call %clostypeStructType* @closure_pointer_type()
  %43 = getelementptr %clostypeStructType** %39, i64 1
  store %clostypeStructType* %42, %clostypeStructType** %43, align 8
  %44 = call %clostypeStructType* @closure_sint_type()
  %45 = getelementptr %clostypeStructType** %39, i64 2
  store %clostypeStructType* %44, %clostypeStructType** %45, align 8
  %46 = call %clostypeStructType* @closure_pointer_type()
  %47 = getelementptr %clostypeStructType** %39, i64 3
  store %clostypeStructType* %46, %clostypeStructType** %47, align 8
  %"arg store bitcast6" = bitcast %procStructType* %29 to i8*
  %48 = load i8*** %40, align 8
  store i8* %"arg store bitcast6", i8** %48, align 8
  %49 = getelementptr i8*** %40, i64 1
  %50 = load i8*** %49, align 8
  store i8* %30, i8** %50, align 8
  %ptrtoint = inttoptr i64 %i.036 to i8*
  %51 = getelementptr i8*** %40, i64 2
  %52 = load i8*** %51, align 8
  store i8* %ptrtoint, i8** %52, align 8
  %53 = getelementptr i8*** %40, i64 3
  %54 = load i8*** %53, align 8
  store i8* %1, i8** %54, align 8
  call void @priv_queue_routine(%privqStructType* %34, %closureStructType* %38, %procStructType* %0)
  call void @priv_queue_unlock(%privqStructType* %34, %procStructType* %0)
  %getProc = load %procStructType** %31, align 8
  %55 = call %privqStructType* @proc_get_queue(%procStructType* %0, %procStructType* %getProc)
  call void @priv_queue_lock(%privqStructType* %55, %procStructType* %0)
  %56 = load i8** %33, align 8
  %57 = call %clostypeStructType* @closure_void_type()
  %58 = alloca i8***, align 8
  %59 = alloca %clostypeStructType**, align 8
  %60 = call %closureStructType* @closure_new(i8* bitcast (void (%procStructType*, %Worker*)* @worker_stub2 to i8*), %clostypeStructType* %57, i64 2, i8**** %58, %clostypeStructType*** %59)
  %61 = load %clostypeStructType*** %59, align 8
  %62 = load i8**** %58, align 8
  %63 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %63, %clostypeStructType** %61, align 8
  %64 = call %clostypeStructType* @closure_pointer_type()
  %65 = getelementptr %clostypeStructType** %61, i64 1
  store %clostypeStructType* %64, %clostypeStructType** %65, align 8
  %"arg store bitcast9" = bitcast %procStructType* %getProc to i8*
  %66 = load i8*** %62, align 8
  store i8* %"arg store bitcast9", i8** %66, align 8
  %67 = getelementptr i8*** %62, i64 1
  %68 = load i8*** %67, align 8
  store i8* %56, i8** %68, align 8
  call void @priv_queue_routine(%privqStructType* %55, %closureStructType* %60, %procStructType* %0)
  call void @priv_queue_unlock(%privqStructType* %55, %procStructType* %0)
  %69 = getelementptr inbounds i8** %21, i64 %i.036
  store i8* %28, i8** %69, align 8
  %"genBinOp generated operation10" = add i64 %i.036, 1
  %exitcond = icmp eq i64 %"genBinOp generated operation10", 2
  br i1 %exitcond, label %"loop condition11.preheader", label %"loop body"
}

define i64 @main() {
mainStart:
  %0 = tail call %syncdataStructType* @sync_data_new(i64 32000)
  %1 = tail call %procStructType* @proc_new_root(%syncdataStructType* %0, void (%procStructType*)* @__Prod_Cons_Test_main)
  tail call void @create_executors(%syncdataStructType* %0, i64 4)
  %2 = tail call %notifierStructType* @notifier_spawn(%syncdataStructType* %0)
  tail call void @notifier_join(%notifierStructType* %2)
  tail call void @join_executors()
  tail call void @sync_data_free(%syncdataStructType* %0)
  ret i64 0
}

define void @__Data_make(%procStructType* nocapture, %Data* nocapture) nounwind {
makeStartB:
  %2 = getelementptr %Data* %1, i64 0, i32 0
  store i64 0, i64* %2, align 8
  ret void
}

define i64 @__Data_get_value(%procStructType* nocapture, %Data* nocapture) nounwind readonly {
get_valueStartB:
  %2 = getelementptr %Data* %1, i64 0, i32 0
  %3 = load i64* %2, align 8
  ret i64 %3
}

define void @__Data_incr(%procStructType* nocapture, %Data* nocapture) nounwind {
incrStartB:
  %2 = getelementptr %Data* %1, i64 0, i32 0
  %3 = load i64* %2, align 8
  %"genBinOp generated operation" = add i64 %3, 1
  store i64 %"genBinOp generated operation", i64* %2, align 8
  ret void
}

define void @__Worker_make(%procStructType* nocapture, %Worker* nocapture, i64, %separate_wrapper*) nounwind {
makeStartB:
  %4 = getelementptr %Worker* %1, i64 0, i32 1
  store i64 %2, i64* %4, align 8
  %5 = getelementptr %Worker* %1, i64 0, i32 0
  store %separate_wrapper* %3, %separate_wrapper** %5, align 8
  ret void
}

declare void @worker_stub2(%procStructType*, %Worker* nocapture)

define void @__Worker_run(%procStructType*, %Worker* nocapture) {
runStartB:
  %2 = getelementptr %Worker* %1, i64 0, i32 0
  %3 = getelementptr %Worker* %1, i64 0, i32 1
  br label %"lock block.preheader"

"lock block.preheader":                           ; preds = %"sep body block", %runStartB
  %i.014 = phi i64 [ 1, %runStartB ], [ %"genBinOp generated operation4", %"sep body block" ]
  br label %"lock block"

"after loop":                                     ; preds = %"sep body block"
  %4 = call i64 @write(i32 1, i8* getelementptr inbounds ([14 x i8]* @"Worker done \0A_global", i64 0, i64 0), i64 13) nounwind
  ret void

"lock block":                                     ; preds = %"wait cond retry block", %"lock block.preheader"
  %5 = load %separate_wrapper** %2, align 8
  %6 = getelementptr %separate_wrapper* %5, i64 0, i32 0
  %getProc = load %procStructType** %6, align 8
  %7 = call %privqStructType* @proc_get_queue(%procStructType* %0, %procStructType* %getProc)
  call void @priv_queue_lock(%privqStructType* %7, %procStructType* %0)
  %8 = load %separate_wrapper** %2, align 8
  %9 = getelementptr %separate_wrapper* %8, i64 0, i32 0
  %10 = load %procStructType** %9, align 8
  %11 = getelementptr %separate_wrapper* %8, i64 0, i32 1
  %12 = load i8** %11, align 8
  %13 = call %clostypeStructType* @closure_sint_type()
  %14 = alloca i8***, align 8
  %15 = alloca %clostypeStructType**, align 8
  %16 = call %closureStructType* @closure_new(i8* bitcast (i64 (%procStructType*, %Data*)* @__Data_get_value to i8*), %clostypeStructType* %13, i64 2, i8**** %14, %clostypeStructType*** %15)
  %17 = load %clostypeStructType*** %15, align 8
  %18 = load i8**** %14, align 8
  %19 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %19, %clostypeStructType** %17, align 8
  %20 = call %clostypeStructType* @closure_pointer_type()
  %21 = getelementptr %clostypeStructType** %17, i64 1
  store %clostypeStructType* %20, %clostypeStructType** %21, align 8
  %"arg store bitcast" = bitcast %procStructType* %10 to i8*
  %22 = load i8*** %18, align 8
  store i8* %"arg store bitcast", i8** %22, align 8
  %23 = getelementptr i8*** %18, i64 1
  %24 = load i8*** %23, align 8
  store i8* %12, i8** %24, align 8
  %closure_result = alloca i64, align 8
  %closure_result_cast = bitcast i64* %closure_result to i8*
  call void @priv_queue_function(%privqStructType* %7, %closureStructType* %16, i8* %closure_result_cast, %procStructType* %0)
  %25 = load i64* %closure_result, align 8
  %"genBinOp generated operation1" = srem i64 %25, 2
  %26 = load i64* %3, align 8
  %"equality check" = icmp eq i64 %"genBinOp generated operation1", %26
  br i1 %"equality check", label %"sep body block", label %"wait cond retry block"

"sep body block":                                 ; preds = %"lock block"
  %27 = load %separate_wrapper** %2, align 8
  %28 = getelementptr %separate_wrapper* %27, i64 0, i32 0
  %29 = load %procStructType** %28, align 8
  %30 = getelementptr %separate_wrapper* %27, i64 0, i32 1
  %31 = load i8** %30, align 8
  %32 = call %clostypeStructType* @closure_void_type()
  %33 = alloca i8***, align 8
  %34 = alloca %clostypeStructType**, align 8
  %35 = call %closureStructType* @closure_new(i8* bitcast (void (%procStructType*, %Data*)* @__Data_incr to i8*), %clostypeStructType* %32, i64 2, i8**** %33, %clostypeStructType*** %34)
  %36 = load %clostypeStructType*** %34, align 8
  %37 = load i8**** %33, align 8
  %38 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %38, %clostypeStructType** %36, align 8
  %39 = call %clostypeStructType* @closure_pointer_type()
  %40 = getelementptr %clostypeStructType** %36, i64 1
  store %clostypeStructType* %39, %clostypeStructType** %40, align 8
  %"arg store bitcast3" = bitcast %procStructType* %29 to i8*
  %41 = load i8*** %37, align 8
  store i8* %"arg store bitcast3", i8** %41, align 8
  %42 = getelementptr i8*** %37, i64 1
  %43 = load i8*** %42, align 8
  store i8* %31, i8** %43, align 8
  call void @priv_queue_routine(%privqStructType* %7, %closureStructType* %35, %procStructType* %0)
  call void @priv_queue_unlock(%privqStructType* %7, %procStructType* %0)
  %"genBinOp generated operation4" = add i64 %i.014, 1
  %exitcond = icmp eq i64 %"genBinOp generated operation4", 161
  br i1 %exitcond, label %"after loop", label %"lock block.preheader"

"wait cond retry block":                          ; preds = %"lock block"
  call void @priv_queue_unlock(%privqStructType* %7, %procStructType* %0)
  %44 = load %separate_wrapper** %2, align 8
  %45 = getelementptr %separate_wrapper* %44, i64 0, i32 0
  %getProc2 = load %procStructType** %45, align 8
  call void @proc_wait_for_available(%procStructType* %getProc2, %procStructType* %0)
  br label %"lock block"
}

declare void @llvm.lifetime.start(i64, i8* nocapture) nounwind

declare void @llvm.lifetime.end(i64, i8* nocapture) nounwind

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
  %3 = bitcast i8* %2 to i64*
  store i64 0, i64* %3, align 8
  %4 = getelementptr i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* getelementptr inbounds ([1 x i8]* @_global, i64 0, i64 0), i8** %5, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop condition.backedge", %int_to_strStartB
  %str.010 = phi %struct.string* [ %"casting char ptr to typed ptr", %int_to_strStartB ], [ %"casting char ptr to typed ptr.i", %"loop condition.backedge" ]
  %rest.09 = phi i64 [ %1, %int_to_strStartB ], [ %"genBinOp generated operation2", %"loop condition.backedge" ]
  %"genBinOp generated operation1" = srem i64 %rest.09, 10
  %6 = trunc i64 %"genBinOp generated operation1" to i8
  %"genBinOp generated operation2" = sdiv i64 %rest.09, 10
  %"genBinOp generated operation3" = add i8 %6, 48
  %7 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i" = bitcast i8* %7 to %struct.string*
  %8 = getelementptr %struct.string* %str.010, i64 0, i32 0
  %9 = load i64* %8, align 8
  %"genBinOp generated operation.i" = add i64 %9, 1
  %10 = bitcast i8* %7 to i64*
  store i64 %"genBinOp generated operation.i", i64* %10, align 8
  %11 = getelementptr i8* %7, i64 8
  %12 = bitcast i8* %11 to i8**
  %"genBinOp generated operation.i.i" = add i64 %9, 2
  %13 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i") nounwind
  store i8* %13, i8** %12, align 8
  %14 = getelementptr inbounds i8* %13, i64 %"genBinOp generated operation.i"
  store i8 0, i8* %14, align 1
  store i8 %"genBinOp generated operation3", i8* %13, align 1
  %"genBinOp generated operation119.i" = icmp slt i64 %9, 1
  br i1 %"genBinOp generated operation119.i", label %"loop condition.backedge", label %"loop body.lr.ph.i"

"loop condition.backedge":                        ; preds = %"loop body.i", %"loop body"
  %rest.09.off = add i64 %rest.09, 9
  %15 = icmp ult i64 %rest.09.off, 19
  br i1 %15, label %"after loop", label %"loop body"

"loop body.lr.ph.i":                              ; preds = %"loop body"
  %16 = getelementptr %struct.string* %str.010, i64 0, i32 1
  %.pre.i = load i8** %16, align 8
  br label %"loop body.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.020.i = phi i64 [ 1, %"loop body.lr.ph.i" ], [ %"genBinOp generated operation2.i", %"loop body.i" ]
  %"genBinOp generated operation.i18.i" = add i64 %i.020.i, -1
  %17 = getelementptr inbounds i8* %.pre.i, i64 %"genBinOp generated operation.i18.i"
  %18 = load i8* %17, align 1
  %19 = getelementptr inbounds i8* %13, i64 %i.020.i
  store i8 %18, i8* %19, align 1
  %"genBinOp generated operation2.i" = add i64 %i.020.i, 1
  %"genBinOp generated operation1.i" = icmp sgt i64 %"genBinOp generated operation2.i", %9
  br i1 %"genBinOp generated operation1.i", label %"loop condition.backedge", label %"loop body.i"

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
  %3 = getelementptr %struct.string* %1, i64 0, i32 0
  store i64 %2, i64* %3, align 8
  %4 = getelementptr %struct.string* %1, i64 0, i32 1
  %"genBinOp generated operation" = add i64 %2, 1
  %5 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation") nounwind
  store i8* %5, i8** %4, align 8
  %6 = getelementptr inbounds i8* %5, i64 %2
  store i8 0, i8* %6, align 1
  ret void
}

define void @__String_make_with_pointer(%procStructType* nocapture, %struct.string* nocapture, i64, i8*) nounwind {
make_with_pointerStartB:
  %4 = getelementptr %struct.string* %1, i64 0, i32 0
  store i64 %2, i64* %4, align 8
  %5 = getelementptr %struct.string* %1, i64 0, i32 1
  store i8* %3, i8** %5, align 8
  ret void
}

define i8 @__String_item(%procStructType* nocapture, %struct.string* nocapture, i64) nounwind readonly {
itemStartB:
  %3 = getelementptr %struct.string* %1, i64 0, i32 1
  %4 = load i8** %3, align 8
  %"genBinOp generated operation" = add i64 %2, -1
  %5 = getelementptr inbounds i8* %4, i64 %"genBinOp generated operation"
  %6 = load i8* %5, align 1
  ret i8 %6
}

define i1 @__String_starts_with(%procStructType* nocapture, %struct.string* nocapture, %struct.string* nocapture) nounwind readonly {
starts_withStartB:
  %3 = getelementptr %struct.string* %2, i64 0, i32 0
  %4 = load i64* %3, align 8
  %5 = getelementptr %struct.string* %1, i64 0, i32 0
  %6 = load i64* %5, align 8
  %"genBinOp generated operation" = icmp sgt i64 %4, %6
  br i1 %"genBinOp generated operation", label %merge, label %"loop condition.preheader"

"loop condition.preheader":                       ; preds = %starts_withStartB
  %"genBinOp generated operation111" = icmp slt i64 %4, 1
  br i1 %"genBinOp generated operation111", label %merge, label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %"loop condition.preheader"
  %7 = getelementptr %struct.string* %1, i64 0, i32 1
  %8 = load i8** %7, align 8
  %9 = getelementptr %struct.string* %2, i64 0, i32 1
  %10 = load i8** %9, align 8
  br label %"loop body"

merge:                                            ; preds = %"loop condition.after loop_crit_edge", %"loop condition.preheader", %starts_withStartB
  %Result.0 = phi i1 [ false, %starts_withStartB ], [ %phitmp, %"loop condition.after loop_crit_edge" ], [ true, %"loop condition.preheader" ]
  ret i1 %Result.0

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.012 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation3", %"loop body" ]
  %"genBinOp generated operation.i" = add i64 %i.012, -1
  %11 = getelementptr inbounds i8* %8, i64 %"genBinOp generated operation.i"
  %12 = load i8* %11, align 1
  %13 = getelementptr inbounds i8* %10, i64 %"genBinOp generated operation.i"
  %14 = load i8* %13, align 1
  %"equality check" = icmp ne i8 %12, %14
  %"genBinOp generated operation3" = add i64 %i.012, 1
  %"genBinOp generated operation1" = icmp sgt i64 %"genBinOp generated operation3", %4
  %"genBinOp generated operation2" = or i1 %"genBinOp generated operation1", %"equality check"
  br i1 %"genBinOp generated operation2", label %"loop condition.after loop_crit_edge", label %"loop body"

"loop condition.after loop_crit_edge":            ; preds = %"loop body"
  %phitmp = xor i1 %"equality check", true
  br label %merge
}

define i1 @__String_equals(%procStructType* nocapture, %struct.string* nocapture, %struct.string* nocapture) nounwind readonly {
equalsStartB:
  %3 = getelementptr %struct.string* %2, i64 0, i32 0
  %4 = load i64* %3, align 8
  %5 = getelementptr %struct.string* %1, i64 0, i32 0
  %6 = load i64* %5, align 8
  %"equality check" = icmp eq i64 %4, %6
  br i1 %"equality check", label %"loop condition.preheader", label %merge

"loop condition.preheader":                       ; preds = %equalsStartB
  %"genBinOp generated operation11" = icmp slt i64 %4, 1
  br i1 %"genBinOp generated operation11", label %merge, label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %"loop condition.preheader"
  %7 = getelementptr %struct.string* %1, i64 0, i32 1
  %8 = load i8** %7, align 8
  %9 = getelementptr %struct.string* %2, i64 0, i32 1
  %10 = load i8** %9, align 8
  br label %"loop body"

merge:                                            ; preds = %"loop condition.after loop_crit_edge", %"loop condition.preheader", %equalsStartB
  %Result.0 = phi i1 [ false, %equalsStartB ], [ %phitmp, %"loop condition.after loop_crit_edge" ], [ true, %"loop condition.preheader" ]
  ret i1 %Result.0

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.012 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation3", %"loop body" ]
  %"genBinOp generated operation.i" = add i64 %i.012, -1
  %11 = getelementptr inbounds i8* %8, i64 %"genBinOp generated operation.i"
  %12 = load i8* %11, align 1
  %13 = getelementptr inbounds i8* %10, i64 %"genBinOp generated operation.i"
  %14 = load i8* %13, align 1
  %"equality check2" = icmp ne i8 %12, %14
  %"genBinOp generated operation3" = add i64 %i.012, 1
  %"genBinOp generated operation" = icmp sgt i64 %"genBinOp generated operation3", %4
  %"genBinOp generated operation1" = or i1 %"genBinOp generated operation", %"equality check2"
  br i1 %"genBinOp generated operation1", label %"loop condition.after loop_crit_edge", label %"loop body"

"loop condition.after loop_crit_edge":            ; preds = %"loop body"
  %phitmp = xor i1 %"equality check2", true
  br label %merge
}

define i64 @__String_find(%procStructType* nocapture, %struct.string* nocapture, i8) nounwind readonly {
findStartB:
  %3 = getelementptr %struct.string* %1, i64 0, i32 0
  %4 = load i64* %3, align 8
  %"genBinOp generated operation11" = icmp slt i64 %4, 1
  br i1 %"genBinOp generated operation11", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %findStartB
  %5 = getelementptr %struct.string* %1, i64 0, i32 1
  %6 = load i8** %5, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.012 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation2", %"loop body" ]
  %"genBinOp generated operation.i" = add i64 %i.012, -1
  %7 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation.i"
  %8 = load i8* %7, align 1
  %"equality check" = icmp eq i8 %8, %2
  %"genBinOp generated operation2" = add i64 %i.012, 1
  %"genBinOp generated operation" = icmp sgt i64 %"genBinOp generated operation2", %4
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
  %4 = getelementptr %struct.string* %1, i64 0, i32 0
  %5 = load i64* %4, align 8
  %6 = getelementptr %struct.string* %2, i64 0, i32 0
  %7 = load i64* %6, align 8
  %"genBinOp generated operation" = add i64 %7, %5
  %8 = bitcast i8* %3 to i64*
  store i64 %"genBinOp generated operation", i64* %8, align 8
  %9 = getelementptr i8* %3, i64 8
  %10 = bitcast i8* %9 to i8**
  %"genBinOp generated operation.i" = add i64 %"genBinOp generated operation", 1
  %11 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %11, i8** %10, align 8
  %12 = getelementptr inbounds i8* %11, i64 %"genBinOp generated operation"
  store i8 0, i8* %12, align 1
  %"genBinOp generated operation128" = icmp slt i64 %5, 1
  br i1 %"genBinOp generated operation128", label %"loop condition4.preheader", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %appendStartB
  %13 = getelementptr %struct.string* %1, i64 0, i32 1
  %.pre30 = load i8** %13, align 8
  br label %"loop body"

"loop condition4.preheader":                      ; preds = %"loop body", %appendStartB
  %i.0.lcssa = phi i64 [ 1, %appendStartB ], [ %"genBinOp generated operation3", %"loop body" ]
  %"genBinOp generated operation826" = icmp sgt i64 %i.0.lcssa, %"genBinOp generated operation"
  br i1 %"genBinOp generated operation826", label %"after loop6", label %"loop body5.lr.ph"

"loop body5.lr.ph":                               ; preds = %"loop condition4.preheader"
  %14 = getelementptr %struct.string* %2, i64 0, i32 1
  %.pre = load i8** %14, align 8
  br label %"loop body5"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.029 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation3", %"loop body" ]
  %"genBinOp generated operation2" = add i64 %i.029, -1
  %15 = getelementptr inbounds i8* %.pre30, i64 %"genBinOp generated operation2"
  %16 = load i8* %15, align 1
  %17 = getelementptr inbounds i8* %11, i64 %"genBinOp generated operation2"
  store i8 %16, i8* %17, align 1
  %"genBinOp generated operation3" = add i64 %i.029, 1
  %"genBinOp generated operation1" = icmp sgt i64 %"genBinOp generated operation3", %5
  br i1 %"genBinOp generated operation1", label %"loop condition4.preheader", label %"loop body"

"loop body5":                                     ; preds = %"loop body5", %"loop body5.lr.ph"
  %i.127 = phi i64 [ %i.0.lcssa, %"loop body5.lr.ph" ], [ %"genBinOp generated operation11", %"loop body5" ]
  %"genBinOp generated operation9" = add i64 %i.127, -1
  %"genBinOp generated operation.i24" = sub i64 %"genBinOp generated operation9", %5
  %18 = getelementptr inbounds i8* %.pre, i64 %"genBinOp generated operation.i24"
  %19 = load i8* %18, align 1
  %20 = getelementptr inbounds i8* %11, i64 %"genBinOp generated operation9"
  store i8 %19, i8* %20, align 1
  %"genBinOp generated operation11" = add i64 %i.127, 1
  %"genBinOp generated operation8" = icmp sgt i64 %"genBinOp generated operation11", %"genBinOp generated operation"
  br i1 %"genBinOp generated operation8", label %"after loop6", label %"loop body5"

"after loop6":                                    ; preds = %"loop body5", %"loop condition4.preheader"
  ret %struct.string* %"casting char ptr to typed ptr"
}

define noalias %struct.string* @__String_prepend_char(%procStructType* nocapture, %struct.string* nocapture, i8) nounwind {
prepend_charStartB:
  %3 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %3 to %struct.string*
  %4 = getelementptr %struct.string* %1, i64 0, i32 0
  %5 = load i64* %4, align 8
  %"genBinOp generated operation" = add i64 %5, 1
  %6 = bitcast i8* %3 to i64*
  store i64 %"genBinOp generated operation", i64* %6, align 8
  %7 = getelementptr i8* %3, i64 8
  %8 = bitcast i8* %7 to i8**
  %"genBinOp generated operation.i" = add i64 %5, 2
  %9 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %9, i8** %8, align 8
  %10 = getelementptr inbounds i8* %9, i64 %"genBinOp generated operation"
  store i8 0, i8* %10, align 1
  store i8 %2, i8* %9, align 1
  %"genBinOp generated operation119" = icmp slt i64 %5, 1
  br i1 %"genBinOp generated operation119", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %prepend_charStartB
  %11 = getelementptr %struct.string* %1, i64 0, i32 1
  %.pre = load i8** %11, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.020 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation2", %"loop body" ]
  %"genBinOp generated operation.i18" = add i64 %i.020, -1
  %12 = getelementptr inbounds i8* %.pre, i64 %"genBinOp generated operation.i18"
  %13 = load i8* %12, align 1
  %14 = getelementptr inbounds i8* %9, i64 %i.020
  store i8 %13, i8* %14, align 1
  %"genBinOp generated operation2" = add i64 %i.020, 1
  %"genBinOp generated operation1" = icmp sgt i64 %"genBinOp generated operation2", %5
  br i1 %"genBinOp generated operation1", label %"after loop", label %"loop body"

"after loop":                                     ; preds = %"loop body", %prepend_charStartB
  ret %struct.string* %"casting char ptr to typed ptr"
}

define noalias %struct.string* @__String_append_char(%procStructType* nocapture, %struct.string* nocapture, i8) nounwind {
append_charStartB:
  %3 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %3 to %struct.string*
  %4 = getelementptr %struct.string* %1, i64 0, i32 0
  %5 = load i64* %4, align 8
  %"genBinOp generated operation" = add i64 %5, 1
  %6 = bitcast i8* %3 to i64*
  store i64 %"genBinOp generated operation", i64* %6, align 8
  %7 = getelementptr i8* %3, i64 8
  %8 = bitcast i8* %7 to i8**
  %"genBinOp generated operation.i" = add i64 %5, 2
  %9 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %9, i8** %8, align 8
  %10 = getelementptr inbounds i8* %9, i64 %"genBinOp generated operation"
  store i8 0, i8* %10, align 1
  %"genBinOp generated operation115" = icmp slt i64 %5, 1
  br i1 %"genBinOp generated operation115", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %append_charStartB
  %11 = getelementptr %struct.string* %1, i64 0, i32 1
  %.pre = load i8** %11, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %"genBinOp generated operation417" = phi i64 [ 0, %"loop body.lr.ph" ], [ %i.016, %"loop body" ]
  %i.016 = phi i64 [ 1, %"loop body.lr.ph" ], [ %"genBinOp generated operation3", %"loop body" ]
  %12 = getelementptr inbounds i8* %.pre, i64 %"genBinOp generated operation417"
  %13 = load i8* %12, align 1
  %14 = getelementptr inbounds i8* %9, i64 %"genBinOp generated operation417"
  store i8 %13, i8* %14, align 1
  %"genBinOp generated operation3" = add i64 %i.016, 1
  %"genBinOp generated operation1" = icmp sgt i64 %"genBinOp generated operation3", %5
  br i1 %"genBinOp generated operation1", label %"after loop", label %"loop body"

"after loop":                                     ; preds = %"loop body", %append_charStartB
  %"genBinOp generated operation4.lcssa" = phi i64 [ 0, %append_charStartB ], [ %i.016, %"loop body" ]
  %15 = getelementptr inbounds i8* %9, i64 %"genBinOp generated operation4.lcssa"
  store i8 %2, i8* %15, align 1
  ret %struct.string* %"casting char ptr to typed ptr"
}

define noalias %struct.string* @__String_substring(%procStructType* nocapture, %struct.string* nocapture, i64, i64) nounwind {
substringStartB:
  %4 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %4 to %struct.string*
  %"genBinOp generated operation" = sub i64 %3, %2
  %5 = bitcast i8* %4 to i64*
  store i64 %"genBinOp generated operation", i64* %5, align 8
  %6 = getelementptr i8* %4, i64 8
  %7 = bitcast i8* %6 to i8**
  %"genBinOp generated operation.i" = add i64 %"genBinOp generated operation", 1
  %8 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %8, i8** %7, align 8
  %9 = getelementptr inbounds i8* %8, i64 %"genBinOp generated operation"
  store i8 0, i8* %9, align 1
  %"genBinOp generated operation112" = icmp slt i64 %2, %3
  br i1 %"genBinOp generated operation112", label %"loop body.lr.ph", label %"after loop"

"loop body.lr.ph":                                ; preds = %substringStartB
  %10 = getelementptr %struct.string* %1, i64 0, i32 1
  %.pre = load i8** %10, align 8
  br label %"loop body"

"loop body":                                      ; preds = %"loop body", %"loop body.lr.ph"
  %i.013 = phi i64 [ %2, %"loop body.lr.ph" ], [ %"genBinOp generated operation3", %"loop body" ]
  %"genBinOp generated operation2" = sub i64 %i.013, %2
  %"genBinOp generated operation.i11" = add i64 %i.013, -1
  %11 = getelementptr inbounds i8* %.pre, i64 %"genBinOp generated operation.i11"
  %12 = load i8* %11, align 1
  %13 = getelementptr inbounds i8* %8, i64 %"genBinOp generated operation2"
  store i8 %12, i8* %13, align 1
  %"genBinOp generated operation3" = add i64 %i.013, 1
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
  %3 = getelementptr %Socket* %1, i64 0, i32 0
  store i64 %2, i64* %3, align 8
  ret void
}

define void @__Socket_make_server(%procStructType* nocapture, %Socket* nocapture, i64) nounwind {
merge:
  %local_addr.i.i = alloca %struct.sockaddr_in, align 4
  %enable.i.i = alloca i32, align 4
  %3 = getelementptr %Socket* %1, i64 0, i32 0
  %4 = bitcast i32* %enable.i.i to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %4) nounwind
  %5 = call i32 @socket(i32 2, i32 1, i32 0) nounwind
  store i32 1, i32* %enable.i.i, align 4
  %6 = call i32 @setsockopt(i32 %5, i32 1, i32 2, i8* %4, i32 4) nounwind
  call void @llvm.lifetime.end(i64 -1, i8* %4) nounwind
  %7 = zext i32 %5 to i64
  store i64 %7, i64* %3, align 8
  %8 = bitcast %struct.sockaddr_in* %local_addr.i.i to i8*
  call void @llvm.lifetime.start(i64 -1, i8* %8) nounwind
  %9 = getelementptr inbounds %struct.sockaddr_in* %local_addr.i.i, i64 0, i32 0
  store i16 2, i16* %9, align 4
  %10 = getelementptr inbounds %struct.sockaddr_in* %local_addr.i.i, i64 0, i32 2, i32 0
  store i32 0, i32* %10, align 4
  %11 = trunc i64 %2 to i16
  %12 = call zeroext i16 @htons(i16 zeroext %11) nounwind readnone
  %13 = getelementptr inbounds %struct.sockaddr_in* %local_addr.i.i, i64 0, i32 1
  store i16 %12, i16* %13, align 2
  %14 = bitcast %struct.sockaddr_in* %local_addr.i.i to %struct.sockaddr*
  %15 = call i32 @bind(i32 %5, %struct.sockaddr* %14, i32 16) nounwind
  call void @llvm.lifetime.end(i64 -1, i8* %8) nounwind
  %"equality check1" = icmp eq i32 %15, 0
  br i1 %"equality check1", label %merge4, label %then2

then2:                                            ; preds = %merge
  %16 = call i64 @write(i32 1, i8* getelementptr inbounds ([23 x i8]* @"Could not bind socket\0A_global", i64 0, i64 0), i64 22) nounwind
  call void @__Prelude_exit_with(i64 1)
  unreachable

merge4:                                           ; preds = %merge
  ret void
}

define void @__Socket_listen(%procStructType* nocapture, %Socket* nocapture, i64) nounwind {
listenStartB:
  %3 = getelementptr %Socket* %1, i64 0, i32 0
  %4 = load i64* %3, align 8
  %5 = trunc i64 %4 to i32
  %6 = trunc i64 %2 to i32
  %7 = tail call i32 @listen(i32 %5, i32 %6) nounwind
  %"equality check" = icmp eq i32 %7, 0
  br i1 %"equality check", label %merge, label %then

then:                                             ; preds = %listenStartB
  tail call void @__Prelude_exit_with(i64 1)
  unreachable

merge:                                            ; preds = %listenStartB
  ret void
}

define noalias %Socket* @__Socket_accept(%procStructType* nocapture, %Socket* nocapture) nounwind {
merge:
  %2 = getelementptr %Socket* %1, i64 0, i32 0
  %3 = load i64* %2, align 8
  %4 = trunc i64 %3 to i32
  %5 = tail call i32 @accept(i32 %4, %struct.sockaddr* null, i32* null) nounwind
  %6 = zext i32 %5 to i64
  %7 = tail call noalias i8* @malloc(i64 8) nounwind
  %"casting char ptr to typed ptr1" = bitcast i8* %7 to %Socket*
  %8 = bitcast i8* %7 to i64*
  store i64 %6, i64* %8, align 8
  ret %Socket* %"casting char ptr to typed ptr1"
}

define noalias %struct.string* @__Socket_recv(%procStructType* nocapture, %Socket* nocapture) nounwind {
recvStartB:
  %2 = tail call noalias i8* @malloc(i64 16) nounwind
  %3 = bitcast i8* %2 to i64*
  %4 = getelementptr i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  %6 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %6, i8** %5, align 8
  %7 = getelementptr inbounds i8* %6, i64 1024
  store i8 0, i8* %7, align 1
  %8 = getelementptr %Socket* %1, i64 0, i32 0
  %9 = load i64* %8, align 8
  %10 = trunc i64 %9 to i32
  %11 = tail call i64 @recv(i32 %10, i8* %6, i64 1024, i32 0) nounwind
  store i64 %11, i64* %3, align 8
  switch i64 %11, label %else [
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
  %3 = getelementptr %Socket* %1, i64 0, i32 0
  %4 = load i64* %3, align 8
  %5 = getelementptr %struct.string* %2, i64 0, i32 1
  %6 = load i8** %5, align 8
  %7 = getelementptr %struct.string* %2, i64 0, i32 0
  %8 = load i64* %7, align 8
  %9 = trunc i64 %4 to i32
  %sext.i = shl i64 %8, 32
  %10 = ashr exact i64 %sext.i, 32
  %11 = tail call i64 @send(i32 %9, i8* %6, i64 %10, i32 0) nounwind
  %12 = and i64 %11, 4294967295
  ret i64 %12
}

define void @__Socket_send_all(%procStructType* nocapture, %Socket* nocapture, %struct.string* nocapture) nounwind {
send_allStartB:
  %3 = getelementptr %struct.string* %2, i64 0, i32 0
  %4 = load i64* %3, align 8
  %"genBinOp generated operation5" = icmp slt i64 %4, 1
  br i1 %"genBinOp generated operation5", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %send_allStartB
  %5 = getelementptr %Socket* %1, i64 0, i32 0
  br label %"loop body"

"loop body":                                      ; preds = %"loop condition.backedge", %"loop body.lr.ph"
  %6 = phi i64 [ %4, %"loop body.lr.ph" ], [ %"genBinOp generated operation.i", %"loop condition.backedge" ]
  %7 = phi i64* [ %3, %"loop body.lr.ph" ], [ %17, %"loop condition.backedge" ]
  %str.06 = phi %struct.string* [ %2, %"loop body.lr.ph" ], [ %"casting char ptr to typed ptr.i", %"loop condition.backedge" ]
  %8 = load i64* %5, align 8
  %9 = getelementptr %struct.string* %str.06, i64 0, i32 1
  %10 = load i8** %9, align 8
  %11 = trunc i64 %8 to i32
  %sext.i.i = shl i64 %6, 32
  %12 = ashr exact i64 %sext.i.i, 32
  %13 = tail call i64 @send(i32 %11, i8* %10, i64 %12, i32 0) nounwind
  %14 = and i64 %13, 4294967295
  %"genBinOp generated operation1" = add i64 %14, 1
  %15 = load i64* %7, align 8
  %"genBinOp generated operation2" = add i64 %15, 1
  %16 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i" = bitcast i8* %16 to %struct.string*
  %"genBinOp generated operation.i" = sub i64 %"genBinOp generated operation2", %"genBinOp generated operation1"
  %17 = bitcast i8* %16 to i64*
  store i64 %"genBinOp generated operation.i", i64* %17, align 8
  %18 = getelementptr i8* %16, i64 8
  %19 = bitcast i8* %18 to i8**
  %"genBinOp generated operation.i.i" = add i64 %"genBinOp generated operation.i", 1
  %20 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i") nounwind
  store i8* %20, i8** %19, align 8
  %21 = getelementptr inbounds i8* %20, i64 %"genBinOp generated operation.i"
  store i8 0, i8* %21, align 1
  %"genBinOp generated operation112.i" = icmp slt i64 %"genBinOp generated operation1", %"genBinOp generated operation2"
  br i1 %"genBinOp generated operation112.i", label %"loop body.lr.ph.i", label %"loop condition.backedge"

"loop condition.backedge":                        ; preds = %"loop body.i", %"loop body"
  %"genBinOp generated operation" = icmp slt i64 %"genBinOp generated operation.i", 1
  br i1 %"genBinOp generated operation", label %"after loop", label %"loop body"

"loop body.lr.ph.i":                              ; preds = %"loop body"
  %.pre.i = load i8** %9, align 8
  br label %"loop body.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.013.i = phi i64 [ %"genBinOp generated operation1", %"loop body.lr.ph.i" ], [ %"genBinOp generated operation3.i", %"loop body.i" ]
  %"genBinOp generated operation2.i" = sub i64 %i.013.i, %"genBinOp generated operation1"
  %"genBinOp generated operation.i11.i" = add i64 %i.013.i, -1
  %22 = getelementptr inbounds i8* %.pre.i, i64 %"genBinOp generated operation.i11.i"
  %23 = load i8* %22, align 1
  %24 = getelementptr inbounds i8* %20, i64 %"genBinOp generated operation2.i"
  store i8 %23, i8* %24, align 1
  %"genBinOp generated operation3.i" = add i64 %i.013.i, 1
  %exitcond.i = icmp eq i64 %"genBinOp generated operation3.i", %"genBinOp generated operation2"
  br i1 %exitcond.i, label %"loop condition.backedge", label %"loop body.i"

"after loop":                                     ; preds = %"loop condition.backedge", %send_allStartB
  ret void
}

define void @__Socket_close(%procStructType* nocapture, %Socket* nocapture) nounwind {
closeStartB:
  %2 = getelementptr %Socket* %1, i64 0, i32 0
  %3 = load i64* %2, align 8
  %4 = trunc i64 %3 to i32
  %5 = tail call i32 @close(i32 %4) nounwind
  ret void
}

define void @__Socket_Buffer_make(%procStructType* nocapture, %Socket_Buffer* nocapture, %Socket*) nounwind {
makeStartB:
  %3 = getelementptr %Socket_Buffer* %1, i64 0, i32 0
  store %Socket* %2, %Socket** %3, align 8
  %4 = getelementptr %Socket_Buffer* %1, i64 0, i32 1
  %5 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %5 to %struct.string*
  %6 = bitcast i8* %5 to i64*
  store i64 0, i64* %6, align 8
  %7 = getelementptr i8* %5, i64 8
  %8 = bitcast i8* %7 to i8**
  store i8* getelementptr inbounds ([1 x i8]* @_global, i64 0, i64 0), i8** %8, align 8
  store %struct.string* %"casting char ptr to typed ptr", %struct.string** %4, align 8
  ret void
}

define %struct.string* @__Socket_Buffer_read_line(%procStructType* nocapture, %Socket_Buffer* nocapture) nounwind {
read_lineStartB:
  %2 = getelementptr %Socket_Buffer* %1, i64 0, i32 1
  %3 = load %struct.string** %2, align 8
  %"equality check.i" = icmp eq %struct.string* %3, null
  br i1 %"equality check.i", label %then, label %else.i

else.i:                                           ; preds = %read_lineStartB
  %4 = getelementptr %struct.string* %3, i64 0, i32 0
  %5 = load i64* %4, align 8
  %"genBinOp generated operation11.i.i" = icmp slt i64 %5, 1
  br i1 %"genBinOp generated operation11.i.i", label %__String_find.exit31.i, label %"loop body.lr.ph.i.i"

"loop body.lr.ph.i.i":                            ; preds = %else.i
  %6 = getelementptr %struct.string* %3, i64 0, i32 1
  %7 = load i8** %6, align 8
  br label %"loop body.i.i"

"loop body.i.i":                                  ; preds = %"loop body.i.i", %"loop body.lr.ph.i.i"
  %i.012.i.i = phi i64 [ 1, %"loop body.lr.ph.i.i" ], [ %"genBinOp generated operation2.i.i", %"loop body.i.i" ]
  %"genBinOp generated operation.i.i.i" = add i64 %i.012.i.i, -1
  %8 = getelementptr inbounds i8* %7, i64 %"genBinOp generated operation.i.i.i"
  %9 = load i8* %8, align 1
  %"equality check.i.i" = icmp eq i8 %9, 13
  %"genBinOp generated operation2.i.i" = add i64 %i.012.i.i, 1
  %"genBinOp generated operation.i.i" = icmp sgt i64 %"genBinOp generated operation2.i.i", %5
  %"genBinOp generated operation1.i.i" = or i1 %"genBinOp generated operation.i.i", %"equality check.i.i"
  br i1 %"genBinOp generated operation1.i.i", label %"loop body.i27.i", label %"loop body.i.i"

"loop body.i27.i":                                ; preds = %"loop body.i27.i", %"loop body.i.i"
  %i.012.i21.i = phi i64 [ %"genBinOp generated operation2.i24.i", %"loop body.i27.i" ], [ 1, %"loop body.i.i" ]
  %"genBinOp generated operation.i.i22.i" = add i64 %i.012.i21.i, -1
  %10 = getelementptr inbounds i8* %7, i64 %"genBinOp generated operation.i.i22.i"
  %11 = load i8* %10, align 1
  %"equality check.i23.i" = icmp eq i8 %11, 10
  %"genBinOp generated operation2.i24.i" = add i64 %i.012.i21.i, 1
  %"genBinOp generated operation.i25.i" = icmp sgt i64 %"genBinOp generated operation2.i24.i", %5
  %"genBinOp generated operation1.i26.i" = or i1 %"genBinOp generated operation.i25.i", %"equality check.i23.i"
  br i1 %"genBinOp generated operation1.i26.i", label %"loop condition.after loop_crit_edge.i29.i", label %"loop body.i27.i"

"loop condition.after loop_crit_edge.i29.i":      ; preds = %"loop body.i27.i"
  %phitmp.i.i = select i1 %"equality check.i.i", i64 %i.012.i.i, i64 -1
  %phitmp.i28.i = select i1 %"equality check.i23.i", i64 %i.012.i21.i, i64 -1
  br label %__String_find.exit31.i

__String_find.exit31.i:                           ; preds = %"loop condition.after loop_crit_edge.i29.i", %else.i
  %found.0.lcssa.i33.i = phi i64 [ %phitmp.i.i, %"loop condition.after loop_crit_edge.i29.i" ], [ -1, %else.i ]
  %found.0.lcssa.i30.i = phi i64 [ %phitmp.i28.i, %"loop condition.after loop_crit_edge.i29.i" ], [ -1, %else.i ]
  %"genBinOp generated operation.i" = add i64 %found.0.lcssa.i33.i, 1
  %"equality check1.i" = icmp eq i64 %found.0.lcssa.i30.i, %"genBinOp generated operation.i"
  br i1 %"equality check1.i", label %then2.i, label %elseIf25.i

then2.i:                                          ; preds = %__String_find.exit31.i
  %"genBinOp generated operation33.i" = add i64 %found.0.lcssa.i30.i, 1
  br label %__Socket_Buffer_find_cutoff.exit

elseIf9.i:                                        ; preds = %elseIf13.i
  %"genBinOp generated operation11.i" = icmp slt i64 %found.0.lcssa.i30.i, %found.0.lcssa.i33.i
  %"genBinOp generated operation12.i" = add i64 %found.0.lcssa.i30.i, 1
  %"genBinOp generated operation12..i" = select i1 %"genBinOp generated operation11.i", i64 %"genBinOp generated operation12.i", i64 1
  br label %__Socket_Buffer_find_cutoff.exit

elseIf13.i:                                       ; preds = %elseIf17.i
  %"genBinOp generated operation15.i" = icmp slt i64 %found.0.lcssa.i33.i, %found.0.lcssa.i30.i
  br i1 %"genBinOp generated operation15.i", label %__Socket_Buffer_find_cutoff.exit, label %elseIf9.i

elseIf17.i:                                       ; preds = %elseIf25.i
  %"equality check20.i" = icmp eq i64 %found.0.lcssa.i33.i, -1
  %"equality check22.i" = icmp ne i64 %found.0.lcssa.i30.i, -1
  %"genBinOp generated operation23.i" = and i1 %"equality check20.i", %"equality check22.i"
  br i1 %"genBinOp generated operation23.i", label %elseIfThen18.i, label %elseIf13.i

elseIfThen18.i:                                   ; preds = %elseIf17.i
  %"genBinOp generated operation24.i" = add i64 %found.0.lcssa.i30.i, 1
  br label %__Socket_Buffer_find_cutoff.exit

elseIf25.i:                                       ; preds = %__String_find.exit31.i
  %"equality check28.i" = icmp eq i64 %found.0.lcssa.i30.i, -1
  %"equality check30.i" = icmp ne i64 %found.0.lcssa.i33.i, -1
  %"genBinOp generated operation31.i" = and i1 %"equality check28.i", %"equality check30.i"
  br i1 %"genBinOp generated operation31.i", label %__Socket_Buffer_find_cutoff.exit, label %elseIf17.i

__Socket_Buffer_find_cutoff.exit:                 ; preds = %elseIf25.i, %elseIfThen18.i, %elseIf13.i, %elseIf9.i, %then2.i
  %12 = phi i64 [ %"genBinOp generated operation12..i", %elseIf9.i ], [ %"genBinOp generated operation33.i", %then2.i ], [ %"genBinOp generated operation24.i", %elseIfThen18.i ], [ %"genBinOp generated operation.i", %elseIf13.i ], [ %"genBinOp generated operation.i", %elseIf25.i ]
  %"equality check" = icmp eq i64 %12, 1
  br i1 %"equality check", label %then, label %else

then:                                             ; preds = %__Socket_Buffer_find_cutoff.exit, %read_lineStartB
  %13 = getelementptr %Socket_Buffer* %1, i64 0, i32 0
  %14 = load %Socket** %13, align 8
  %15 = tail call noalias i8* @malloc(i64 16) nounwind
  %16 = bitcast i8* %15 to i64*
  %17 = getelementptr i8* %15, i64 8
  %18 = bitcast i8* %17 to i8**
  %19 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %19, i8** %18, align 8
  %20 = getelementptr inbounds i8* %19, i64 1024
  store i8 0, i8* %20, align 1
  %21 = getelementptr %Socket* %14, i64 0, i32 0
  %22 = load i64* %21, align 8
  %23 = trunc i64 %22 to i32
  %24 = tail call i64 @recv(i32 %23, i8* %19, i64 1024, i32 0) nounwind
  store i64 %24, i64* %16, align 8
  switch i64 %24, label %"loop condition.preheader" [
    i64 -1, label %then16
    i64 0, label %then16
  ]

"loop condition.preheader":                       ; preds = %then
  %"equality check6180" = icmp eq i8* %15, null
  br i1 %"equality check6180", label %then16, label %"loop body.lr.ph"

else:                                             ; preds = %__Socket_Buffer_find_cutoff.exit
  %25 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i43" = bitcast i8* %25 to %struct.string*
  %"genBinOp generated operation.i44" = add i64 %12, -1
  %26 = bitcast i8* %25 to i64*
  store i64 %"genBinOp generated operation.i44", i64* %26, align 8
  %27 = getelementptr i8* %25, i64 8
  %28 = bitcast i8* %27 to i8**
  %29 = tail call noalias i8* @malloc(i64 %12) nounwind
  store i8* %29, i8** %28, align 8
  %30 = getelementptr inbounds i8* %29, i64 %"genBinOp generated operation.i44"
  store i8 0, i8* %30, align 1
  %"genBinOp generated operation112.i45" = icmp sgt i64 %12, 1
  br i1 %"genBinOp generated operation112.i45", label %"loop body.lr.ph.i47", label %__String_substring.exit54

"loop body.lr.ph.i47":                            ; preds = %else
  %31 = getelementptr %struct.string* %3, i64 0, i32 1
  %.pre.i46 = load i8** %31, align 8
  br label %"loop body.i53"

"loop body.i53":                                  ; preds = %"loop body.i53", %"loop body.lr.ph.i47"
  %i.013.i48 = phi i64 [ 1, %"loop body.lr.ph.i47" ], [ %"genBinOp generated operation3.i51", %"loop body.i53" ]
  %"genBinOp generated operation2.i49" = add i64 %i.013.i48, -1
  %32 = getelementptr inbounds i8* %.pre.i46, i64 %"genBinOp generated operation2.i49"
  %33 = load i8* %32, align 1
  %34 = getelementptr inbounds i8* %29, i64 %"genBinOp generated operation2.i49"
  store i8 %33, i8* %34, align 1
  %"genBinOp generated operation3.i51" = add i64 %i.013.i48, 1
  %exitcond.i52 = icmp eq i64 %"genBinOp generated operation3.i51", %12
  br i1 %exitcond.i52, label %__String_substring.exit54, label %"loop body.i53"

__String_substring.exit54:                        ; preds = %"loop body.i53", %else
  %35 = load i64* %4, align 8
  %"genBinOp generated operation20" = add i64 %35, 1
  %36 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i55" = bitcast i8* %36 to %struct.string*
  %"genBinOp generated operation.i56" = sub i64 %"genBinOp generated operation20", %12
  %37 = bitcast i8* %36 to i64*
  store i64 %"genBinOp generated operation.i56", i64* %37, align 8
  %38 = getelementptr i8* %36, i64 8
  %39 = bitcast i8* %38 to i8**
  %"genBinOp generated operation.i.i57" = add i64 %"genBinOp generated operation.i56", 1
  %40 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i57") nounwind
  store i8* %40, i8** %39, align 8
  %41 = getelementptr inbounds i8* %40, i64 %"genBinOp generated operation.i56"
  store i8 0, i8* %41, align 1
  %"genBinOp generated operation112.i58" = icmp slt i64 %12, %"genBinOp generated operation20"
  br i1 %"genBinOp generated operation112.i58", label %"loop body.lr.ph.i60", label %__String_substring.exit67

"loop body.lr.ph.i60":                            ; preds = %__String_substring.exit54
  %42 = getelementptr %struct.string* %3, i64 0, i32 1
  %.pre.i59 = load i8** %42, align 8
  br label %"loop body.i66"

"loop body.i66":                                  ; preds = %"loop body.i66", %"loop body.lr.ph.i60"
  %i.013.i61 = phi i64 [ %12, %"loop body.lr.ph.i60" ], [ %"genBinOp generated operation3.i64", %"loop body.i66" ]
  %"genBinOp generated operation2.i62" = sub i64 %i.013.i61, %12
  %"genBinOp generated operation.i11.i63" = add i64 %i.013.i61, -1
  %43 = getelementptr inbounds i8* %.pre.i59, i64 %"genBinOp generated operation.i11.i63"
  %44 = load i8* %43, align 1
  %45 = getelementptr inbounds i8* %40, i64 %"genBinOp generated operation2.i62"
  store i8 %44, i8* %45, align 1
  %"genBinOp generated operation3.i64" = add i64 %i.013.i61, 1
  %exitcond.i65 = icmp eq i64 %"genBinOp generated operation3.i64", %"genBinOp generated operation20"
  br i1 %exitcond.i65, label %__String_substring.exit67, label %"loop body.i66"

__String_substring.exit67:                        ; preds = %"loop body.i66", %__String_substring.exit54
  store %struct.string* %"casting char ptr to typed ptr.i55", %struct.string** %2, align 8
  br label %merge

merge:                                            ; preds = %then16, %__String_substring.exit, %"after loop", %__String_substring.exit67
  %Result.0 = phi %struct.string* [ %114, %then16 ], [ %Result.1175, %"after loop" ], [ %"casting char ptr to typed ptr.i43", %__String_substring.exit67 ], [ %"casting char ptr to typed ptr.i142", %__String_substring.exit ]
  ret %struct.string* %Result.0

else.i70:                                         ; preds = %__String_substring.exit, %"loop body.lr.ph"
  %Result.1175 = phi %struct.string* [ null, %"loop body.lr.ph" ], [ %"casting char ptr to typed ptr.i142", %__String_substring.exit ]
  %46 = load i64* %81, align 8
  %"genBinOp generated operation11.i.i69" = icmp slt i64 %46, 1
  br i1 %"genBinOp generated operation11.i.i69", label %__String_find.exit31.i94, label %"loop body.lr.ph.i.i71"

"loop body.lr.ph.i.i71":                          ; preds = %else.i70
  %47 = load i8** %83, align 8
  br label %"loop body.i.i78"

"loop body.i.i78":                                ; preds = %"loop body.i.i78", %"loop body.lr.ph.i.i71"
  %i.012.i.i72 = phi i64 [ 1, %"loop body.lr.ph.i.i71" ], [ %"genBinOp generated operation2.i.i75", %"loop body.i.i78" ]
  %"genBinOp generated operation.i.i.i73" = add i64 %i.012.i.i72, -1
  %48 = getelementptr inbounds i8* %47, i64 %"genBinOp generated operation.i.i.i73"
  %49 = load i8* %48, align 1
  %"equality check.i.i74" = icmp eq i8 %49, 13
  %"genBinOp generated operation2.i.i75" = add i64 %i.012.i.i72, 1
  %"genBinOp generated operation.i.i76" = icmp sgt i64 %"genBinOp generated operation2.i.i75", %46
  %"genBinOp generated operation1.i.i77" = or i1 %"genBinOp generated operation.i.i76", %"equality check.i.i74"
  br i1 %"genBinOp generated operation1.i.i77", label %"loop body.i27.i87", label %"loop body.i.i78"

"loop body.i27.i87":                              ; preds = %"loop body.i27.i87", %"loop body.i.i78"
  %i.012.i21.i81 = phi i64 [ %"genBinOp generated operation2.i24.i84", %"loop body.i27.i87" ], [ 1, %"loop body.i.i78" ]
  %"genBinOp generated operation.i.i22.i82" = add i64 %i.012.i21.i81, -1
  %50 = getelementptr inbounds i8* %47, i64 %"genBinOp generated operation.i.i22.i82"
  %51 = load i8* %50, align 1
  %"equality check.i23.i83" = icmp eq i8 %51, 10
  %"genBinOp generated operation2.i24.i84" = add i64 %i.012.i21.i81, 1
  %"genBinOp generated operation.i25.i85" = icmp sgt i64 %"genBinOp generated operation2.i24.i84", %46
  %"genBinOp generated operation1.i26.i86" = or i1 %"genBinOp generated operation.i25.i85", %"equality check.i23.i83"
  br i1 %"genBinOp generated operation1.i26.i86", label %"loop condition.after loop_crit_edge.i29.i89", label %"loop body.i27.i87"

"loop condition.after loop_crit_edge.i29.i89":    ; preds = %"loop body.i27.i87"
  %phitmp.i.i79 = select i1 %"equality check.i.i74", i64 %i.012.i.i72, i64 -1
  %phitmp.i28.i88 = select i1 %"equality check.i23.i83", i64 %i.012.i21.i81, i64 -1
  br label %__String_find.exit31.i94

__String_find.exit31.i94:                         ; preds = %"loop condition.after loop_crit_edge.i29.i89", %else.i70
  %found.0.lcssa.i33.i90 = phi i64 [ %phitmp.i.i79, %"loop condition.after loop_crit_edge.i29.i89" ], [ -1, %else.i70 ]
  %found.0.lcssa.i30.i91 = phi i64 [ %phitmp.i28.i88, %"loop condition.after loop_crit_edge.i29.i89" ], [ -1, %else.i70 ]
  %"genBinOp generated operation.i92" = add i64 %found.0.lcssa.i33.i90, 1
  %"equality check1.i93" = icmp eq i64 %found.0.lcssa.i30.i91, %"genBinOp generated operation.i92"
  br i1 %"equality check1.i93", label %then2.i98, label %elseIf25.i114

then2.i98:                                        ; preds = %__String_find.exit31.i94
  %"genBinOp generated operation33.i97" = add i64 %found.0.lcssa.i30.i91, 1
  br label %__Socket_Buffer_find_cutoff.exit115

elseIf9.i102:                                     ; preds = %elseIf13.i104
  %"genBinOp generated operation11.i99" = icmp slt i64 %found.0.lcssa.i30.i91, %found.0.lcssa.i33.i90
  %"genBinOp generated operation12.i100" = add i64 %found.0.lcssa.i30.i91, 1
  %"genBinOp generated operation12..i101" = select i1 %"genBinOp generated operation11.i99", i64 %"genBinOp generated operation12.i100", i64 1
  br label %__Socket_Buffer_find_cutoff.exit115

elseIf13.i104:                                    ; preds = %elseIf17.i108
  %"genBinOp generated operation15.i103" = icmp slt i64 %found.0.lcssa.i33.i90, %found.0.lcssa.i30.i91
  br i1 %"genBinOp generated operation15.i103", label %__Socket_Buffer_find_cutoff.exit115, label %elseIf9.i102

elseIf17.i108:                                    ; preds = %elseIf25.i114
  %"equality check20.i105" = icmp eq i64 %found.0.lcssa.i33.i90, -1
  %"equality check22.i106" = icmp ne i64 %found.0.lcssa.i30.i91, -1
  %"genBinOp generated operation23.i107" = and i1 %"equality check20.i105", %"equality check22.i106"
  br i1 %"genBinOp generated operation23.i107", label %elseIfThen18.i110, label %elseIf13.i104

elseIfThen18.i110:                                ; preds = %elseIf17.i108
  %"genBinOp generated operation24.i109" = add i64 %found.0.lcssa.i30.i91, 1
  br label %__Socket_Buffer_find_cutoff.exit115

elseIf25.i114:                                    ; preds = %__String_find.exit31.i94
  %"equality check28.i111" = icmp eq i64 %found.0.lcssa.i30.i91, -1
  %"equality check30.i112" = icmp ne i64 %found.0.lcssa.i33.i90, -1
  %"genBinOp generated operation31.i113" = and i1 %"equality check28.i111", %"equality check30.i112"
  br i1 %"genBinOp generated operation31.i113", label %__Socket_Buffer_find_cutoff.exit115, label %elseIf17.i108

__Socket_Buffer_find_cutoff.exit115:              ; preds = %elseIf25.i114, %elseIfThen18.i110, %elseIf13.i104, %elseIf9.i102, %then2.i98
  %52 = phi i64 [ %"genBinOp generated operation12..i101", %elseIf9.i102 ], [ %"genBinOp generated operation33.i97", %then2.i98 ], [ %"genBinOp generated operation24.i109", %elseIfThen18.i110 ], [ %"genBinOp generated operation.i92", %elseIf13.i104 ], [ %"genBinOp generated operation.i92", %elseIf25.i114 ]
  %"equality check7" = icmp eq i64 %52, 1
  %53 = load %struct.string** %2, align 8
  br i1 %"equality check7", label %then8, label %else9

"after loop":                                     ; preds = %"loop condition.outer.backedge"
  br i1 %"equality check6", label %then16, label %merge

then8:                                            ; preds = %__Socket_Buffer_find_cutoff.exit115
  %54 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i116" = bitcast i8* %54 to %struct.string*
  %55 = getelementptr %struct.string* %53, i64 0, i32 0
  %56 = load i64* %55, align 8
  %57 = load i64* %81, align 8
  %"genBinOp generated operation.i117" = add i64 %57, %56
  %58 = bitcast i8* %54 to i64*
  store i64 %"genBinOp generated operation.i117", i64* %58, align 8
  %59 = getelementptr i8* %54, i64 8
  %60 = bitcast i8* %59 to i8**
  %"genBinOp generated operation.i.i118" = add i64 %"genBinOp generated operation.i117", 1
  %61 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i118") nounwind
  store i8* %61, i8** %60, align 8
  %62 = getelementptr inbounds i8* %61, i64 %"genBinOp generated operation.i117"
  store i8 0, i8* %62, align 1
  %"genBinOp generated operation128.i" = icmp slt i64 %56, 1
  br i1 %"genBinOp generated operation128.i", label %"loop condition4.preheader.i", label %"loop body.lr.ph.i119"

"loop body.lr.ph.i119":                           ; preds = %then8
  %63 = getelementptr %struct.string* %53, i64 0, i32 1
  %.pre30.i = load i8** %63, align 8
  br label %"loop body.i123"

"loop condition4.preheader.i":                    ; preds = %"loop body.i123", %then8
  %i.0.lcssa.i = phi i64 [ 1, %then8 ], [ %"genBinOp generated operation3.i122", %"loop body.i123" ]
  %"genBinOp generated operation826.i" = icmp sgt i64 %i.0.lcssa.i, %"genBinOp generated operation.i117"
  br i1 %"genBinOp generated operation826.i", label %__String_append.exit, label %"loop body5.lr.ph.i"

"loop body5.lr.ph.i":                             ; preds = %"loop condition4.preheader.i"
  %.pre.i120 = load i8** %83, align 8
  br label %"loop body5.i"

"loop body.i123":                                 ; preds = %"loop body.i123", %"loop body.lr.ph.i119"
  %i.029.i = phi i64 [ 1, %"loop body.lr.ph.i119" ], [ %"genBinOp generated operation3.i122", %"loop body.i123" ]
  %"genBinOp generated operation2.i121" = add i64 %i.029.i, -1
  %64 = getelementptr inbounds i8* %.pre30.i, i64 %"genBinOp generated operation2.i121"
  %65 = load i8* %64, align 1
  %66 = getelementptr inbounds i8* %61, i64 %"genBinOp generated operation2.i121"
  store i8 %65, i8* %66, align 1
  %"genBinOp generated operation3.i122" = add i64 %i.029.i, 1
  %"genBinOp generated operation1.i" = icmp sgt i64 %"genBinOp generated operation3.i122", %56
  br i1 %"genBinOp generated operation1.i", label %"loop condition4.preheader.i", label %"loop body.i123"

"loop body5.i":                                   ; preds = %"loop body5.i", %"loop body5.lr.ph.i"
  %i.127.i = phi i64 [ %i.0.lcssa.i, %"loop body5.lr.ph.i" ], [ %"genBinOp generated operation11.i124", %"loop body5.i" ]
  %"genBinOp generated operation9.i" = add i64 %i.127.i, -1
  %"genBinOp generated operation.i24.i" = sub i64 %"genBinOp generated operation9.i", %56
  %67 = getelementptr inbounds i8* %.pre.i120, i64 %"genBinOp generated operation.i24.i"
  %68 = load i8* %67, align 1
  %69 = getelementptr inbounds i8* %61, i64 %"genBinOp generated operation9.i"
  store i8 %68, i8* %69, align 1
  %"genBinOp generated operation11.i124" = add i64 %i.127.i, 1
  %"genBinOp generated operation8.i" = icmp sgt i64 %"genBinOp generated operation11.i124", %"genBinOp generated operation.i117"
  br i1 %"genBinOp generated operation8.i", label %__String_append.exit, label %"loop body5.i"

__String_append.exit:                             ; preds = %"loop body5.i", %"loop condition4.preheader.i"
  store %struct.string* %"casting char ptr to typed ptr.i116", %struct.string** %2, align 8
  %70 = load %Socket** %13, align 8
  %71 = tail call noalias i8* @malloc(i64 16) nounwind
  %72 = bitcast i8* %71 to i64*
  %73 = getelementptr i8* %71, i64 8
  %74 = bitcast i8* %73 to i8**
  %75 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %75, i8** %74, align 8
  %76 = getelementptr inbounds i8* %75, i64 1024
  store i8 0, i8* %76, align 1
  %77 = getelementptr %Socket* %70, i64 0, i32 0
  %78 = load i64* %77, align 8
  %79 = trunc i64 %78 to i32
  %80 = tail call i64 @recv(i32 %79, i8* %75, i64 1024, i32 0) nounwind
  store i64 %80, i64* %72, align 8
  switch i64 %80, label %"loop condition.outer.backedge" [
    i64 -1, label %then16
    i64 0, label %then16
  ]

"loop condition.outer.backedge":                  ; preds = %__String_append.exit
  %"equality check2173" = icmp ne %struct.string* %Result.1175, null
  %"equality check6" = icmp eq i8* %71, null
  %"genBinOp generated operation174" = or i1 %"equality check2173", %"equality check6"
  br i1 %"genBinOp generated operation174", label %"after loop", label %"loop body.lr.ph"

"loop body.lr.ph":                                ; preds = %"loop condition.outer.backedge", %"loop condition.preheader"
  %new_str.0.ph167181.in = phi i8* [ %71, %"loop condition.outer.backedge" ], [ %15, %"loop condition.preheader" ]
  %81 = bitcast i8* %new_str.0.ph167181.in to i64*
  %82 = getelementptr i8* %new_str.0.ph167181.in, i64 8
  %83 = bitcast i8* %82 to i8**
  br label %else.i70

else9:                                            ; preds = %__Socket_Buffer_find_cutoff.exit115
  %"genBinOp generated operation.i131" = add i64 %52, -1
  %84 = tail call noalias i8* @malloc(i64 %52) nounwind
  %85 = getelementptr inbounds i8* %84, i64 %"genBinOp generated operation.i131"
  store i8 0, i8* %85, align 1
  %"genBinOp generated operation112.i132" = icmp sgt i64 %52, 1
  br i1 %"genBinOp generated operation112.i132", label %"loop body.lr.ph.i134", label %__String_substring.exit141

"loop body.lr.ph.i134":                           ; preds = %else9
  %.pre.i133 = load i8** %83, align 8
  br label %"loop body.i140"

"loop body.i140":                                 ; preds = %"loop body.i140", %"loop body.lr.ph.i134"
  %i.013.i135 = phi i64 [ 1, %"loop body.lr.ph.i134" ], [ %"genBinOp generated operation3.i138", %"loop body.i140" ]
  %"genBinOp generated operation2.i136" = add i64 %i.013.i135, -1
  %86 = getelementptr inbounds i8* %.pre.i133, i64 %"genBinOp generated operation2.i136"
  %87 = load i8* %86, align 1
  %88 = getelementptr inbounds i8* %84, i64 %"genBinOp generated operation2.i136"
  store i8 %87, i8* %88, align 1
  %"genBinOp generated operation3.i138" = add i64 %i.013.i135, 1
  %exitcond.i139 = icmp eq i64 %"genBinOp generated operation3.i138", %52
  br i1 %exitcond.i139, label %__String_substring.exit141, label %"loop body.i140"

__String_substring.exit141:                       ; preds = %"loop body.i140", %else9
  %89 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i142" = bitcast i8* %89 to %struct.string*
  %90 = getelementptr %struct.string* %53, i64 0, i32 0
  %91 = load i64* %90, align 8
  %"genBinOp generated operation.i143" = add i64 %"genBinOp generated operation.i131", %91
  %92 = bitcast i8* %89 to i64*
  store i64 %"genBinOp generated operation.i143", i64* %92, align 8
  %93 = getelementptr i8* %89, i64 8
  %94 = bitcast i8* %93 to i8**
  %"genBinOp generated operation.i.i144" = add i64 %52, %91
  %95 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i144") nounwind
  store i8* %95, i8** %94, align 8
  %96 = getelementptr inbounds i8* %95, i64 %"genBinOp generated operation.i143"
  store i8 0, i8* %96, align 1
  %"genBinOp generated operation128.i145" = icmp slt i64 %91, 1
  br i1 %"genBinOp generated operation128.i145", label %"loop condition4.preheader.i150", label %"loop body.lr.ph.i147"

"loop body.lr.ph.i147":                           ; preds = %__String_substring.exit141
  %97 = getelementptr %struct.string* %53, i64 0, i32 1
  %.pre30.i146 = load i8** %97, align 8
  br label %"loop body.i157"

"loop condition4.preheader.i150":                 ; preds = %"loop body.i157", %__String_substring.exit141
  %i.0.lcssa.i148 = phi i64 [ 1, %__String_substring.exit141 ], [ %"genBinOp generated operation3.i155", %"loop body.i157" ]
  %"genBinOp generated operation826.i149" = icmp sgt i64 %i.0.lcssa.i148, %"genBinOp generated operation.i143"
  br i1 %"genBinOp generated operation826.i149", label %__String_append.exit164, label %"loop body5.i163"

"loop body.i157":                                 ; preds = %"loop body.i157", %"loop body.lr.ph.i147"
  %i.029.i153 = phi i64 [ 1, %"loop body.lr.ph.i147" ], [ %"genBinOp generated operation3.i155", %"loop body.i157" ]
  %"genBinOp generated operation2.i154" = add i64 %i.029.i153, -1
  %98 = getelementptr inbounds i8* %.pre30.i146, i64 %"genBinOp generated operation2.i154"
  %99 = load i8* %98, align 1
  %100 = getelementptr inbounds i8* %95, i64 %"genBinOp generated operation2.i154"
  store i8 %99, i8* %100, align 1
  %"genBinOp generated operation3.i155" = add i64 %i.029.i153, 1
  %"genBinOp generated operation1.i156" = icmp sgt i64 %"genBinOp generated operation3.i155", %91
  br i1 %"genBinOp generated operation1.i156", label %"loop condition4.preheader.i150", label %"loop body.i157"

"loop body5.i163":                                ; preds = %"loop body5.i163", %"loop condition4.preheader.i150"
  %i.127.i158 = phi i64 [ %"genBinOp generated operation11.i161", %"loop body5.i163" ], [ %i.0.lcssa.i148, %"loop condition4.preheader.i150" ]
  %"genBinOp generated operation9.i159" = add i64 %i.127.i158, -1
  %"genBinOp generated operation.i24.i160" = sub i64 %"genBinOp generated operation9.i159", %91
  %101 = getelementptr inbounds i8* %84, i64 %"genBinOp generated operation.i24.i160"
  %102 = load i8* %101, align 1
  %103 = getelementptr inbounds i8* %95, i64 %"genBinOp generated operation9.i159"
  store i8 %102, i8* %103, align 1
  %"genBinOp generated operation11.i161" = add i64 %i.127.i158, 1
  %"genBinOp generated operation8.i162" = icmp sgt i64 %"genBinOp generated operation11.i161", %"genBinOp generated operation.i143"
  br i1 %"genBinOp generated operation8.i162", label %__String_append.exit164, label %"loop body5.i163"

__String_append.exit164:                          ; preds = %"loop body5.i163", %"loop condition4.preheader.i150"
  %104 = load i64* %81, align 8
  %"genBinOp generated operation11" = add i64 %104, 1
  %105 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i" = bitcast i8* %105 to %struct.string*
  %"genBinOp generated operation.i37" = sub i64 %"genBinOp generated operation11", %52
  %106 = bitcast i8* %105 to i64*
  store i64 %"genBinOp generated operation.i37", i64* %106, align 8
  %107 = getelementptr i8* %105, i64 8
  %108 = bitcast i8* %107 to i8**
  %"genBinOp generated operation.i.i38" = add i64 %"genBinOp generated operation.i37", 1
  %109 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i38") nounwind
  store i8* %109, i8** %108, align 8
  %110 = getelementptr inbounds i8* %109, i64 %"genBinOp generated operation.i37"
  store i8 0, i8* %110, align 1
  %"genBinOp generated operation112.i" = icmp slt i64 %52, %"genBinOp generated operation11"
  br i1 %"genBinOp generated operation112.i", label %"loop body.lr.ph.i", label %__String_substring.exit

"loop body.lr.ph.i":                              ; preds = %__String_append.exit164
  %.pre.i = load i8** %83, align 8
  br label %"loop body.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.013.i = phi i64 [ %52, %"loop body.lr.ph.i" ], [ %"genBinOp generated operation3.i", %"loop body.i" ]
  %"genBinOp generated operation2.i" = sub i64 %i.013.i, %52
  %"genBinOp generated operation.i11.i" = add i64 %i.013.i, -1
  %111 = getelementptr inbounds i8* %.pre.i, i64 %"genBinOp generated operation.i11.i"
  %112 = load i8* %111, align 1
  %113 = getelementptr inbounds i8* %109, i64 %"genBinOp generated operation2.i"
  store i8 %112, i8* %113, align 1
  %"genBinOp generated operation3.i" = add i64 %i.013.i, 1
  %exitcond.i = icmp eq i64 %"genBinOp generated operation3.i", %"genBinOp generated operation11"
  br i1 %exitcond.i, label %__String_substring.exit, label %"loop body.i"

__String_substring.exit:                          ; preds = %"loop body.i", %__String_append.exit164
  store %struct.string* %"casting char ptr to typed ptr.i", %struct.string** %2, align 8
  %"equality check2" = icmp eq i8* %89, null
  br i1 %"equality check2", label %else.i70, label %merge

then16:                                           ; preds = %__String_append.exit, %__String_append.exit, %"after loop", %"loop condition.preheader", %then, %then
  %114 = load %struct.string** %2, align 8
  store %struct.string* null, %struct.string** %2, align 8
  br label %merge
}

define i64 @__Socket_Buffer_find_cutoff(%procStructType* nocapture, %Socket_Buffer* nocapture, %struct.string*) nounwind readonly {
find_cutoffStartB:
  %"equality check" = icmp eq %struct.string* %2, null
  br i1 %"equality check", label %merge, label %else

else:                                             ; preds = %find_cutoffStartB
  %3 = getelementptr %struct.string* %2, i64 0, i32 0
  %4 = load i64* %3, align 8
  %"genBinOp generated operation11.i" = icmp slt i64 %4, 1
  br i1 %"genBinOp generated operation11.i", label %__String_find.exit31, label %"loop body.lr.ph.i"

"loop body.lr.ph.i":                              ; preds = %else
  %5 = getelementptr %struct.string* %2, i64 0, i32 1
  %6 = load i8** %5, align 8
  br label %"loop body.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.012.i = phi i64 [ 1, %"loop body.lr.ph.i" ], [ %"genBinOp generated operation2.i", %"loop body.i" ]
  %"genBinOp generated operation.i.i" = add i64 %i.012.i, -1
  %7 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation.i.i"
  %8 = load i8* %7, align 1
  %"equality check.i" = icmp eq i8 %8, 13
  %"genBinOp generated operation2.i" = add i64 %i.012.i, 1
  %"genBinOp generated operation.i" = icmp sgt i64 %"genBinOp generated operation2.i", %4
  %"genBinOp generated operation1.i" = or i1 %"genBinOp generated operation.i", %"equality check.i"
  br i1 %"genBinOp generated operation1.i", label %"loop body.lr.ph.i20", label %"loop body.i"

"loop body.lr.ph.i20":                            ; preds = %"loop body.i"
  %phitmp.i = select i1 %"equality check.i", i64 %i.012.i, i64 -1
  br label %"loop body.i27"

"loop body.i27":                                  ; preds = %"loop body.i27", %"loop body.lr.ph.i20"
  %i.012.i21 = phi i64 [ 1, %"loop body.lr.ph.i20" ], [ %"genBinOp generated operation2.i24", %"loop body.i27" ]
  %"genBinOp generated operation.i.i22" = add i64 %i.012.i21, -1
  %9 = getelementptr inbounds i8* %6, i64 %"genBinOp generated operation.i.i22"
  %10 = load i8* %9, align 1
  %"equality check.i23" = icmp eq i8 %10, 10
  %"genBinOp generated operation2.i24" = add i64 %i.012.i21, 1
  %"genBinOp generated operation.i25" = icmp sgt i64 %"genBinOp generated operation2.i24", %4
  %"genBinOp generated operation1.i26" = or i1 %"genBinOp generated operation.i25", %"equality check.i23"
  br i1 %"genBinOp generated operation1.i26", label %"loop condition.after loop_crit_edge.i29", label %"loop body.i27"

"loop condition.after loop_crit_edge.i29":        ; preds = %"loop body.i27"
  %phitmp.i28 = select i1 %"equality check.i23", i64 %i.012.i21, i64 -1
  br label %__String_find.exit31

__String_find.exit31:                             ; preds = %"loop condition.after loop_crit_edge.i29", %else
  %found.0.lcssa.i33 = phi i64 [ %phitmp.i, %"loop condition.after loop_crit_edge.i29" ], [ -1, %else ]
  %found.0.lcssa.i30 = phi i64 [ %phitmp.i28, %"loop condition.after loop_crit_edge.i29" ], [ -1, %else ]
  %"genBinOp generated operation" = add i64 %found.0.lcssa.i33, 1
  %"equality check1" = icmp eq i64 %found.0.lcssa.i30, %"genBinOp generated operation"
  br i1 %"equality check1", label %then2, label %elseIf25

merge:                                            ; preds = %elseIf25, %elseIfThen18, %elseIf13, %then2, %find_cutoffStartB
  %Result.0 = phi i64 [ 1, %find_cutoffStartB ], [ %"genBinOp generated operation33", %then2 ], [ %"genBinOp generated operation24", %elseIfThen18 ], [ %"genBinOp generated operation", %elseIf13 ], [ %"genBinOp generated operation", %elseIf25 ]
  ret i64 %Result.0

then2:                                            ; preds = %__String_find.exit31
  %"genBinOp generated operation33" = add i64 %found.0.lcssa.i30, 1
  br label %merge

elseIf9:                                          ; preds = %elseIf13
  %"genBinOp generated operation11" = icmp slt i64 %found.0.lcssa.i30, %found.0.lcssa.i33
  %"genBinOp generated operation12" = add i64 %found.0.lcssa.i30, 1
  %"genBinOp generated operation12." = select i1 %"genBinOp generated operation11", i64 %"genBinOp generated operation12", i64 1
  ret i64 %"genBinOp generated operation12."

elseIf13:                                         ; preds = %elseIf17
  %"genBinOp generated operation15" = icmp slt i64 %found.0.lcssa.i33, %found.0.lcssa.i30
  br i1 %"genBinOp generated operation15", label %merge, label %elseIf9

elseIf17:                                         ; preds = %elseIf25
  %"equality check20" = icmp eq i64 %found.0.lcssa.i33, -1
  %"equality check22" = icmp ne i64 %found.0.lcssa.i30, -1
  %"genBinOp generated operation23" = and i1 %"equality check20", %"equality check22"
  br i1 %"genBinOp generated operation23", label %elseIfThen18, label %elseIf13

elseIfThen18:                                     ; preds = %elseIf17
  %"genBinOp generated operation24" = add i64 %found.0.lcssa.i30, 1
  br label %merge

elseIf25:                                         ; preds = %__String_find.exit31
  %"equality check28" = icmp eq i64 %found.0.lcssa.i30, -1
  %"equality check30" = icmp ne i64 %found.0.lcssa.i33, -1
  %"genBinOp generated operation31" = and i1 %"equality check28", %"equality check30"
  br i1 %"genBinOp generated operation31", label %merge, label %elseIf17
}

define void @__File_open_read(%procStructType* nocapture, %File* nocapture, %struct.string* nocapture) nounwind {
merge:
  %3 = getelementptr %File* %1, i64 0, i32 0
  %4 = getelementptr inbounds %struct.string* %2, i64 0, i32 1
  %5 = load i8** %4, align 8
  %6 = tail call i32 (i8*, i32, ...)* @open(i8* %5, i32 0) nounwind
  %7 = zext i32 %6 to i64
  store i64 %7, i64* %3, align 8
  ret void
}

define noalias %struct.string* @__File_read(%procStructType* nocapture, %File* nocapture, i64) nounwind {
readStartB:
  %3 = tail call noalias i8* @malloc(i64 16) nounwind
  %4 = bitcast i8* %3 to i64*
  store i64 %2, i64* %4, align 8
  %5 = getelementptr i8* %3, i64 8
  %6 = bitcast i8* %5 to i8**
  %"genBinOp generated operation.i" = add i64 %2, 1
  %7 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i") nounwind
  store i8* %7, i8** %6, align 8
  %8 = getelementptr inbounds i8* %7, i64 %2
  store i8 0, i8* %8, align 1
  %9 = getelementptr %File* %1, i64 0, i32 0
  %10 = load i64* %9, align 8
  %11 = trunc i64 %10 to i32
  %12 = tail call i64 @read(i32 %11, i8* %7, i64 %2) nounwind
  %"genBinOp generated operation" = icmp sgt i64 %12, 0
  br i1 %"genBinOp generated operation", label %then, label %merge

then:                                             ; preds = %readStartB
  %"casting char ptr to typed ptr" = bitcast i8* %3 to %struct.string*
  store i64 %12, i64* %4, align 8
  br label %merge

merge:                                            ; preds = %then, %readStartB
  %Result.0 = phi %struct.string* [ %"casting char ptr to typed ptr", %then ], [ null, %readStartB ]
  ret %struct.string* %Result.0
}

define noalias %struct.string* @__File_read_all(%procStructType* nocapture, %File* nocapture) nounwind {
read_allStartB:
  %2 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr" = bitcast i8* %2 to %struct.string*
  %3 = bitcast i8* %2 to i64*
  store i64 0, i64* %3, align 8
  %4 = getelementptr i8* %2, i64 8
  %5 = bitcast i8* %4 to i8**
  store i8* getelementptr inbounds ([1 x i8]* @_global, i64 0, i64 0), i8** %5, align 8
  %6 = tail call noalias i8* @malloc(i64 16) nounwind
  %7 = bitcast i8* %6 to i64*
  store i64 1024, i64* %7, align 8
  %8 = getelementptr i8* %6, i64 8
  %9 = bitcast i8* %8 to i8**
  %10 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %10, i8** %9, align 8
  %11 = getelementptr inbounds i8* %10, i64 1024
  store i8 0, i8* %11, align 1
  %12 = getelementptr %File* %1, i64 0, i32 0
  %13 = load i64* %12, align 8
  %14 = trunc i64 %13 to i32
  %15 = tail call i64 @read(i32 %14, i8* %10, i64 1024) nounwind
  %"genBinOp generated operation.i10" = icmp sgt i64 %15, 0
  br i1 %"genBinOp generated operation.i10", label %"loop condition.preheader", label %"after loop"

"loop condition.preheader":                       ; preds = %read_allStartB
  store i64 %15, i64* %7, align 8
  %"equality check17" = icmp eq i8* %6, null
  br i1 %"equality check17", label %"after loop", label %"loop body"

"loop body":                                      ; preds = %"loop condition.backedge", %"loop condition.preheader"
  %Result.019 = phi %struct.string* [ %"casting char ptr to typed ptr.i15", %"loop condition.backedge" ], [ %"casting char ptr to typed ptr", %"loop condition.preheader" ]
  %str.018.in = phi i8* [ %35, %"loop condition.backedge" ], [ %6, %"loop condition.preheader" ]
  %16 = tail call noalias i8* @malloc(i64 16) nounwind
  %"casting char ptr to typed ptr.i15" = bitcast i8* %16 to %struct.string*
  %17 = getelementptr %struct.string* %Result.019, i64 0, i32 0
  %18 = load i64* %17, align 8
  %19 = bitcast i8* %str.018.in to i64*
  %20 = load i64* %19, align 8
  %"genBinOp generated operation.i16" = add i64 %20, %18
  %21 = bitcast i8* %16 to i64*
  store i64 %"genBinOp generated operation.i16", i64* %21, align 8
  %22 = getelementptr i8* %16, i64 8
  %23 = bitcast i8* %22 to i8**
  %"genBinOp generated operation.i.i" = add i64 %"genBinOp generated operation.i16", 1
  %24 = tail call noalias i8* @malloc(i64 %"genBinOp generated operation.i.i") nounwind
  store i8* %24, i8** %23, align 8
  %25 = getelementptr inbounds i8* %24, i64 %"genBinOp generated operation.i16"
  store i8 0, i8* %25, align 1
  %"genBinOp generated operation128.i" = icmp slt i64 %18, 1
  br i1 %"genBinOp generated operation128.i", label %"loop condition4.preheader.i", label %"loop body.lr.ph.i"

"loop body.lr.ph.i":                              ; preds = %"loop body"
  %26 = getelementptr %struct.string* %Result.019, i64 0, i32 1
  %.pre30.i = load i8** %26, align 8
  br label %"loop body.i"

"loop condition4.preheader.i":                    ; preds = %"loop body.i", %"loop body"
  %i.0.lcssa.i = phi i64 [ 1, %"loop body" ], [ %"genBinOp generated operation3.i", %"loop body.i" ]
  %"genBinOp generated operation826.i" = icmp sgt i64 %i.0.lcssa.i, %"genBinOp generated operation.i16"
  br i1 %"genBinOp generated operation826.i", label %__String_append.exit, label %"loop body5.lr.ph.i"

"loop body5.lr.ph.i":                             ; preds = %"loop condition4.preheader.i"
  %27 = getelementptr i8* %str.018.in, i64 8
  %28 = bitcast i8* %27 to i8**
  %.pre.i = load i8** %28, align 8
  br label %"loop body5.i"

"loop body.i":                                    ; preds = %"loop body.i", %"loop body.lr.ph.i"
  %i.029.i = phi i64 [ 1, %"loop body.lr.ph.i" ], [ %"genBinOp generated operation3.i", %"loop body.i" ]
  %"genBinOp generated operation2.i" = add i64 %i.029.i, -1
  %29 = getelementptr inbounds i8* %.pre30.i, i64 %"genBinOp generated operation2.i"
  %30 = load i8* %29, align 1
  %31 = getelementptr inbounds i8* %24, i64 %"genBinOp generated operation2.i"
  store i8 %30, i8* %31, align 1
  %"genBinOp generated operation3.i" = add i64 %i.029.i, 1
  %"genBinOp generated operation1.i" = icmp sgt i64 %"genBinOp generated operation3.i", %18
  br i1 %"genBinOp generated operation1.i", label %"loop condition4.preheader.i", label %"loop body.i"

"loop body5.i":                                   ; preds = %"loop body5.i", %"loop body5.lr.ph.i"
  %i.127.i = phi i64 [ %i.0.lcssa.i, %"loop body5.lr.ph.i" ], [ %"genBinOp generated operation11.i", %"loop body5.i" ]
  %"genBinOp generated operation9.i" = add i64 %i.127.i, -1
  %"genBinOp generated operation.i24.i" = sub i64 %"genBinOp generated operation9.i", %18
  %32 = getelementptr inbounds i8* %.pre.i, i64 %"genBinOp generated operation.i24.i"
  %33 = load i8* %32, align 1
  %34 = getelementptr inbounds i8* %24, i64 %"genBinOp generated operation9.i"
  store i8 %33, i8* %34, align 1
  %"genBinOp generated operation11.i" = add i64 %i.127.i, 1
  %"genBinOp generated operation8.i" = icmp sgt i64 %"genBinOp generated operation11.i", %"genBinOp generated operation.i16"
  br i1 %"genBinOp generated operation8.i", label %__String_append.exit, label %"loop body5.i"

__String_append.exit:                             ; preds = %"loop body5.i", %"loop condition4.preheader.i"
  %35 = tail call noalias i8* @malloc(i64 16) nounwind
  %36 = bitcast i8* %35 to i64*
  store i64 1024, i64* %36, align 8
  %37 = getelementptr i8* %35, i64 8
  %38 = bitcast i8* %37 to i8**
  %39 = tail call noalias i8* @malloc(i64 1025) nounwind
  store i8* %39, i8** %38, align 8
  %40 = getelementptr inbounds i8* %39, i64 1024
  store i8 0, i8* %40, align 1
  %41 = load i64* %12, align 8
  %42 = trunc i64 %41 to i32
  %43 = tail call i64 @read(i32 %42, i8* %39, i64 1024) nounwind
  %"genBinOp generated operation.i" = icmp sgt i64 %43, 0
  br i1 %"genBinOp generated operation.i", label %"loop condition.backedge", label %"after loop"

"loop condition.backedge":                        ; preds = %__String_append.exit
  store i64 %43, i64* %36, align 8
  %"equality check" = icmp eq i8* %35, null
  br i1 %"equality check", label %"after loop", label %"loop body"

"after loop":                                     ; preds = %"loop condition.backedge", %__String_append.exit, %"loop condition.preheader", %read_allStartB
  %Result.0.lcssa = phi %struct.string* [ %"casting char ptr to typed ptr", %"loop condition.preheader" ], [ %"casting char ptr to typed ptr.i15", %"loop condition.backedge" ], [ %"casting char ptr to typed ptr", %read_allStartB ], [ %"casting char ptr to typed ptr.i15", %__String_append.exit ]
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
  %"<args>" = alloca i8***, align 8
  %"<argTypes>" = alloca %clostypeStructType**, align 8
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
  %9 = call %closureStructType* @closure_new(i8* bitcast (void (%procStructType*, %Data*)* @__Data_make to i8*), %clostypeStructType* %8, i64 2, i8**** %"<args>", %clostypeStructType*** %"<argTypes>")
  %10 = load %clostypeStructType*** %"<argTypes>", align 8
  %11 = load i8**** %"<args>", align 8
  %12 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %12, %clostypeStructType** %10, align 8
  %13 = call %clostypeStructType* @closure_pointer_type()
  %14 = getelementptr %clostypeStructType** %10, i64 1
  store %clostypeStructType* %13, %clostypeStructType** %14, align 8
  %"arg store bitcast" = bitcast %procStructType* %2 to i8*
  %15 = load i8*** %11, align 8
  store i8* %"arg store bitcast", i8** %15, align 8
  %16 = getelementptr i8*** %11, i64 1
  %17 = load i8*** %16, align 8
  store i8* %3, i8** %17, align 8
  call void @priv_queue_routine(%privqStructType* %7, %closureStructType* %9, %procStructType* %0)
  call void @priv_queue_unlock(%privqStructType* %7, %procStructType* %0)
  %18 = call noalias i8* @malloc(i64 512) nounwind
  %19 = bitcast i8* %18 to i8**
  br label %"loop body"

"loop body":                                      ; preds = %"loop body", %mainStartB
  %i.039 = phi i64 [ 0, %mainStartB ], [ %"genBinOp generated operation10", %"loop body" ]
  %20 = call noalias i8* @malloc(i64 16) nounwind
  %21 = call %procStructType* @proc_new_from_other(%procStructType* %0)
  %22 = call noalias i8* @malloc(i64 16) nounwind
  %23 = bitcast i8* %20 to %procStructType**
  store %procStructType* %21, %procStructType** %23, align 8
  %24 = getelementptr i8* %20, i64 8
  %25 = bitcast i8* %24 to i8**
  store i8* %22, i8** %25, align 8
  %26 = call %privqStructType* @proc_get_queue(%procStructType* %0, %procStructType* %21)
  call void @priv_queue_lock(%privqStructType* %26, %procStructType* %0)
  %"genBinOp generated operation5" = srem i64 %i.039, 2
  %27 = call %clostypeStructType* @closure_void_type()
  %28 = call %closureStructType* @closure_new(i8* bitcast (void (%procStructType*, %Worker*, i64, %separate_wrapper*)* @__Worker_make to i8*), %clostypeStructType* %27, i64 4, i8**** %"<args>", %clostypeStructType*** %"<argTypes>")
  %29 = load %clostypeStructType*** %"<argTypes>", align 8
  %30 = load i8**** %"<args>", align 8
  %31 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %31, %clostypeStructType** %29, align 8
  %32 = call %clostypeStructType* @closure_pointer_type()
  %33 = getelementptr %clostypeStructType** %29, i64 1
  store %clostypeStructType* %32, %clostypeStructType** %33, align 8
  %34 = call %clostypeStructType* @closure_sint_type()
  %35 = getelementptr %clostypeStructType** %29, i64 2
  store %clostypeStructType* %34, %clostypeStructType** %35, align 8
  %36 = call %clostypeStructType* @closure_pointer_type()
  %37 = getelementptr %clostypeStructType** %29, i64 3
  store %clostypeStructType* %36, %clostypeStructType** %37, align 8
  %"arg store bitcast6" = bitcast %procStructType* %21 to i8*
  %38 = load i8*** %30, align 8
  store i8* %"arg store bitcast6", i8** %38, align 8
  %39 = getelementptr i8*** %30, i64 1
  %40 = load i8*** %39, align 8
  store i8* %22, i8** %40, align 8
  %ptrtoint = inttoptr i64 %"genBinOp generated operation5" to i8*
  %41 = getelementptr i8*** %30, i64 2
  %42 = load i8*** %41, align 8
  store i8* %ptrtoint, i8** %42, align 8
  %43 = getelementptr i8*** %30, i64 3
  %44 = load i8*** %43, align 8
  store i8* %1, i8** %44, align 8
  call void @priv_queue_routine(%privqStructType* %26, %closureStructType* %28, %procStructType* %0)
  call void @priv_queue_unlock(%privqStructType* %26, %procStructType* %0)
  %getProc = load %procStructType** %23, align 8
  %45 = call %privqStructType* @proc_get_queue(%procStructType* %0, %procStructType* %getProc)
  call void @priv_queue_lock(%privqStructType* %45, %procStructType* %0)
  %46 = load i8** %25, align 8
  %47 = call %clostypeStructType* @closure_void_type()
  %48 = call %closureStructType* @closure_new(i8* bitcast (void (%procStructType*, %Worker*)* @worker_stub2 to i8*), %clostypeStructType* %47, i64 2, i8**** %"<args>", %clostypeStructType*** %"<argTypes>")
  %49 = load %clostypeStructType*** %"<argTypes>", align 8
  %50 = load i8**** %"<args>", align 8
  %51 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %51, %clostypeStructType** %49, align 8
  %52 = call %clostypeStructType* @closure_pointer_type()
  %53 = getelementptr %clostypeStructType** %49, i64 1
  store %clostypeStructType* %52, %clostypeStructType** %53, align 8
  %"arg store bitcast9" = bitcast %procStructType* %getProc to i8*
  %54 = load i8*** %50, align 8
  store i8* %"arg store bitcast9", i8** %54, align 8
  %55 = getelementptr i8*** %50, i64 1
  %56 = load i8*** %55, align 8
  store i8* %46, i8** %56, align 8
  call void @priv_queue_routine(%privqStructType* %45, %closureStructType* %48, %procStructType* %0)
  call void @priv_queue_unlock(%privqStructType* %45, %procStructType* %0)
  %57 = getelementptr inbounds i8** %19, i64 %i.039
  store i8* %20, i8** %57, align 8
  %"genBinOp generated operation10" = add i64 %i.039, 1
  %exitcond40 = icmp eq i64 %"genBinOp generated operation10", 64
  br i1 %exitcond40, label %"loop body12", label %"loop body"

"loop body12":                                    ; preds = %"loop body12", %"loop body"
  %i.138 = phi i64 [ %"genBinOp generated operation16", %"loop body12" ], [ 0, %"loop body" ]
  %58 = getelementptr inbounds i8** %19, i64 %i.138
  %59 = load i8** %58, align 8
  %60 = bitcast i8* %59 to %procStructType**
  %getProc15 = load %procStructType** %60, align 8
  call void @proc_shutdown(%procStructType* %getProc15, %procStructType* %0)
  %"genBinOp generated operation16" = add i64 %i.138, 1
  %exitcond = icmp eq i64 %"genBinOp generated operation16", 64
  br i1 %exitcond, label %"after loop13", label %"loop body12"

"after loop13":                                   ; preds = %"loop body12"
  %getProc17 = load %procStructType** %4, align 8
  call void @proc_shutdown(%procStructType* %getProc17, %procStructType* %0)
  call void @proc_deref_priv_queues(%procStructType* %0)
  ret void
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

declare void @worker_stub1(%procStructType*, %Worker* nocapture)
declare void @worker_stub2(%procStructType*, %Worker* nocapture)

define void @__Worker_run(%procStructType*, %Worker* nocapture) {
runStartB:
  %"<args>" = alloca i8***, align 8
  %"<argTypes>" = alloca %clostypeStructType**, align 8
  %"<closResult>" = alloca i64, align 8
  %2 = getelementptr %Worker* %1, i64 0, i32 0
  %closure_result_cast = bitcast i64* %"<closResult>" to i8*
  %3 = getelementptr %Worker* %1, i64 0, i32 1
  br label %"get queues block"

"after loop":                                     ; preds = %"sep body block"
  %4 = call i64 @write(i32 1, i8* getelementptr inbounds ([14 x i8]* @"Worker done \0A_global", i64 0, i64 0), i64 13) nounwind
  ret void

"get queues block":                               ; preds = %"sep body block", %runStartB
  %i.014 = phi i64 [ 1, %runStartB ], [ %"genBinOp generated operation4", %"sep body block" ]
  %5 = load %separate_wrapper** %2, align 8
  %6 = getelementptr %separate_wrapper* %5, i64 0, i32 0
  %getProc = load %procStructType** %6, align 8
  %7 = call %privqStructType* @proc_get_queue(%procStructType* %0, %procStructType* %getProc)
  br label %"lock block"

"lock block":                                     ; preds = %"wait cond retry block", %"get queues block"
  call void @priv_queue_lock(%privqStructType* %7, %procStructType* %0)
  %8 = load %separate_wrapper** %2, align 8
  %9 = getelementptr %separate_wrapper* %8, i64 0, i32 0
  %10 = load %procStructType** %9, align 8
  %11 = getelementptr %separate_wrapper* %8, i64 0, i32 1
  %12 = load i8** %11, align 8
  %13 = call %clostypeStructType* @closure_sint_type()
  %14 = call %closureStructType* @closure_new(i8* bitcast (i64 (%procStructType*, %Data*)* @__Data_get_value to i8*), %clostypeStructType* %13, i64 2, i8**** %"<args>", %clostypeStructType*** %"<argTypes>")
  %15 = load %clostypeStructType*** %"<argTypes>", align 8
  %16 = load i8**** %"<args>", align 8
  %17 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %17, %clostypeStructType** %15, align 8
  %18 = call %clostypeStructType* @closure_pointer_type()
  %19 = getelementptr %clostypeStructType** %15, i64 1
  store %clostypeStructType* %18, %clostypeStructType** %19, align 8
  %"arg store bitcast" = bitcast %procStructType* %10 to i8*
  %20 = load i8*** %16, align 8
  store i8* %"arg store bitcast", i8** %20, align 8
  %21 = getelementptr i8*** %16, i64 1
  %22 = load i8*** %21, align 8
  store i8* %12, i8** %22, align 8
  call void @priv_queue_function(%privqStructType* %7, %closureStructType* %14, i8* %closure_result_cast, %procStructType* %0)
  %23 = load i64* %"<closResult>", align 8
  %"genBinOp generated operation1" = srem i64 %23, 2
  %24 = load i64* %3, align 8
  %"equality check" = icmp eq i64 %"genBinOp generated operation1", %24
  br i1 %"equality check", label %"sep body block", label %"wait cond retry block"

"sep body block":                                 ; preds = %"lock block"
  %25 = load %separate_wrapper** %2, align 8
  %26 = getelementptr %separate_wrapper* %25, i64 0, i32 0
  %27 = load %procStructType** %26, align 8
  %28 = getelementptr %separate_wrapper* %25, i64 0, i32 1
  %29 = load i8** %28, align 8
  %30 = call %clostypeStructType* @closure_void_type()
  %31 = call %closureStructType* @closure_new(i8* bitcast (void (%procStructType*, %Data*)* @__Data_incr to i8*), %clostypeStructType* %30, i64 2, i8**** %"<args>", %clostypeStructType*** %"<argTypes>")
  %32 = load %clostypeStructType*** %"<argTypes>", align 8
  %33 = load i8**** %"<args>", align 8
  %34 = call %clostypeStructType* @closure_pointer_type()
  store %clostypeStructType* %34, %clostypeStructType** %32, align 8
  %35 = call %clostypeStructType* @closure_pointer_type()
  %36 = getelementptr %clostypeStructType** %32, i64 1
  store %clostypeStructType* %35, %clostypeStructType** %36, align 8
  %"arg store bitcast3" = bitcast %procStructType* %27 to i8*
  %37 = load i8*** %33, align 8
  store i8* %"arg store bitcast3", i8** %37, align 8
  %38 = getelementptr i8*** %33, i64 1
  %39 = load i8*** %38, align 8
  store i8* %29, i8** %39, align 8
  call void @priv_queue_routine(%privqStructType* %7, %closureStructType* %31, %procStructType* %0)
  call void @priv_queue_unlock(%privqStructType* %7, %procStructType* %0)
  %"genBinOp generated operation4" = add i64 %i.014, 1
  %exitcond = icmp eq i64 %"genBinOp generated operation4", 5001
  br i1 %exitcond, label %"after loop", label %"get queues block"

"wait cond retry block":                          ; preds = %"lock block"
  call void @priv_queue_unlock(%privqStructType* %7, %procStructType* %0)
  %40 = load %separate_wrapper** %2, align 8
  %41 = getelementptr %separate_wrapper* %40, i64 0, i32 0
  %getProc2 = load %procStructType** %41, align 8
  call void @proc_wait_for_available(%procStructType* %getProc2, %procStructType* %0)
  br label %"lock block"
}

declare void @llvm.lifetime.start(i64, i8* nocapture) nounwind

declare void @llvm.lifetime.end(i64, i8* nocapture) nounwind

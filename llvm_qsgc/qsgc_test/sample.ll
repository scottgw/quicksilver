declare void @llvm.gcroot(i8**, i8*)
declare void @fakegc()

define void @f(i8* %y) gc "qsgc" {
entry:
  %x = alloca i8*
  %z = alloca i64
;  %ptrx = bitcast i32* %x to i8**
  store i8* %y, i8** %x
  call void @llvm.gcroot(i8** %x, i8* null)
  call void @g()
  call void @g()
 ret void
}

define void @g() gc "qsgc" {
entry:
  call void @fakegc()
  ret void
}

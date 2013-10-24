declare void @llvm.gcroot(i8**, i8*)
declare void @fakegc()

define void @f(i32* %y) gc "qsgc" {
entry:
  %x = alloca i32*
  %z = alloca i64
  %ptrx = bitcast i32** %x to i8**
  store i32* %y, i32** %x
  call void @llvm.gcroot(i8** %ptrx, i8* null)
  call void @g()
  call void @g()
 ret void
}

define void @g() gc "qsgc" {
entry:
  call void @fakegc()
  ret void
}

; ModuleID = 'amy-module'
source_filename = "<string>"

declare i8* @GC_malloc(i64)

define private i64 @f(i64 %x) {
entry:
  %y = add i64 %x, -1
  %res2 = sub i64 3, %y
  %res3 = icmp slt i64 5, %res2
  switch i1 %res3, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %0 = alloca i64
  store i64 100, i64* %0
  %1 = load i64, i64* %0
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %2 = alloca i64
  store i64 200, i64* %2
  %3 = load i64, i64* %2
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %1, %case.0.ret ], [ %3, %case.1.ret ]
  ret i64 %ret
}

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 2, i64* %0
  %x = load i64, i64* %0
  %res4 = call i64 @f(i64 %x)
  %ret = call i64 @f(i64 %res4)
  ret i64 %ret
}

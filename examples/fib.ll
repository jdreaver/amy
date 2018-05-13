; Generated from examples/fib.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = call i64 @fib(i64 10)
  %1 = alloca i64
  store i64 %0, i64* %1
  %ret = load i64, i64* %1
  ret i64 %0
}

define private i64 @fib(i64 %x) {
entry:
  %0 = alloca i64
  store i64 %x, i64* %0
  %c31 = load i64, i64* %0
  switch i64 %x, label %case.default.ret [
    i64 0, label %case.0.ret
    i64 1, label %case.1.ret
  ]

case.default.ret:                                 ; preds = %entry
  %res34 = sub i64 %x, 1
  %1 = call i64 @fib(i64 %res34)
  %2 = alloca i64
  store i64 %1, i64* %2
  %res35 = load i64, i64* %2
  %res36 = sub i64 %x, 2
  %3 = call i64 @fib(i64 %res36)
  %4 = alloca i64
  store i64 %3, i64* %4
  %res37 = load i64, i64* %4
  %5 = add i64 %res35, %res37
  br label %case.end.ret

case.0.ret:                                       ; preds = %entry
  %6 = alloca i64
  store i64 0, i64* %6
  %7 = load i64, i64* %6
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %8 = alloca i64
  store i64 1, i64* %8
  %9 = load i64, i64* %8
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret, %case.default.ret
  %ret = phi i64 [ %5, %case.default.ret ], [ 0, %case.0.ret ], [ 1, %case.1.ret ]
  ret i64 %ret
}


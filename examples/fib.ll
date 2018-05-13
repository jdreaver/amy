; Generated from examples/fib.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %ret = call i64 @fib(i64 10)
  ret i64 %ret
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
  %res34 = sub i64 %c31, 1
  %res35 = call i64 @fib(i64 %res34)
  %res36 = sub i64 %c31, 2
  %res37 = call i64 @fib(i64 %res36)
  %1 = add i64 %res35, %res37
  br label %case.end.ret

case.0.ret:                                       ; preds = %entry
  %2 = alloca i64
  store i64 0, i64* %2
  %3 = load i64, i64* %2
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %4 = alloca i64
  store i64 1, i64* %4
  %5 = load i64, i64* %4
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret, %case.default.ret
  %ret = phi i64 [ %1, %case.default.ret ], [ %3, %case.0.ret ], [ %5, %case.1.ret ]
  ret i64 %ret
}


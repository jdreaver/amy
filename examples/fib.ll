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
  switch i64 %x, label %case.default.ret [
    i64 0, label %case.0.ret
    i64 1, label %case.1.ret
  ]

case.default.ret:                                 ; preds = %entry
  %0 = alloca i64
  store i64 %x, i64* %0
  %c15 = load i64, i64* %0
  %res18 = sub i64 %c15, 1
  %res19 = call i64 @fib(i64 %res18)
  %res20 = sub i64 %c15, 2
  %res21 = call i64 @fib(i64 %res20)
  %1 = add i64 %res19, %res21
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


; Generated from examples/fib.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = call i64 @fib(i64 10)
  ret i64 %0
}

define private i64 @fib(i64 %x) {
entry:
  %0 = alloca i64
  store i64 %x, i64* %0
  %c31 = load i64, i64* %0
  switch i64 %x, label %case.default.0 [
    i64 0, label %case.0.0
    i64 1, label %case.1.0
  ]

case.default.0:                                   ; preds = %entry
  %1 = sub i64 %x, 1
  %2 = alloca i64
  store i64 %1, i64* %2
  %res34 = load i64, i64* %2
  %3 = call i64 @fib(i64 %res34)
  %4 = alloca i64
  store i64 %3, i64* %4
  %res35 = load i64, i64* %4
  %5 = sub i64 %x, 2
  %6 = alloca i64
  store i64 %5, i64* %6
  %res36 = load i64, i64* %6
  %7 = call i64 @fib(i64 %res36)
  %8 = alloca i64
  store i64 %7, i64* %8
  %res37 = load i64, i64* %8
  %9 = add i64 %res35, %res37
  br label %case.end.0

case.0.0:                                         ; preds = %entry
  br label %case.end.0

case.1.0:                                         ; preds = %entry
  br label %case.end.0

case.end.0:                                       ; preds = %case.1.0, %case.0.0, %case.default.0
  %end.0 = phi i64 [ %9, %case.default.0 ], [ 0, %case.0.0 ], [ 1, %case.1.0 ]
  ret i64 %end.0
}


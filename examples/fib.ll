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
  switch i64 %x, label %case.default.0 [
    i64 0, label %case.0.0
    i64 1, label %case.1.0
  ]

case.default.0:                                   ; preds = %entry
  %0 = sub i64 %x, 1
  %1 = call i64 @fib(i64 %0)
  %2 = sub i64 %x, 2
  %3 = call i64 @fib(i64 %2)
  %4 = add i64 %1, %3
  br label %case.end.0

case.0.0:                                         ; preds = %entry
  br label %case.end.0

case.1.0:                                         ; preds = %entry
  br label %case.end.0

case.end.0:                                       ; preds = %case.1.0, %case.0.0, %case.default.0
  %end.0 = phi i64 [ %4, %case.default.0 ], [ 0, %case.0.0 ], [ 1, %case.1.0 ]
  ret i64 %end.0
}


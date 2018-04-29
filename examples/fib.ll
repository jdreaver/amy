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
  %0 = icmp eq i64 %x, 0
  switch i1 %0, label %case.01 [
    i1 true, label %case.01
    i1 false, label %case.11
  ]

case.01:                                          ; preds = %entry, %entry
  br label %case.end.1

case.11:                                          ; preds = %entry
  %1 = icmp eq i64 %x, 1
  switch i1 %1, label %case.03 [
    i1 true, label %case.03
    i1 false, label %case.13
  ]

case.03:                                          ; preds = %case.11, %case.11
  br label %case.end.3

case.13:                                          ; preds = %case.11
  %2 = sub i64 %x, 1
  %3 = call i64 @fib(i64 %2)
  %4 = sub i64 %x, 2
  %5 = call i64 @fib(i64 %4)
  %6 = add i64 %3, %5
  br label %case.end.3

case.end.3:                                       ; preds = %case.13, %case.03
  %end.3 = phi i64 [ 1, %case.03 ], [ %6, %case.13 ]
  br label %case.end.1

case.end.1:                                       ; preds = %case.end.3, %case.01
  %end.1 = phi i64 [ 0, %case.01 ], [ %end.3, %case.end.3 ]
  ret i64 %end.1
}


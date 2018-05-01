; Generated from examples/primops.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = call i64 @f(i64 2)
  %1 = call i64 @f(i64 %0)
  ret i64 %1
}

define private i64 @f(i64 %x) {
entry:
  %0 = add i64 %x, -1
  %1 = sub i64 3, %0
  %2 = icmp slt i64 5, %1
  switch i1 %2, label %case.0.4 [
    i64 1, label %case.0.4
    i64 0, label %case.1.4
  ]

case.0.4:                                         ; preds = %entry, %entry
  br label %case.end.4

case.1.4:                                         ; preds = %entry
  br label %case.end.4

case.end.4:                                       ; preds = %case.1.4, %case.0.4
  %end.4 = phi i64 [ 100, %case.0.4 ], [ 200, %case.1.4 ]
  ret i64 %end.4
}


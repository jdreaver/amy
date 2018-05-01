; Generated from examples/let.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

declare i64 @abs(i64)

define i64 @main() {
entry:
  switch i64 1, label %case.0.0 [
    i64 1, label %case.0.0
    i64 0, label %case.1.0
  ]

case.0.0:                                         ; preds = %entry, %entry
  %0 = call i64 @f(i64 100)
  %1 = call i64 @abs(i64 %0)
  br label %case.end.0

case.1.0:                                         ; preds = %entry
  %2 = call i64 @f(i64 200)
  %3 = call i64 @abs(i64 %2)
  br label %case.end.0

case.end.0:                                       ; preds = %case.1.0, %case.0.0
  %end.0 = phi i64 [ %1, %case.0.0 ], [ %3, %case.1.0 ]
  %4 = add i64 %end.0, 2
  switch i64 %4, label %case.default.6 [
    i64 1, label %case.0.6
  ]

case.default.6:                                   ; preds = %case.end.0
  %5 = sub i64 %4, 3
  br label %case.end.6

case.0.6:                                         ; preds = %case.end.0
  %6 = call i64 @g()
  br label %case.end.6

case.end.6:                                       ; preds = %case.0.6, %case.default.6
  %end.6 = phi i64 [ %5, %case.default.6 ], [ %6, %case.0.6 ]
  switch i64 %end.6, label %case.default.9 [
  ]

case.default.9:                                   ; preds = %case.end.6
  br label %case.end.9

case.end.9:                                       ; preds = %case.default.9
  %end.9 = phi i64 [ %end.6, %case.default.9 ]
  switch i64 0, label %case.0.10 [
    i64 0, label %case.0.10
  ]

case.0.10:                                        ; preds = %case.end.9, %case.end.9
  br label %case.end.10

case.end.10:                                      ; preds = %case.0.10
  %end.10 = phi i64 [ %end.9, %case.0.10 ]
  switch i64 1, label %case.0.11 [
    i64 0, label %case.0.11
    i64 1, label %case.1.11
  ]

case.0.11:                                        ; preds = %case.end.10, %case.end.10
  br label %case.end.11

case.1.11:                                        ; preds = %case.end.10
  br label %case.end.11

case.end.11:                                      ; preds = %case.1.11, %case.0.11
  %end.11 = phi i64 [ 1, %case.0.11 ], [ 2, %case.1.11 ]
  %7 = add i64 %end.10, %end.0
  %8 = add i64 %end.11, %7
  ret i64 %8
}

define private i64 @f(i64 %x) {
entry:
  switch i64 1, label %case.0.0 [
    i64 1, label %case.0.0
    i64 0, label %case.1.0
  ]

case.0.0:                                         ; preds = %entry, %entry
  %0 = call i64 @abs(i64 %x)
  br label %case.end.0

case.1.0:                                         ; preds = %entry
  %1 = call i64 @threeHundred()
  br label %case.end.0

case.end.0:                                       ; preds = %case.1.0, %case.0.0
  %end.0 = phi i64 [ %0, %case.0.0 ], [ %1, %case.1.0 ]
  ret i64 %end.0
}

define private i64 @g() {
entry:
  ret i64 1
}

define private i64 @threeHundred() {
entry:
  ret i64 100
}


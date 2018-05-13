; Generated from examples/let.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

declare i64 @abs(i64)

define i64 @main() {
entry:
  %0 = alloca i1
  store i1 true, i1* %0
  %c33 = load i1, i1* %0
  switch i1 true, label %case.0.0 [
    i1 true, label %case.0.0
    i1 false, label %case.1.0
  ]

case.0.0:                                         ; preds = %entry, %entry
  %1 = call i64 @f(i64 100)
  %2 = alloca i64
  store i64 %1, i64* %2
  %x37 = load i64, i64* %2
  %3 = call i64 @abs(i64 %x37)
  br label %case.end.0

case.1.0:                                         ; preds = %entry
  %4 = call i64 @threeHundred()
  %5 = alloca i64
  store i64 %4, i64* %5
  %x38 = load i64, i64* %5
  %6 = call i64 @f(i64 %x38)
  %7 = alloca i64
  store i64 %6, i64* %7
  %x39 = load i64, i64* %7
  %8 = call i64 @abs(i64 %x39)
  br label %case.end.0

case.end.0:                                       ; preds = %case.1.0, %case.0.0
  %end.0 = phi i64 [ %3, %case.0.0 ], [ %8, %case.1.0 ]
  %9 = alloca i64
  store i64 %end.0, i64* %9
  %x = load i64, i64* %9
  %10 = alloca i64
  store i64 %x, i64* %10
  %y = load i64, i64* %10
  %11 = add i64 %x, %y
  ret i64 %11
}

define private i64 @f(i64 %x) {
entry:
  %0 = alloca i1
  store i1 true, i1* %0
  %c34 = load i1, i1* %0
  switch i1 true, label %case.0.0 [
    i1 true, label %case.0.0
    i1 false, label %case.1.0
  ]

case.0.0:                                         ; preds = %entry, %entry
  %1 = call i64 @abs(i64 %x)
  br label %case.end.0

case.1.0:                                         ; preds = %entry
  %2 = call i64 @threeHundred()
  %3 = alloca i64
  store i64 %2, i64* %3
  %res40 = load i64, i64* %3
  br label %case.end.0

case.end.0:                                       ; preds = %case.1.0, %case.0.0
  %end.0 = phi i64 [ %1, %case.0.0 ], [ %res40, %case.1.0 ]
  ret i64 %end.0
}

define private i64 @threeHundred() {
entry:
  ret i64 300
}


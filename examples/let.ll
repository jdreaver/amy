; Generated from examples/let.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%MyInt = type { i1, i64* }
%MySum = type { i1, i64* }

declare i64 @abs(i64)

define i64 @main() {
entry:
  switch i1 true, label %case.0.0 [
    i1 true, label %case.0.0
    i1 false, label %case.1.0
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
  %6 = alloca %MyInt
  %7 = getelementptr %MyInt, %MyInt* %6, i32 0, i32 0
  store i1 false, i1* %7
  %8 = alloca i64
  store i64 %5, i64* %8
  %9 = getelementptr %MyInt, %MyInt* %6, i32 0, i32 1
  store i64* %8, i64** %9
  br label %case.end.6

case.0.6:                                         ; preds = %case.end.0
  %10 = call %MyInt* @g()
  br label %case.end.6

case.end.6:                                       ; preds = %case.0.6, %case.default.6
  %end.6 = phi %MyInt* [ %6, %case.default.6 ], [ %10, %case.0.6 ]
  %11 = getelementptr %MyInt, %MyInt* %end.6, i32 0, i32 0
  %12 = load i1, i1* %11
  switch i1 %12, label %case.0.13 [
    i1 false, label %case.0.13
  ]

case.0.13:                                        ; preds = %case.end.6, %case.end.6
  %13 = getelementptr %MyInt, %MyInt* %end.6, i32 0, i32 1
  %14 = load i64*, i64** %13
  %15 = load i64, i64* %14
  br label %case.end.13

case.end.13:                                      ; preds = %case.0.13
  %end.13 = phi i64 [ %15, %case.0.13 ]
  switch i1 false, label %case.0.19 [
    i1 false, label %case.0.19
  ]

case.0.19:                                        ; preds = %case.end.13, %case.end.13
  br label %case.end.19

case.end.19:                                      ; preds = %case.0.19
  %end.19 = phi i64 [ %end.13, %case.0.19 ]
  %16 = call i8 @myEnum()
  switch i8 %16, label %case.0.21 [
    i8 0, label %case.0.21
    i8 1, label %case.1.21
    i8 2, label %case.2.21
  ]

case.0.21:                                        ; preds = %case.end.19, %case.end.19
  br label %case.end.21

case.1.21:                                        ; preds = %case.end.19
  br label %case.end.21

case.2.21:                                        ; preds = %case.end.19
  br label %case.end.21

case.end.21:                                      ; preds = %case.2.21, %case.1.21, %case.0.21
  %end.21 = phi i64 [ 1, %case.0.21 ], [ 2, %case.1.21 ], [ 3, %case.2.21 ]
  %17 = add i64 %end.19, %end.0
  %18 = add i64 %end.21, %17
  ret i64 %18
}

define private i64 @f(i64 %x) {
entry:
  switch i1 true, label %case.0.0 [
    i1 true, label %case.0.0
    i1 false, label %case.1.0
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

define private %MyInt* @g() {
entry:
  %0 = alloca %MyInt
  %1 = getelementptr %MyInt, %MyInt* %0, i32 0, i32 0
  store i1 false, i1* %1
  %2 = alloca i64
  store i64 1, i64* %2
  %3 = getelementptr %MyInt, %MyInt* %0, i32 0, i32 1
  store i64* %2, i64** %3
  ret %MyInt* %0
}

define private i8 @myEnum() {
entry:
  ret i8 1
}

define private %MySum* @mySum() {
entry:
  %0 = alloca %MySum
  %1 = getelementptr %MySum, %MySum* %0, i32 0, i32 0
  store i1 true, i1* %1
  %2 = alloca double
  store double 1.100000e+00, double* %2
  %3 = bitcast double* %2 to i64*
  %4 = getelementptr %MySum, %MySum* %0, i32 0, i32 1
  store i64* %3, i64** %4
  ret %MySum* %0
}

define private i64 @threeHundred() {
entry:
  ret i64 100
}


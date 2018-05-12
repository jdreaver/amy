; Generated from examples/let.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%MySum = type { i8, i64* }

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
  %6 = alloca %MySum
  %7 = getelementptr %MySum, %MySum* %6, i32 0, i32 0
  store i8 0, i8* %7
  %8 = alloca i64
  store i64 %5, i64* %8
  %9 = getelementptr %MySum, %MySum* %6, i32 0, i32 1
  store i64* %8, i64** %9
  br label %case.end.6

case.0.6:                                         ; preds = %case.end.0
  %10 = call %MySum* @mySum()
  br label %case.end.6

case.end.6:                                       ; preds = %case.0.6, %case.default.6
  %end.6 = phi %MySum* [ %6, %case.default.6 ], [ %10, %case.0.6 ]
  %11 = getelementptr %MySum, %MySum* %end.6, i32 0, i32 0
  %12 = load i8, i8* %11
  switch i8 %12, label %case.0.13 [
    i8 0, label %case.0.13
    i8 1, label %case.1.13
    i8 2, label %case.2.13
  ]

case.0.13:                                        ; preds = %case.end.6, %case.end.6
  %13 = getelementptr %MySum, %MySum* %end.6, i32 0, i32 1
  %14 = load i64*, i64** %13
  %15 = load i64, i64* %14
  br label %case.end.13

case.1.13:                                        ; preds = %case.end.6
  %16 = getelementptr %MySum, %MySum* %end.6, i32 0, i32 1
  %17 = load i64*, i64** %16
  %18 = bitcast i64* %17 to double*
  %19 = load double, double* %18
  br label %case.end.13

case.2.13:                                        ; preds = %case.end.6
  br label %case.end.13

case.end.13:                                      ; preds = %case.2.13, %case.1.13, %case.0.13
  %end.13 = phi i64 [ %15, %case.0.13 ], [ 0, %case.1.13 ], [ 1, %case.2.13 ]
  switch i1 false, label %case.0.23 [
    i1 false, label %case.0.23
  ]

case.0.23:                                        ; preds = %case.end.13, %case.end.13
  br label %case.end.23

case.end.23:                                      ; preds = %case.0.23
  %end.23 = phi i64 [ %end.13, %case.0.23 ]
  %20 = call i8 @myEnum()
  switch i8 %20, label %case.0.25 [
    i8 0, label %case.0.25
    i8 1, label %case.1.25
    i8 2, label %case.2.25
  ]

case.0.25:                                        ; preds = %case.end.23, %case.end.23
  br label %case.end.25

case.1.25:                                        ; preds = %case.end.23
  br label %case.end.25

case.2.25:                                        ; preds = %case.end.23
  br label %case.end.25

case.end.25:                                      ; preds = %case.2.25, %case.1.25, %case.0.25
  %end.25 = phi i64 [ 1, %case.0.25 ], [ 2, %case.1.25 ], [ 3, %case.2.25 ]
  %21 = add i64 %end.23, %end.0
  %22 = add i64 %end.25, %21
  ret i64 %22
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

define private i8 @myEnum() {
entry:
  ret i8 1
}

define private %MySum* @mySum() {
entry:
  switch i1 true, label %case.0.0 [
    i1 true, label %case.0.0
    i1 false, label %case.1.0
  ]

case.0.0:                                         ; preds = %entry, %entry
  %0 = alloca %MySum
  %1 = getelementptr %MySum, %MySum* %0, i32 0, i32 0
  store i8 1, i8* %1
  %2 = alloca double
  store double 1.100000e+00, double* %2
  %3 = bitcast double* %2 to i64*
  %4 = getelementptr %MySum, %MySum* %0, i32 0, i32 1
  store i64* %3, i64** %4
  br label %case.end.0

case.1.0:                                         ; preds = %entry
  %5 = alloca %MySum
  %6 = getelementptr %MySum, %MySum* %5, i32 0, i32 0
  store i8 2, i8* %6
  br label %case.end.0

case.end.0:                                       ; preds = %case.1.0, %case.0.0
  %end.0 = phi %MySum* [ %0, %case.0.0 ], [ %5, %case.1.0 ]
  ret %MySum* %end.0
}

define private i64 @threeHundred() {
entry:
  ret i64 100
}


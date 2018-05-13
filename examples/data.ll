; Generated from examples/data.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%MySum = type { i8, i64* }

define i64 @main() {
entry:
  %0 = alloca %MySum
  %1 = getelementptr %MySum, %MySum* %0, i32 0, i32 0
  store i8 1, i8* %1
  %2 = alloca double
  store double 0x401F333333333333, double* %2
  %3 = bitcast double* %2 to i64*
  %4 = getelementptr %MySum, %MySum* %0, i32 0, i32 1
  store i64* %3, i64** %4
  %5 = alloca %MySum*
  store %MySum* %0, %MySum** %5
  %x = load %MySum*, %MySum** %5
  %6 = alloca i8
  store i8 1, i8* %6
  %y = load i8, i8* %6
  %7 = alloca %MySum*
  store %MySum* %x, %MySum** %7
  %c46 = load %MySum*, %MySum** %7
  %8 = getelementptr %MySum, %MySum* %x, i32 0, i32 0
  %9 = load i8, i8* %8
  %10 = getelementptr %MySum, %MySum* %x, i32 0, i32 1
  %11 = load i64*, i64** %10
  switch i8 %9, label %case.0.7 [
    i8 0, label %case.0.7
    i8 1, label %case.1.7
    i8 2, label %case.2.7
  ]

case.0.7:                                         ; preds = %entry, %entry
  %12 = load i64, i64* %11
  %13 = alloca i64
  store i64 %12, i64* %13
  %_u47 = load i64, i64* %13
  br label %case.end.7

case.1.7:                                         ; preds = %entry
  %14 = bitcast i64* %11 to double*
  %15 = load double, double* %14
  %16 = alloca double
  store double %15, double* %16
  %_u48 = load double, double* %16
  %17 = call i64 @f(double %_u48, i8 %y)
  br label %case.end.7

case.2.7:                                         ; preds = %entry
  br label %case.end.7

case.end.7:                                       ; preds = %case.2.7, %case.1.7, %case.0.7
  %end.7 = phi i64 [ 0, %case.0.7 ], [ %17, %case.1.7 ], [ 1, %case.2.7 ]
  ret i64 %end.7
}

define private i64 @f(double %x, i8 %enum) {
entry:
  %0 = alloca i8
  store i8 %enum, i8* %0
  %c49 = load i8, i8* %0
  switch i8 %enum, label %case.0.0 [
    i8 0, label %case.0.0
    i8 1, label %case.1.0
    i8 2, label %case.2.0
  ]

case.0.0:                                         ; preds = %entry, %entry
  br label %case.end.0

case.1.0:                                         ; preds = %entry
  %1 = fptoui double %x to i64
  br label %case.end.0

case.2.0:                                         ; preds = %entry
  br label %case.end.0

case.end.0:                                       ; preds = %case.2.0, %case.1.0, %case.0.0
  %end.0 = phi i64 [ 2, %case.0.0 ], [ %1, %case.1.0 ], [ 3, %case.2.0 ]
  ret i64 %end.0
}


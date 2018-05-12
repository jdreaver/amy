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
  %5 = getelementptr %MySum, %MySum* %0, i32 0, i32 0
  %6 = load i8, i8* %5
  %7 = getelementptr %MySum, %MySum* %0, i32 0, i32 1
  %8 = load i64*, i64** %7
  switch i8 %6, label %case.0.5 [
    i8 0, label %case.0.5
    i8 1, label %case.1.5
    i8 2, label %case.2.5
  ]

case.0.5:                                         ; preds = %entry, %entry
  %9 = load i64, i64* %8
  br label %case.end.5

case.1.5:                                         ; preds = %entry
  %10 = bitcast i64* %8 to double*
  %11 = load double, double* %10
  %12 = call i64 @f(double %11, i8 1)
  br label %case.end.5

case.2.5:                                         ; preds = %entry
  br label %case.end.5

case.end.5:                                       ; preds = %case.2.5, %case.1.5, %case.0.5
  %end.5 = phi i64 [ 0, %case.0.5 ], [ %12, %case.1.5 ], [ 1, %case.2.5 ]
  ret i64 %end.5
}

define private i64 @f(double %x, i8 %enum) {
entry:
  switch i8 %enum, label %case.0.0 [
    i8 0, label %case.0.0
    i8 1, label %case.1.0
    i8 2, label %case.2.0
  ]

case.0.0:                                         ; preds = %entry, %entry
  br label %case.end.0

case.1.0:                                         ; preds = %entry
  %0 = fptoui double %x to i64
  br label %case.end.0

case.2.0:                                         ; preds = %entry
  br label %case.end.0

case.end.0:                                       ; preds = %case.2.0, %case.1.0, %case.0.0
  %end.0 = phi i64 [ 2, %case.0.0 ], [ %0, %case.1.0 ], [ 3, %case.2.0 ]
  ret i64 %end.0
}


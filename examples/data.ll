; Generated from examples/data.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%MySum = type { i8, i64* }

define i64 @main() {
entry:
  %x = alloca %MySum
  %0 = getelementptr %MySum, %MySum* %x, i32 0, i32 0
  store i8 1, i8* %0
  %1 = alloca double
  store double 0x401F333333333333, double* %1
  %2 = bitcast double* %1 to i64*
  %3 = getelementptr %MySum, %MySum* %x, i32 0, i32 1
  store i64* %2, i64** %3
  %4 = alloca i8
  store i8 1, i8* %4
  %y52 = load i8, i8* %4
  %5 = alloca i8
  store i8 %y52, i8* %5
  %y = load i8, i8* %5
  %6 = alloca %MySum*
  store %MySum* %x, %MySum** %6
  %c46 = load %MySum*, %MySum** %6
  %7 = getelementptr %MySum, %MySum* %x, i32 0, i32 0
  %8 = load i8, i8* %7
  %9 = getelementptr %MySum, %MySum* %x, i32 0, i32 1
  %10 = load i64*, i64** %9
  switch i8 %8, label %case.0.ret [
    i8 0, label %case.0.ret
    i8 1, label %case.1.ret
    i8 2, label %case.2.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u47 = load i64, i64* %10
  %11 = alloca i64
  store i64 0, i64* %11
  %12 = load i64, i64* %11
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %13 = bitcast i64* %10 to double*
  %_u48 = load double, double* %13
  %14 = call i64 @f(double %_u48, i8 %y)
  br label %case.end.ret

case.2.ret:                                       ; preds = %entry
  %15 = alloca i64
  store i64 1, i64* %15
  %16 = load i64, i64* %15
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.2.ret, %case.1.ret, %case.0.ret
  %ret = phi i64 [ %12, %case.0.ret ], [ %14, %case.1.ret ], [ %16, %case.2.ret ]
  ret i64 %ret
}

define private i64 @f(double %x, i8 %enum) {
entry:
  %0 = alloca i8
  store i8 %enum, i8* %0
  %c49 = load i8, i8* %0
  switch i8 %enum, label %case.0.ret [
    i8 0, label %case.0.ret
    i8 1, label %case.1.ret
    i8 2, label %case.2.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %1 = alloca i64
  store i64 2, i64* %1
  %2 = load i64, i64* %1
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %3 = fptoui double %x to i64
  br label %case.end.ret

case.2.ret:                                       ; preds = %entry
  %4 = alloca i64
  store i64 3, i64* %4
  %5 = load i64, i64* %4
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.2.ret, %case.1.ret, %case.0.ret
  %ret = phi i64 [ %2, %case.0.ret ], [ %3, %case.1.ret ], [ %5, %case.2.ret ]
  ret i64 %ret
}


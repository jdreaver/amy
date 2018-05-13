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
  %6 = getelementptr %MySum, %MySum* %x, i32 0, i32 0
  %7 = load i8, i8* %6
  %8 = getelementptr %MySum, %MySum* %x, i32 0, i32 1
  %9 = load i64*, i64** %8
  switch i8 %7, label %case.0.ret [
    i8 0, label %case.0.ret
    i8 1, label %case.1.ret
    i8 2, label %case.2.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u47 = load i64, i64* %9
  %10 = alloca i64
  store i64 0, i64* %10
  %11 = load i64, i64* %10
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %12 = bitcast i64* %9 to double*
  %_u48 = load double, double* %12
  %13 = call i64 @f(double %_u48, i8 %y)
  br label %case.end.ret

case.2.ret:                                       ; preds = %entry
  %14 = alloca i64
  store i64 1, i64* %14
  %15 = load i64, i64* %14
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.2.ret, %case.1.ret, %case.0.ret
  %ret = phi i64 [ %11, %case.0.ret ], [ %13, %case.1.ret ], [ %15, %case.2.ret ]
  ret i64 %ret
}

define private i64 @f(double %x, i8 %enum) {
entry:
  switch i8 %enum, label %case.0.ret [
    i8 0, label %case.0.ret
    i8 1, label %case.1.ret
    i8 2, label %case.2.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %0 = alloca i64
  store i64 2, i64* %0
  %1 = load i64, i64* %0
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %2 = fptoui double %x to i64
  br label %case.end.ret

case.2.ret:                                       ; preds = %entry
  %3 = alloca i64
  store i64 3, i64* %3
  %4 = load i64, i64* %3
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.2.ret, %case.1.ret, %case.0.ret
  %ret = phi i64 [ %1, %case.0.ret ], [ %2, %case.1.ret ], [ %4, %case.2.ret ]
  ret i64 %ret
}


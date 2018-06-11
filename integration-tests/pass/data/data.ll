; ModuleID = 'amy-module'
source_filename = "<string>"

%MySum = type { i8, i64* }
%Nat = type { i1, i64* }

declare i8* @GC_malloc(i64)

define i64 @main() {
entry:
  %0 = call i8* @GC_malloc(i64 ptrtoint (%MySum* getelementptr (%MySum, %MySum* null, i32 1) to i64))
  %x = bitcast i8* %0 to %MySum*
  %x1 = alloca %MySum
  %1 = getelementptr %MySum, %MySum* %x1, i32 0, i32 0
  store i8 1, i8* %1
  %2 = call i8* @GC_malloc(i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64))
  %3 = bitcast i8* %2 to double*
  store double 0x401F333333333333, double* %3
  %4 = bitcast double* %3 to i64*
  %5 = getelementptr %MySum, %MySum* %x1, i32 0, i32 1
  store i64* %4, i64** %5
  %6 = alloca i8
  store i8 1, i8* %6
  %y = load i8, i8* %6
  %7 = call i8* @GC_malloc(i64 ptrtoint (%Nat* getelementptr (%Nat, %Nat* null, i32 1) to i64))
  %z1 = bitcast i8* %7 to %Nat*
  %z12 = alloca %Nat
  %8 = getelementptr %Nat, %Nat* %z12, i32 0, i32 0
  store i1 false, i1* %8
  %9 = call i8* @GC_malloc(i64 ptrtoint (%Nat* getelementptr (%Nat, %Nat* null, i32 1) to i64))
  %z2 = bitcast i8* %9 to %Nat*
  %z23 = alloca %Nat
  %10 = getelementptr %Nat, %Nat* %z23, i32 0, i32 0
  store i1 true, i1* %10
  %11 = bitcast %Nat* %z12 to i64*
  %12 = getelementptr %Nat, %Nat* %z23, i32 0, i32 1
  store i64* %11, i64** %12
  %13 = call i8* @GC_malloc(i64 ptrtoint (%Nat* getelementptr (%Nat, %Nat* null, i32 1) to i64))
  %z = bitcast i8* %13 to %Nat*
  %z4 = alloca %Nat
  %14 = getelementptr %Nat, %Nat* %z4, i32 0, i32 0
  store i1 true, i1* %14
  %15 = bitcast %Nat* %z23 to i64*
  %16 = getelementptr %Nat, %Nat* %z4, i32 0, i32 1
  store i64* %15, i64** %16
  %17 = getelementptr %MySum, %MySum* %x1, i32 0, i32 0
  %18 = load i8, i8* %17
  %19 = getelementptr %MySum, %MySum* %x1, i32 0, i32 1
  %20 = load i64*, i64** %19
  switch i8 %18, label %case.0.ret [
    i8 0, label %case.0.ret
    i8 1, label %case.1.ret
    i8 2, label %case.2.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u2 = load i64, i64* %20
  %21 = alloca i64
  store i64 0, i64* %21
  %22 = load i64, i64* %21
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %23 = bitcast i64* %20 to double*
  %_u3 = load double, double* %23
  %24 = call i64 @f(double %_u3, i8 %y)
  br label %case.end.ret

case.2.ret:                                       ; preds = %entry
  %25 = call i64 @countNat(%Nat* %z4)
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.2.ret, %case.1.ret, %case.0.ret
  %ret = phi i64 [ %22, %case.0.ret ], [ %24, %case.1.ret ], [ %25, %case.2.ret ]
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

define private i64 @countNat(%Nat* %n) {
entry:
  %0 = getelementptr %Nat, %Nat* %n, i32 0, i32 0
  %1 = load i1, i1* %0
  %2 = getelementptr %Nat, %Nat* %n, i32 0, i32 1
  %3 = load i64*, i64** %2
  switch i1 %1, label %case.0.ret [
    i1 false, label %case.0.ret
    i1 true, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %4 = alloca i64
  store i64 0, i64* %4
  %5 = load i64, i64* %4
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %_u6 = bitcast i64* %3 to %Nat*
  %res3 = call i64 @countNat(%Nat* %_u6)
  %6 = add i64 1, %res3
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %5, %case.0.ret ], [ %6, %case.1.ret ]
  ret i64 %ret
}

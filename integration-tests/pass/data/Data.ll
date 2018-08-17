; ModuleID = 'amy-module'
source_filename = "<string>"

%Nat = type { i1, i64* }
%MySum = type { i8, i64* }

declare i8* @GC_malloc(i64)

define i64 @f(double %x, i8 %enum) {
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

define i64 @countNat(%Nat* %n) {
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
  %_u3 = bitcast i64* %3 to %Nat*
  %res1 = call i64 @countNat(%Nat* %_u3)
  %6 = add i64 1, %res1
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %5, %case.0.ret ], [ %6, %case.1.ret ]
  ret i64 %ret
}

define i64 @main() {
entry:
  %0 = getelementptr %Nat, %Nat* null, i32 1
  %1 = ptrtoint %Nat* %0 to i64
  %2 = call i8* @GC_malloc(i64 %1)
  %z2 = bitcast i8* %2 to %Nat*
  %z21 = alloca %Nat
  %3 = getelementptr %Nat, %Nat* %z21, i32 0, i32 0
  store i1 false, i1* %3
  %4 = getelementptr %Nat, %Nat* null, i32 1
  %5 = ptrtoint %Nat* %4 to i64
  %6 = call i8* @GC_malloc(i64 %5)
  %z3 = bitcast i8* %6 to %Nat*
  %z32 = alloca %Nat
  %7 = getelementptr %Nat, %Nat* %z32, i32 0, i32 0
  store i1 true, i1* %7
  %8 = bitcast %Nat* %z21 to i64*
  %9 = getelementptr %Nat, %Nat* %z32, i32 0, i32 1
  store i64* %8, i64** %9
  %10 = getelementptr %Nat, %Nat* null, i32 1
  %11 = ptrtoint %Nat* %10 to i64
  %12 = call i8* @GC_malloc(i64 %11)
  %z = bitcast i8* %12 to %Nat*
  %z4 = alloca %Nat
  %13 = getelementptr %Nat, %Nat* %z4, i32 0, i32 0
  store i1 true, i1* %13
  %14 = bitcast %Nat* %z32 to i64*
  %15 = getelementptr %Nat, %Nat* %z4, i32 0, i32 1
  store i64* %14, i64** %15
  %16 = alloca i8
  store i8 1, i8* %16
  %y = load i8, i8* %16
  %17 = getelementptr %MySum, %MySum* null, i32 1
  %18 = ptrtoint %MySum* %17 to i64
  %19 = call i8* @GC_malloc(i64 %18)
  %x = bitcast i8* %19 to %MySum*
  %x5 = alloca %MySum
  %20 = getelementptr %MySum, %MySum* %x5, i32 0, i32 0
  store i8 1, i8* %20
  %21 = getelementptr double, double* null, i32 1
  %22 = ptrtoint double* %21 to i64
  %23 = call i8* @GC_malloc(i64 %22)
  %24 = bitcast i8* %23 to double*
  store double 0x401F333333333333, double* %24
  %25 = bitcast double* %24 to i64*
  %26 = getelementptr %MySum, %MySum* %x5, i32 0, i32 1
  store i64* %25, i64** %26
  %27 = getelementptr %MySum, %MySum* %x5, i32 0, i32 0
  %28 = load i8, i8* %27
  %29 = getelementptr %MySum, %MySum* %x5, i32 0, i32 1
  %30 = load i64*, i64** %29
  switch i8 %28, label %case.0.ret [
    i8 0, label %case.0.ret
    i8 1, label %case.1.ret
    i8 2, label %case.2.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u5 = load i64, i64* %30
  %31 = alloca i64
  store i64 0, i64* %31
  %32 = load i64, i64* %31
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %33 = bitcast i64* %30 to double*
  %_u6 = load double, double* %33
  %34 = call i64 @f(double %_u6, i8 %y)
  br label %case.end.ret

case.2.ret:                                       ; preds = %entry
  %35 = call i64 @countNat(%Nat* %z4)
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.2.ret, %case.1.ret, %case.0.ret
  %ret = phi i64 [ %32, %case.0.ret ], [ %34, %case.1.ret ], [ %35, %case.2.ret ]
  ret i64 %ret
}

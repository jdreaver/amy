; Generated from examples/data.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%MySum = type { i8, i64* }
%Nat = type { i1, i64* }

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
  %y = load i8, i8* %4
  %z31 = alloca %Nat
  %5 = getelementptr %Nat, %Nat* %z31, i32 0, i32 0
  store i1 false, i1* %5
  %z32 = alloca %Nat
  %6 = getelementptr %Nat, %Nat* %z32, i32 0, i32 0
  store i1 true, i1* %6
  %7 = bitcast %Nat* %z31 to i64*
  %8 = getelementptr %Nat, %Nat* %z32, i32 0, i32 1
  store i64* %7, i64** %8
  %z = alloca %Nat
  %9 = getelementptr %Nat, %Nat* %z, i32 0, i32 0
  store i1 true, i1* %9
  %10 = bitcast %Nat* %z32 to i64*
  %11 = getelementptr %Nat, %Nat* %z, i32 0, i32 1
  store i64* %10, i64** %11
  %12 = getelementptr %MySum, %MySum* %x, i32 0, i32 0
  %13 = load i8, i8* %12
  %14 = getelementptr %MySum, %MySum* %x, i32 0, i32 1
  %15 = load i64*, i64** %14
  switch i8 %13, label %case.0.ret [
    i8 0, label %case.0.ret
    i8 1, label %case.1.ret
    i8 2, label %case.2.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u24 = load i64, i64* %15
  %16 = alloca i64
  store i64 0, i64* %16
  %17 = load i64, i64* %16
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %18 = bitcast i64* %15 to double*
  %_u25 = load double, double* %18
  %19 = call i64 @f(double %_u25, i8 %y)
  br label %case.end.ret

case.2.ret:                                       ; preds = %entry
  %20 = call i64 @countNat(%Nat* %z)
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.2.ret, %case.1.ret, %case.0.ret
  %ret = phi i64 [ %17, %case.0.ret ], [ %19, %case.1.ret ], [ %20, %case.2.ret ]
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
  %_u28 = bitcast i64* %3 to %Nat*
  %res33 = call i64 @countNat(%Nat* %_u28)
  %6 = add i64 1, %res33
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %5, %case.0.ret ], [ %6, %case.1.ret ]
  ret i64 %ret
}


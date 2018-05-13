; Generated from examples/let.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

declare i64 @abs(i64)

define i64 @main() {
entry:
  %0 = alloca i1
  store i1 true, i1* %0
  %x37 = load i1, i1* %0
  %1 = alloca i1
  store i1 %x37, i1* %1
  %c33 = load i1, i1* %1
  switch i1 %x37, label %case.0.x [
    i1 true, label %case.0.x
    i1 false, label %case.1.x
  ]

case.0.x:                                         ; preds = %entry, %entry
  %2 = call i64 @f(i64 100)
  %3 = alloca i64
  store i64 %2, i64* %3
  %x38 = load i64, i64* %3
  %4 = call i64 @abs(i64 %x38)
  %5 = alloca i64
  store i64 %4, i64* %5
  %6 = load i64, i64* %5
  br label %case.end.x

case.1.x:                                         ; preds = %entry
  %7 = call i64 @threeHundred()
  %8 = alloca i64
  store i64 %7, i64* %8
  %x39 = load i64, i64* %8
  %9 = call i64 @f(i64 %x39)
  %10 = alloca i64
  store i64 %9, i64* %10
  %x40 = load i64, i64* %10
  %11 = call i64 @abs(i64 %x40)
  %12 = alloca i64
  store i64 %11, i64* %12
  %13 = load i64, i64* %12
  br label %case.end.x

case.end.x:                                       ; preds = %case.1.x, %case.0.x
  %x = phi i64 [ %4, %case.0.x ], [ %11, %case.1.x ]
  %14 = alloca i64
  store i64 %x, i64* %14
  %y = load i64, i64* %14
  %ret = add i64 %x, %y
  ret i64 %ret
}

define private i64 @f(i64 %x) {
entry:
  %0 = alloca i1
  store i1 true, i1* %0
  %res41 = load i1, i1* %0
  %1 = alloca i1
  store i1 %res41, i1* %1
  %c34 = load i1, i1* %1
  switch i1 %res41, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %2 = call i64 @abs(i64 %x)
  %3 = alloca i64
  store i64 %2, i64* %3
  %4 = load i64, i64* %3
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %5 = call i64 @threeHundred()
  %6 = alloca i64
  store i64 %5, i64* %6
  %res42 = load i64, i64* %6
  %7 = alloca i64
  store i64 %res42, i64* %7
  %8 = load i64, i64* %7
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %2, %case.0.ret ], [ %res42, %case.1.ret ]
  ret i64 %ret
}

define private i64 @threeHundred() {
entry:
  %0 = alloca i64
  store i64 300, i64* %0
  %ret = load i64, i64* %0
  ret i64 300
}


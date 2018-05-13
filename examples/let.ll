; Generated from examples/let.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

declare i64 @abs(i64)

define i64 @main() {
entry:
  %0 = alloca i1
  store i1 true, i1* %0
  %c33 = load i1, i1* %0
  switch i1 true, label %case.0.x [
    i1 true, label %case.0.x
    i1 false, label %case.1.x
  ]

case.0.x:                                         ; preds = %entry, %entry
  %1 = call i64 @f(i64 100)
  %2 = alloca i64
  store i64 %1, i64* %2
  %x37 = load i64, i64* %2
  %3 = call i64 @abs(i64 %x37)
  %4 = alloca i64
  store i64 %3, i64* %4
  %5 = load i64, i64* %4
  br label %case.end.x

case.1.x:                                         ; preds = %entry
  %6 = call i64 @threeHundred()
  %7 = alloca i64
  store i64 %6, i64* %7
  %x38 = load i64, i64* %7
  %8 = call i64 @f(i64 %x38)
  %9 = alloca i64
  store i64 %8, i64* %9
  %x39 = load i64, i64* %9
  %10 = call i64 @abs(i64 %x39)
  %11 = alloca i64
  store i64 %10, i64* %11
  %12 = load i64, i64* %11
  br label %case.end.x

case.end.x:                                       ; preds = %case.1.x, %case.0.x
  %x = phi i64 [ %3, %case.0.x ], [ %10, %case.1.x ]
  %13 = alloca i64
  store i64 %x, i64* %13
  %y = load i64, i64* %13
  %ret = add i64 %x, %y
  ret i64 %ret
}

define private i64 @f(i64 %x) {
entry:
  %0 = alloca i1
  store i1 true, i1* %0
  %c34 = load i1, i1* %0
  switch i1 true, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %1 = call i64 @abs(i64 %x)
  %2 = alloca i64
  store i64 %1, i64* %2
  %3 = load i64, i64* %2
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %4 = call i64 @threeHundred()
  %5 = alloca i64
  store i64 %4, i64* %5
  %res40 = load i64, i64* %5
  %6 = alloca i64
  store i64 %res40, i64* %6
  %7 = load i64, i64* %6
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %1, %case.0.ret ], [ %res40, %case.1.ret ]
  ret i64 %ret
}

define private i64 @threeHundred() {
entry:
  %0 = alloca i64
  store i64 300, i64* %0
  %ret = load i64, i64* %0
  ret i64 300
}


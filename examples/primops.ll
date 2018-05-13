; Generated from examples/primops.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 2, i64* %0
  %x = load i64, i64* %0
  %1 = call i64 @f(i64 %x)
  %2 = alloca i64
  store i64 %1, i64* %2
  %res34 = load i64, i64* %2
  %3 = call i64 @f(i64 %res34)
  ret i64 %3
}

define private i64 @f(i64 %x) {
entry:
  %0 = alloca i64
  store i64 -1, i64* %0
  %y35 = load i64, i64* %0
  %1 = add i64 %x, %y35
  %2 = alloca i64
  store i64 %1, i64* %2
  %y = load i64, i64* %2
  %3 = sub i64 3, %y
  %4 = alloca i64
  store i64 %3, i64* %4
  %res36 = load i64, i64* %4
  %5 = icmp slt i64 5, %res36
  %6 = alloca i1
  store i1 %5, i1* %6
  %res37 = load i1, i1* %6
  %7 = alloca i1
  store i1 %res37, i1* %7
  %c31 = load i1, i1* %7
  switch i1 %res37, label %case.0.8 [
    i1 true, label %case.0.8
    i1 false, label %case.1.8
  ]

case.0.8:                                         ; preds = %entry, %entry
  br label %case.end.8

case.1.8:                                         ; preds = %entry
  br label %case.end.8

case.end.8:                                       ; preds = %case.1.8, %case.0.8
  %end.8 = phi i64 [ 100, %case.0.8 ], [ 200, %case.1.8 ]
  ret i64 %end.8
}


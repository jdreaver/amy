; Generated from examples/let.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

declare i64 @abs(i64)

define i64 @main() {
entry:
  %0 = alloca i1
  store i1 true, i1* %0
  %x37 = load i1, i1* %0
  switch i1 %x37, label %case.0.x [
    i1 true, label %case.0.x
    i1 false, label %case.1.x
  ]

case.0.x:                                         ; preds = %entry, %entry
  %x38 = call i64 @f(i64 100)
  %1 = call i64 @abs(i64 %x38)
  br label %case.end.x

case.1.x:                                         ; preds = %entry
  %x39 = call i64 @threeHundred()
  %x40 = call i64 @f(i64 %x39)
  %2 = call i64 @abs(i64 %x40)
  br label %case.end.x

case.end.x:                                       ; preds = %case.1.x, %case.0.x
  %x = phi i64 [ %1, %case.0.x ], [ %2, %case.1.x ]
  %3 = alloca i64
  store i64 %x, i64* %3
  %y = load i64, i64* %3
  %ret = add i64 %x, %y
  ret i64 %ret
}

define private i64 @f(i64 %x) {
entry:
  %0 = alloca i1
  store i1 true, i1* %0
  %res41 = load i1, i1* %0
  switch i1 %res41, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %1 = call i64 @abs(i64 %x)
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %res42 = call i64 @threeHundred()
  %2 = alloca i64
  store i64 %res42, i64* %2
  %3 = load i64, i64* %2
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %1, %case.0.ret ], [ %3, %case.1.ret ]
  ret i64 %ret
}

define private i64 @threeHundred() {
entry:
  %0 = alloca i64
  store i64 300, i64* %0
  %ret = load i64, i64* %0
  ret i64 %ret
}


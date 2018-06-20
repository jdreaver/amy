; ModuleID = 'amy-module'
source_filename = "<string>"

declare i64 @abs(i64)

define private i64 @threeHundred() {
entry:
  %0 = alloca i64
  store i64 300, i64* %0
  %ret = load i64, i64* %0
  ret i64 %ret
}

define private i64 @f(i64 %x) {
entry:
  switch i1 true, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %0 = call i64 @abs(i64 %x)
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %res1 = call i64 @threeHundred()
  %1 = alloca i64
  store i64 %res1, i64* %1
  %2 = load i64, i64* %1
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %0, %case.0.ret ], [ %2, %case.1.ret ]
  ret i64 %ret
}

define i64 @main() {
entry:
  switch i1 true, label %case.0.x [
    i1 true, label %case.0.x
    i1 false, label %case.1.x
  ]

case.0.x:                                         ; preds = %entry, %entry
  %x2 = call i64 @f(i64 100)
  %0 = call i64 @abs(i64 %x2)
  br label %case.end.x

case.1.x:                                         ; preds = %entry
  %x3 = call i64 @threeHundred()
  %x4 = call i64 @f(i64 %x3)
  %1 = call i64 @abs(i64 %x4)
  br label %case.end.x

case.end.x:                                       ; preds = %case.1.x, %case.0.x
  %x = phi i64 [ %0, %case.0.x ], [ %1, %case.1.x ]
  %2 = alloca i64
  store i64 %x, i64* %2
  %y = load i64, i64* %2
  %ret = add i64 %x, %y
  ret i64 %ret
}

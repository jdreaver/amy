; Generated from examples/primops.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 2, i64* %0
  %x = load i64, i64* %0
  %res34 = call i64 @f(i64 %x)
  %ret = call i64 @f(i64 %res34)
  ret i64 %ret
}

define private i64 @f(i64 %x) {
entry:
  %y = add i64 %x, -1
  %res36 = sub i64 3, %y
  %res37 = icmp slt i64 5, %res36
  %0 = alloca i1
  store i1 %res37, i1* %0
  %c31 = load i1, i1* %0
  switch i1 %res37, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %1 = alloca i64
  store i64 100, i64* %1
  %2 = load i64, i64* %1
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %3 = alloca i64
  store i64 200, i64* %3
  %4 = load i64, i64* %3
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %2, %case.0.ret ], [ %4, %case.1.ret ]
  ret i64 %ret
}


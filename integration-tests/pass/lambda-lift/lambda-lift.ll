; ModuleID = 'amy-module'
source_filename = "<string>"

declare i8* @GC_malloc(i64)

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 2, i64* %0
  %z = load i64, i64* %0
  %1 = alloca i64
  store i64 1, i64* %1
  %a = load i64, i64* %1
  %res1 = call i64 @"const_$4"(i64 2, i64 1)
  %res2 = call i64 @"f_$3"(i64 %z, i64 %res1)
  %ret = call i64 @"g_$6"(i64 %a, i64 %z, i64 %res2)
  ret i64 %ret
}

define private i64 @"const_$4"(i64 %x, i64 %y) {
entry:
  %ret = call i64 @"id_$1"(i64 %y)
  ret i64 %ret
}

define private i64 @"f_$3"(i64 %z, i64 %x) {
entry:
  %ret = add i64 %z, %x
  ret i64 %ret
}

define private i64 @"g_$6"(i64 %a, i64 %z, i64 %x) {
entry:
  %res3 = icmp slt i64 %x, 0
  switch i1 %res3, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %0 = alloca i64
  store i64 100, i64* %0
  %1 = load i64, i64* %0
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %res4 = sub i64 %x, %z
  %2 = call i64 @"g'_$5"(i64 %a, i64 %z, i64 %res4)
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %1, %case.0.ret ], [ %2, %case.1.ret ]
  ret i64 %ret
}

define private i64 @"g'_$5"(i64 %a, i64 %z, i64 %x) {
entry:
  %res5 = add i64 %x, %a
  %ret = call i64 @"g_$6"(i64 %a, i64 %z, i64 %res5)
  ret i64 %ret
}

define private i64 @"id_$1"(i64 %x) {
entry:
  %ret = call i64 @"id'_$2"(i64 %x)
  ret i64 %ret
}

define private i64 @"id'_$2"(i64 %y) {
entry:
  %0 = alloca i64
  store i64 %y, i64* %0
  %ret = load i64, i64* %0
  ret i64 %ret
}

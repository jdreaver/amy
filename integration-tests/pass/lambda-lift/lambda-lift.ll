; ModuleID = 'amy-module'
source_filename = "<string>"

%struct.Closure = type { i8, %struct.Closure* (i64*)*, i8, i64* }

declare i8* @GC_malloc(i64)

declare %struct.Closure* @call_closure(%struct.Closure*, i8, i64*)

declare %struct.Closure* @create_closure(i8, %struct.Closure* (i64*)*)

define private %struct.Closure* @"lambda3_$4_closure_wrapper"(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = load i64, i64* %0
  %2 = getelementptr i64, i64* %env, i32 1
  %3 = load i64, i64* %2
  %4 = call i64 @"lambda3_$4"(i64 %1, i64 %3)
  %5 = inttoptr i64 %4 to %struct.Closure*
  ret %struct.Closure* %5
}

define private %struct.Closure* @"lambda8_$9_closure_wrapper"(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = load i64, i64* %0
  %2 = getelementptr i64, i64* %env, i32 1
  %3 = load i64, i64* %2
  %4 = call i64 @"lambda8_$9"(i64 %1, i64 %3)
  %5 = inttoptr i64 %4 to %struct.Closure*
  ret %struct.Closure* %5
}

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 2, i64* %0
  %z = load i64, i64* %0
  %"lambda3_$4_closure1" = call %struct.Closure* @create_closure(i8 2, %struct.Closure* (i64*)* @"lambda3_$4_closure_wrapper")
  %1 = call i8* @GC_malloc(i64 64)
  %2 = bitcast i8* %1 to i64*
  %3 = getelementptr i64, i64* %2, i32 0
  store i64 %z, i64* %3
  %4 = call %struct.Closure* @call_closure(%struct.Closure* %"lambda3_$4_closure1", i8 1, i64* %2)
  %5 = alloca %struct.Closure*
  store %struct.Closure* %4, %struct.Closure** %5
  %f = load %struct.Closure*, %struct.Closure** %5
  %6 = alloca i64
  store i64 1, i64* %6
  %a = load i64, i64* %6
  %res2 = call i64 @"const_$5"(i64 2, i64 1)
  %7 = call i8* @GC_malloc(i64 64)
  %8 = bitcast i8* %7 to i64*
  %9 = getelementptr i64, i64* %8, i32 0
  store i64 %res2, i64* %9
  %10 = call %struct.Closure* @call_closure(%struct.Closure* %f, i8 1, i64* %8)
  %res3 = ptrtoint %struct.Closure* %10 to i64
  %ret = call i64 @"g_$7"(i64 %a, i64 %z, i64 %res3)
  ret i64 %ret
}

define private i64 @"id'_$2"(i64 %x, i64 %y) {
entry:
  %0 = alloca i64
  store i64 %x, i64* %0
  %ret = load i64, i64* %0
  ret i64 %ret
}

define private i64 @"id_$1"(i64 %x) {
entry:
  %ret = call i64 @"id'_$2"(i64 %x, i64 %x)
  ret i64 %ret
}

define private i64 @"lambda3_$4"(i64 %z, i64 %x) {
entry:
  %ret = add i64 %z, %x
  ret i64 %ret
}

define private i64 @"const_$5"(i64 %x, i64 %y) {
entry:
  %ret = call i64 @"id_$1"(i64 %y)
  ret i64 %ret
}

define private i64 @"lambda8_$9"(i64 %x, i64 %y) {
entry:
  %ret = add i64 %x, %y
  ret i64 %ret
}

define private i64 @"g'_$6"(i64 %a, i64 %z, i64 %x) {
entry:
  %"lambda8_$9_closure4" = call %struct.Closure* @create_closure(i8 2, %struct.Closure* (i64*)* @"lambda8_$9_closure_wrapper")
  %0 = call i8* @GC_malloc(i64 64)
  %1 = bitcast i8* %0 to i64*
  %2 = getelementptr i64, i64* %1, i32 0
  store i64 %x, i64* %2
  %3 = call %struct.Closure* @call_closure(%struct.Closure* %"lambda8_$9_closure4", i8 1, i64* %1)
  %4 = alloca %struct.Closure*
  store %struct.Closure* %3, %struct.Closure** %4
  %res5 = load %struct.Closure*, %struct.Closure** %4
  %5 = call i8* @GC_malloc(i64 64)
  %6 = bitcast i8* %5 to i64*
  %7 = getelementptr i64, i64* %6, i32 0
  store i64 %a, i64* %7
  %8 = call %struct.Closure* @call_closure(%struct.Closure* %res5, i8 1, i64* %6)
  %res6 = ptrtoint %struct.Closure* %8 to i64
  %ret = call i64 @"g_$7"(i64 %a, i64 %z, i64 %res6)
  ret i64 %ret
}

define private i64 @"g_$7"(i64 %a, i64 %z, i64 %x) {
entry:
  %res7 = icmp slt i64 %x, 0
  switch i1 %res7, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %0 = alloca i64
  store i64 100, i64* %0
  %1 = load i64, i64* %0
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %res8 = sub i64 %x, %z
  %2 = call i64 @"g'_$6"(i64 %a, i64 %z, i64 %res8)
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %1, %case.0.ret ], [ %2, %case.1.ret ]
  ret i64 %ret
}

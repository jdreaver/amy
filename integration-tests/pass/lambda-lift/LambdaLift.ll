; ModuleID = 'amy-module'
source_filename = "<string>"

%struct.Closure = type { i8, %struct.Closure* (i64*)*, i8, i64* }
%Maybe = type { i1, i64* }

declare i8* @GC_malloc(i64)

declare %struct.Closure* @call_closure(%struct.Closure*, i8, i64*)

declare %struct.Closure* @create_closure(i8, %struct.Closure* (i64*)*)

define private %struct.Closure* @"lambda12_$13_closure_wrapper"(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = load i64, i64* %0
  %2 = getelementptr i64, i64* %env, i32 1
  %3 = load i64, i64* %2
  %4 = call i64 @"lambda12_$13"(i64 %1, i64 %3)
  %5 = inttoptr i64 %4 to %struct.Closure*
  ret %struct.Closure* %5
}

define private %struct.Closure* @"lambda2_$3_closure_wrapper"(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = bitcast i64* %0 to i64**
  %2 = load i64*, i64** %1
  %3 = call %Maybe* @"lambda2_$3"(i64* %2)
  %4 = bitcast %Maybe* %3 to %struct.Closure*
  ret %struct.Closure* %4
}

define private %struct.Closure* @"lambda7_$8_closure_wrapper"(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = load i64, i64* %0
  %2 = getelementptr i64, i64* %env, i32 1
  %3 = load i64, i64* %2
  %4 = call i64 @"lambda7_$8"(i64 %1, i64 %3)
  %5 = inttoptr i64 %4 to %struct.Closure*
  ret %struct.Closure* %5
}

define %struct.Closure* @mkJust() {
entry:
  %"lambda2_$3_closure1" = call %struct.Closure* @create_closure(i8 1, %struct.Closure* (i64*)* @"lambda2_$3_closure_wrapper")
  %0 = alloca %struct.Closure*
  store %struct.Closure* %"lambda2_$3_closure1", %struct.Closure** %0
  %ret = load %struct.Closure*, %struct.Closure** %0
  ret %struct.Closure* %ret
}

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 2, i64* %0
  %z = load i64, i64* %0
  %"lambda7_$8_closure2" = call %struct.Closure* @create_closure(i8 2, %struct.Closure* (i64*)* @"lambda7_$8_closure_wrapper")
  %1 = call i8* @GC_malloc(i64 64)
  %2 = bitcast i8* %1 to i64*
  %3 = getelementptr i64, i64* %2, i32 0
  store i64 %z, i64* %3
  %4 = call %struct.Closure* @call_closure(%struct.Closure* %"lambda7_$8_closure2", i8 1, i64* %2)
  %5 = alloca %struct.Closure*
  store %struct.Closure* %4, %struct.Closure** %5
  %f = load %struct.Closure*, %struct.Closure** %5
  %6 = alloca i64
  store i64 1, i64* %6
  %a = load i64, i64* %6
  %res3 = call i64 @"const'_$9"(i64 2, i64 1)
  %7 = call i8* @GC_malloc(i64 64)
  %8 = bitcast i8* %7 to i64*
  %9 = getelementptr i64, i64* %8, i32 0
  store i64 %res3, i64* %9
  %10 = call %struct.Closure* @call_closure(%struct.Closure* %f, i8 1, i64* %8)
  %res4 = ptrtoint %struct.Closure* %10 to i64
  %ret = call i64 @"g_$11"(i64 %a, i64 %z, i64 %res4)
  ret i64 %ret
}

define %Maybe* @"lambda2_$3"(i64* %_x1) {
entry:
  %0 = getelementptr %Maybe, %Maybe* null, i32 1
  %1 = ptrtoint %Maybe* %0 to i64
  %2 = call i8* @GC_malloc(i64 %1)
  %ret = bitcast i8* %2 to %Maybe*
  %ret1 = alloca %Maybe
  %3 = getelementptr %Maybe, %Maybe* %ret1, i32 0, i32 0
  store i1 true, i1* %3
  %4 = getelementptr %Maybe, %Maybe* %ret1, i32 0, i32 1
  store i64* %_x1, i64** %4
  ret %Maybe* %ret1
}

define i64 @"id''_$5"(i64 %x, i64 %y) {
entry:
  %0 = alloca i64
  store i64 %x, i64* %0
  %ret = load i64, i64* %0
  ret i64 %ret
}

define i64 @"id'_$4"(i64 %x) {
entry:
  %ret = call i64 @"id''_$5"(i64 %x, i64 %x)
  ret i64 %ret
}

define i64 @"lambda7_$8"(i64 %z, i64 %_x6) {
entry:
  %ret = add i64 %z, %_x6
  ret i64 %ret
}

define i64 @"const'_$9"(i64 %x, i64 %y) {
entry:
  %ret = call i64 @"id'_$4"(i64 %y)
  ret i64 %ret
}

define i64 @"lambda12_$13"(i64 %x, i64 %y) {
entry:
  %ret = add i64 %x, %y
  ret i64 %ret
}

define i64 @"g'_$10"(i64 %a, i64 %z, i64 %x) {
entry:
  %"lambda12_$13_closure5" = call %struct.Closure* @create_closure(i8 2, %struct.Closure* (i64*)* @"lambda12_$13_closure_wrapper")
  %0 = call i8* @GC_malloc(i64 64)
  %1 = bitcast i8* %0 to i64*
  %2 = getelementptr i64, i64* %1, i32 0
  store i64 %x, i64* %2
  %3 = call %struct.Closure* @call_closure(%struct.Closure* %"lambda12_$13_closure5", i8 1, i64* %1)
  %4 = alloca %struct.Closure*
  store %struct.Closure* %3, %struct.Closure** %4
  %res6 = load %struct.Closure*, %struct.Closure** %4
  %5 = call i8* @GC_malloc(i64 64)
  %6 = bitcast i8* %5 to i64*
  %7 = getelementptr i64, i64* %6, i32 0
  store i64 %a, i64* %7
  %8 = call %struct.Closure* @call_closure(%struct.Closure* %res6, i8 1, i64* %6)
  %res7 = ptrtoint %struct.Closure* %8 to i64
  %ret = call i64 @"g_$11"(i64 %a, i64 %z, i64 %res7)
  ret i64 %ret
}

define i64 @"g_$11"(i64 %a, i64 %z, i64 %x) {
entry:
  %res8 = icmp slt i64 %x, 0
  switch i1 %res8, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %0 = alloca i64
  store i64 100, i64* %0
  %1 = load i64, i64* %0
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %res9 = sub i64 %x, %z
  %2 = call i64 @"g'_$10"(i64 %a, i64 %z, i64 %res9)
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %1, %case.0.ret ], [ %2, %case.1.ret ]
  ret i64 %ret
}

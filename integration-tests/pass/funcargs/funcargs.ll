; ModuleID = 'amy-module'
source_filename = "<string>"

%struct.Closure = type { i8, void (...)*, i8, i64* }

declare i8* @GC_malloc(i64)

declare %struct.Closure* @call_closure(%struct.Closure*, i8, i64*)

declare %struct.Closure* @create_closure(i8, void (...)*, i8, i64*)

define private i64 @myAdd_closure_wrapper(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = load i64, i64* %0
  %2 = getelementptr i64, i64* %env, i32 1
  %3 = load i64, i64* %2
  %4 = call i64 @myAdd(i64 %1, i64 %3)
  ret i64 %4
}

define private i64 @myAdd(i64 %x, i64 %y) {
entry:
  %ret = add i64 %x, %y
  ret i64 %ret
}

define private i64 @apply(%struct.Closure* %f) {
entry:
  %0 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %1 = bitcast i8* %0 to [2 x i64]*
  %2 = getelementptr [2 x i64], [2 x i64]* %1, i32 1, i32 0
  store i64 1, i64* %2
  %3 = getelementptr [2 x i64], [2 x i64]* %1, i32 1, i32 1
  store i64 2, i64* %3
  %4 = bitcast [2 x i64]* %1 to i64*
  %5 = call %struct.Closure* @call_closure(%struct.Closure* %f, i8 2, i64* %4)
  %ret = ptrtoint %struct.Closure* %5 to i64
  ret i64 %ret
}

define i64 @main() {
entry:
  %0 = call i8* @GC_malloc(i64 0)
  %1 = bitcast i8* %0 to [0 x i64]*
  %2 = bitcast [0 x i64]* %1 to i64*
  %myAdd_closure1 = call %struct.Closure* @create_closure(i8 2, void (...)* bitcast (i64 (i64*)* @myAdd_closure_wrapper to void (...)*), i8 0, i64* %2)
  %ret = call i64 @apply(%struct.Closure* %myAdd_closure1)
  ret i64 %ret
}

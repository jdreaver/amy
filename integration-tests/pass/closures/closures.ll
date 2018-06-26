; ModuleID = 'amy-module'
source_filename = "<string>"

%struct.Closure = type { i8, void (...)*, i8, i64* }

declare i8* @GC_malloc(i64)

declare %struct.Closure* @call_closure(%struct.Closure*, i8, i64*)

declare %struct.Closure* @create_closure(i8, void (...)*, i8, i64*)

define private %struct.Closure* @inc_closure_wrapper(i64* %env) {
entry:
  %0 = call %struct.Closure* @inc()
  ret %struct.Closure* %0
}

define private %struct.Closure* @myAdd_closure_wrapper(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = load i64, i64* %0
  %2 = getelementptr i64, i64* %env, i32 1
  %3 = bitcast i64* %2 to double*
  %4 = load double, double* %3
  %5 = call i64 @myAdd(i64 %1, double %4)
  %6 = inttoptr i64 %5 to %struct.Closure*
  ret %struct.Closure* %6
}

define private i64 @myAdd(i64 %x, double %y) {
entry:
  %res1 = fptoui double %y to i64
  %ret = add i64 %x, %res1
  ret i64 %ret
}

define private %struct.Closure* @inc() {
entry:
  %0 = call i8* @GC_malloc(i64 0)
  %1 = bitcast i8* %0 to i64*
  %myAdd_closure2 = call %struct.Closure* @create_closure(i8 2, void (...)* bitcast (%struct.Closure* (i64*)* @myAdd_closure_wrapper to void (...)*), i8 0, i64* %1)
  %2 = call i8* @GC_malloc(i64 64)
  %3 = bitcast i8* %2 to i64*
  %4 = getelementptr i64, i64* %3, i32 0
  store i64 1, i64* %4
  %5 = call %struct.Closure* @call_closure(%struct.Closure* %myAdd_closure2, i8 1, i64* %3)
  %6 = alloca %struct.Closure*
  store %struct.Closure* %5, %struct.Closure** %6
  %ret = load %struct.Closure*, %struct.Closure** %6
  ret %struct.Closure* %ret
}

define i64 @main() {
entry:
  %0 = call i8* @GC_malloc(i64 0)
  %1 = bitcast i8* %0 to i64*
  %inc_closure3 = call %struct.Closure* @create_closure(i8 0, void (...)* bitcast (%struct.Closure* (i64*)* @inc_closure_wrapper to void (...)*), i8 0, i64* %1)
  %2 = call i8* @GC_malloc(i64 64)
  %3 = bitcast i8* %2 to i64*
  %4 = getelementptr i64, i64* %3, i32 0
  %5 = bitcast i64* %4 to double*
  store double 2.100000e+00, double* %5
  %6 = call %struct.Closure* @call_closure(%struct.Closure* %inc_closure3, i8 1, i64* %3)
  %ret = ptrtoint %struct.Closure* %6 to i64
  ret i64 %ret
}

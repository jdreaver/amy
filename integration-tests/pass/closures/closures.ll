; ModuleID = 'amy-module'
source_filename = "<string>"

%struct.Closure = type { i8, void (...)*, i8, i64* }

declare i8* @GC_malloc(i64)

declare %struct.Closure* @call_closure(%struct.Closure*, i8, i64*)

declare %struct.Closure* @create_closure(i8, void (...)*, i8, i64*)

define private %struct.Closure* @incDouble_closure_wrapper(i64* %env) {
entry:
  %0 = call %struct.Closure* @incDouble()
  ret %struct.Closure* %0
}

define private %struct.Closure* @inc_closure_wrapper(i64* %env) {
entry:
  %0 = call %struct.Closure* @inc()
  ret %struct.Closure* %0
}

define private %struct.Closure* @myAddDouble_closure_wrapper(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = bitcast i64* %0 to double*
  %2 = load double, double* %1
  %3 = getelementptr i64, i64* %env, i32 1
  %4 = bitcast i64* %3 to double*
  %5 = load double, double* %4
  %6 = call double @myAddDouble(double %2, double %5)
  %7 = bitcast double %6 to i64
  %8 = inttoptr i64 %7 to %struct.Closure*
  ret %struct.Closure* %8
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

define private double @myAddDouble(double %x, double %y) {
entry:
  %ret = fadd double %x, %y
  ret double %ret
}

define private i64 @myAdd(i64 %x, double %y) {
entry:
  %res1 = fptoui double %y to i64
  %ret = add i64 %x, %res1
  ret i64 %ret
}

define private %struct.Closure* @incDouble() {
entry:
  %0 = call i8* @GC_malloc(i64 0)
  %1 = bitcast i8* %0 to i64*
  %myAddDouble_closure2 = call %struct.Closure* @create_closure(i8 2, void (...)* bitcast (%struct.Closure* (i64*)* @myAddDouble_closure_wrapper to void (...)*), i8 0, i64* %1)
  %2 = call i8* @GC_malloc(i64 64)
  %3 = bitcast i8* %2 to i64*
  %4 = getelementptr i64, i64* %3, i32 0
  %5 = bitcast i64* %4 to double*
  store double 1.010000e+00, double* %5
  %6 = call %struct.Closure* @call_closure(%struct.Closure* %myAddDouble_closure2, i8 1, i64* %3)
  %7 = alloca %struct.Closure*
  store %struct.Closure* %6, %struct.Closure** %7
  %ret = load %struct.Closure*, %struct.Closure** %7
  ret %struct.Closure* %ret
}

define private %struct.Closure* @inc() {
entry:
  %0 = call i8* @GC_malloc(i64 0)
  %1 = bitcast i8* %0 to i64*
  %myAdd_closure3 = call %struct.Closure* @create_closure(i8 2, void (...)* bitcast (%struct.Closure* (i64*)* @myAdd_closure_wrapper to void (...)*), i8 0, i64* %1)
  %2 = call i8* @GC_malloc(i64 64)
  %3 = bitcast i8* %2 to i64*
  %4 = getelementptr i64, i64* %3, i32 0
  store i64 1, i64* %4
  %5 = call %struct.Closure* @call_closure(%struct.Closure* %myAdd_closure3, i8 1, i64* %3)
  %6 = alloca %struct.Closure*
  store %struct.Closure* %5, %struct.Closure** %6
  %ret = load %struct.Closure*, %struct.Closure** %6
  ret %struct.Closure* %ret
}

define i64 @main() {
entry:
  %0 = call i8* @GC_malloc(i64 0)
  %1 = bitcast i8* %0 to i64*
  %incDouble_closure4 = call %struct.Closure* @create_closure(i8 0, void (...)* bitcast (%struct.Closure* (i64*)* @incDouble_closure_wrapper to void (...)*), i8 0, i64* %1)
  %2 = call i8* @GC_malloc(i64 64)
  %3 = bitcast i8* %2 to i64*
  %4 = getelementptr i64, i64* %3, i32 0
  %5 = bitcast i64* %4 to double*
  store double 2.010000e+00, double* %5
  %6 = call %struct.Closure* @call_closure(%struct.Closure* %incDouble_closure4, i8 1, i64* %3)
  %7 = ptrtoint %struct.Closure* %6 to i64
  %res5 = bitcast i64 %7 to double
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to i64*
  %inc_closure6 = call %struct.Closure* @create_closure(i8 0, void (...)* bitcast (%struct.Closure* (i64*)* @inc_closure_wrapper to void (...)*), i8 0, i64* %9)
  %10 = call i8* @GC_malloc(i64 64)
  %11 = bitcast i8* %10 to i64*
  %12 = getelementptr i64, i64* %11, i32 0
  %13 = bitcast i64* %12 to double*
  store double %res5, double* %13
  %14 = call %struct.Closure* @call_closure(%struct.Closure* %inc_closure6, i8 1, i64* %11)
  %ret = ptrtoint %struct.Closure* %14 to i64
  ret i64 %ret
}

; ModuleID = 'amy-module'
source_filename = "<string>"

%struct.Closure = type { i8, %struct.Closure* (i64*)*, i8, i64* }

declare i8* @GC_malloc(i64)

declare %struct.Closure* @call_closure(%struct.Closure*, i8, i64*)

declare %struct.Closure* @create_closure(i8, %struct.Closure* (i64*)*)

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

define private %struct.Closure* @"lambda1_$2_closure_wrapper"(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = bitcast i64* %0 to double*
  %2 = load double, double* %1
  %3 = getelementptr i64, i64* %env, i32 1
  %4 = bitcast i64* %3 to double*
  %5 = load double, double* %4
  %6 = call double @"lambda1_$2"(double %2, double %5)
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

define private %struct.Closure* @myAddDouble(double %x) {
entry:
  %"lambda1_$2_closure1" = call %struct.Closure* @create_closure(i8 2, %struct.Closure* (i64*)* @"lambda1_$2_closure_wrapper")
  %0 = call i8* @GC_malloc(i64 64)
  %1 = bitcast i8* %0 to i64*
  %2 = getelementptr i64, i64* %1, i32 0
  %3 = bitcast i64* %2 to double*
  store double %x, double* %3
  %4 = call %struct.Closure* @call_closure(%struct.Closure* %"lambda1_$2_closure1", i8 1, i64* %1)
  %5 = alloca %struct.Closure*
  store %struct.Closure* %4, %struct.Closure** %5
  %ret = load %struct.Closure*, %struct.Closure** %5
  ret %struct.Closure* %ret
}

define private i64 @myAdd(i64 %x, double %y) {
entry:
  %res2 = fptoui double %y to i64
  %ret = add i64 %x, %res2
  ret i64 %ret
}

define private %struct.Closure* @incDouble() {
entry:
  %ret = call %struct.Closure* @myAddDouble(double 1.010000e+00)
  ret %struct.Closure* %ret
}

define private %struct.Closure* @inc() {
entry:
  %myAdd_closure3 = call %struct.Closure* @create_closure(i8 2, %struct.Closure* (i64*)* @myAdd_closure_wrapper)
  %0 = call i8* @GC_malloc(i64 64)
  %1 = bitcast i8* %0 to i64*
  %2 = getelementptr i64, i64* %1, i32 0
  store i64 1, i64* %2
  %3 = call %struct.Closure* @call_closure(%struct.Closure* %myAdd_closure3, i8 1, i64* %1)
  %4 = alloca %struct.Closure*
  store %struct.Closure* %3, %struct.Closure** %4
  %ret = load %struct.Closure*, %struct.Closure** %4
  ret %struct.Closure* %ret
}

define i64 @main() {
entry:
  %incDouble_closure4 = call %struct.Closure* @create_closure(i8 0, %struct.Closure* (i64*)* @incDouble_closure_wrapper)
  %0 = call i8* @GC_malloc(i64 64)
  %1 = bitcast i8* %0 to i64*
  %2 = getelementptr i64, i64* %1, i32 0
  %3 = bitcast i64* %2 to double*
  store double 2.010000e+00, double* %3
  %4 = call %struct.Closure* @call_closure(%struct.Closure* %incDouble_closure4, i8 1, i64* %1)
  %5 = ptrtoint %struct.Closure* %4 to i64
  %res5 = bitcast i64 %5 to double
  %inc_closure6 = call %struct.Closure* @create_closure(i8 0, %struct.Closure* (i64*)* @inc_closure_wrapper)
  %6 = call i8* @GC_malloc(i64 64)
  %7 = bitcast i8* %6 to i64*
  %8 = getelementptr i64, i64* %7, i32 0
  %9 = bitcast i64* %8 to double*
  store double %res5, double* %9
  %10 = call %struct.Closure* @call_closure(%struct.Closure* %inc_closure6, i8 1, i64* %7)
  %ret = ptrtoint %struct.Closure* %10 to i64
  ret i64 %ret
}

define private double @"lambda1_$2"(double %x, double %y) {
entry:
  %ret = fadd double %x, %y
  ret double %ret
}

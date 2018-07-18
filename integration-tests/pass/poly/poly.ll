; ModuleID = 'amy-module'
source_filename = "<string>"

declare i8* @GC_malloc(i64)

declare i64* @const(i64*, i64*)

declare i64* @id(i64*)

define i64 @main() {
entry:
  %0 = getelementptr double, double* null, i32 1
  %1 = ptrtoint double* %0 to i64
  %2 = call i8* @GC_malloc(i64 %1)
  %3 = bitcast i8* %2 to double*
  store double 3.100000e+00, double* %3
  %4 = bitcast double* %3 to i64*
  %5 = call i64* @id(i64* %4)
  %6 = bitcast i64* %5 to double*
  %res1 = load double, double* %6
  %7 = getelementptr double, double* null, i32 1
  %8 = ptrtoint double* %7 to i64
  %9 = call i8* @GC_malloc(i64 %8)
  %10 = bitcast i8* %9 to double*
  store double %res1, double* %10
  %11 = bitcast double* %10 to i64*
  %12 = getelementptr double, double* null, i32 1
  %13 = ptrtoint double* %12 to i64
  %14 = call i8* @GC_malloc(i64 %13)
  %15 = bitcast i8* %14 to double*
  store double 5.100000e+00, double* %15
  %16 = bitcast double* %15 to i64*
  %17 = call i64* @const(i64* %11, i64* %16)
  %18 = bitcast i64* %17 to double*
  %res2 = load double, double* %18
  %ret = fptoui double %res2 to i64
  ret i64 %ret
}

; ModuleID = 'amy-module'
source_filename = "<string>"

declare i8* @malloc(i64)

define i64 @main() {
entry:
  %0 = call i8* @malloc(i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64))
  %1 = bitcast i8* %0 to double*
  store double 3.100000e+00, double* %1
  %2 = bitcast double* %1 to i64*
  %3 = call i64* @id(i64* %2)
  %4 = bitcast i64* %3 to double*
  %res1 = load double, double* %4
  %5 = call i8* @malloc(i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64))
  %6 = bitcast i8* %5 to double*
  store double %res1, double* %6
  %7 = bitcast double* %6 to i64*
  %8 = call i8* @malloc(i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64))
  %9 = bitcast i8* %8 to double*
  store double 5.100000e+00, double* %9
  %10 = bitcast double* %9 to i64*
  %11 = call i64* @const(i64* %7, i64* %10)
  %12 = bitcast i64* %11 to double*
  %res2 = load double, double* %12
  %ret = fptoui double %res2 to i64
  ret i64 %ret
}

define private i64* @id(i64* %x) {
entry:
  %0 = alloca i64*
  store i64* %x, i64** %0
  %ret = load i64*, i64** %0
  ret i64* %ret
}

define private i64* @const(i64* %x, i64* %y) {
entry:
  %0 = alloca i64*
  store i64* %x, i64** %0
  %ret = load i64*, i64** %0
  ret i64* %ret
}

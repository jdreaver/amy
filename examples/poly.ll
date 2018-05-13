; Generated from examples/poly.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = alloca double
  store double 3.100000e+00, double* %0
  %1 = bitcast double* %0 to i64*
  %2 = call i64* @id(i64* %1)
  %3 = bitcast i64* %2 to double*
  %res38 = load double, double* %3
  %4 = alloca double
  store double %res38, double* %4
  %5 = bitcast double* %4 to i64*
  %6 = alloca double
  store double 5.100000e+00, double* %6
  %7 = bitcast double* %6 to i64*
  %8 = call i64* @const(i64* %5, i64* %7)
  %9 = bitcast i64* %8 to double*
  %res39 = load double, double* %9
  %ret = fptoui double %res39 to i64
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


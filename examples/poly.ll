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
  %4 = load double, double* %3
  %5 = alloca double
  store double %4, double* %5
  %res38 = load double, double* %5
  %6 = alloca double
  store double %res38, double* %6
  %7 = bitcast double* %6 to i64*
  %8 = alloca double
  store double 5.100000e+00, double* %8
  %9 = bitcast double* %8 to i64*
  %10 = call i64* @const(i64* %7, i64* %9)
  %11 = bitcast i64* %10 to double*
  %12 = load double, double* %11
  %13 = alloca double
  store double %12, double* %13
  %res39 = load double, double* %13
  %14 = fptoui double %res39 to i64
  ret i64 %14
}

define private i64* @id(i64* %x) {
entry:
  ret i64* %x
}

define private i64* @const(i64* %x, i64* %y) {
entry:
  ret i64* %x
}


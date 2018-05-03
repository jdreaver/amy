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
  %6 = bitcast double* %5 to i64*
  %7 = alloca double
  store double 5.100000e+00, double* %7
  %8 = bitcast double* %7 to i64*
  %9 = call i64* @const(i64* %6, i64* %8)
  %10 = bitcast i64* %9 to double*
  %11 = load double, double* %10
  %12 = fptoui double %11 to i64
  ret i64 %12
}

define private i64* @id(i64* %x) {
entry:
  ret i64* %x
}

define private i64* @const(i64* %x, i64* %y) {
entry:
  ret i64* %x
}


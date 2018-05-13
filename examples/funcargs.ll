; Generated from examples/funcargs.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = call i64 @apply(i64 (i64, i64)* @myAdd)
  %1 = alloca i64
  store i64 %0, i64* %1
  %ret = load i64, i64* %1
  ret i64 %0
}

define private i64 @apply(i64 (i64, i64)* %f) {
entry:
  %0 = call i64 %f(i64 1, i64 2)
  %1 = alloca i64
  store i64 %0, i64* %1
  %ret = load i64, i64* %1
  ret i64 %0
}

define private i64 @myAdd(i64 %x, i64 %y) {
entry:
  %ret = add i64 %x, %y
  ret i64 %ret
}


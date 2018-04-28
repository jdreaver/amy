; Generated from examples/funcargs.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = call i64 @apply(i64 (i64, i64)* @myAdd)
  ret i64 %0
}

define private i64 @apply(i64 (i64, i64)* %f) {
entry:
  %0 = call i64 %f(i64 1, i64 2)
  ret i64 %0
}

define private i64 @myAdd(i64 %x, i64 %y) {
entry:
  %0 = add i64 %x, %y
  ret i64 %0
}


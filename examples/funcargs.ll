; Generated from examples/funcargs.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i32 @main() {
entry:
  %0 = call i32 @apply(i32 (i32, i32)* @myAdd)
  ret i32 %0
}

define i32 @apply(i32 (i32, i32)* %f) {
entry:
  %0 = call i32 %f(i32 1, i32 2)
  ret i32 %0
}

define i32 @myAdd(i32 %x, i32 %y) {
entry:
  %0 = add i32 %x, %y
  ret i32 %0
}


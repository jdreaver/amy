; ModuleID = 'amy-module'
source_filename = "<string>"

declare i8* @GC_malloc(i64)

define private i64 @myAdd(i64 %x, i64 %y) {
entry:
  %ret = add i64 %x, %y
  ret i64 %ret
}

define private i64 @apply(i64 (i64, i64)* %f) {
entry:
  %ret = call i64 %f(i64 1, i64 2)
  ret i64 %ret
}

define i64 @main() {
entry:
  %ret = call i64 @apply(i64 (i64, i64)* @myAdd)
  ret i64 %ret
}

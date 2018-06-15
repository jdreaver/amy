; ModuleID = 'amy-module'
source_filename = "<string>"

declare i8* @GC_malloc(i64)

define i64 @main() {
entry:
  %ret = call i64 @"id_$1"(i64 1)
  ret i64 %ret
}

define private i64 @"id_$1"(i64 %x) {
entry:
  %0 = alloca i64
  store i64 %x, i64* %0
  %ret = load i64, i64* %0
  ret i64 %ret
}

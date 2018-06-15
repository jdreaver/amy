; ModuleID = 'amy-module'
source_filename = "<string>"

declare i8* @GC_malloc(i64)

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 2, i64* %0
  %z = load i64, i64* %0
  %res1 = call i64 @"const_$3"(i64 2, i64 1)
  %ret = call i64 @"f_$2"(i64 %z, i64 %res1)
  ret i64 %ret
}

define private i64 @"const_$3"(i64 %x, i64 %y) {
entry:
  %ret = call i64 @"id_$1"(i64 %y)
  ret i64 %ret
}

define private i64 @"f_$2"(i64 %z, i64 %x) {
entry:
  %ret = add i64 %z, %x
  ret i64 %ret
}

define private i64 @"id_$1"(i64 %x) {
entry:
  %0 = alloca i64
  store i64 %x, i64* %0
  %ret = load i64, i64* %0
  ret i64 %ret
}

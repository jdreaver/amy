; ModuleID = 'amy-module'
source_filename = "<string>"

declare i8* @GC_malloc(i64)

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 1, i64* %0
  %a = load i64, i64* %0
  %1 = alloca i64
  store i64 2, i64* %1
  %b = load i64, i64* %1
  %2 = alloca i64
  store i64 3, i64* %2
  %c = load i64, i64* %2
  %3 = alloca i64
  store i64 4, i64* %3
  %d = load i64, i64* %3
  %4 = alloca i64
  store i64 5, i64* %4
  %e = load i64, i64* %4
  %5 = alloca i64
  store i64 %a, i64* %5
  %ret = load i64, i64* %5
  ret i64 %ret
}

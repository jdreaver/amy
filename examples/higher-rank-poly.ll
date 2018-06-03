; Generated from examples/higher-rank-poly.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 1, i64* %0
  %1 = call i64* @idFancy(i64* (i64*)* @id, i64* %0)
  %ret = load i64, i64* %1
  ret i64 %ret
}

define private i64* @id(i64* %x) {
entry:
  %0 = alloca i64*
  store i64* %x, i64** %0
  %ret = load i64*, i64** %0
  ret i64* %ret
}

define private i64* @idFancy(i64* (i64*)* %f, i64* %x) {
entry:
  %ret = call i64* %f(i64* %x)
  ret i64* %ret
}


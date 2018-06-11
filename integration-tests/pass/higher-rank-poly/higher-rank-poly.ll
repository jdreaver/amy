; ModuleID = 'amy-module'
source_filename = "<string>"

declare i8* @malloc(i64)

define i64 @main() {
entry:
  %0 = call i8* @malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %1 = bitcast i8* %0 to i64*
  store i64 1, i64* %1
  %2 = call i64* @idFancy(i64* (i64*)* @id, i64* %1)
  %ret = load i64, i64* %2
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

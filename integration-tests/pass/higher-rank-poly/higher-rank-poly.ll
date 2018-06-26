; ModuleID = 'amy-module'
source_filename = "<string>"

%struct.Closure = type { i8, void (...)*, i8, i64* }

declare i8* @GC_malloc(i64)

declare %struct.Closure* @call_closure(%struct.Closure*, i8, i64*)

declare %struct.Closure* @create_closure(i8, void (...)*, i8, i64*)

define private %struct.Closure* @id_closure_wrapper(i64* %env) {
entry:
  %0 = getelementptr i64, i64* %env, i32 0
  %1 = bitcast i64* %0 to i64**
  %2 = load i64*, i64** %1
  %3 = call i64* @id(i64* %2)
  %4 = bitcast i64* %3 to %struct.Closure*
  ret %struct.Closure* %4
}

define private i64* @idFancy(%struct.Closure* %f, i64* %x) {
entry:
  %0 = call i8* @GC_malloc(i64 64)
  %1 = bitcast i8* %0 to i64*
  %2 = getelementptr i64, i64* %1, i32 0
  %3 = bitcast i64* %2 to i64**
  store i64* %x, i64** %3
  %4 = call %struct.Closure* @call_closure(%struct.Closure* %f, i8 1, i64* %1)
  %ret = bitcast %struct.Closure* %4 to i64*
  ret i64* %ret
}

define private i64* @id(i64* %x) {
entry:
  %0 = alloca i64*
  store i64* %x, i64** %0
  %ret = load i64*, i64** %0
  ret i64* %ret
}

define i64 @main() {
entry:
  %0 = call i8* @GC_malloc(i64 0)
  %1 = bitcast i8* %0 to i64*
  %id_closure1 = call %struct.Closure* @create_closure(i8 1, void (...)* bitcast (%struct.Closure* (i64*)* @id_closure_wrapper to void (...)*), i8 0, i64* %1)
  %2 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %3 = bitcast i8* %2 to i64*
  store i64 1, i64* %3
  %4 = call i64* @idFancy(%struct.Closure* %id_closure1, i64* %3)
  %ret = load i64, i64* %4
  ret i64 %ret
}

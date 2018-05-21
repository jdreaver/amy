; Generated from examples/records.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%List = type { i1, i64* }

define i64 @main() {
entry:
  %0 = alloca i64
  store i64 1, i64* %0
  %ret = load i64, i64* %0
  ret i64 %ret
}

define private { i64* } @g(i64* %x) {
entry:
  %ret = alloca { i64* }
  %0 = alloca i64*
  store i64* %x, i64** %0
  %x1 = load i64*, i64** %0
  %1 = getelementptr { i64* }, { i64* }* %ret, i32 0, i32 0
  store i64* %x1, i64** %1
  ret { i64* }* %ret
}

define private %List* @h(i64* %x) {
entry:
  %res14 = alloca { i64*, %List* }
  %0 = alloca i64*
  store i64* %x, i64** %0
  %car = load i64*, i64** %0
  %1 = getelementptr { i64*, %List* }, { i64*, %List* }* %res14, i32 0, i32 0
  store i64* %car, i64** %1
  %cdr13 = alloca { i64*, %List* }
  %2 = alloca i64*
  store i64* %x, i64** %2
  %car1 = load i64*, i64** %2
  %3 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr13, i32 0, i32 0
  store i64* %car1, i64** %3
  %cdr12 = alloca %List
  %4 = getelementptr %List, %List* %cdr12, i32 0, i32 0
  store i1 false, i1* %4
  %5 = alloca %List*
  store %List* %cdr12, %List** %5
  %cdr = load %List*, %List** %5
  %6 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr13, i32 0, i32 1
  store %List* %cdr, %List** %6
  %cdr2 = alloca %List
  %7 = getelementptr %List, %List* %cdr2, i32 0, i32 0
  store i1 true, i1* %7
  %8 = alloca { i64*, %List* }
  store { i64*, %List* }* %cdr13, { i64*, %List* }* %8
  %9 = bitcast { i64*, %List* }* %8 to i64*
  %10 = getelementptr %List, %List* %cdr2, i32 0, i32 1
  store i64* %9, i64** %10
  %11 = getelementptr { i64*, %List* }, { i64*, %List* }* %res14, i32 0, i32 1
  store %List* %cdr2, %List** %11
  %ret = alloca %List
  %12 = getelementptr %List, %List* %ret, i32 0, i32 0
  store i1 true, i1* %12
  %13 = alloca { i64*, %List* }
  store { i64*, %List* }* %res14, { i64*, %List* }* %13
  %14 = bitcast { i64*, %List* }* %13 to i64*
  %15 = getelementptr %List, %List* %ret, i32 0, i32 1
  store i64* %14, i64** %15
  ret %List* %ret
}


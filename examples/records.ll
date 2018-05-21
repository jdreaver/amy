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

define private { i64*, i64 }* @g(i64* %x) {
entry:
  %ret = alloca { i64*, i64 }
  %0 = getelementptr { i64*, i64 }, { i64*, i64 }* %ret, i32 0, i32 0
  store i64* %x, i64** %0
  %1 = getelementptr { i64*, i64 }, { i64*, i64 }* %ret, i32 0, i32 1
  store i64 1, i64* %1
  ret { i64*, i64 }* %ret
}

define private %List* @h(i64* %x) {
entry:
  %cdr12 = alloca %List
  %0 = getelementptr %List, %List* %cdr12, i32 0, i32 0
  store i1 false, i1* %0
  %cdr13 = alloca { i64*, %List* }
  %1 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr13, i32 0, i32 0
  store i64* %x, i64** %1
  %2 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr13, i32 0, i32 1
  store %List* %cdr12, %List** %2
  %cdr14 = alloca %List
  %3 = getelementptr %List, %List* %cdr14, i32 0, i32 0
  store i1 true, i1* %3
  %4 = bitcast { i64*, %List* }* %cdr13 to i64*
  %5 = getelementptr %List, %List* %cdr14, i32 0, i32 1
  store i64* %4, i64** %5
  %res15 = alloca { i64*, %List* }
  %6 = getelementptr { i64*, %List* }, { i64*, %List* }* %res15, i32 0, i32 0
  store i64* %x, i64** %6
  %7 = getelementptr { i64*, %List* }, { i64*, %List* }* %res15, i32 0, i32 1
  store %List* %cdr14, %List** %7
  %ret = alloca %List
  %8 = getelementptr %List, %List* %ret, i32 0, i32 0
  store i1 true, i1* %8
  %9 = bitcast { i64*, %List* }* %res15 to i64*
  %10 = getelementptr %List, %List* %ret, i32 0, i32 1
  store i64* %9, i64** %10
  ret %List* %ret
}


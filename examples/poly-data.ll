; Generated from examples/poly-data.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%MyType = type { i1, i64* }

define i64 @main() {
entry:
  %res77 = call %MyType* @f()
  %0 = getelementptr %MyType, %MyType* %res77, i32 0, i32 0
  %1 = load i1, i1* %0
  %2 = getelementptr %MyType, %MyType* %res77, i32 0, i32 1
  %3 = load i64*, i64** %2
  switch i1 %1, label %case.0.ret [
    i1 false, label %case.0.ret
    i1 true, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u70 = load i64, i64* %3
  %4 = alloca i64
  store i64 %_u70, i64* %4
  %5 = load i64, i64* %4
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %6 = alloca i64*
  store i64* %3, i64** %6
  %_u71 = load i64*, i64** %6
  %7 = alloca i64
  store i64 2, i64* %7
  %8 = load i64, i64* %7
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi i64 [ %5, %case.0.ret ], [ %8, %case.1.ret ]
  ret i64 %ret
}

define private %MyType* @f() {
entry:
  %res78 = alloca %MyType
  %0 = getelementptr %MyType, %MyType* %res78, i32 0, i32 0
  store i1 false, i1* %0
  %1 = alloca i64
  store i64 42, i64* %1
  %2 = getelementptr %MyType, %MyType* %res78, i32 0, i32 1
  store i64* %1, i64** %2
  %3 = getelementptr %MyType, %MyType* %res78, i32 0, i32 0
  %4 = load i1, i1* %3
  %5 = getelementptr %MyType, %MyType* %res78, i32 0, i32 1
  %6 = load i64*, i64** %5
  switch i1 %4, label %case.0.ret [
    i1 false, label %case.0.ret
    i1 true, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u73 = load i64, i64* %6
  %7 = alloca %MyType
  %8 = getelementptr %MyType, %MyType* %7, i32 0, i32 0
  store i1 false, i1* %8
  %9 = alloca i64
  store i64 %_u73, i64* %9
  %10 = getelementptr %MyType, %MyType* %7, i32 0, i32 1
  store i64* %9, i64** %10
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %11 = alloca i64*
  store i64* %6, i64** %11
  %_u74 = load i64*, i64** %11
  %12 = alloca %MyType
  %13 = getelementptr %MyType, %MyType* %12, i32 0, i32 0
  store i1 true, i1* %13
  %14 = getelementptr %MyType, %MyType* %12, i32 0, i32 1
  store i64* %_u74, i64** %14
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi %MyType* [ %7, %case.0.ret ], [ %12, %case.1.ret ]
  ret %MyType* %ret
}

define private %MyType* @g(i64* %x) {
entry:
  %ret = alloca %MyType
  %0 = getelementptr %MyType, %MyType* %ret, i32 0, i32 0
  store i1 false, i1* %0
  %1 = getelementptr %MyType, %MyType* %ret, i32 0, i32 1
  store i64* %x, i64** %1
  ret %MyType* %ret
}

define private %MyType* @h() {
entry:
  %ret = alloca %MyType
  %0 = getelementptr %MyType, %MyType* %ret, i32 0, i32 0
  store i1 true, i1* %0
  %1 = alloca i64
  store i64 1, i64* %1
  %2 = getelementptr %MyType, %MyType* %ret, i32 0, i32 1
  store i64* %1, i64** %2
  ret %MyType* %ret
}


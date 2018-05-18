; Generated from examples/poly-data.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%Either = type { i1, i64* }

define i64 @main() {
entry:
  %res77 = call %Either* @f()
  %0 = getelementptr %Either, %Either* %res77, i32 0, i32 0
  %1 = load i1, i1* %0
  %2 = getelementptr %Either, %Either* %res77, i32 0, i32 1
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

define private %Either* @f() {
entry:
  %res78 = alloca %Either
  %0 = getelementptr %Either, %Either* %res78, i32 0, i32 0
  store i1 false, i1* %0
  %1 = alloca i64
  store i64 42, i64* %1
  %2 = getelementptr %Either, %Either* %res78, i32 0, i32 1
  store i64* %1, i64** %2
  %3 = getelementptr %Either, %Either* %res78, i32 0, i32 0
  %4 = load i1, i1* %3
  %5 = getelementptr %Either, %Either* %res78, i32 0, i32 1
  %6 = load i64*, i64** %5
  switch i1 %4, label %case.0.ret [
    i1 false, label %case.0.ret
    i1 true, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u73 = load i64, i64* %6
  %7 = alloca %Either
  %8 = getelementptr %Either, %Either* %7, i32 0, i32 0
  store i1 false, i1* %8
  %9 = alloca i64
  store i64 %_u73, i64* %9
  %10 = getelementptr %Either, %Either* %7, i32 0, i32 1
  store i64* %9, i64** %10
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %11 = alloca i64*
  store i64* %6, i64** %11
  %_u74 = load i64*, i64** %11
  %12 = alloca %Either
  %13 = getelementptr %Either, %Either* %12, i32 0, i32 0
  store i1 true, i1* %13
  %14 = getelementptr %Either, %Either* %12, i32 0, i32 1
  store i64* %_u74, i64** %14
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi %Either* [ %7, %case.0.ret ], [ %12, %case.1.ret ]
  ret %Either* %ret
}

define private %Either* @g(i64* %x) {
entry:
  %ret = alloca %Either
  %0 = getelementptr %Either, %Either* %ret, i32 0, i32 0
  store i1 false, i1* %0
  %1 = getelementptr %Either, %Either* %ret, i32 0, i32 1
  store i64* %x, i64** %1
  ret %Either* %ret
}

define private %Either* @h() {
entry:
  %ret = alloca %Either
  %0 = getelementptr %Either, %Either* %ret, i32 0, i32 0
  store i1 true, i1* %0
  %1 = alloca i64
  store i64 1, i64* %1
  %2 = getelementptr %Either, %Either* %ret, i32 0, i32 1
  store i64* %1, i64** %2
  ret %Either* %ret
}


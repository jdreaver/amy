; Generated from examples/poly-data.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%Either = type { i1, i64* }

define i64 @main() {
entry:
  %res67 = call %Either* @f()
  %0 = getelementptr %Either, %Either* %res67, i32 0, i32 0
  %1 = load i1, i1* %0
  %2 = getelementptr %Either, %Either* %res67, i32 0, i32 1
  %3 = load i64*, i64** %2
  switch i1 %1, label %case.0.ret [
    i1 false, label %case.0.ret
    i1 true, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u60 = load i64, i64* %3
  %4 = alloca i64
  store i64 %_u60, i64* %4
  %5 = load i64, i64* %4
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %_u61 = load i64, i64* %3
  %res68 = call %Either* @h()
  %6 = getelementptr %Either, %Either* %res68, i32 0, i32 0
  %7 = load i1, i1* %6
  %8 = getelementptr %Either, %Either* %res68, i32 0, i32 1
  %9 = load i64*, i64** %8
  switch i1 %7, label %case.0.6 [
    i1 false, label %case.0.6
    i1 true, label %case.1.6
  ]

case.0.6:                                         ; preds = %case.1.ret, %case.1.ret
  %_u56 = load i64, i64* %9
  %10 = alloca i64
  store i64 %_u56, i64* %10
  %11 = load i64, i64* %10
  br label %case.end.6

case.1.6:                                         ; preds = %case.1.ret
  %_u57 = bitcast i64* %9 to %Either*
  %12 = getelementptr %Either, %Either* %_u57, i32 0, i32 0
  %13 = load i1, i1* %12
  %14 = getelementptr %Either, %Either* %_u57, i32 0, i32 1
  %15 = load i64*, i64** %14
  switch i1 %13, label %case.0.13 [
    i1 false, label %case.0.13
    i1 true, label %case.1.13
  ]

case.0.13:                                        ; preds = %case.1.6, %case.1.6
  %_u58 = load i64, i64* %15
  %16 = alloca i64
  store i64 %_u58, i64* %16
  %17 = load i64, i64* %16
  br label %case.end.13

case.1.13:                                        ; preds = %case.1.6
  %_u59 = load i64, i64* %15
  %18 = alloca i64
  store i64 %_u61, i64* %18
  %19 = load i64, i64* %18
  br label %case.end.13

case.end.13:                                      ; preds = %case.1.13, %case.0.13
  %20 = phi i64 [ %17, %case.0.13 ], [ %19, %case.1.13 ]
  br label %case.end.6

case.end.6:                                       ; preds = %case.end.13, %case.0.6
  %21 = phi i64 [ %11, %case.0.6 ], [ %20, %case.end.13 ]
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.end.6, %case.0.ret
  %ret = phi i64 [ %5, %case.0.ret ], [ %21, %case.end.6 ]
  ret i64 %ret
}

define private %Either* @f() {
entry:
  %res69 = alloca %Either
  %0 = getelementptr %Either, %Either* %res69, i32 0, i32 0
  store i1 false, i1* %0
  %1 = alloca i64
  store i64 42, i64* %1
  %2 = getelementptr %Either, %Either* %res69, i32 0, i32 1
  store i64* %1, i64** %2
  %3 = getelementptr %Either, %Either* %res69, i32 0, i32 0
  %4 = load i1, i1* %3
  %5 = getelementptr %Either, %Either* %res69, i32 0, i32 1
  %6 = load i64*, i64** %5
  switch i1 %4, label %case.0.ret [
    i1 false, label %case.0.ret
    i1 true, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u63 = load i64, i64* %6
  %7 = alloca %Either
  %8 = getelementptr %Either, %Either* %7, i32 0, i32 0
  store i1 false, i1* %8
  %9 = alloca i64
  store i64 %_u63, i64* %9
  %10 = getelementptr %Either, %Either* %7, i32 0, i32 1
  store i64* %9, i64** %10
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %11 = alloca i64*
  store i64* %6, i64** %11
  %_u64 = load i64*, i64** %11
  %12 = alloca %Either
  %13 = getelementptr %Either, %Either* %12, i32 0, i32 0
  store i1 true, i1* %13
  %14 = getelementptr %Either, %Either* %12, i32 0, i32 1
  store i64* %_u64, i64** %14
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi %Either* [ %7, %case.0.ret ], [ %12, %case.1.ret ]
  ret %Either* %ret
}

define private %Either* @h() {
entry:
  %res70 = alloca %Either
  %0 = getelementptr %Either, %Either* %res70, i32 0, i32 0
  store i1 true, i1* %0
  %1 = alloca i64
  store i64 1, i64* %1
  %2 = getelementptr %Either, %Either* %res70, i32 0, i32 1
  store i64* %1, i64** %2
  %ret = alloca %Either
  %3 = getelementptr %Either, %Either* %ret, i32 0, i32 0
  store i1 true, i1* %3
  %4 = bitcast %Either* %res70 to i64*
  %5 = getelementptr %Either, %Either* %ret, i32 0, i32 1
  store i64* %4, i64** %5
  ret %Either* %ret
}


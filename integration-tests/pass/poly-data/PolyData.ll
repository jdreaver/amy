; ModuleID = 'amy-module'
source_filename = "<string>"

%Either = type { i1, i64* }

declare i8* @GC_malloc(i64)

define %Either* @h() {
entry:
  %0 = getelementptr %Either, %Either* null, i32 1
  %1 = ptrtoint %Either* %0 to i64
  %2 = call i8* @GC_malloc(i64 %1)
  %res1 = bitcast i8* %2 to %Either*
  %res11 = alloca %Either
  %3 = getelementptr %Either, %Either* %res11, i32 0, i32 0
  store i1 true, i1* %3
  %4 = getelementptr i64, i64* null, i32 1
  %5 = ptrtoint i64* %4 to i64
  %6 = call i8* @GC_malloc(i64 %5)
  %7 = bitcast i8* %6 to i64*
  store i64 1, i64* %7
  %8 = getelementptr %Either, %Either* %res11, i32 0, i32 1
  store i64* %7, i64** %8
  %9 = getelementptr %Either, %Either* null, i32 1
  %10 = ptrtoint %Either* %9 to i64
  %11 = call i8* @GC_malloc(i64 %10)
  %ret = bitcast i8* %11 to %Either*
  %ret2 = alloca %Either
  %12 = getelementptr %Either, %Either* %ret2, i32 0, i32 0
  store i1 true, i1* %12
  %13 = bitcast %Either* %res11 to i64*
  %14 = getelementptr %Either, %Either* %ret2, i32 0, i32 1
  store i64* %13, i64** %14
  ret %Either* %ret2
}

define %Either* @f() {
entry:
  %0 = getelementptr %Either, %Either* null, i32 1
  %1 = ptrtoint %Either* %0 to i64
  %2 = call i8* @GC_malloc(i64 %1)
  %res2 = bitcast i8* %2 to %Either*
  %res21 = alloca %Either
  %3 = getelementptr %Either, %Either* %res21, i32 0, i32 0
  store i1 false, i1* %3
  %4 = getelementptr i64, i64* null, i32 1
  %5 = ptrtoint i64* %4 to i64
  %6 = call i8* @GC_malloc(i64 %5)
  %7 = bitcast i8* %6 to i64*
  store i64 42, i64* %7
  %8 = getelementptr %Either, %Either* %res21, i32 0, i32 1
  store i64* %7, i64** %8
  %9 = getelementptr %Either, %Either* %res21, i32 0, i32 0
  %10 = load i1, i1* %9
  %11 = getelementptr %Either, %Either* %res21, i32 0, i32 1
  %12 = load i64*, i64** %11
  switch i1 %10, label %case.0.ret [
    i1 false, label %case.0.ret
    i1 true, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u2 = load i64, i64* %12
  %13 = getelementptr %Either, %Either* null, i32 1
  %14 = ptrtoint %Either* %13 to i64
  %15 = call i8* @GC_malloc(i64 %14)
  %16 = bitcast i8* %15 to %Either*
  %17 = alloca %Either
  %18 = getelementptr %Either, %Either* %17, i32 0, i32 0
  store i1 false, i1* %18
  %19 = getelementptr i64, i64* null, i32 1
  %20 = ptrtoint i64* %19 to i64
  %21 = call i8* @GC_malloc(i64 %20)
  %22 = bitcast i8* %21 to i64*
  store i64 %_u2, i64* %22
  %23 = getelementptr %Either, %Either* %17, i32 0, i32 1
  store i64* %22, i64** %23
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %24 = alloca i64*
  store i64* %12, i64** %24
  %_u3 = load i64*, i64** %24
  %25 = getelementptr %Either, %Either* null, i32 1
  %26 = ptrtoint %Either* %25 to i64
  %27 = call i8* @GC_malloc(i64 %26)
  %28 = bitcast i8* %27 to %Either*
  %29 = alloca %Either
  %30 = getelementptr %Either, %Either* %29, i32 0, i32 0
  store i1 true, i1* %30
  %31 = getelementptr %Either, %Either* %29, i32 0, i32 1
  store i64* %_u3, i64** %31
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi %Either* [ %17, %case.0.ret ], [ %29, %case.1.ret ]
  ret %Either* %ret
}

define i64 @main() {
entry:
  %res3 = call %Either* @f()
  %0 = getelementptr %Either, %Either* %res3, i32 0, i32 0
  %1 = load i1, i1* %0
  %2 = getelementptr %Either, %Either* %res3, i32 0, i32 1
  %3 = load i64*, i64** %2
  switch i1 %1, label %case.0.ret [
    i1 false, label %case.0.ret
    i1 true, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %_u10 = load i64, i64* %3
  %4 = alloca i64
  store i64 %_u10, i64* %4
  %5 = load i64, i64* %4
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %_u11 = load i64, i64* %3
  %res4 = call %Either* @h()
  %6 = getelementptr %Either, %Either* %res4, i32 0, i32 0
  %7 = load i1, i1* %6
  %8 = getelementptr %Either, %Either* %res4, i32 0, i32 1
  %9 = load i64*, i64** %8
  switch i1 %7, label %case.0.6 [
    i1 false, label %case.0.6
    i1 true, label %case.1.6
  ]

case.0.6:                                         ; preds = %case.1.ret, %case.1.ret
  %_u6 = load i64, i64* %9
  %10 = alloca i64
  store i64 %_u6, i64* %10
  %11 = load i64, i64* %10
  br label %case.end.6

case.1.6:                                         ; preds = %case.1.ret
  %_u7 = bitcast i64* %9 to %Either*
  %12 = getelementptr %Either, %Either* %_u7, i32 0, i32 0
  %13 = load i1, i1* %12
  %14 = getelementptr %Either, %Either* %_u7, i32 0, i32 1
  %15 = load i64*, i64** %14
  switch i1 %13, label %case.0.13 [
    i1 false, label %case.0.13
    i1 true, label %case.1.13
  ]

case.0.13:                                        ; preds = %case.1.6, %case.1.6
  %_u8 = load i64, i64* %15
  %16 = alloca i64
  store i64 %_u8, i64* %16
  %17 = load i64, i64* %16
  br label %case.end.13

case.1.13:                                        ; preds = %case.1.6
  %_u9 = load i64, i64* %15
  %18 = alloca i64
  store i64 %_u11, i64* %18
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

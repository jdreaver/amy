; ModuleID = 'amy-module'
source_filename = "<string>"

%List = type { i1, i64* }

declare i8* @GC_malloc(i64)

define { i64*, i64*, i64* }* @q({ i64*, i64*, i64* }* %r) {
entry:
  %0 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %r, i32 0, i32 0
  %x1 = load i64*, i64** %0
  %1 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %r, i32 0, i32 1
  %y2 = load i64*, i64** %1
  %2 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %r, i32 0, i32 2
  %z3 = load i64*, i64** %2
  %3 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* null, i32 1
  %4 = ptrtoint { i64*, i64*, i64* }* %3 to i64
  %5 = call i8* @GC_malloc(i64 %4)
  %ret = bitcast i8* %5 to { i64*, i64*, i64* }*
  %6 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %ret, i32 0, i32 0
  %7 = bitcast i64** %6 to i64**
  store i64* %x1, i64** %7
  %8 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %ret, i32 0, i32 1
  %9 = bitcast i64** %8 to i64**
  store i64* %y2, i64** %9
  %10 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %ret, i32 0, i32 2
  %11 = bitcast i64** %10 to i64**
  store i64* %z3, i64** %11
  ret { i64*, i64*, i64* }* %ret
}

define %List* @h(i64* %x) {
entry:
  %0 = getelementptr %List, %List* null, i32 1
  %1 = ptrtoint %List* %0 to i64
  %2 = call i8* @GC_malloc(i64 %1)
  %cdr4 = bitcast i8* %2 to %List*
  %cdr41 = alloca %List
  %3 = getelementptr %List, %List* %cdr41, i32 0, i32 0
  store i1 false, i1* %3
  %4 = getelementptr { i64*, %List* }, { i64*, %List* }* null, i32 1
  %5 = ptrtoint { i64*, %List* }* %4 to i64
  %6 = call i8* @GC_malloc(i64 %5)
  %cdr5 = bitcast i8* %6 to { i64*, %List* }*
  %7 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr5, i32 0, i32 0
  %8 = bitcast i64** %7 to i64**
  store i64* %x, i64** %8
  %9 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr5, i32 0, i32 1
  %10 = bitcast %List** %9 to %List**
  store %List* %cdr41, %List** %10
  %11 = getelementptr %List, %List* null, i32 1
  %12 = ptrtoint %List* %11 to i64
  %13 = call i8* @GC_malloc(i64 %12)
  %cdr6 = bitcast i8* %13 to %List*
  %cdr62 = alloca %List
  %14 = getelementptr %List, %List* %cdr62, i32 0, i32 0
  store i1 true, i1* %14
  %15 = bitcast { i64*, %List* }* %cdr5 to i64*
  %16 = getelementptr %List, %List* %cdr62, i32 0, i32 1
  store i64* %15, i64** %16
  %17 = getelementptr { i64*, %List* }, { i64*, %List* }* null, i32 1
  %18 = ptrtoint { i64*, %List* }* %17 to i64
  %19 = call i8* @GC_malloc(i64 %18)
  %res7 = bitcast i8* %19 to { i64*, %List* }*
  %20 = getelementptr { i64*, %List* }, { i64*, %List* }* %res7, i32 0, i32 0
  %21 = bitcast i64** %20 to i64**
  store i64* %x, i64** %21
  %22 = getelementptr { i64*, %List* }, { i64*, %List* }* %res7, i32 0, i32 1
  %23 = bitcast %List** %22 to %List**
  store %List* %cdr62, %List** %23
  %24 = getelementptr %List, %List* null, i32 1
  %25 = ptrtoint %List* %24 to i64
  %26 = call i8* @GC_malloc(i64 %25)
  %ret = bitcast i8* %26 to %List*
  %ret3 = alloca %List
  %27 = getelementptr %List, %List* %ret3, i32 0, i32 0
  store i1 true, i1* %27
  %28 = bitcast { i64*, %List* }* %res7 to i64*
  %29 = getelementptr %List, %List* %ret3, i32 0, i32 1
  store i64* %28, i64** %29
  ret %List* %ret3
}

define { i64*, i64 }* @g(i64* %x) {
entry:
  %0 = getelementptr { i64*, i64 }, { i64*, i64 }* null, i32 1
  %1 = ptrtoint { i64*, i64 }* %0 to i64
  %2 = call i8* @GC_malloc(i64 %1)
  %ret = bitcast i8* %2 to { i64*, i64 }*
  %3 = getelementptr { i64*, i64 }, { i64*, i64 }* %ret, i32 0, i32 0
  %4 = bitcast i64** %3 to i64**
  store i64* %x, i64** %4
  %5 = getelementptr { i64*, i64 }, { i64*, i64 }* %ret, i32 0, i32 1
  store i64 1, i64* %5
  ret { i64*, i64 }* %ret
}

define i64 @addXY({ i64, i64 }* %r) {
entry:
  %0 = getelementptr { i64, i64 }, { i64, i64 }* %r, i32 0, i32 0
  %res8 = load i64, i64* %0
  %1 = getelementptr { i64, i64 }, { i64, i64 }* %r, i32 0, i32 1
  %res9 = load i64, i64* %1
  %ret = add i64 %res8, %res9
  ret i64 %ret
}

define i64 @main() {
entry:
  %0 = getelementptr { i64, i64 }, { i64, i64 }* null, i32 1
  %1 = ptrtoint { i64, i64 }* %0 to i64
  %2 = call i8* @GC_malloc(i64 %1)
  %res10 = bitcast i8* %2 to { i64, i64 }*
  %3 = getelementptr { i64, i64 }, { i64, i64 }* %res10, i32 0, i32 0
  store i64 1, i64* %3
  %4 = getelementptr { i64, i64 }, { i64, i64 }* %res10, i32 0, i32 1
  store i64 2, i64* %4
  %ret = call i64 @addXY({ i64, i64 }* %res10)
  ret i64 %ret
}

define { i64, i1 }* @a() {
entry:
  switch i1 true, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %0 = getelementptr { i64, i1 }, { i64, i1 }* null, i32 1
  %1 = ptrtoint { i64, i1 }* %0 to i64
  %2 = call i8* @GC_malloc(i64 %1)
  %3 = bitcast i8* %2 to { i64, i1 }*
  %4 = getelementptr { i64, i1 }, { i64, i1 }* %3, i32 0, i32 0
  store i64 1, i64* %4
  %5 = getelementptr { i64, i1 }, { i64, i1 }* %3, i32 0, i32 1
  %6 = bitcast i1* %5 to i1*
  store i1 true, i1* %6
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %7 = getelementptr { i64, i1 }, { i64, i1 }* null, i32 1
  %8 = ptrtoint { i64, i1 }* %7 to i64
  %9 = call i8* @GC_malloc(i64 %8)
  %10 = bitcast i8* %9 to { i64, i1 }*
  %11 = getelementptr { i64, i1 }, { i64, i1 }* %10, i32 0, i32 0
  store i64 2, i64* %11
  %12 = getelementptr { i64, i1 }, { i64, i1 }* %10, i32 0, i32 1
  %13 = bitcast i1* %12 to i1*
  store i1 false, i1* %13
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi { i64, i1 }* [ %3, %case.0.ret ], [ %10, %case.1.ret ]
  ret { i64, i1 }* %ret
}

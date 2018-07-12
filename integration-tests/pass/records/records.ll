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
  %3 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 3))
  %ret = bitcast i8* %3 to { i64*, i64*, i64* }*
  %4 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %ret, i32 0, i32 0
  store i64* %x1, i64** %4
  %5 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %ret, i32 0, i32 1
  store i64* %y2, i64** %5
  %6 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %ret, i32 0, i32 2
  store i64* %z3, i64** %6
  ret { i64*, i64*, i64* }* %ret
}

define %List* @h(i64* %x) {
entry:
  %0 = call i8* @GC_malloc(i64 ptrtoint (%List* getelementptr (%List, %List* null, i32 1) to i64))
  %cdr4 = bitcast i8* %0 to %List*
  %cdr41 = alloca %List
  %1 = getelementptr %List, %List* %cdr41, i32 0, i32 0
  store i1 false, i1* %1
  %2 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %cdr5 = bitcast i8* %2 to { i64*, %List* }*
  %3 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr5, i32 0, i32 0
  store i64* %x, i64** %3
  %4 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr5, i32 0, i32 1
  store %List* %cdr41, %List** %4
  %5 = call i8* @GC_malloc(i64 ptrtoint (%List* getelementptr (%List, %List* null, i32 1) to i64))
  %cdr6 = bitcast i8* %5 to %List*
  %cdr62 = alloca %List
  %6 = getelementptr %List, %List* %cdr62, i32 0, i32 0
  store i1 true, i1* %6
  %7 = bitcast { i64*, %List* }* %cdr5 to i64*
  %8 = getelementptr %List, %List* %cdr62, i32 0, i32 1
  store i64* %7, i64** %8
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %res7 = bitcast i8* %9 to { i64*, %List* }*
  %10 = getelementptr { i64*, %List* }, { i64*, %List* }* %res7, i32 0, i32 0
  store i64* %x, i64** %10
  %11 = getelementptr { i64*, %List* }, { i64*, %List* }* %res7, i32 0, i32 1
  store %List* %cdr62, %List** %11
  %12 = call i8* @GC_malloc(i64 ptrtoint (%List* getelementptr (%List, %List* null, i32 1) to i64))
  %ret = bitcast i8* %12 to %List*
  %ret3 = alloca %List
  %13 = getelementptr %List, %List* %ret3, i32 0, i32 0
  store i1 true, i1* %13
  %14 = bitcast { i64*, %List* }* %res7 to i64*
  %15 = getelementptr %List, %List* %ret3, i32 0, i32 1
  store i64* %14, i64** %15
  ret %List* %ret3
}

define { i64*, i64 }* @g(i64* %x) {
entry:
  %0 = call i8* @GC_malloc(i64 ptrtoint ({ i64*, i64 }* getelementptr ({ i64*, i64 }, { i64*, i64 }* null, i32 1) to i64))
  %ret = bitcast i8* %0 to { i64*, i64 }*
  %1 = getelementptr { i64*, i64 }, { i64*, i64 }* %ret, i32 0, i32 0
  store i64* %x, i64** %1
  %2 = getelementptr { i64*, i64 }, { i64*, i64 }* %ret, i32 0, i32 1
  store i64 1, i64* %2
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
  %0 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i64 2))
  %res10 = bitcast i8* %0 to { i64, i64 }*
  %1 = getelementptr { i64, i64 }, { i64, i64 }* %res10, i32 0, i32 0
  store i64 1, i64* %1
  %2 = getelementptr { i64, i64 }, { i64, i64 }* %res10, i32 0, i32 1
  store i64 2, i64* %2
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
  %0 = call i8* @GC_malloc(i64 ptrtoint ({ i64, i1 }* getelementptr ({ i64, i1 }, { i64, i1 }* null, i32 1) to i64))
  %1 = bitcast i8* %0 to { i64, i1 }*
  %2 = getelementptr { i64, i1 }, { i64, i1 }* %1, i32 0, i32 0
  store i64 1, i64* %2
  %3 = getelementptr { i64, i1 }, { i64, i1 }* %1, i32 0, i32 1
  store i1 true, i1* %3
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %4 = call i8* @GC_malloc(i64 ptrtoint ({ i64, i1 }* getelementptr ({ i64, i1 }, { i64, i1 }* null, i32 1) to i64))
  %5 = bitcast i8* %4 to { i64, i1 }*
  %6 = getelementptr { i64, i1 }, { i64, i1 }* %5, i32 0, i32 0
  store i64 2, i64* %6
  %7 = getelementptr { i64, i1 }, { i64, i1 }* %5, i32 0, i32 1
  store i1 false, i1* %7
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi { i64, i1 }* [ %1, %case.0.ret ], [ %5, %case.1.ret ]
  ret { i64, i1 }* %ret
}

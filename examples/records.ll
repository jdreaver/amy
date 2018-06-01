; Generated from examples/records.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

%List = type { i1, i64* }

define i64 @main() {
entry:
  %res1 = alloca { i64, i64 }
  %0 = getelementptr { i64, i64 }, { i64, i64 }* %res1, i32 0, i32 0
  store i64 1, i64* %0
  %1 = getelementptr { i64, i64 }, { i64, i64 }* %res1, i32 0, i32 1
  store i64 2, i64* %1
  %ret = call i64 @addXY({ i64, i64 }* %res1)
  ret i64 %ret
}

define private i64 @addXY({ i64, i64 }* %r) {
entry:
  %0 = getelementptr { i64, i64 }, { i64, i64 }* %r, i32 0, i32 0
  %res2 = load i64, i64* %0
  %1 = getelementptr { i64, i64 }, { i64, i64 }* %r, i32 0, i32 1
  %res3 = load i64, i64* %1
  %ret = add i64 %res2, %res3
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
  %cdr4 = alloca %List
  %0 = getelementptr %List, %List* %cdr4, i32 0, i32 0
  store i1 false, i1* %0
  %cdr5 = alloca { i64*, %List* }
  %1 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr5, i32 0, i32 0
  store i64* %x, i64** %1
  %2 = getelementptr { i64*, %List* }, { i64*, %List* }* %cdr5, i32 0, i32 1
  store %List* %cdr4, %List** %2
  %cdr6 = alloca %List
  %3 = getelementptr %List, %List* %cdr6, i32 0, i32 0
  store i1 true, i1* %3
  %4 = bitcast { i64*, %List* }* %cdr5 to i64*
  %5 = getelementptr %List, %List* %cdr6, i32 0, i32 1
  store i64* %4, i64** %5
  %res7 = alloca { i64*, %List* }
  %6 = getelementptr { i64*, %List* }, { i64*, %List* }* %res7, i32 0, i32 0
  store i64* %x, i64** %6
  %7 = getelementptr { i64*, %List* }, { i64*, %List* }* %res7, i32 0, i32 1
  store %List* %cdr6, %List** %7
  %ret = alloca %List
  %8 = getelementptr %List, %List* %ret, i32 0, i32 0
  store i1 true, i1* %8
  %9 = bitcast { i64*, %List* }* %res7 to i64*
  %10 = getelementptr %List, %List* %ret, i32 0, i32 1
  store i64* %9, i64** %10
  ret %List* %ret
}

define private { i64, i1 }* @a() {
entry:
  switch i1 true, label %case.0.ret [
    i1 true, label %case.0.ret
    i1 false, label %case.1.ret
  ]

case.0.ret:                                       ; preds = %entry, %entry
  %0 = alloca { i64, i1 }
  %1 = getelementptr { i64, i1 }, { i64, i1 }* %0, i32 0, i32 0
  store i64 1, i64* %1
  %2 = getelementptr { i64, i1 }, { i64, i1 }* %0, i32 0, i32 1
  store i1 true, i1* %2
  br label %case.end.ret

case.1.ret:                                       ; preds = %entry
  %3 = alloca { i64, i1 }
  %4 = getelementptr { i64, i1 }, { i64, i1 }* %3, i32 0, i32 0
  store i64 2, i64* %4
  %5 = getelementptr { i64, i1 }, { i64, i1 }* %3, i32 0, i32 1
  store i1 false, i1* %5
  br label %case.end.ret

case.end.ret:                                     ; preds = %case.1.ret, %case.0.ret
  %ret = phi { i64, i1 }* [ %0, %case.0.ret ], [ %3, %case.1.ret ]
  ret { i64, i1 }* %ret
}

define private { i64*, i64*, i64* }* @q({ i64*, i64*, i64* }* %r) {
entry:
  %0 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %r, i32 0, i32 0
  %x8 = load i64*, i64** %0
  %1 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %r, i32 0, i32 1
  %y9 = load i64*, i64** %1
  %2 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %r, i32 0, i32 2
  %z10 = load i64*, i64** %2
  %ret = alloca { i64*, i64*, i64* }
  %3 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %ret, i32 0, i32 0
  store i64* %x8, i64** %3
  %4 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %ret, i32 0, i32 1
  store i64* %y9, i64** %4
  %5 = getelementptr { i64*, i64*, i64* }, { i64*, i64*, i64* }* %ret, i32 0, i32 2
  store i64* %z10, i64** %5
  ret { i64*, i64*, i64* }* %ret
}


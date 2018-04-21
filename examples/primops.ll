; Generated from examples/primops.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = call i64 @f(i64 2)
  %1 = call i64 @f(i64 %0)
  ret i64 %1
}

define i64 @f(i64 %x) {
entry:
  %0 = add i64 %x, -1
  %1 = sub i64 3, %0
  %2 = icmp slt i64 5, %1
  %test.4 = icmp eq i1 true, %2
  br i1 %test.4, label %if.then.4, label %if.else.4

if.then.4:                                        ; preds = %entry
  br label %if.end.4

if.else.4:                                        ; preds = %entry
  br label %if.end.4

if.end.4:                                         ; preds = %if.else.4, %if.then.4
  %end.4 = phi i64 [ 100, %if.then.4 ], [ 200, %if.else.4 ]
  ret i64 %end.4
}


; Generated from examples/primops.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i32 @main() {
entry:
  %0 = call i32 @f(i32 2)
  %1 = call i32 @f(i32 %0)
  ret i32 %1
}

define i32 @f(i32 %x) {
entry:
  %0 = add i32 %x, -1
  %1 = sub i32 3, %0
  %2 = icmp slt i32 5, %1
  %test.4 = icmp eq i1 true, %2
  br i1 %test.4, label %if.then.4, label %if.else.4

if.then.4:                                        ; preds = %entry
  br label %if.end.4

if.else.4:                                        ; preds = %entry
  br label %if.end.4

if.end.4:                                         ; preds = %if.else.4, %if.then.4
  %end.4 = phi i32 [ 100, %if.then.4 ], [ 200, %if.else.4 ]
  ret i32 %end.4
}


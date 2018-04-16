; Generated from examples/primops.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i32 @main() {
entry:
  %0 = call i32 @f(i32 2)
  ret i32 %0
}

define i32 @f(i32 %x) {
entry:
  %0 = add i32 %x, 5
  %1 = sub i32 3, %0
  %2 = icmp slt i32 5, %1
  %test.3 = icmp eq i1 true, %2
  br i1 %test.3, label %if.then.3, label %if.else.3

if.then.3:                                        ; preds = %entry
  br label %if.end.3

if.else.3:                                        ; preds = %entry
  br label %if.end.3

if.end.3:                                         ; preds = %if.else.3, %if.then.3
  %end.3 = phi i32 [ 100, %if.then.3 ], [ 200, %if.else.3 ]
  ret i32 %end.3
}


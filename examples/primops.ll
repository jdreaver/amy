; Generated from examples/primops.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i32 @main() {
entry:
  br i1 true, label %if.then.1, label %if.else.1

if.then.1:                                        ; preds = %entry
  br label %if.end.1

if.else.1:                                        ; preds = %entry
  br label %if.end.1

if.end.1:                                         ; preds = %if.else.1, %if.then.1
  %0 = phi i32 [ 100, %if.then.1 ], [ 200, %if.else.1 ]
  ret i32 %0
}


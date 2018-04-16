; Generated from examples/primops.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i32 @main() {
entry:
  br i1 true, label %if.then.2, label %if.else.2

if.then.2:                                        ; preds = %entry
  br label %if.end.2

if.else.2:                                        ; preds = %entry
  br label %if.end.2

if.end.2:                                         ; preds = %if.else.2, %if.then.2
  %end.2 = phi i32 [ 100, %if.then.2 ], [ 200, %if.else.2 ]
  ret i32 %end.2
}


; Generated from examples/let.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

declare i32 @abs(i32)

define i32 @main() {
entry:
  br i1 true, label %if.then.0, label %if.else.0

if.then.0:                                        ; preds = %entry
  %0 = call i32 @f(i32 100)
  %1 = call i32 @abs(i32 %0)
  br label %if.end.0

if.else.0:                                        ; preds = %entry
  %2 = call i32 @f(i32 200)
  %3 = call i32 @abs(i32 %2)
  br label %if.end.0

if.end.0:                                         ; preds = %if.else.0, %if.then.0
  %end.0 = phi i32 [ %1, %if.then.0 ], [ %3, %if.else.0 ]
  ret i32 %end.0
}

define i32 @f(i32 %x) {
entry:
  br i1 true, label %if.then.0, label %if.else.0

if.then.0:                                        ; preds = %entry
  %0 = call i32 @abs(i32 %x)
  br label %if.end.0

if.else.0:                                        ; preds = %entry
  br label %if.end.0

if.end.0:                                         ; preds = %if.else.0, %if.then.0
  %end.0 = phi i32 [ %0, %if.then.0 ], [ 300, %if.else.0 ]
  ret i32 %end.0
}


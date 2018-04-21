; Generated from examples/fib.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i32 @main() {
entry:
  %0 = call i32 @fib(i32 10)
  ret i32 %0
}

define i32 @fib(i32 %x) {
entry:
  %0 = icmp eq i32 %x, 0
  %test.1 = icmp eq i1 true, %0
  br i1 %test.1, label %if.then.1, label %if.else.1

if.then.1:                                        ; preds = %entry
  br label %if.end.1

if.else.1:                                        ; preds = %entry
  %1 = icmp eq i32 %x, 1
  %test.3 = icmp eq i1 true, %1
  br i1 %test.3, label %if.then.3, label %if.else.3

if.then.3:                                        ; preds = %if.else.1
  br label %if.end.3

if.else.3:                                        ; preds = %if.else.1
  %2 = sub i32 %x, 1
  %3 = call i32 @fib(i32 %2)
  %4 = sub i32 %x, 2
  %5 = call i32 @fib(i32 %4)
  %6 = add i32 %3, %5
  br label %if.end.3

if.end.3:                                         ; preds = %if.else.3, %if.then.3
  %end.3 = phi i32 [ 1, %if.then.3 ], [ %6, %if.else.3 ]
  br label %if.end.1

if.end.1:                                         ; preds = %if.end.3, %if.then.1
  %end.1 = phi i32 [ 0, %if.then.1 ], [ %end.3, %if.end.3 ]
  ret i32 %end.1
}


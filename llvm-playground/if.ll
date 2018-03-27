define i32 @main() {
entry:
  %0 = icmp eq i1 0, 1
  br i1 %0, label %if.then.0, label %if.else.0

if.then.0:
  br label %if.end.0

if.else.0:
  br label %if.end.0

if.end.0:
  %1 = phi i32 [2, %if.then.0], [3, %if.else.0]
  ret i32 %1
}

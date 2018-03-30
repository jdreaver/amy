; I manually modified func_pointer.ll and func_pointer.c to simplify the
; clang-optimized code. It is much clearer because there aren't arbitrary
; bitcasts everywhere.

@.str = private unnamed_addr constant [14 x i8] c"value is: %d\0A\00"

define private i32 @add(i32, i32) {
  %3 = add nsw i32 %1, %0
  ret i32 %3
}

define private i32 @sub(i32, i32) {
  %3 = sub nsw i32 %0, %1
  ret i32 %3
}

define private void @print(i32, i32, i32 (i32, i32)* nocapture) {
  %4 = call i32 (i32, i32) %2(i32 %0, i32 %1) #3
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str, i64 0, i64 0), i32 %4)
  ret void
}

declare i32 @printf(i8* nocapture readonly, ...)

define i32 @main() local_unnamed_addr #1 {
  call void @print(i32 100, i32 200, i32 (i32, i32)* @add)
  call void @print(i32 100, i32 200, i32 (i32, i32)* @sub)
  ret i32 0
}

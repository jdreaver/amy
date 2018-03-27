define double @foo(double %a, double %b) {
entry:
  %multmp = fmul double %a, %a
  %multmp1 = fmul double 2.000000e+00, %a
  %multmp2 = fmul double %multmp1, %b
  %addtmp = fadd double %multmp, %multmp2
  %multmp3 = fmul double %b, %b
  %addtmp4 = fadd double %addtmp, %multmp3
  ret double %addtmp4
}

define double @bar(double %a) {
entry:
  %calltmp = call double @foo(double %a, double 4.000000e+00)
  ; %calltmp1 = call double @bar(double 3.133700e+04)
  %addtmp = fadd double %calltmp, 1.112
  ret double %addtmp
}

@formatString = private constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %res = call double @bar(double 1.234000e+00)
  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @formatString , i32 0, i32 0), double %res)
  ret i32 0
}


; define i32 @main() {
; entry:
;   %d = shl i32 2, 3
;   %pt = getelementptr [2 x i8], [2 x i8]* @formatString, i32 0, i32 0
;   call i32 (i8*, ...) @printf(i8* %pt, i32 %d)
;   ret i32 1
; }
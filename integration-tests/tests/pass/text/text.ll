; ModuleID = 'amy-module'
source_filename = "<string>"

@"$str.1" = private global [7 x i8] c"Hello!\00"

define i64 @main() {
entry:
  %0 = alloca [7 x i8]*
  store [7 x i8]* @"$str.1", [7 x i8]** %0
  %x = load [7 x i8]*, [7 x i8]** %0
  %1 = alloca i64
  store i64 0, i64* %1
  %ret = load i64, i64* %1
  ret i64 %ret
}


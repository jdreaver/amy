; ModuleID = 'amy-module'
source_filename = "<string>"

@"$str.2" = private global [7 x i8] c"Hello!\00"

define i64 @main() {
entry:
  %x1 = call i8* @hello()
  %0 = alloca i8*
  store i8* %x1, i8** %0
  %x = load i8*, i8** %0
  %1 = alloca i64
  store i64 0, i64* %1
  %ret = load i64, i64* %1
  ret i64 %ret
}

define private i8* @hello() {
entry:
  %0 = alloca i8*
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @"$str.2", i32 0, i32 0), i8** %0
  %ret = load i8*, i8** %0
  ret i8* %ret
}


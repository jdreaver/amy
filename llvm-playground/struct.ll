%struct.MyStruct = type { i32, i32 }

define i32 @main() {
entry:
  ; Allocate struct
  %0 = alloca %struct.MyStruct
  %1 = getelementptr %struct.MyStruct, %struct.MyStruct* %0, i32 0, i32 0
  store i32 15, i32* %1
  %2 = getelementptr %struct.MyStruct, %struct.MyStruct* %0, i32 0, i32 1
  store i32 75, i32* %2

  ; Get struct fields back and add them
  %3 = getelementptr %struct.MyStruct, %struct.MyStruct* %0, i32 0, i32 0
  %4 = load i32, i32* %3
  %5 = getelementptr %struct.MyStruct, %struct.MyStruct* %0, i32 0, i32 1
  %6 = load i32, i32* %5

  ; Compute result and return
  %7 = add nsw i32 %4, %6
  ret i32 %7
}

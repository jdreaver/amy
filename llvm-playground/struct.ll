%struct.MyStruct = type { i32, i32 }

define i32 @main() {
entry:
  ; Allocate struct
  %0 = call %struct.MyStruct* @makeStruct()

  ; Get struct fields back and add them
  %1 = getelementptr %struct.MyStruct, %struct.MyStruct* %0, i32 0, i32 0
  %2 = load i32, i32* %1
  %3 = getelementptr %struct.MyStruct, %struct.MyStruct* %0, i32 0, i32 1
  %4 = load i32, i32* %3

  ; Compute result and return
  %5 = add nsw i32 %2, %4
  ret i32 %5
}

define %struct.MyStruct* @makeStruct() {
entry:
  ; Allocate struct
  %0 = alloca %struct.MyStruct
  %1 = getelementptr %struct.MyStruct, %struct.MyStruct* %0, i32 0, i32 0
  store i32 15, i32* %1
  %2 = getelementptr %struct.MyStruct, %struct.MyStruct* %0, i32 0, i32 1
  store i32 75, i32* %2

  ret %struct.MyStruct* %0
}
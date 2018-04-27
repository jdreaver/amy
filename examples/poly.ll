; Generated from examples/poly.amy

; ModuleID = 'amy-module'
source_filename = "<string>"

define i64 @main() {
entry:
  %0 = call i64* @id(i64* inttoptr (i64 3 to i64*))
  %1 = ptrtoint i64* %0 to i64
  %2 = inttoptr i64 %1 to i64*
  %3 = call i64* @const(i64* %2, i64* inttoptr (i64 5 to i64*))
  %4 = ptrtoint i64* %3 to i64
  ret i64 %4
}

define i64* @id(i64* %x) {
entry:
  ret i64* %x
}

define i64* @const(i64* %x, i64* %y) {
entry:
  ret i64* %x
}


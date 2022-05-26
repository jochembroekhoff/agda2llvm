;; libc

declare void @printf(i8*, ...)

;; string constants

@str.unexpected_primitive = private constant [28 x i8] c"AGDA: unexpected primitive\0A\00"

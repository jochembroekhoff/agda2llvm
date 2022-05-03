declare fastcc ptr @agda.alloc.value(i64 %sz) alwaysinline

define i64 @main() {
    %1 = call fastcc ptr @agda.alloc.value(i64 123)
    ret i64 0
}

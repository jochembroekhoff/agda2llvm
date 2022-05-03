; Boehm GC
declare void @GC_init()
declare ptr @GC_malloc(i64)

; libc
declare ptr @malloc(i64)
declare void @free(ptr)

define fastcc ptr @agda.alloc.value(i64 %sz) alwaysinline {
    %ptr = call ptr @GC_malloc(i64 %sz)
    ret ptr %ptr
}

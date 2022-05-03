;; Agda basic structures

; struct eval { struct value * (*value_ptr)(void *); void *record; }
%agda.struct.eval = type { %agda.struct.value* (i8*)*, i8* }
; struct value { enum {value_fn,value_data} type; union {struct eval fn; void *value;} }
%agda.struct.value = type { i64, [2 x i64] }
%agda.struct.value.fn = type { i64, %agda.struct.eval } ; tag=0
%agda.struct.value.value = type { i64, %agda.data.base* } ; tag=1
; struct thunk { bool evaluated; union { struct eval eval; struct value *value; } }
%agda.struct.thunk = type { i64, [16 x i8] }
%agda.struct.thunk.eval = type { i64, %agda.struct.eval } ; evaluated=false
%agda.struct.thunk.value = type { i64, %agda.struct.value* } ; evaluated=true
; struct data_base { size_t ID; size_t CASE; }
%agda.data.base = type { i64, i64 }

;; Agda allocators

declare
%agda.struct.value*
@agda.alloc.value()

declare
%agda.struct.thunk*
@agda.alloc.thunk()

declare
%agda.data.base*
@agda.alloc.data(i64 %sz)

;; Agda evaluation

declare
%agda.struct.value*
@agda.eval.appl.0(%agda.struct.thunk* %appl)

declare
%agda.struct.value*
@agda.eval.main(%agda.struct.thunk*(i8*)* %main_fn)

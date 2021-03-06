;; Agda basic structures

; struct eval { struct value * (*value_ptr)(void *); void *record; }
%agda.struct.eval = type { %agda.struct.value* (%agda.struct.frame*, %agda.struct.thunk*)*, %agda.struct.frame* }
; struct value { enum {value_fn,value_data} type; union {struct eval fn; void *value;} }
%agda.struct.value = type { i64, [2 x i64] }
%agda.struct.value.fn = type { i64, %agda.struct.eval } ; tag=0
%agda.struct.value.data = type { i64, %agda.data.base* } ; tag=1
%agda.struct.value.lit_nat = type { i64, i64 } ; tag=2
%agda.struct.value.lit_w64 = type { i64, i64 } ; tag=3
%agda.struct.value.lit_f64 = type { i64, double } ; tag=4
%agda.struct.value.lit_str = type { i64, i8* } ; tag=5
%agda.struct.value.lit_chr = type { i64, i8 } ; tag=6
; struct thunk { bool evaluated; union { struct eval eval; struct value *value; } }
%agda.struct.thunk = type { i64, [16 x i8] }
%agda.struct.thunk.eval = type { i64, %agda.struct.eval } ; evaluated=false
%agda.struct.thunk.value = type { i64, %agda.struct.value* } ; evaluated=true
; struct frame { struct thunk *elem; struct frame *prev; }
%agda.struct.frame = type { %agda.struct.thunk*, %agda.struct.frame* }
; struct data_base { size_t IDX; size_t CASE; struct frame *content; }
%agda.data.base = type { i64, i64, %agda.struct.frame* }

;; Agda allocators

declare
%agda.struct.value*
@agda.alloc.value()

declare
%agda.struct.thunk*
@agda.alloc.thunk()

declare
%agda.data.base*
@agda.alloc.data(i64)

;; Agda evaluation

declare
%agda.struct.value*
@agda.eval.force(%agda.struct.thunk*)

declare
%agda.struct.value*
@agda.eval.appl.n(%agda.struct.thunk*, ...)

declare
i64
@agda.eval.case.data(%agda.struct.thunk*)

declare
i64
@agda.eval.case.lit_nat(%agda.struct.thunk*)

declare
%agda.struct.value*
@agda.eval.main(%agda.struct.thunk*(%agda.struct.frame*)*)

;; Agda stack

declare
void
@agda.record.push_replace(%agda.struct.frame**, %agda.struct.thunk*)

declare
%agda.struct.thunk*
@agda.record.get(%agda.struct.frame*, i64)

declare
%agda.struct.frame*
@agda.record.extract(%agda.struct.frame*, i64, %agda.struct.thunk*)

;; Miscenallaneous

declare
void
@exit(i64)

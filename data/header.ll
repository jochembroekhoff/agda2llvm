;;;; BEGIN AGDA HEADER ;;;;

;; Agda basic structures

; struct eval { struct value * (*value_ptr)(void *); void *record; }
%agda.struct.eval = type { %agda.struct.value* (%agda.struct.frame*, %agda.struct.thunk*)*, %agda.struct.frame* }
; struct value { enum {value_fn,value_data} type; union {struct eval fn; void *value;} }
%agda.struct.value = type { i64, [2 x i64] }
%agda.struct.value.fn = type { i64, %agda.struct.eval } ; tag=0
%agda.struct.value.value = type { i64, %agda.data.base* } ; tag=1
; struct thunk { bool evaluated; union { struct eval eval; struct value *value; } }
%agda.struct.thunk = type { i64, [16 x i8] }
%agda.struct.thunk.eval = type { i64, %agda.struct.eval } ; evaluated=false
%agda.struct.thunk.value = type { i64, %agda.struct.value* } ; evaluated=true
; struct frame { struct thunk *elem; struct frame *prev; }
%agda.struct.frame = type { %agda.struct.thunk*, %agda.struct.frame* }
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
@agda.alloc.data(i64)

;; Agda evaluation

declare
%agda.struct.value*
@agda.eval.appl.0(%agda.struct.thunk*)

declare
%agda.struct.value*
@agda.eval.appl.1(%agda.struct.thunk*, %agda.struct.thunk*)

declare
%agda.struct.value*
@agda.eval.appl.n(%agda.struct.thunk*, ...)

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

;;;; END AGDA HEADER ;;;;

;;;; START AGDA OUTPUT ;;;;


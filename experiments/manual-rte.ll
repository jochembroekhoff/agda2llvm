;; Agda table instantiation
; TODO:
@agda.table.ctor_arity = external unnamed_addr constant i64*
@agda.table.ctor_name = external unnamed_addr constant i8**



;;;; BEGIN AGDA HEADER ;;;;

;; Agda basic structures

; struct eval { struct value * (*value_ptr)(void *); void *record; }
%agda.struct.eval = type { %agda.struct.value* (i8*)*, i8* }
; struct value { enum {value_fn,value_data} type; union {struct eval fn; void *value;} }
%agda.struct.value = type { i64, [2 x i64] }
%agda.struct.value.fn = type { i64, %agda.struct.eval } ; tag=0
%agda.struct.value.data = type { i64, %agda.data.base* } ; tag=1
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
@agda.alloc.data(i64)

;; Agda evaluation

declare
%agda.struct.value*
@agda.eval.appl.0(%agda.struct.thunk*)

declare
%agda.struct.value*
@agda.eval.main(%agda.struct.thunk*(i8*)*)

;;;; END AGDA HEADER ;;;;

;; Data structures

%agda.data.Dummy = type { %agda.data.base }

;; Data constructors

define
%agda.struct.value*
@agda.ctor.Dummy.dummy2(i8* %record)
{
    ; initialize data base
    %data_base = call %agda.data.base* @agda.alloc.data(i64 16)
    ; configure data base
    %data_base_id = getelementptr %agda.data.base, %agda.data.base* %data_base, i32 0, i32 0
    store i64 0, i64* %data_base_id
    %data_base_case = getelementptr %agda.data.base, %agda.data.base* %data_base, i32 0, i32 1
    store i64 1, i64* %data_base_case

    ; configure other properties
    %data = bitcast %agda.data.base* %data_base to %agda.data.Dummy*
    ; (nothing to populate)

    ; initialize value holder
    %v = call %agda.struct.value* @agda.alloc.value()
    ; configure value holder
    %v_tag = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    store i64 1, i64* %v_tag ; tag=1 is value
    %v_data = bitcast %agda.struct.value* %v to %agda.struct.value.data*
    %v_data_data = getelementptr %agda.struct.value.data, %agda.struct.value.data* %v_data, i32 0, i32 1
    store %agda.data.base* %data_base, %agda.data.base** %v_data_data

    ; return the produced value
    ret %agda.struct.value* %v
}

;; Normal functions

define
%agda.struct.value*
@agda.fn.dummy2.body(i8* %record)
{
    ; #dummy2

    ; initialize function value holder
    %v = call %agda.struct.value* @agda.alloc.value()
    ; configure value holder
    %v_tag = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    store i64 0, i64* %v_tag ; tag=0 is function
    %v_eval = bitcast %agda.struct.value* %v to %agda.struct.value.fn*
    %v_eval_ptr = getelementptr %agda.struct.value.fn, %agda.struct.value.fn* %v_eval, i32 0, i32 1, i32 0
    store %agda.struct.value*(i8*)* @agda.ctor.Dummy.dummy2, %agda.struct.value*(i8*)** %v_eval_ptr
    %v_eval_record = getelementptr  %agda.struct.value.fn, %agda.struct.value.fn* %v_eval, i32 0, i32 1, i32 1
    store i8* null, i8** %v_eval_record

    ; %appl = call %agda.struct.thunk* @agda.fn.dummy2(i8* null)
    ; %v = call %agda.struct.value* @agda.eval.appl.0(%agda.struct.thunk* %appl)

    ret %agda.struct.value* %v
}

define
%agda.struct.thunk*
@agda.fn.dummy2(i8* %record)
{
    ; dummy2 = \-> #dummy2

    ; construct thunk holder
    %thunk_raw = call %agda.struct.thunk* @agda.alloc.thunk()
    ; configure non-evaluated setting
    %thunk_eval = bitcast %agda.struct.thunk* %thunk_raw to %agda.struct.thunk.eval*
    ; (store eval flag = false)
    %thunk_eval_flag = getelementptr %agda.struct.thunk.eval, %agda.struct.thunk.eval* %thunk_eval, i32 0, i32 0 
    %false = zext i1 false to i64
    store i64 %false, i64* %thunk_eval_flag
    ; (store function pointer)
    %thunk_eval_ptr = getelementptr %agda.struct.thunk.eval, %agda.struct.thunk.eval* %thunk_eval, i32 0, i32 1, i32 0
    store %agda.struct.value*(i8*)* @agda.fn.dummy2.body, %agda.struct.value* (i8*)** %thunk_eval_ptr
    ; (store function record)
    %thunk_eval_record = getelementptr %agda.struct.thunk.eval, %agda.struct.thunk.eval* %thunk_eval, i32 0, i32 1, i32 1
    store i8* null, i8** %thunk_eval_record

    ret %agda.struct.thunk* %thunk_raw
}

;; Main (temporary)

define
i64
@main()
{
    %v = call %agda.struct.value* @agda.eval.main(%agda.struct.thunk*(i8*)* @agda.fn.dummy2)
    ret i64 0
}

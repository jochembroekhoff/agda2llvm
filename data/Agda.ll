;; Boehm GC

declare void @GC_init()
declare i8* @GC_malloc(i64)

;; libc

declare i8* @malloc(i64)
declare void @free(i8*)
declare void @printf(i8*, ...)

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

;; Agda table reference

@agda.table.ctor_arity = external unnamed_addr constant i64*
@agda.table.ctor_name = external unnamed_addr constant i8**

;; Agda allocators

define
%agda.struct.value*
@agda.alloc.value()
{
    %ptr = call i8* @GC_malloc(i64 24)
    %ptr_ = bitcast i8* %ptr to %agda.struct.value*
    ret %agda.struct.value* %ptr_
}

define
%agda.struct.thunk*
@agda.alloc.thunk()
{
    %ptr = call i8* @GC_malloc(i64 24)
    %ptr_ = bitcast i8* %ptr to %agda.struct.thunk*
    ret %agda.struct.thunk* %ptr_
}

define
%agda.data.base*
@agda.alloc.data(i64 %sz)
{
    %ptr = call i8* @GC_malloc(i64 %sz)
    %ptr_ = bitcast i8* %ptr to %agda.data.base*
    ret %agda.data.base* %ptr_
}

;; Agda evaluation

define internal
fastcc
%agda.struct.value*
@agda.eval.eval(%agda.struct.eval* %eval)
{
    ; retrieve function pointer
    %fn_ptr_ptr = getelementptr %agda.struct.eval, %agda.struct.eval* %eval, i32 0, i32 0
    %fn_ptr = load %agda.struct.value* (i8*)*, %agda.struct.value* (i8*)** %fn_ptr_ptr
    ; retrieve record pointer
    %record_ptr_ptr = getelementptr %agda.struct.eval, %agda.struct.eval* %eval, i32 0, i32 1
    %record_ptr = load i8*, i8** %record_ptr_ptr
    ; call function
    %res = call %agda.struct.value* %fn_ptr (i8* %record_ptr)
    ; immediately return the value
    ret %agda.struct.value* %res
}

define internal
fastcc
%agda.struct.value*
@agda.eval.force(%agda.struct.thunk* %thunk)
{
    ; lookup the evaluation state
    %evaluated_ptr = getelementptr %agda.struct.thunk, %agda.struct.thunk* %thunk, i32 0, i32 0
    %evaluated = load i64, i64* %evaluated_ptr
    %evaluated_trunc = trunc i64 %evaluated to i1

    ; pre-load value pointer-pointer
    %thunk_v = bitcast %agda.struct.thunk* %thunk to %agda.struct.thunk.value*
    %value_ptr_ptr = getelementptr %agda.struct.thunk.value, %agda.struct.thunk.value* %thunk_v, i32 0, i32 1

    ; continue to evaluation if not evaluated yet
    br i1 %evaluated_trunc, label %AlreadyEvaluated, label %NotEvaluated

NotEvaluated:
    ; set evaluation flag to true
    store i64 1, i64* %evaluated_ptr
    ; access evaluation data struct
    %thunk_e = bitcast %agda.struct.thunk* %thunk to %agda.struct.thunk.eval*
    %eval_ptr = getelementptr %agda.struct.thunk.eval, %agda.struct.thunk.eval* %thunk_e, i32 0, i32 1
    ; call the evaluation
    %new_value = call fastcc %agda.struct.value* @agda.eval.eval(%agda.struct.eval* %eval_ptr)
    ; store the new value
    store %agda.struct.value* %new_value, %agda.struct.value** %value_ptr_ptr
    ; continue as if the value as evaluated
    br label %AlreadyEvaluated

AlreadyEvaluated:
    ; return the value pointer
    %final_value = load %agda.struct.value*, %agda.struct.value** %value_ptr_ptr
    ret %agda.struct.value* %final_value
}

@.str.TypeIncorrect = private constant [16 x i8] c"TypeIncorrect!\0A\00"

define
%agda.struct.value*
@agda.eval.appl.0(%agda.struct.thunk* %appl)
{
    ; evaluate the value to apply 0 arguments to
    %v = call fastcc %agda.struct.value* @agda.eval.force(%agda.struct.thunk* %appl)

    ; value must be a function, otherwise can't apply. check that now
    %v_tag_ptr = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    %v_tag = load i64, i64* %v_tag_ptr
    %v_tag_correct = icmp eq i64 %v_tag, 0 ; tag=0 is function
    br i1 %v_tag_correct, label %TypeCorrect, label %TypeIncorrect

TypeCorrect:
    ; retrieve the function evaluation info
    %v_fn = bitcast %agda.struct.value* %v to %agda.struct.value.fn*
    %eval_info = getelementptr %agda.struct.value.fn, %agda.struct.value.fn* %v_fn, i32 0, i32 1
    ; evaluate the function
    %eval_res = call fastcc %agda.struct.value* @agda.eval.eval(%agda.struct.eval* %eval_info)
    ; return the evaluation result
    ret %agda.struct.value* %eval_res

TypeIncorrect:
    call void (i8*, ...) @printf(i8* getelementptr ([16 x i8], [16 x i8]* @.str.TypeIncorrect, i32 0, i32 0))
    ret %agda.struct.value* null
}

; "Hello %p (TAG: %zu, ID: %zu, CASE: %zu)\n"
@fmt = private constant [42 x i8] c"Hello: %p (TAG: %zu, ID: %zu, CASE: %zu)\0A\00"

define
%agda.struct.value*
@agda.eval.main(%agda.struct.thunk*(i8*)* %main_fn)
{
    ; ensure that the garbage collector is initialized
    call void @GC_init()
    ; call the main function to construct the root thunk
    %main_thunk = call %agda.struct.thunk* %main_fn(i8* null)
    ; evaluate the result of the main function
    %v = call %agda.struct.value* @agda.eval.appl.0(%agda.struct.thunk* %main_thunk)
    ; print the value pointer
    %v_value = bitcast %agda.struct.value* %v to %agda.struct.value.value*
    %v_tag_ptr = getelementptr %agda.struct.value.value, %agda.struct.value.value* %v_value, i32 0, i32 0
    %v_tag = load i64, i64* %v_tag_ptr
    %v_base_ptr_ptr = getelementptr %agda.struct.value.value, %agda.struct.value.value* %v_value, i32 0, i32 1
    %v_base_ptr = load %agda.data.base*, %agda.data.base** %v_base_ptr_ptr
    %v_id_ptr = getelementptr %agda.data.base, %agda.data.base* %v_base_ptr, i32 0, i32 0
    %v_id = load i64, i64* %v_id_ptr
    %v_case_ptr = getelementptr %agda.data.base, %agda.data.base* %v_base_ptr, i32 0, i32 1
    %v_case = load i64, i64* %v_case_ptr
    call void(i8*, ...) @printf(i8* getelementptr ([42 x i8], [42 x i8]* @fmt, i32 0, i32 0)
        , %agda.struct.value* %v
        , i64 %v_tag
        , i64 %v_id
        , i64 %v_case
        )
    ret %agda.struct.value* %v
}

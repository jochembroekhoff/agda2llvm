;; Boehm GC

declare void @GC_init()
declare i8* @GC_malloc(i64)

;; libc

declare i8* @malloc(i64)
declare void @free(i8*)
declare void @printf(i8*, ...)

;; LLVM

%struct.va_list = type { i32, i32, i8*, i8* }
declare void @llvm.va_start(i8*)
declare void @llvm.va_end(i8*)

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
; struct data_base { size_t IDX; size_t CASE; struct frame *content; }
%agda.data.base = type { i64, i64, %agda.struct.frame* }

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
%agda.struct.value*
@agda.eval.eval(%agda.struct.eval* %eval, %agda.struct.thunk* %arg)
{
    ; retrieve function pointer
    %fn_ptr_ptr = getelementptr %agda.struct.eval, %agda.struct.eval* %eval, i32 0, i32 0
    %fn_ptr = load %agda.struct.value* (%agda.struct.frame*, %agda.struct.thunk*)*, %agda.struct.value* (%agda.struct.frame*, %agda.struct.thunk*)** %fn_ptr_ptr
    ; retrieve record pointer
    %record_ptr_ptr = getelementptr %agda.struct.eval, %agda.struct.eval* %eval, i32 0, i32 1
    %record_ptr = load %agda.struct.frame*, %agda.struct.frame** %record_ptr_ptr
    ; call function
    %res = call %agda.struct.value* %fn_ptr (%agda.struct.frame* %record_ptr, %agda.struct.thunk* %arg)
    ; immediately return the value
    ret %agda.struct.value* %res
}

define internal
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
    %new_value = call %agda.struct.value* @agda.eval.eval(%agda.struct.eval* %eval_ptr, %agda.struct.thunk* null)
    ; store the new value
    store %agda.struct.value* %new_value, %agda.struct.value** %value_ptr_ptr
    ; continue as if the value as evaluated
    br label %AlreadyEvaluated

AlreadyEvaluated:
    ; return the value pointer
    %final_value = load %agda.struct.value*, %agda.struct.value** %value_ptr_ptr
    ret %agda.struct.value* %final_value
}

define
%agda.struct.value*
@agda.eval.appl.0(%agda.struct.thunk* %appl)
{
    ; evaluate the value, this is basically just an alias to force (which is private)
    %v = call %agda.struct.value* @agda.eval.force(%agda.struct.thunk* %appl)
    ret %agda.struct.value* %v
}

@.str.TypeIncorrect = private constant [35 x i8] c"AGDA: cannot apply to non-fn data\0A\00"

define
internal
%agda.struct.value*
@agda.eval.appl.do_checked(%agda.struct.value* %v, %agda.struct.thunk* %arg)
{
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
    %eval_res = call %agda.struct.value* @agda.eval.eval(%agda.struct.eval* %eval_info, %agda.struct.thunk* %arg)
    ; return the evaluation result
    ret %agda.struct.value* %eval_res

TypeIncorrect:
    call void (i8*, ...) @printf(i8* getelementptr ([35 x i8], [35 x i8]* @.str.TypeIncorrect, i32 0, i32 0))
    ret %agda.struct.value* null
}

@str.appln_loop = private constant [19 x i8] c"AGDA: appl.n iter\0A\00"
@str.appln_end = private constant [18 x i8] c"AGDA: appl.n end\0A\00"

define
%agda.struct.value*
@agda.eval.appl.n(%agda.struct.thunk* %appl, ...)
{
begin:
    ; init result value holder
    %v = alloca %agda.struct.value*

    ; init varargs
    %ap = alloca %struct.va_list
    %ap_raw = bitcast %struct.va_list* %ap to i8*
    call void @llvm.va_start(i8* %ap_raw)

    ; force the subject to get the initial value to apply to
    %v_initial = call %agda.struct.value* @agda.eval.force(%agda.struct.thunk* %appl)
    store %agda.struct.value* %v_initial, %agda.struct.value** %v

    br label %loopBegin

loopBegin:
    ; load the next arg from the varargs
    %arg = va_arg %struct.va_list* %ap, %agda.struct.thunk*
    ; check if the end is reached, i.e. when arg==NULL
    %arg_null = icmp eq %agda.struct.thunk* %arg, null
    br i1 %arg_null, label %end, label %loopBody

loopBody:
    ; debug print
    call void(i8*, ...) @printf(i8* getelementptr ([19 x i8], [19 x i8]* @str.appln_loop, i32 0, i32 0))
    ; apply the current arg to the current function
    %v_ptr0 = load %agda.struct.value*, %agda.struct.value** %v
    %v_next = call %agda.struct.value* @agda.eval.appl.do_checked(%agda.struct.value* %v_ptr0, %agda.struct.thunk* %arg)
    ; if the returned value is NULL, some error occurred in do_checked
    %v_next_null = icmp eq %agda.struct.value* %v_next, null
    br i1 %v_next_null, label %error, label %loopPrepNext

loopPrepNext:
    ; persist v_next for the next iteration
    store %agda.struct.value* %v_next, %agda.struct.value** %v
    br label %loopBegin

end:
    ; debug print
    call void(i8*, ...) @printf(i8* getelementptr ([18 x i8], [18 x i8]* @str.appln_end, i32 0, i32 0))
    ; finalize the varargs and return the last value
    call void @llvm.va_end(i8* %ap_raw)
    %v_ptr1 = load %agda.struct.value*, %agda.struct.value** %v
    ret %agda.struct.value* %v_ptr1

error:
    ; finalize the varargs and return NULL
    call void @llvm.va_end(i8* %ap_raw)
    ret %agda.struct.value* null
}

define
i64
@agda.eval.case.data(%agda.struct.thunk* %subj_v_thunk)
{
    ; force the thunk to obtain the value
    %v = call %agda.struct.value* @agda.eval.force(%agda.struct.thunk* %subj_v_thunk)

    ; value must be data, otherwise can't extract identification. check that now
    %v_tag_ptr = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    %v_tag = load i64, i64* %v_tag_ptr
    %v_tag_correct = icmp eq i64 %v_tag, 1 ; tag=1 is data
    br i1 %v_tag_correct, label %TypeCorrect, label %TypeIncorrect

TypeCorrect:
    ; retrieve the data holder
    %v_data = bitcast %agda.struct.value* %v to %agda.struct.value.value*
    %data_base_ptr = getelementptr %agda.struct.value.value, %agda.struct.value.value* %v_data, i32 0, i32 1
    %data_base = load %agda.data.base*, %agda.data.base** %data_base_ptr
    ; get IDX and CASE fields from the data base struct
    %data_idx_ptr = getelementptr %agda.data.base, %agda.data.base* %data_base, i32 0, i32 0
    %data_case_ptr = getelementptr %agda.data.base, %agda.data.base* %data_base, i32 0, i32 1
    %data_idx = load i64, i64* %data_idx_ptr
    %data_case = load i64, i64* %data_case_ptr
    ; compute and return case_id
    %case_id = add i64 %data_idx, %data_case
    ret i64 %case_id

TypeIncorrect:
    call void (i8*, ...) @printf(i8* getelementptr ([35 x i8], [35 x i8]* @.str.TypeIncorrect, i32 0, i32 0))
    ret i64 0
}

@fmt = private constant [62 x i8] c"AGDA: result: %p (TAG: %zu, ID: %zu, CASE: %zu, content: %p)\0A\00"
@str.force_is_null = private constant [19 x i8] c"AGDA: result null\0A\00"

define
%agda.struct.value*
@agda.eval.main(%agda.struct.thunk*(%agda.struct.frame*)* %main_fn)
{
    ; ensure that the garbage collector is initialized
    call void @GC_init()
    ; call the main definition to construct the root thunk
    %main_thunk = call %agda.struct.thunk* %main_fn(%agda.struct.frame* null)
    ; evaluate the result of the main definition
    %v = call %agda.struct.value* @agda.eval.force(%agda.struct.thunk* %main_thunk)
    ; make sure value is not null before attempting to print its contents
    %v_null = icmp eq %agda.struct.value* null, %v
    br i1 %v_null, label %v_is_null, label %v_not_null

v_is_null:
    call void(i8*, ...) @printf(i8* getelementptr ([19 x i8], [19 x i8]* @str.force_is_null, i32 0, i32 0))
    br label %end

v_not_null:
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
    %v_content_ptr = getelementptr %agda.data.base, %agda.data.base* %v_base_ptr, i32 0, i32 2
    %v_content = load %agda.struct.frame*, %agda.struct.frame** %v_content_ptr
    call void(i8*, ...) @printf(i8* getelementptr ([62 x i8], [62 x i8]* @fmt, i32 0, i32 0)
        , %agda.struct.value* %v
        , i64 %v_tag
        , i64 %v_id
        , i64 %v_case
        , %agda.struct.frame* %v_content
        )
    br label %end

end:
    ret %agda.struct.value* %v
}

;; Agda stack

@str.record_push_replace = private constant [51 x i8] c"AGDA: record.push_replace: curr=%p elem=%p new=%p\0A\00"

define
void
@agda.record.push_replace(%agda.struct.frame** %curr, %agda.struct.thunk* %elem)
{
    %alloc = call i8* @GC_malloc(i64 16)
    %frame = bitcast i8* %alloc to %agda.struct.frame*

    ; set frame->elem
    %frame_elem_ptr = getelementptr %agda.struct.frame, %agda.struct.frame* %frame, i32 0, i32 0
    store %agda.struct.thunk* %elem, %agda.struct.thunk** %frame_elem_ptr

    ; set frame->prev
    %frame_prev_ptr = getelementptr %agda.struct.frame, %agda.struct.frame* %frame, i32 0, i32 1
    %curr_deref = load %agda.struct.frame*, %agda.struct.frame** %curr
    store %agda.struct.frame* %curr_deref, %agda.struct.frame** %frame_prev_ptr

    ; debug print
    call void(i8*, ...) @printf(i8* getelementptr ([51 x i8], [51 x i8]* @str.record_push_replace, i32 0, i32 0)
        , %agda.struct.frame* %curr_deref
        , %agda.struct.thunk* %elem
        , %agda.struct.frame* %frame
        )

    ; replace callee's pointer
    store %agda.struct.frame* %frame, %agda.struct.frame** %curr

    ret void
}

@str.record_get_dbg = private constant [27 x i8] c"AGDA: record.get: %p[%zu]\0A\00"

define
%agda.struct.thunk*
@agda.record.get(%agda.struct.frame* %curr, i64 %idx)
{
    ; debug print
    call void(i8*, ...) @printf(i8* getelementptr ([27 x i8], [27 x i8]* @str.record_get_dbg, i32 0, i32 0)
        , %agda.struct.frame* %curr
        , i64 %idx
        )
    ; make sure %curr isn't null
    %curr_null = icmp eq %agda.struct.frame* null, %curr
    br i1 %curr_null, label %null, label %ok

null:
    ret %agda.struct.thunk* null

ok:
    ; check if %idx is already zero
    %idx_zero = icmp eq i64 0, %idx
    br i1 %idx_zero, label %fin, label %next

next:
    %idx_less = sub i64 %idx, 1
    %prev_ptr = getelementptr %agda.struct.frame, %agda.struct.frame* %curr, i32 0, i32 1
    %prev = load %agda.struct.frame*, %agda.struct.frame** %prev_ptr
    %res = call %agda.struct.thunk* @agda.record.get(%agda.struct.frame* %prev, i64 %idx_less)
    ret %agda.struct.thunk* %res

fin:
    %elem_ptr = getelementptr %agda.struct.frame, %agda.struct.frame* %curr, i32 0, i32 0
    %elem = load %agda.struct.thunk*, %agda.struct.thunk** %elem_ptr
    ret %agda.struct.thunk* %elem
}

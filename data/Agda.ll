;; Boehm GC

declare void @GC_init()
declare i8* @GC_malloc(i64)

;; libc

declare i8* @malloc(i64)
declare void @free(i8*)
declare void @printf(i8*, ...)
declare void @putchar(i64)

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

;; Agda table reference

@agda.table.ctor_arity = external unnamed_addr constant i64*
@agda.table.ctor_name = external unnamed_addr constant i8**

;; Agda allocators

@agda_alloc_managed = internal alias i8*(i64), i8*(i64)* @GC_malloc

define
%agda.struct.value*
@agda.alloc.value()
{
    %ptr = call i8* @agda_alloc_managed(i64 24)
    %ptr_ = bitcast i8* %ptr to %agda.struct.value*
    ret %agda.struct.value* %ptr_
}

define
%agda.struct.thunk*
@agda.alloc.thunk()
{
    %ptr = call i8* @agda_alloc_managed(i64 24)
    %ptr_ = bitcast i8* %ptr to %agda.struct.thunk*
    ret %agda.struct.thunk* %ptr_
}

define
%agda.data.base*
@agda.alloc.data(i64 %sz)
{
    %ptr = call i8* @agda_alloc_managed(i64 %sz)
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
    %v_data = bitcast %agda.struct.value* %v to %agda.struct.value.data*
    %data_base_ptr = getelementptr %agda.struct.value.data, %agda.struct.value.data* %v_data, i32 0, i32 1
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

define
i64
@agda.eval.case.lit_nat(%agda.struct.thunk* %subj_v_thunk)
{
    ; force the thunk to obtain the value
    %v = call %agda.struct.value* @agda.eval.force(%agda.struct.thunk* %subj_v_thunk)

    ; value must be data, otherwise can't extract identification. check that now
    %v_tag_ptr = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    %v_tag = load i64, i64* %v_tag_ptr
    %v_tag_correct = icmp eq i64 %v_tag, 2 ; tag=2 is lit_nat
    br i1 %v_tag_correct, label %TypeCorrect, label %TypeIncorrect

TypeCorrect:
    ; retrieve the underlying value
    %v_lit_nat = bitcast %agda.struct.value* %v to %agda.struct.value.lit_nat*
    %v_content_nat_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_lit_nat, i32 0, i32 1
    %v_content_nat = load i64, i64* %v_content_nat_ptr
    ret i64 %v_content_nat

TypeIncorrect:
    call void (i8*, ...) @printf(i8* getelementptr ([35 x i8], [35 x i8]* @.str.TypeIncorrect, i32 0, i32 0))
    ret i64 0
}

@str.force_is_null = private constant [19 x i8] c"AGDA: result null\0A\00"
@str.main_res_pfx = private constant [15 x i8] c"AGDA: result: \00"

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
    ; debug-print the value contents
    call void(i8*, ...) @printf(i8* getelementptr ([15 x i8], [15 x i8]* @str.main_res_pfx, i32 0, i32 0))
    call void @agda.debug.print.value(%agda.struct.value* %v)
    call void @putchar(i64 10) ; '\n'
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
    ; dereference current pointer and create a new frame with it
    %curr_deref = load %agda.struct.frame*, %agda.struct.frame** %curr
    %frame = call %agda.struct.frame* @agda.record.internal.alloc(%agda.struct.thunk* %elem, %agda.struct.frame* %curr_deref)

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
@str.record_get_dbg_fin = private constant [26 x i8] c"AGDA: record.get: res=%p\0A\00"

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
    ; debug print
    call void(i8*, ...) @printf(i8* getelementptr ([26 x i8], [26 x i8]* @str.record_get_dbg_fin, i32 0, i32 0)
        , %agda.struct.thunk* %elem
        )
    ret %agda.struct.thunk* %elem
}

define
internal
%agda.struct.frame*
@agda.record.internal.alloc(%agda.struct.thunk* %elem, %agda.struct.frame* %prev)
{
    %alloc = call i8* @agda_alloc_managed(i64 16)
    %frame = bitcast i8* %alloc to %agda.struct.frame*

    ; set frame->elem
    %frame_elem_ptr = getelementptr %agda.struct.frame, %agda.struct.frame* %frame, i32 0, i32 0
    store %agda.struct.thunk* %elem, %agda.struct.thunk** %frame_elem_ptr

    ; set frame->prev
    %frame_prev_ptr = getelementptr %agda.struct.frame, %agda.struct.frame* %frame, i32 0, i32 1
    store %agda.struct.frame* %prev, %agda.struct.frame** %frame_prev_ptr

    ; return
    ret %agda.struct.frame* %frame
}

define
internal
%agda.struct.frame*
@agda.record.extract.copy_frame(%agda.struct.frame* %curr, i64 %arity, %agda.struct.frame* %src)
{
    %at_basecase = icmp eq i64 %arity, 0
    br i1 %at_basecase, label %basecase, label %recurse

basecase:
    ret %agda.struct.frame* %curr

recurse:
    ; get current element and prev pointer
    %src_elem_ptr = getelementptr %agda.struct.frame, %agda.struct.frame* %src, i32 0, i32 0
    %src_prev_ptr = getelementptr %agda.struct.frame, %agda.struct.frame* %src, i32 0, i32 1
    %src_elem = load %agda.struct.thunk*, %agda.struct.thunk** %src_elem_ptr
    %src_prev = load %agda.struct.frame*, %agda.struct.frame** %src_prev_ptr

    ; recurse to get the _actual_ prev pointer
    %arity_min1 = sub i64 %arity, 1
    %prev = call %agda.struct.frame* @agda.record.extract.copy_frame(%agda.struct.frame* %curr, i64 %arity_min1, %agda.struct.frame* %src_prev)

    ; allocate and return new frame
    %new = call %agda.struct.frame* @agda.record.internal.alloc(%agda.struct.thunk* %src_elem, %agda.struct.frame* %prev)
    ret %agda.struct.frame* %new
}

define
%agda.struct.frame*
@agda.record.extract(%agda.struct.frame* %curr, i64 %arity, %agda.struct.thunk* %subj)
{
    ; force the thunk to obtain the value
    %v = call %agda.struct.value* @agda.eval.force(%agda.struct.thunk* %subj)

    ; value must be data, otherwise can't extract anything. check that now
    %v_tag_ptr = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    %v_tag = load i64, i64* %v_tag_ptr
    %v_tag_correct = icmp eq i64 %v_tag, 1 ; tag=1 is data
    br i1 %v_tag_correct, label %TypeCorrect, label %TypeIncorrect

TypeCorrect:
    ; retrieve the data holder
    %v_data = bitcast %agda.struct.value* %v to %agda.struct.value.data*
    %data_base_ptr = getelementptr %agda.struct.value.data, %agda.struct.value.data* %v_data, i32 0, i32 1
    %data_base = load %agda.data.base*, %agda.data.base** %data_base_ptr
    ; get DATA field from the data base struct
    %data_record_ptr = getelementptr %agda.data.base, %agda.data.base* %data_base, i32 0, i32 2
    %data_record = load %agda.struct.frame*, %agda.struct.frame** %data_record_ptr
    ; push data on top of the current frame
    %new_frame = call %agda.struct.frame* @agda.record.extract.copy_frame(%agda.struct.frame* %curr, i64 %arity, %agda.struct.frame* %data_record)
    ret %agda.struct.frame* %new_frame

TypeIncorrect:
    call void (i8*, ...) @printf(i8* getelementptr ([35 x i8], [35 x i8]* @.str.TypeIncorrect, i32 0, i32 0))
    ret %agda.struct.frame* null
}

;; Debugging

@str.fn = private constant [3 x i8] c"fn\00"
@str.fmt.zd = private constant [4 x i8] c"%zd\00"
@str.fmt.zu = private constant [4 x i8] c"%zu\00"
@str.fmt.data = private constant [29 x i8] c"(id=%zu case=%zu content=%p)\00"

define
void
@agda.debug.print.value(%agda.struct.value* %v)
{
    ; load and switch on tag value
    %v_tag_ptr = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    %v_tag = load i64, i64* %v_tag_ptr

    switch i64 %v_tag, label %case_unk
    [
        i64 0, label %case_fn
        i64 1, label %case_data
        i64 2, label %case_lit_nat
        i64 3, label %case_lit_w64
        ; TODO-cases:
        ;i64 4, label %case_lit_f64
        ;i64 5, label %case_lit_str
        ;i64 6, label %case_lit_chr
    ]

case_unk:
    call void @putchar(i64 63) ; '?'
    ret void

case_fn:
    call void (i8*, ...) @printf(i8* getelementptr ([3 x i8], [3 x i8]* @str.fn, i32 0, i32 0))
    ret void

case_data:
    %v_data = bitcast %agda.struct.value* %v to %agda.struct.value.data*
    %v_content_data_ptr = getelementptr %agda.struct.value.data, %agda.struct.value.data* %v_data, i32 0, i32 1
    %v_content_data = load %agda.data.base*, %agda.data.base** %v_content_data_ptr
    call void @agda.debug.print.data(%agda.data.base* %v_content_data)
    ret void

case_lit_nat:
    %v_lit_nat = bitcast %agda.struct.value* %v to %agda.struct.value.lit_nat*
    %v_content_nat_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_lit_nat, i32 0, i32 1
    %v_content_nat = load i64, i64* %v_content_nat_ptr
    call void (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @str.fmt.zd, i32 0, i32 0)
    , i64 %v_content_nat
    )
    ret void

case_lit_w64:
    %v_lit_w64 = bitcast %agda.struct.value* %v to %agda.struct.value.lit_w64*
    %v_content_w64_ptr = getelementptr %agda.struct.value.lit_w64, %agda.struct.value.lit_w64* %v_lit_w64, i32 0, i32 1
    %v_content_w64 = load i64, i64* %v_content_w64_ptr
    call void (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @str.fmt.zu, i32 0, i32 0)
    , i64 %v_content_w64
    )
    ret void
}

define
void
@agda.debug.print.data(%agda.data.base* %data)
{
    %id_ptr = getelementptr %agda.data.base, %agda.data.base* %data, i32 0, i32 0
    %id = load i64, i64* %id_ptr
    %case_ptr = getelementptr %agda.data.base, %agda.data.base* %data, i32 0, i32 1
    %case = load i64, i64* %case_ptr
    %content_ptr = getelementptr %agda.data.base, %agda.data.base* %data, i32 0, i32 2
    %content = load %agda.struct.frame*, %agda.struct.frame** %content_ptr
    call void(i8*, ...) @printf(i8* getelementptr ([29 x i8], [29 x i8]* @str.fmt.data, i32 0, i32 0)
        , i64 %id
        , i64 %case
        , %agda.struct.frame* %content
        )
    ret void
}

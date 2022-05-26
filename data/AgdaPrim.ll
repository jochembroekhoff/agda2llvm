;;;; BEGIN AGDA HEADER ;;;;

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
@agda.eval.appl.0(%agda.struct.thunk*)

declare
%agda.struct.value*
@agda.eval.appl.n(%agda.struct.thunk*, ...)

declare
i64
@agda.eval.case.data(%agda.struct.thunk*)

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

;;;; END AGDA HEADER ;;;;

;;;; START AGDA OUTPUT ;;;;

declare
%agda.struct.value*
@agda.builtin_refs.make_true(%agda.struct.frame*, %agda.struct.thunk*)
declare
%agda.struct.value*
@agda.builtin_refs.make_false(%agda.struct.frame*, %agda.struct.thunk*)

define internal
%agda.struct.value*
@agdaBool(i1 %bool)
{
    br i1 %bool, label %true, label %false
true:
    %v_true = call %agda.struct.value* @agda.builtin_refs.make_true(%agda.struct.frame* null, %agda.struct.thunk* null)
    ret %agda.struct.value* %v_true
false:
    %v_false = call %agda.struct.value* @agda.builtin_refs.make_false(%agda.struct.frame* null, %agda.struct.thunk* null)
    ret %agda.struct.value* %v_false
}

define
%agda.struct.value*
@agda.prim.impl.add(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
{
    ; get left value
    %v_l_lit_nat = bitcast %agda.struct.value* %v_l_raw to %agda.struct.value.lit_nat*
    %v_l_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_l_lit_nat, i32 0, i32 1
    %v_l = load i64, i64* %v_l_ptr

    ; get right value
    %v_r_lit_nat = bitcast %agda.struct.value* %v_r_raw to %agda.struct.value.lit_nat*
    %v_r_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_r_lit_nat, i32 0, i32 1
    %v_r = load i64, i64* %v_r_ptr

    ; compute result
    %v_result = add i64 %v_l, %v_r

    ; box the result and return it
    %v = call %agda.struct.value*() @agda.alloc.value()
    %v_tag = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    store i64 2, i64* %v_tag
    %v_lit = bitcast %agda.struct.value* %v to %agda.struct.value.lit_nat*
    %lit = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_lit, i32 0, i32 1
    store i64 %v_result, i64* %lit
    ret %agda.struct.value* %v
}

define
%agda.struct.value*
@agda.prim.impl.sub(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
{
    ; get left value
    %v_l_lit_nat = bitcast %agda.struct.value* %v_l_raw to %agda.struct.value.lit_nat*
    %v_l_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_l_lit_nat, i32 0, i32 1
    %v_l = load i64, i64* %v_l_ptr

    ; get right value
    %v_r_lit_nat = bitcast %agda.struct.value* %v_r_raw to %agda.struct.value.lit_nat*
    %v_r_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_r_lit_nat, i32 0, i32 1
    %v_r = load i64, i64* %v_r_ptr

    ; compute result
    %v_result = sub i64 %v_l, %v_r

    ; box the result and return it
    %v = call %agda.struct.value*() @agda.alloc.value()
    %v_tag = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    store i64 2, i64* %v_tag
    %v_lit = bitcast %agda.struct.value* %v to %agda.struct.value.lit_nat*
    %lit = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_lit, i32 0, i32 1
    store i64 %v_result, i64* %lit
    ret %agda.struct.value* %v
}

define
%agda.struct.value*
@agda.prim.impl.mul(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
{
    ; get left value
    %v_l_lit_nat = bitcast %agda.struct.value* %v_l_raw to %agda.struct.value.lit_nat*
    %v_l_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_l_lit_nat, i32 0, i32 1
    %v_l = load i64, i64* %v_l_ptr

    ; get right value
    %v_r_lit_nat = bitcast %agda.struct.value* %v_r_raw to %agda.struct.value.lit_nat*
    %v_r_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_r_lit_nat, i32 0, i32 1
    %v_r = load i64, i64* %v_r_ptr

    ; compute result
    %v_result = mul i64 %v_l, %v_r

    ; box the result and return it
    %v = call %agda.struct.value*() @agda.alloc.value()
    %v_tag = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
    store i64 2, i64* %v_tag
    %v_lit = bitcast %agda.struct.value* %v to %agda.struct.value.lit_nat*
    %lit = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_lit, i32 0, i32 1
    store i64 %v_result, i64* %lit
    ret %agda.struct.value* %v
}

define
%agda.struct.value*
@agda.prim.impl.eqi(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
{
    ; get left value
    %v_l_lit_nat = bitcast %agda.struct.value* %v_l_raw to %agda.struct.value.lit_nat*
    %v_l_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_l_lit_nat, i32 0, i32 1
    %v_l = load i64, i64* %v_l_ptr

    ; get right value
    %v_r_lit_nat = bitcast %agda.struct.value* %v_r_raw to %agda.struct.value.lit_nat*
    %v_r_ptr = getelementptr %agda.struct.value.lit_nat, %agda.struct.value.lit_nat* %v_r_lit_nat, i32 0, i32 1
    %v_r = load i64, i64* %v_r_ptr

    ; compute result
    %v_result = icmp eq i64 %v_l, %v_r

    ; box and return the resulting boolean
    %v = call %agda.struct.value* @agdaBool(i1 %v_result)
    ret %agda.struct.value* %v
}

; primNatPlus =alias= add
define
%agda.struct.value*
@agda.prim.impl.primNatPlus(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
{
    %res = call %agda.struct.value* @agda.prim.impl.add(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
    ret %agda.struct.value* %res
}

; primNatPlus =alias= sub
define
%agda.struct.value*
@agda.prim.impl.primNatMinus(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
{
    ; TODO: replace with actual 'monus' behavior
    %res = call %agda.struct.value* @agda.prim.impl.sub(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
    ret %agda.struct.value* %res
}

; primNatTimes =alias= mul
define
%agda.struct.value*
@agda.prim.impl.primNatTimes(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
{
    %res = call %agda.struct.value* @agda.prim.impl.mul(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
    ret %agda.struct.value* %res
}

; primNatEquality =alias= eqi
define
%agda.struct.value*
@agda.prim.impl.primNatEquality(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
{
    %res = call %agda.struct.value* @agda.prim.impl.eqi(%agda.struct.value* %v_l_raw, %agda.struct.value* %v_r_raw)
    ret %agda.struct.value* %res
}

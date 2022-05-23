"""
Script to generate Agda primitive wrappers, in LLVM IR format.
Output is to be linked with the other parts of the Agda runtime.
"""

PRIMITIVES = [
    ("add", 2),
    ("sub", 2),
    ("mul", 2),
    # ("quot", 2),
    # ("rem", 2),
    # ("geq", 2),
    # ("lt", 2),
    # ("lt64", 2),
    ("eqi", 2),
    # ("eqf", 2),
    # ("eqs", 2),
    # ("eqc", 2),
    # ("ito64", 2),
    # ("64toi", 2),
]


def gen_prim(name: str, arity: int) -> str:
    assert arity == 2, "currently only == /2 primitives can be generated"
    res = f"""\
        define
        %agda.struct.thunk*
        @agda.prim.{name}(%agda.struct.frame* %record)
        {{
            %thunk_raw = call %agda.struct.thunk* @agda.alloc.thunk()
            %thunk_flag = getelementptr %agda.struct.thunk, %agda.struct.thunk* %thunk_raw, i32 0, i32 0
            %thunk_flag_bool = zext i1 false to i64
            store i64 %thunk_flag_bool, i64* %thunk_flag
            %thunk_eval = bitcast %agda.struct.thunk* %thunk_raw to %agda.struct.thunk.eval*
            %thunk_eval_ptr = getelementptr %agda.struct.thunk.eval, %agda.struct.thunk.eval* %thunk_eval, i32 0, i32 1, i32 0
            store %agda.struct.value*(%agda.struct.frame*, %agda.struct.thunk*)* @agda.prim.{name}--body, %agda.struct.value*(%agda.struct.frame*, %agda.struct.thunk*)** %thunk_eval_ptr
            %thunk_eval_record = getelementptr %agda.struct.thunk.eval, %agda.struct.thunk.eval* %thunk_eval, i32 0, i32 1, i32 1
            ; NOTE: current record is explicitly not passed, because we are sure that the primitive impl won't reference anything in parent scopes
            store %agda.struct.frame* null, %agda.struct.frame** %thunk_eval_record
            ret %agda.struct.thunk* %thunk_raw
        }}
        define
        %agda.struct.value*
        @agda.prim.{name}--body(%agda.struct.frame* %record, %agda.struct.thunk* %arg)
        {{
            %v = call %agda.struct.value*() @agda.alloc.value()
            %v_tag = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
            store i64 0, i64* %v_tag
            %v_fn = bitcast %agda.struct.value* %v to %agda.struct.value.fn*
            %v_fn_ptr = getelementptr %agda.struct.value.fn, %agda.struct.value.fn* %v_fn, i32 0, i32 1, i32 0
            store %agda.struct.value*(%agda.struct.frame*, %agda.struct.thunk*)* @agda.prim.{name}--lam-0, %agda.struct.value*(%agda.struct.frame*, %agda.struct.thunk*)** %v_fn_ptr
            %v_fn_record = getelementptr %agda.struct.value.fn, %agda.struct.value.fn* %v_fn, i32 0, i32 1, i32 1
            store %agda.struct.frame* %record, %agda.struct.frame** %v_fn_record
            ret %agda.struct.value* %v
        }}
        """

    for i in range(arity):
        if i == arity-1:
            res += f"""\
                declare
                %agda.struct.value*
                @agda.prim.impl.{name}(%agda.struct.value*, %agda.struct.value*)

                define
                %agda.struct.value*
                @agda.prim.{name}--lam-{i}(%agda.struct.frame* %record, %agda.struct.thunk* %arg_r_thunk)
                {{
                    ; (1) get right-hand argument and force it (current item being pushed to stack)
                    %arg_r = call %agda.struct.value* @agda.eval.appl.0(%agda.struct.thunk* %arg_r_thunk)
                    ; (2) get left-hand argument and force it (it's on the record)
                    %arg_l_thunk = call %agda.struct.thunk* @agda.record.get(%agda.struct.frame* %record, i64 0)
                    %arg_l = call %agda.struct.value* @agda.eval.appl.0(%agda.struct.thunk* %arg_l_thunk)
                    ; (3) call&ret @agda.prim.impl.{name}(arg_l, arg_r)
                    %res = call %agda.struct.value* @agda.prim.impl.{name}(%agda.struct.value* %arg_l, %agda.struct.value* %arg_r)
                    ret %agda.struct.value* %res
                }}
                """
        else:
            res += f"""\
                define
                %agda.struct.value*
                @agda.prim.{name}--lam-{i}(%agda.struct.frame* %record_orig, %agda.struct.thunk* %arg)
                {{
                    %record_work_ptr = alloca %agda.struct.frame*
                    store %agda.struct.frame* %record_orig, %agda.struct.frame** %record_work_ptr
                    call void(%agda.struct.frame**, %agda.struct.thunk*) @agda.record.push_replace(%agda.struct.frame** %record_work_ptr, %agda.struct.thunk* %arg)
                    %record = load %agda.struct.frame*, %agda.struct.frame** %record_work_ptr
                    %v = call %agda.struct.value*() @agda.alloc.value()
                    %v_tag = getelementptr %agda.struct.value, %agda.struct.value* %v, i32 0, i32 0
                    store i64 0, i64* %v_tag
                    %v_fn = bitcast %agda.struct.value* %v to %agda.struct.value.fn*
                    %v_fn_ptr = getelementptr %agda.struct.value.fn, %agda.struct.value.fn* %v_fn, i32 0, i32 1, i32 0
                    store %agda.struct.value*(%agda.struct.frame*, %agda.struct.thunk*)* @agda.prim.{name}--lam-{i+1}, %agda.struct.value*(%agda.struct.frame*, %agda.struct.thunk*)** %v_fn_ptr
                    %v_fn_record = getelementptr %agda.struct.value.fn, %agda.struct.value.fn* %v_fn, i32 0, i32 1, i32 1
                    store %agda.struct.frame* %record, %agda.struct.frame** %v_fn_record
                    ret %agda.struct.value* %v
                }}
                """

    return res


def main():
    # Generate curried wrappers
    with open("gen/AgdaPrimWrap.ll", "w") as f:
        # Copy header contents
        with open("header.ll", "r") as f_header:
            for line in f_header:
                f.write(line)

        # Write primitive wrappers
        for prim_name, prim_arity in PRIMITIVES:
            f.write(gen_prim(prim_name, prim_arity))
            f.write("\n")

    # Generate header
    with open("gen/header-prim.ll", "w") as f:
        f.write(";; Agda Primitives")
        f.write("\n\n")
        for prim_name, _ in PRIMITIVES:
            f.write(f"""\
                declare
                %agda.struct.thunk*
                @agda.prim.{prim_name}(%agda.struct.frame*)
                """)

if __name__ == "__main__":
    main()

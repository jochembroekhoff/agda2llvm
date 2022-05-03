#include <gc/gc.h>
#include <inttypes.h>

#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#define AGDA_ALLOC(sz) GC_MALLOC(sz)
#define AGDA_ALLOC_VALUE() (struct AGDA_Value *)AGDA_ALLOC(sizeof(struct AGDA_Value))
#define AGDA_ALLOC_THUNK() (struct AGDA_Thunk *)AGDA_ALLOC(sizeof(struct AGDA_Thunk))
#define AGDA_ALLOC_DATA(name) (struct AGDA_Data_##name *)AGDA_ALLOC(sizeof(struct AGDA_Data_##name))

/*
AGDA:

data Dummy : Set where
    dummy1 : Dummy
    dummy2 : Dummy

maker : Dummy -> Dummy
maker = dummy2

result = maker

-- expected output: dummy2

LAMBDA FORM:

#dummy1/0
#dummy2/0
maker = \-> (#dummy2)
result = \-> (maker)
*/

#define SOMETHING void*

static
size_t AGDA_Table_CtorArity[];
static
char* AGDA_Table_CtorName[];

struct AGDA_Eval;
struct AGDA_Value;
struct AGDA_Thunk;
struct AGDA_DataBase;

struct AGDA_Eval {
    struct AGDA_Value * (*Ptr)(SOMETHING);
    SOMETHING Record;
};

struct AGDA_Value {
    enum { VALUE_Function, VALUE_Data } TYPE;
    union {
        struct AGDA_Eval Function;
        struct AGDA_DataBase *Data;
    };
};

struct AGDA_Thunk {
    bool evaluated;
    union {
        struct AGDA_Eval eval;
        struct AGDA_Value *value;
    };
};

struct AGDA_DataBase {
    size_t ID;
    size_t CASE;
};

static
void
AGDA_Init(void)
{
    GC_INIT();
}

static
struct AGDA_Value *
AGDA_Eval(const struct AGDA_Eval *eval)
{
    return eval->Ptr(eval->Record);
}

static
struct AGDA_Value *
AGDA_Force(struct AGDA_Thunk *thunk)
{
    if (thunk->evaluated) {
        printf("[Force] THUNK(%p) already evaluated\n", thunk);
        goto ret;
    }
    printf("[Force] THUNK(%p) evaluating...\n", thunk);
    thunk->evaluated = true;
    thunk->value = AGDA_Eval(&thunk->eval);
    printf("[Force] THUNK(%p) value=%p\n", thunk, thunk->value);
ret:
    return thunk->value;
}

static
struct AGDA_Value *
AGDA_Appl_0(struct AGDA_Thunk *appl)
{
    printf("[Appl_0] THUNK(%p) apply with no args...\n", appl);
    struct AGDA_Value *v = AGDA_Force(appl);
    if (v->TYPE != VALUE_Function) {
        printf("[Appl_0] THUNK(%p) is not Function\n", appl);
        return NULL;
    }
    return AGDA_Eval(&v->Function);
}

/** create malloc'd pretty representation */
static
char *
AGDA_ValuePretty(struct AGDA_Value *value)
{
    char *pretty = malloc(1024);
    pretty[0] = '\0';
    size_t len = 0;

    switch (value->TYPE) {
    case VALUE_Function:
        strcat(pretty, "(fn)");
        len += strlen("(fn)");
        break;
    case VALUE_Data: {
        struct AGDA_DataBase *data = value->Data;
        size_t ctor_idx = data->ID + data->CASE;
        size_t arity = AGDA_Table_CtorArity[ctor_idx];
        const char *name = AGDA_Table_CtorName[ctor_idx];
        strcat(pretty, name);
        len += strlen(name);
        break;
    }
    }

    pretty[len] = '\0';
    return pretty;
}

static
void
AGDA_Main(struct AGDA_Thunk *(*entry)(void))
{
    AGDA_Init();
    struct AGDA_Value *v = AGDA_Force(entry());
    printf("VALUE: %p\n", v);
    char *v_pretty = AGDA_ValuePretty(v);
    printf("CONTENT: %s\n", v_pretty);
    free(v_pretty);
}

/****/

static
size_t AGDA_Table_CtorArity[] = {
    0,
    0,
};
static
char* AGDA_Table_CtorName[] = {
    "Dummy.dummy1",
    "Dummy.dummy2",
};

enum { DATA_Dummy_dummy1, DATA_Dummy_dummy2 } AGDA_DataCase_Dummy;
struct AGDA_Data_Dummy {
    struct AGDA_DataBase BASE;
    union {
        // NO CONTENT YET
    };
};
#define AGDA_ID_BASE_Dummy 0

static
struct AGDA_Value *
CTOR_Dummy_dummy2(SOMETHING record)
{
    struct AGDA_Data_Dummy *data = AGDA_ALLOC_DATA(Dummy);
    data->BASE.ID = AGDA_ID_BASE_Dummy;
    data->BASE.CASE = DATA_Dummy_dummy2;

    struct AGDA_Value *v = AGDA_ALLOC_VALUE();
    v->TYPE = VALUE_Data;
    v->Data = (struct AGDA_DataBase *)data;
    return v;
}

static
struct AGDA_Value *
maker__lam0(SOMETHING record)
{
    // (#dummy1)

    struct AGDA_Value *v = AGDA_ALLOC_VALUE();
    v->TYPE = VALUE_Function;
    v->Function.Ptr = CTOR_Dummy_dummy2;
    v->Function.Record = NULL;
    return v;
}

static
struct AGDA_Thunk *
maker()
{
    // maker = \-> (#dummy1)

    struct AGDA_Thunk *res = AGDA_ALLOC_THUNK();
    res->evaluated = false;
    res->eval.Ptr = maker__lam0;
    res->eval.Record = NULL;
    return res;
}

static
struct AGDA_Value *
result__lam0(SOMETHING record)
{
    // (maker)

    struct AGDA_Thunk *appl = maker();
    return AGDA_Appl_0(appl);
}

static
struct AGDA_Thunk *
result(void)
{
    // result = \-> (maker)

    struct AGDA_Thunk *res = AGDA_ALLOC_THUNK();
    res->evaluated = false;
    res->eval.Ptr = result__lam0;
    res->eval.Record = NULL;
    return res;
}

/****/

int
main()
{
    AGDA_Main(result);
    return 0;
}

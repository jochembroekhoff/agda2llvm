data Bool : Set where
    true : Bool
    false : Bool
    perhaps : Bool -> Bool -> Bool

-- | Demo function which should be compiled to a double lambda with a three-case inner body
not : Bool -> Bool -> Bool
not true b = false
not false b = b
not (perhaps a b) _ = not a b

main = not true false

{-

not--lam-1(record, p) =
    RET CASE $0 {
        ALT #true/0 = {
            appl = #false
            RET APPL appl
        }
        ALT #false/0 = {
            appl = GET 0
            RET APPL appl
        }
        ALT #perhaps/2 = {
            appl = not
            arg0 = GET 1
            arg1 = GET 0
            RET APPL appl
        }
        FALLBACK = { ERROR UNREACHABLE }
    }

not--lam-0(record, p) =
    record' = PUSH record p
    VALUE.fn{not--lam-1; record'}

not(_) =
    THUNK.value{
        RET VALUE.fn{not--lam-0; NULL}
    }

main() =
    THUNK.delay{
        appl = not
        arg0 = #true
        arg1 = #false
        RET APPL appl arg0 arg1
    }

-}

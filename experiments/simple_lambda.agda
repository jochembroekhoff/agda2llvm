data Dummy : Set where
    dummy1 : Dummy -> Dummy
    dummy2 : Dummy

id : {A : Set} -> A -> A
id a = a

call : Dummy -> Dummy
call a = dummy1 (id a)
main = call dummy2

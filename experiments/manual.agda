data Dummy : Set where
    dummy1 : Dummy -> Dummy
    dummy2 : Dummy

call : Dummy -> Dummy
call a = dummy1 a
main = call dummy2
-- main = dummy2

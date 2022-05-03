data Pair (A B : Set) : Set where
    pair : A -> B -> Pair A B

snd : {A B : Set} -> Pair A B -> B
snd (pair _ right) = right

data Dummy : Set where
    dummy1 : Dummy
    dummy2 : Dummy

_$_ : {A B : Set} -> (A -> B) -> A -> B
_$_ fn arg = fn arg

result : Dummy
result = snd $ (pair dummy1 dummy2)
-- expected output: dummy2

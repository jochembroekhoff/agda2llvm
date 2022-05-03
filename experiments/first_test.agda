module first_test where

data MyList : Set where
    _::_ : MyList -> MyList -> MyList
    [] : MyList

data Nat : Set where
    zero : Nat
    suc : Nat -> Nat

{-# BUILTIN NATURAL Nat #-}

data Bool : Set where
  true : Bool
  false : Bool
otherwise = true
{-# BUILTIN BOOL Bool #-}

-- list of the empty list
a = [] :: []
b : MyList -> MyList -> MyList
-- b (cons _ _ ) = a
-- b nil = a
b _ _ = a

_$_ : {A B : Set} -> (A -> B) -> A -> B
_$_ fn arg = fn arg

c : Nat -> Nat
c (suc zero) = zero
c zero = suc zero
c _ = suc (suc zero)

data _×_ (A B : Set) : Set where
  _,_ : A → B → (A × B)

uncurry : {a b c : Set} → (a → b → c) → (a × b) → c
uncurry fn (a , b) = fn a b

snd_curry : {a b : Set} -> a -> b -> b
snd_curry a b = b

call_uncurry : Nat -> Nat -> Nat
call_uncurry a b = uncurry snd_curry (c a , c b)

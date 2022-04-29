module first_test where

data MyList : Set where
    _::_ : MyList -> MyList -> MyList
    [] : MyList

data Nat : Set where
    zero : Nat
    suc : Nat -> Nat

{-# BUILTIN NATURAL Nat #-}

-- list of the empty list
a = [] :: []
b : MyList -> MyList -> MyList
-- b (cons _ _ ) = a
-- b nil = a
b _ _ = a

c = suc zero

data _×_ (A B : Set) : Set where
  _,_ : A → B → (A × B)

uncurry : {a b c : Set} → (a → b → c) → (a × b) → c
uncurry fn (a , b) = fn a b

my_fn : {a b : Set} -> a -> b -> b
my_fn a b = b
call_uncurry = (uncurry my_fn) (c , c)

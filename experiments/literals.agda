open import Agda.Builtin.Nat

countDown : Nat -> Nat
countDown zero = 123
countDown (suc n) = countDown n + 1

a = 4
main = countDown (2 * a)

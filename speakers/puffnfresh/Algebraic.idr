module Algebraic

-- Example functions

or : Bool -> Bool -> Bool
or p q = p || q

xor : Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

-- Idempotency

Idempotent : (a -> a) -> a -> Type
Idempotent f a = f (f a) = f a

-- Involution

notNotTrue : not (not True) = True
notNotTrue = Refl

notNotFalse : not (not False) = False
notNotFalse = Refl

notNot : (b : Bool) -> not (not b) = b
notNot False = Refl
notNot True = Refl

Involution : (a -> a) -> a -> Type
Involution f a = f (f a) = a

xorInvolution : (a : Bool) -> (b : Bool) -> xor a (xor a b) = b
xorInvolution False False = Refl
xorInvolution False True = Refl
xorInvolution True False = Refl
xorInvolution True True = Refl

-- Uninhabited

notNotIdempotent : (b : Bool) -> Idempotent not b -> Void
notNotIdempotent False Refl impossible
notNotIdempotent True Refl impossible

succNotIdempotent : (n : Nat) -> Idempotent S n -> Void
succNotIdempotent Z Refl impossible
succNotIdempotent (S k) prf =
  succNotIdempotent k (succInjective (S (S k)) (S k) prf)

-- Both

idempotentInvolution : Idempotent f a -> Involution f a -> f a = a
idempotentInvolution idem invo = ?idempotentInvolution

---------- Proofs ----------

Algebraic.idempotentInvolution1 = proof
  intros
  rewrite idem
  rewrite sym invo
  trivial


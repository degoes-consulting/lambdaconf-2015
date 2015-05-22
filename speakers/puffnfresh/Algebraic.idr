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
notNotTrue = ?notNotTrue

notNotFalse : not (not False) = False
notNotFalse = ?notNotFalse

notNot : (b : Bool) -> not (not b) = b
notNot b = ?notNot

Involution : (a -> a) -> a -> Type
Involution f a = f (f a) = a

xorInvolution : (a : Bool) -> (b : Bool) -> Involution (xor a) b
xorInvolution a b = ?xorInvolution

-- Uninhabited

notNotIdempotent : (b : Bool) -> Idempotent not b -> Void
notNotIdempotent b prf = ?notNotIdempotent

succNotIdempotent : (n : Nat) -> Idempotent S n -> Void
succNotIdempotent n prf = ?succNotIdempotent

-- Both

idempotentInvolution : Idempotent f a -> Involution f a -> f a = a
idempotentInvolution idem invo = ?idempotentInvolution

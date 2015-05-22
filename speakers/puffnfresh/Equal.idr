module Equal

-- Equality type

data Equal : x -> y -> Type where
  Reflexive : Equal a a

-- Simple examples

bool : Equal Bool Bool
bool = ?bool

true : Equal True True
true = ?true

two : Equal 2 (1 + 1)
two = ?two

-- Helpers

transitive : Equal a b -> Equal b c -> Equal a c
transitive ab bc = ?transitive

symmetric : Equal a b -> Equal b a
symmetric ab = ?symmetric

congruent : (f : t -> t) -> Equal a b -> Equal (f a) (f b)
congruent = ?congruent

-- Stepping it up

plusZero : (n : Nat) -> Equal (plus n Z) n
plusZero n = ?plusZero

-- Built-in equality

plusZero' : (n : Nat) -> plus n Z = n
plusZero' n = ?plusZero'

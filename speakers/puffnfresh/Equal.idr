module Equal

-- Equality type

data Equal : x -> y -> Type where
  Reflexive : Equal a a

-- Simple examples

bool : Equal Bool Bool
bool = Reflexive

true : Equal True True
true = Reflexive

two : Equal 2 (1 + 1)
two = Reflexive

-- Helpers

transitive : Equal a b -> Equal b c -> Equal a c
transitive Reflexive Reflexive = Reflexive

symmetric : Equal a b -> Equal b a
symmetric Reflexive = Reflexive

congruent : (f : t -> x) -> Equal a b -> Equal (f a) (f b)
congruent f Reflexive = Reflexive

-- Stepping it up

plusZero : (n : Nat) -> Equal (plus n Z) n
plusZero Z = the (Equal Z Z) Reflexive
plusZero (S k) =
  let induction = plusZero k
  in congruent S induction

-- Built-in equality

plusZero' : (n : Nat) -> plus n Z = n
plusZero' Z = Refl
plusZero' (S k) =
  let induction = plusZero' k
  in ?plusZero'_2

---------- Proofs ----------

Equal.plusZero'_2 = proof
  intros
  rewrite induction
  trivial


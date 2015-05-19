{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Algebraically where
import Control.Arrow
import Data.List (inits, tails)


{-
***
Notes and Exercises in Algebraic Program Derivation
Gershom Bazerman, May 2015
***

Tested with: GHC 7.10, GHC 7.8

The following are a set of notes and exercises to introduce the very basic notions of program derivation in the style of "The Algebra of Programming" (Bird and de Moor, 1997). That book provides the best account of this material and much more. However, there is a long and storied history of work in this field. See for instance:

 "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire" and follow on work by Meijer, Fokkinga and Paterson (1991)
"Algorithmics -- Towards programming as a mathematical activity" (Meertens, 1987)
"An Exploration of the Bird-Meertens Formalism" (Backhouse, 1989)
"Algebraic Data Types and Program Transformation" (Malcolm, 1990)
"Upwards and downwards accumulations on trees" (Gibbons, 1992)
-}

--------------------------------------------------------------

-- Part 1: Fixpoints of Functors, and Algebras on Functors

-- equational reasoning requires equations
-- level up equational reasoning by studying equations more powerful than simple substitutions.
-- data types and functions on them come equipped with laws beyond the obvious.

-- strictly positive recursive types can be written as the fixpoints of "signature functors" like so.

data NatF r = ZeroF | SuccF r

data ListF a r = NilF | ConsF a r

data TreeF a r = LeafF a | BranchF r r

-- Isomorphism between algebras as sets of things to do in each case, and functions out of a functor that encompasses all cases.

type LAlg a b = (a -> b -> b, b)

listFold :: LAlg a b -> ListF a b -> b
listFold (c, z) NilF = z
listFold (c, z) (ConsF a r) = c a r

mkListAlg :: (ListF a b -> b) -> LAlg a b
mkListAlg f = (\x xs -> f (ConsF x xs) , f NilF)

{-
Exercise: Show this is an isomorphism

Exercise: Write the same thing for Trees
-}


listMap :: (a -> b) -> ListF c a -> ListF c b
listMap f NilF = NilF
listMap f (ConsF x y) = ConsF x (f y)

fold :: (a -> b -> b, b) -> [a] -> b
fold (c, z) = foldr c z

a = ((:),[])

{-
exercise: write as pair algebras

1: Sum
2: Length

Now, write them as functor algebras (ListF Int Int -> Int)

substitute and convince yourself these are the same.
-}

-- Part 2: Universal Properties of Folds and equational reasoning

{-
universal property of folds

if h = fold f
<==>
h . listFold a = listFold f . listMap h
-}

lfa :: ListF a [a] -> [a]
lfa = listFold a

up1 :: ([a] -> b) -> LAlg a b -> ListF a [a] -> b
up1 h f = h . listFold a

up2 :: ([a] -> b) -> LAlg a b -> ListF a [a] -> b
up2 h f = listFold f . listMap h

-- Substitute h for fold f, and the following are clearly an identity

up3 :: LAlg a b -> ListF a [a] -> b
up3 f = fold f . listFold a

up4 :: LAlg a b -> ListF a [a] -> b
up4 f = listFold f . listMap (fold f)

-- the universal property shows that this goes the other direction

{-
exercise:

show that up3 and up4 are necessarily the same
-}

{-
exercise:
fold a = id
-}

{-
fold fusion:

h . listFold f = listFold g . listMap h
==>
h . fold f = fold g
-}

ff1 h f g = h . listFold f

ff2 h f g = listFold g . listMap h

{-
*Algebraically> :t ff1
ff1 :: (b -> c) -> LAlg a b -> t -> ListF a b -> c

*Algebraically> :t ff2

ff2 :: (a1 -> c) -> t -> LAlg a c -> ListF a a1 -> c

*Algebraically> :t ff1 `asTypeOf` ff2

ff1 `asTypeOf` ff2 :: (b -> c) -> LAlg a b -> LAlg a c -> ListF a b -> c

h :: b -> c
f :: LALg a b
g :: LAlg a c


h . fold f :: [a] -> c
fold g :: [a] -> c
-}

{-
Exercise: Take h to be show, f to be sum.

Determine g.

-}

sumAlg = ((+),0)
ffex1 :: ListF Integer Integer -> String
ffex1 = show . listFold sumAlg

ffex2 :: ListF Integer Integer -> String
ffex2 = g . listMap (show :: Integer -> String)
    where g :: ListF Integer String -> String
          g = undefined -- ?

{-
exercise: show that

listFold a . fold (mkListAlg (listMap (listFold a))) == id

fold (mkListAlg (listMap (listFold a))) . listFold a == id
-}

--------------------------------------------------------------
-- Part 3: The Banana Split Threorem

bs1, bs2, bs3, bs4, bs5, bs6 :: LAlg a b -> LAlg a c -> ListF a [a] -> (b,c)
bs1 c1 c2 = (fold c1 &&& fold c2) . listFold a
-- split fusion
bs2 c1 c2 = fold c1 . listFold a &&& fold c2 . listFold a
-- properties of fold
bs3 c1 c2 = listFold c1 . listMap (fold c1) &&& listFold c2 . listMap (fold c2)
-- split expansion
bs4 c1 c2 = listFold c1 . listMap (fst . (fold c1 &&& fold c2)) &&&
            listFold c2 . listMap (snd . (fold c1 &&& fold c2))
-- functor splitting
bs5 c1 c2 = listFold c1 . listMap fst . listMap (fold c1 &&& fold c2) &&&
            listFold c2 . listMap snd . listMap (fold c1 &&& fold c2)
-- split fusion (backwards)
bs6 c1 c2 = (listFold c1 . listMap fst &&& listFold c2 . listMap snd) . listMap (fold c1 &&& fold c2)

{-
by the universal property of lists, bs1 == bs6 implies that
(fold c1 &&& fold c2) === fold (listFold c1 . listMap fst &&& listFold c2 . listMap snd)
-}
bsResult1, bsResult2 :: LAlg a b -> LAlg a c -> [a] -> (b,c)
bsResult1 c1 c2 = fold c1 &&& fold c2
bsResult2 c1 c2 = fold (mkListAlg (listFold c1 . listMap fst &&& listFold c2 . listMap snd))


{-
exercise, use the banana split theorem to write a single pass average.
-}

{-
exercise: write the bs theorem as an operation on LAlg explicitly, and argue why it is the same.
-}

--------------------------------------------------------------
-- Part 4: Maximum Segment Sum
-- inspired heavily by http://www.iis.sinica.edu.tw/~scm/2010/maximum-segment-sum-origin-and-derivation/

-- A) list homomorphisms

concatList :: [[a]] -> [a]
concatList = foldr (++) []
-- concatList is a natural transformation -- i.e.
-- map f . concatList = concatList . map (map f)

{-
h :: [a] -> a

h [h xs, h ys] == h (xs ++ ys)
==>
h . concat = h . map h

In such a circumstance, we say h is a "list homomorphism" and it is a special fold that is associative and with a zero element. -- i.e. it is given by the action of a monoid.

Other homomorphism laws:

define m xs ys = h [xs,ys]

now, h = foldr m z . map (\x -> h [x])

(where foldr itself may be executed in parallel as it is a fold over a monoidal operation)

Observe: list homomorphisms are subject to _map_ and _reduce_.
-}

{-
Exercise: Give examples of list homomorphisms, and write algebras for them.

Hint: Look at Data.Monoid
-}

-- B) Scanr, inits fusion fusion

tailsAlg :: LAlg a [[a]]
tailsAlg = (go,[[]])
    where go x [] = [[x]]
          go x (y:ys) = (x : y) : y : ys

{-
fold tailsAlg [1,2,3,4]

[[1,2,3,4],[2,3,4],[3,4],[4],[]]
-}

-- note this is reversed from a typical scanr.
scanRight :: LAlg a b -> LAlg a [b]
scanRight (c,z) = (go,[z])
    where go x [] = [c x z]
          go x (y:ys) = c x y : y : ys

{-
By inspection, tails is initial with regards to rightward scans.

scanRight alg = map (fold alg) . fold tailsAlg

rememember fusion
h . listFold f = listFold g . listMap h

take:
h = map (fold alg)
f = tailsAlg
g = scanRight alg

and we see

map (fold alg) . listFold tailsAlg :: ListF a [[a]] -> [a]
listFold (scanRight alg) . listMap (map (fold alg)) :: ListF a [[a]] -> [a]

the two are equal, and therefore by fusion, scanRight alg = map (fold alg) . tailsAlg

Or:

scanr f e = map (foldr f e) . tails

-}

{-
note this is an inefficient inits and scan..
-}

initsAlg :: LAlg a [[a]]
initsAlg = (go,[])
    where go x [] = [] : [x] : []
          go x xs = [] : map (x:) xs
{-
*Algebraically> fold initsAlg [1,2,3,4,5]
[[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
-}

scanLeft :: LAlg a b -> LAlg a [b]
scanLeft (c,z) = (go,[])
    where go x [] = [z,c x z]
          go x ys = z : map (c x) ys


{-
inits fusion

map (fold alg) . fold initsAlg :: ListF a [[a]] -> [a]
listFold (scanLeft alg) . listMap (map (fold alg)) :: ListF a [[a]] -> [a]

these two are equal, therefore by fusion, scanLeft alg = map (fold alg) . initsAlg

Or:

scanl f e = map (foldl f e) . inits
-}

-- C) The problem

testData :: [Integer]
testData = [1,2,3,5,-100,5,1000,-1000,-4,25]

segments :: [Integer] -> [[Integer]]
segments = concat . map inits . tails

mss0 :: [Integer] -> Integer
mss0 = maximum . map sum . segments



mss1, mss2, mss3, mss4 :: [Integer] -> Integer
--inline segments
mss1 = maximum . map sum . concatList        . map inits . tails
-- by naturality of concat
mss2 = maximum . concatList  . map (map sum) . map inits . tails
-- by maximum being a list homomorphism
mss3 = maximum . map maximum . map (map sum) . map inits . tails
-- by map functorality / fusion
mss4 = maximum . map (maximum . map sum . inits) . tails

-- We now denote maximum . map sum . tails as maximum prefix sum
-- maximum segment sum is the maximum of the prefix sums of the tails
-- we wish to manipulate mps into a fold, so that we can use scanr fusion.

mps0,mps1,mps2,mps3,mps4 :: [Integer] -> Integer
mps0 = maximum . map sum . inits

--inits fusion

mps1 = maximum . scanl (+) 0

--expand maximum

mps2 = foldr max 0 . scanl (+) 0

--fold/scan fusion

{-
I believe the following is correct:

if h and g are list homomorphisms, and
g x (h y z) = h (g x y) (g x z) -- i.e. h distributes over g

then

foldr h z . scanr g z = foldr (\x y -> h (g x y) z)

If it is not, then it holds in this specific case, and can be carried through by reasoning directly on (+) and max.
-}

mps3 = foldr (\x y -> max 0 (x + y)) 0

zmax x y = max 0 (x + y)

mps4 = foldr zmax 0


mss5,mss6 :: [Integer] -> Integer

--subsitute in the new mps
mss5 = maximum . map (foldr zmax 0) . tails

-- tails fusion
mss6 = maximum . scanr zmax 0

-- we've gone from quadratic time to linear time!

{-
Exercise: observe that while zmax is derived from two list homomorphisms, zmax itself is not a list homomorphism. Argue why.
-}

--------------------------------------------------------------
-- Part 5: Parallel Prefix Sum

{-
Recall that inits as we gave it had complexity O(N^2).

Note that as a foldl, inits may have complexity O(N), and tails does have such complexity as given.

Can we do better?

One school says, obviously not.

Another says, think parallel!

For simplicity we work with suffix sums (generalized scanr), and assume an O(1) list concatenation

-}

{-
Exercise 1: given an arbitrary operator "op" and an arbitrary z, (scanr op z) is not a list homomorphism. Name a counterexample.
-}

{-
Recall: a list homomorphism is a function h and an operator `op` such that:

h xs `op` h ys = h (xs ++ ys) -- i.e. you can combine then translate, or translate than combine.

This induces a monoid with `op` as the action, and h [] as the unit.

Now we wish to define a function of type

scanOp :: (a -> a -> a) -> [a] -> [a] -> [a]

this function obeys the property that:

scanOp op (scanr op z xs) (scanr op z ys) = scanr op z (xs ++ ys)

-- i.e. we want scanOp to be the monoidal operation induced by "scanr op z"

-}

scanOp :: (a -> a -> a) -> [a] -> [a] -> [a]
-- obvious
scanOp op xs [] = xs
scanOp op [] ys = ys

-- scanOp op xs ys = ?
{-

By the laws of scanr, we know that:

scanr op z (x:xs) = op x (head (scanr op z xs)) : scanr op z xs

We also know that:

scanr op z (xs ++ ys) = _ + scanr op z ys

From this we can conclude that if op is associative,

scanr op z (xs ++ ys) = map (`op` (head (scanr op z ys))) (init $ scanr op z xs) ++ scanr op z ys

From this property we can write our scanOp as desired.

-}

scanOp op xs (y:ys) = map (`op` y) (init xs) ++ (y:ys)

{-
Checking some cases we see that indeed for (op,z) a monoid algebra:

foldr (scanOp op) [] . map (scanr op z) === scanr op z

and hence scanr may be written as a map reduce, and parallelized with time complexity O(log(N))

See: "Extracting and implementing list homomorphisms in parallel program development" (Gorlatch, 1999).

This includes an application of a generalized method to turn maximum segment sum into a homomorphism as well.
-}


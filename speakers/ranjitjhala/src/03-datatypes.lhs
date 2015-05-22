<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--totality"       @-}
{-@ LIQUID "--diff"           @-}

module DataTypes where

import Prelude hiding (replicate, (++), sum, init, length, map, filter, foldr, foldr1)

map         :: (a -> b) -> List a -> List b
foldr1      :: (a -> a -> a) -> List a -> a
head        :: List a -> a
tail        :: List a -> List a
init, init' :: (Int -> a) -> Int -> List a
-- append      :: List a -> List a -> List a
-- filter      :: (a -> Bool) -> List a -> List a
impossible         :: String -> a
average     :: List Int -> Int
-- wtAverage   :: List (Int, Int) -> Int

infixr 9 :::

{-@ data List [length] a = Emp | (:::) {hd :: a, tl :: List a } @-}
{-@ invariant {v: List a | 0 <= length v} @-}

{-@ type Nat      = {v:Int | v >= 0} @-}
{-@ type Pos      = {v:Int | v >  0} @-}

{-@ impossible :: {v:_ | false} -> a @-}
impossible = error

{-@ average :: ListNE Int -> Int @-}
average xs = total `div` n
  where
    total   = foldr1 (+) xs
    n       = length xs
\end{code}

</div>

<br>
<br>
<br>
<br>
<br>



Data Types
==========

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Example: Lists
--------------

<br>

<div class="fragment">
Lets define our own `List` data type:

<br>

\begin{code}
data List a = Emp               -- Nil
            | (:::) a (List a)  -- Cons
\end{code}
</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Specifying the Length of a List
-------------------------------

<br>

<div class="fragment">
**Measure**

Haskell function with *a single equation per constructor*
</div>

<br>

\begin{code}
{-@ measure length @-}
length :: List a -> Int
length Emp        = 0
length (_ ::: xs) = 1 + length xs
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Specifying the Length of a List
-------------------------------


<br>

**Measure**

*Strengthens* type of data constructor

<br>

<div class="fragment">

\begin{spec} <div/>
data List a where

  Emp   :: {v:List a | length v = 0}

  (:::) :: x:a -> xs:List a
        -> {v:List a | length v = 1 + length xs}
\end{spec}

</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Using Measures
==============

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>





Exercise: *Partial* Functions
-----------------------------

<br>

Fear `head` and `tail` no more!

<br>

<div class="fragment">
\begin{code}
{-@ head        :: List a -> a @-}
head (x ::: _)  = x
head _          = impossible "head"

{-@ tail        :: List a -> List a @-}
tail (_ ::: xs) = xs
tail _          = impossible "tail"
\end{code}

<br>

**Q:** Write types for `head` and `tail` that verify `impossible`.
</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Naming Non-Empty Lists
----------------------

<br>

A convenient *type alias*

<br>

\begin{code}
{-@ type ListNE a = {v:List a| 0 < length v} @-}
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

<div class="slideonly">

`head` and `tail` are Safe
--------------------------

When called with *non-empty* lists:

<br>

\begin{spec}
{-@ head :: ListNE a -> a @-}
head (x ::: _)  = x
head _          = impossible "head"

{-@ tail :: ListNE a -> List a @-}
tail (_ ::: xs) = xs
tail _          = impossible "tail"
\end{spec}

</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>




A Useful Partial Function `foldr1`
----------------------------------

<br>

*Fold* `f` over list initially using *first* element:

<br>

\begin{code}
{-@ foldr1 :: (a -> a -> a) -> ListNE a -> a @-}
foldr1 f (x ::: xs) = foldr f x xs
foldr1 _ _          = impossible "foldr1"

foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ acc Emp        = acc
foldr f acc (x ::: xs) = f x (foldr f acc xs)
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Exercise: `average`
-------------------

<br>

\begin{code}
{-@ average' :: List Int -> Int @-}
average' xs = total `div` n
  where
    total   = foldr1 (+) xs
    n       = length xs
\end{code}

<br>

**Q:** What is a safe input type for `average'`?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Refining Data Types
===================


<br>
<br>

&nbsp; &nbsp; *Make illegal states unrepresentable*

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Example: Year is 12 Months
--------------------------

<br>

\begin{code}
data Year a = Year (List a)
\end{code}

<br>

<div class="fragment">
**Legal Values:** Lists of `12` elements, e.g.

<br>

`"jan" ::: "feb" ::: ... ::: "dec" ::: Emp"`

</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Example: Year is 12 Months
--------------------------

<br>

**Refine Type to Legal Values**

\begin{code}
{-@ data Year a = Year (ListN a 12) @-}
\end{code}

<br>

**Lists Of A Given Size**

\begin{code}
{-@ type ListN a N = {v: List a | length v == N} @-}
\end{code}

<br>

<div class="fragment">
**Make illegal states unrepresentable**

\begin{code}
badYear = Year (1 ::: Emp)
\end{code}
</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Exercise: `map`
---------------

<br>

\begin{code}
{-@ map :: (a -> b) -> xs:List a -> List b @-}
map _ Emp         = Emp
map f (x ::: xs)  = f x ::: map f xs
\end{code}

<br>

<div class="fragment">
**Q:** Can you fix `map` to verify `tempAverage`?

<br>

\begin{code}
data Weather = W { temp :: Int, rain :: Int }

tempAverage :: Year Weather -> Int
tempAverage (Year ms) = average months
  where
    months            = map temp ms
\end{code}
</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Exercise: `init`
----------------

<br>

\begin{code}
{-@ init :: (Int -> a) -> Nat -> List a @-}
init _ 0 = Emp
init f n = f n ::: init f (n-1)
\end{code}

<br>

<div class="fragment">
**Q:** Can you fix the type of `init` so that `sanDiegoTemp` is accepted? 

<br>

\begin{code}
sanDiegoTemp :: Year Int
sanDiegoTemp = Year (init (const 72) 12)
\end{code}
</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Exercise: `init'`
------------------

<br>

\begin{code}
{-@ init' :: (Int -> a) -> n:Nat -> List a @-}
init' f n = go 0
  where
    go i | i < n     = f i ::: go (i+1)
         | otherwise = Emp
\end{code}

<br>

<div class="fragment">
**Q:** For bonus points, fix `init'` so `sanDiegoTemp'`is accepted?

<br>

\begin{code}
sanDiegoTemp' :: Year Int
sanDiegoTemp' = Year (init' (const 72) 12)
\end{code}
</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Recap
-----

<br>
<br>

1. **Refinements:** Types + Predicates
2. **Subtyping:** SMT Implication
3. <div class="fragment">**Measures:** Specify Properties of Data</div>

<br>

<div class="fragment">
**Next: Case Studies**

+ [Insertion Sort](04-case-study-insertsort.html)
+ [Well Scoped Evaluator](05-case-study-eval.html)
+ [Low-level Memory](06-case-study-bytestring.html)
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


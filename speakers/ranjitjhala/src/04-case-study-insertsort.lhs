
<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}

module InsertSort where

import Prelude hiding (sum, length, map, filter, foldr, foldr1)
import qualified Data.Set as S -- hiding (elems, insert)

insert, insertE :: (Ord a) => a -> List a -> List a
sort, sortE     :: (Ord a) => List a -> List a

{-@ measure length @-}
length :: List a -> Int
length Emp        = 0
length (_ ::: xs) = 1 + length xs


data List a  = Emp
             | (:::) { hd :: a, tl :: List a }
             deriving (Eq, Ord, Show)

infixr 9 :::

infixr 9 :<:

-- | Lists of a given size N
{-@ type ListN a N = {v:List a | length v == N } @-}

{-@ type OListE a S = {v:OList a | elemsO v = S} @-}

{-@ measure elemsO @-}
elemsO :: (Ord a) => OList a -> S.Set a
elemsO OEmp       = S.empty
elemsO (x :<: xs) = addElemO x xs

{-@ inline addElemO @-}
addElemO :: (Ord a) => a -> OList a -> S.Set a
addElemO x xs = S.singleton x `S.union` elemsO xs
\end{code}

</div>

Case Study: Insertion Sort
==========================


 {#asdisort}
------------

Recall the simplest sorting algorithm:

<br>

\begin{spec}
sort :: (Ord a) => List a -> List a
sort []           = Emp
sort (x:xs)       = insert x (sort xs)

insert :: (Ord a) => a -> List a -> List a
insert x Emp      = x ::: Emp
insert x (y:::ys)
  | x <= y        = x ::: y ::: ys
  | otherwise     = y ::: insert x ys
\end{spec}


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



Goal: Verified Insertion Sort
-----------------------------

<br>

**Goal:** specify & verify that output:

<br>

1. <div class="fragment">Is the same **size** as the input,</div>
2. <div class="fragment">Has the same **elements** as the input,</div>
3. <div class="fragment">Is in increasing **order**.</div>

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


Property 1: Size
================

 {#pr1}
-------



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



Exercise: `insert`
------------------

**Q:** Can you fix the type of `insert` so `sort` checks?

\begin{code}
{-@ sort :: (Ord a) => xs:List a -> ListN a {length xs} @-}
sort Emp          = Emp
sort (x:::xs)     = insert x (sort xs)

{-@ insert :: (Ord a) => a -> xs:List a -> List a @-}
insert x Emp      = x ::: Emp
insert x (y:::ys)
  | x <= y        = x ::: y ::: ys
  | otherwise     = y ::: insert x ys
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



Property 2: Elements
====================

 {#pr2}
-------


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



Permutation
-----------

<br>

Same size is all fine, how about **same elements** in output?

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


SMT Solvers Reason About Sets
-----------------------------

<div class="fragment">

<br>

Hence, we can write *Set-valued* measures

<br>

Using the `Data.Set` API for convenience

<br>

\begin{spec} <div/>
import qualified Data.Set as S
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

Specifying A `List`s Elements
-----------------------------

<br>

\begin{code}
{-@ measure elems @-}
elems :: (Ord a) => List a -> S.Set a
elems Emp      = S.empty
elems (x:::xs) = addElem x xs

{-@ inline addElem @-}
addElem :: (Ord a) => a -> List a -> S.Set a
addElem x xs = S.singleton x `S.union` elems xs
\end{code}

<br>

<div class="fragment">
`inline` lets us reuse Haskell terms in refinements.
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

Exercise: Verifying Permutation
-------------------------------

<br>

Lets verify that `sortE` returns the same set of elements:

<br>

\begin{code}
{-@ type ListE a S = {v:List a | elems v = S }@-}

{-@ sortE :: (Ord a) => xs:List a -> ListE a {elems xs} @-}
sortE Emp         = Emp
sortE (x:::xs)    = insertE x (sortE xs)

{-@ insertE :: (Ord a) => x:a -> xs:List a -> List a @-}
insertE x Emp     = x ::: Emp
insertE x (y:::ys)
  | x <= y        = x ::: y ::: ys
  | otherwise     = y ::: insertE x ys
\end{code}

<br>

**Q:** Can you fix the type for `insertE` so `sortE` verifies?

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


Property 3: Order
=================

 {#pr3}
-------

<br>

Yes, yes, but does `sort` actually **sort** ?

<br>

<div class="fragment">

How to specify **ordered lists** ?

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

Recall: Refined Data Types
--------------------------

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



Refined Data: Ordered Pairs
---------------------------

<br>

Lets write a type for **ordered pairs**

<br>

\begin{code}
data OrdPair = OP {opX :: Int, opY :: Int}
\end{code}

<br>

<div class="fragment">
**Legal Values** value of `opX < opY`

<br>

\begin{spec}
okPair  = OP 2 4  -- legal
badPair = OP 4 2  -- illegal
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


Exercise: Ordered Pairs
-----------------------

<br>

**Q:** Can you refine the data type to *legal* values only?

<br>

\begin{code}
{-@ data OrdPair = OP { opX :: Int, opY :: Int} @-}

okPair  = OP 2 4  -- legal
badPair = OP 4 2  -- illegal
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
<br>

Refined Data: CSV Tables
------------------------

<br>

\begin{code}
data Csv = Csv {
   hdrs :: List String
 , vals :: List (List Int)
 }

scores  = Csv {
   hdrs =  "Id" ::: "Midterm" ::: "Final" ::: Emp
 , vals = (   1 :::       25  :::      88 ::: Emp)
      ::: (   2 :::       27  :::      83 ::: Emp)
      ::: (   3 :::       19  :::      93 ::: Emp)
      ::: Emp
 }
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


Exercise: Valid CSV Tables
--------------------------

<br>

**Q:** Can you refine `Csv` so `scores'` is rejected?

\begin{code}
{-@ data Csv = Csv {
      hdrs :: List String
    , vals :: List (List Int)
    }                                          @-}

scores' = Csv {
   hdrs =  "Id" ::: "Midterm" ::: "Final" ::: Emp
 , vals = (   1 :::       25  :::      88 ::: Emp)
      ::: (   2 :::                    83 ::: Emp)
      ::: (   3 :::       19  :::      93 ::: Emp)
      ::: Emp
 }
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

Property 3: Ordered Lists
-------------------------

<br>

**Refine** the `List` data type to enforce *ordering**!

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


Lists
-----

<br>

Lets **define** a type for ordered lists

<br>

\begin{code}
data OList a =
      OEmp
    | (:<:) { oHd :: a
            , oTl :: OList a }
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


Ordered Lists
-------------

<br>

Lets **refine** the type to enforce **order**

<br>

\begin{code}
{-@ data OList a =
      OEmp
    | (:<:) { oHd :: a
            , oTl :: OList {v:a | oHd <= v}} @-}
\end{code}

<br>

Head `oHd` is **smaller than every value** `v` in tail `oTl`



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


Ordered Lists
-------------

<br>

*Illegal values unrepresentable*

<br>

\begin{code}
okList :: OList Int
okList = 1 :<: 2 :<: 3 :<: OEmp

badList :: OList Int
badList = 1 :<: 3 :<: 2 :<: OEmp
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


Exercise: Insertion Sort
------------------------

<br>

**Q:** Oops. There's a problem! Can you fix it?

<br>

\begin{code}
{-@ sortO ::  xs:List a -> OListE a {elems xs} @-}
sortO Emp      = OEmp
sortO (x:::xs) = insertO x (sortO xs)

{-@ insertO :: x:a -> xs:_  -> OListE a {addElemO x xs} @-}
insertO x (y :<: ys)
  | x <= y     = y :<: x :<: ys
  | otherwise  = y :<: insertO x ys
insertO x _    = x :<: OEmp
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
<br>


Multiple Measures
=================

 {#multimeas}
-------------

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


Different Measures for `List`
-----------------------------

<br>

We just wrote *two* measures for `List`

<br>

+ `length :: List a -> Nat`
+ `elems  :: List a -> Set a`

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

Multiple Measures are Conjoined
-------------------------------

<br>

Data constructor refinements are **conjoined**

<br>

\begin{spec}
data List a where
  Emp   :: {v:List a |  length v = 0
                 && elems  v = empty}
  (:::) :: x:a
        -> xs:List a
        -> {v:List a |  length v = 1 + length xs
                     && elems v  = addElem x  xs }
\end{spec}

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

Measures vs. Indexed Types
--------------------------

<br>

Unlike [indexed types](http://dl.acm.org/citation.cfm?id=270793), measures ...

<br>

<div class="fragment">

+ **Decouple** properties from data type

+ **Reuse** same data type with different invariants

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

Refinements vs. Full Dependent Types
------------------------------------

<br>

+ *Limited* to **decidable logics** but ...

+ *Offer* massive amounts of **automation**

<br>

<div class="fragment">

Compare with `insertionSort` in:

+ [Haskell-Singletons](https://github.com/goldfirere/singletons/blob/master/tests/compile-and-dump/InsertionSort/InsertionSortImp.hs)
+ [Idris](https://github.com/davidfstr/idris-insertion-sort/tree/master)
+ [Coq](http://www.enseignement.polytechnique.fr/informatique/INF551/TD/TD5/aux/Insert_Sort.v)

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

Continue
--------

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

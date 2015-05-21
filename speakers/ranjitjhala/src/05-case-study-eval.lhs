 {#assdidl}
===========

<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module Eval (Map, Expr (..), plus, eval, topEval, safeEval) where

import Prelude hiding (lookup)
import qualified Data.Set as S

{-@ impossible :: {v: _ | false} -> a @-}
impossible :: String -> a
impossible = error

type Env = Map Var Expr

plus :: Expr -> Expr -> Expr
topEval :: Expr -> Expr
safeEval :: Map Var Expr -> Expr -> Maybe Expr

--------------------------------------------------------
-- | Membership test (SKIP?)
--------------------------------------------------------

{-@ member :: (Eq k) => k:_ -> m:_ -> {v:Bool | Prop v <=> has k m} @-}
member :: (Eq k) => k -> Map k v -> Bool
member k' (Bind k _ m)
  | k' == k   = True
  | otherwise = member k' m
member _  Emp = False

\end{code}
</div>

Case Study: Associative Maps & Evaluation
-----------------------------------------

<br>

We've all been bitten by these!

<br>

\begin{spec}
ghci> :m +Data.Map
ghci> let m = fromList [ ("haskell"   , "lazy")
                       , ("javascript", "eager")]

ghci> m ! "haskell"
"lazy"

ghci> m ! "python"
"*** Exception: key is not in the map
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




Case Study: Associative Maps & Evaluation
-----------------------------------------

<br>

Next, lets see how to use:

<br>

+ Sets to create safe **associative maps**

+ Measures to create **well-scoped evaluators**

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


Associative Maps
================

 {#safemap}
-----------

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


A Simple Associative Map
------------------------

<br>

Lets start with the type definition:

<br>

\begin{code}
data Map k v = Emp
             | Bind k v (Map k v)
               deriving (Eq, Ord, Show)
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



Tracking the Keys in a Map
--------------------------

<br>

We will banish unpleasant exceptions of the form:

\begin{spec} <div/>
*** Exception: key is not in the map ***
\end{spec}

<br>

By tracking the set of defined `keys` in the API

\begin{spec} <div/>
keys   :: Map k v -> Set k

empty  :: {m:Map k v | keys m = emp}

insert :: k:k -> v -> m:Map k v -> {m': Map k v | keys m' = add k m}

lookup :: k:k -> {m: Map | has k m} -> v
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




Specifying the Set of `keys`
----------------------------

<br>

Via a measure similar to `elems` of a `List`:

<br>

\begin{code}
{-@ measure keys @-}
keys :: (Ord k) => Map k v -> S.Set k
keys Emp          = S.empty
keys (Bind k _ m) = add k m
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


The Empty `Map`
---------------

The `empty` map has **no keys**

\begin{code}
{-@ empty :: {m:Map k v | noKeys m} @-}
empty :: Map k v
empty = Emp

{-@ inline noKeys @-}
noKeys :: (Ord k) => Map k v -> Bool
noKeys m = keys m == S.empty
\end{code}


Inserting a New Key-Value Binding
---------------------------------

<br>

Adds the key to those of the old `Map`

<br>

\begin{code}
{-@ insert :: k:_ -> _ -> m:_ -> {v: _ | keys v = add k m } @-}
insert :: k -> v -> Map k v -> Map k v
insert k v m = Bind k v m

{-@ inline add @-}
add :: (Ord k) => k -> Map k v -> S.Set k
add k kvs = S.singleton k `S.union` keys kvs
\end{code}


Exercise: Looking Up a Key's Value
----------------------------------

<br>

**Q:** Urgh! How can we *prevent the impossible* from happening?

\begin{code}
{-@ lookup :: (Eq k) => k:k -> {m:Map k v | has k m} -> v @-}
lookup k' (Bind k v m)
  | k' == k   = v
  | otherwise = lookup k' m
lookup _  Emp = impossible "lookup"

{-@ inline has @-}
has :: (Ord k) => k -> Map k v -> Bool
has k m = k `S.member` keys m -- True

-- HINT
--   keys     :: Map k v -> Set k
--   S.member :: k -> S.Set k -> Bool
\end{code}


Key Not Found Begone!
---------------------

<br>

Errors caught at compile time!

<br>

\begin{code}
test      = [ lookup hs langs   -- Ok
            , lookup py langs   -- Err
            ]
  where
    langs = Bind hs "lazy"  $
            Bind js "eager" $
            Emp
    hs    = "haskell"
    js    = "javascript"
    py    = "python"
\end{code}

Well-Scoped Evaluators
======================

 {#evalapi}
-----------

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



Expressions
-----------

<br>

Lets define a small language of `Expr`

<br>

\begin{code}
data Var  = V String deriving (Eq, Ord, Show)

data Expr = Val  Int
          | Var  Var
          | Plus Expr Expr
          | Let  Var  Expr Expr
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


Values
------

<br>

We can define values as a **refinement** of `Expr`

<br>

\begin{code}
{-@ type Val = {v:Expr | isVal v} @-}

{-@ measure isVal @-}
isVal :: Expr -> Bool
isVal (Val _) = True
isVal _       = False
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



Exercise: Operating on Values
-----------------------------

<br>

**Q:** What's a suitable signature for `plus`?

<br>

\begin{code}
plus (Val i) (Val j) = Val (i+j)
plus _         _     = impossible "plus"
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

Environments
------------

<br>

An `Env`ironment maps `Var`iables to `Val`ues

<br>

\begin{code}
{-@ type Env = Map Var Val @-}
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



Evaluate Using Environments
---------------------------

<br>

We can now `eval` an `Expr` as:

<br>

\begin{code}
{-@ eval :: Env -> Expr -> Val @-}
eval _ i@(Val _)     = i
eval g (Var x)       = lookup x g
eval g (Plus e1 e2)  = plus (eval g e1) (eval g e2)
eval g (Let x e1 e2) = eval gx e2
  where
    gx               = insert x v1 g
    v1               = eval g e1
\end{code}

<br>

**Yikes! `lookup` is rejected!**

<br>

*How to ensure that `Var` is in `Env`?*

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


Free Variables
--------------

<br>

`eval` looks-up `Env` for values of [free variables](http://en.wikipedia.org/wiki/Free_variables_and_bound_variables)

<br>

\begin{code}
{-@ measure free @-}
free               :: Expr -> S.Set Var
free (Val _)       = S.empty
free (Var x)       = S.singleton x
free (Plus e1 e2)  = xs1 `S.union`  xs2
  where
    xs1            = free e1
    xs2            = free e2
free (Let x e1 e2) = xs1 `S.union` (xs2 `S.difference` xs)
  where
    xs1            = free e1
    xs2            = free e2
    xs             = S.singleton x
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



Free Variables
--------------

<br>

For example in `let x = 10 in x + y`

+ `y` occurs free,
+ `x` occurs bound.

<br>

<div class="fragment">

\begin{spec} <div/>
ghci> let e1 = Let (V "x") (Val 10)
                 (Plus (Var (V "x")) (Var (V "y")))

ghci> free e1
      fromList [V "y"]
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



Well-scoped Expressions
-----------------------

<br>

`e` is **well-scoped** in an env `G` if free variables of `e` are defined in `G`:

<br>

\begin{code}
{-@ type ScopedExpr G = {e: Expr | wellScoped G e} @-}

{-@ inline wellScoped @-}
wellScoped :: Env -> Expr -> Bool
wellScoped g e = free e `S.isSubsetOf` keys g
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


Exercise: Well-Scoped `eval`
----------------------------

<br>

**Q:** Can you go back and fix the type of `eval` so it is safe?

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

Exercise: Top-level Evaluation
------------------------------

<br>

A **closed** `Expr` can be evaluated in an **empty** environment.

<br>

\begin{code}
{-@ type ClosedExpr = {e: Expr | closed e} @-}

{-@ topEval :: ClosedExpr -> Val @-}
topEval =  eval Emp
\end{code}

<br>

**Q:** Fix the definition of `closed` so `topEval` is safe?

<br>

\begin{code}
{-@ inline closed @-}
closed :: Expr -> Bool
closed e = True -- EXERCISE
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

Exercise: Checked Top-level Evaluation
--------------------------------------

<br>

`safeEval` should work with **any** `Expr` and `Env`

<br>

**Q:** What is the right check `ok` such that `safeEval` verifies?

<br>

\begin{code}
{-@ safeEval :: Env -> Expr -> Maybe Val @-}
safeEval g e
  | ok        = Just $ eval g e
  | otherwise = Nothing
  where
    ok        = True -- EXERCISE
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

<div class="hidden">

CHEAT SHEET :)

topEval

topEval:
  closed e = free e == S.empty

safeEval:
  ok       = wellScoped g e

</div>

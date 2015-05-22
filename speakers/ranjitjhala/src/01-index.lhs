<div class="hidden">
\begin{code}
main = putStrLn "Easter Egg: to force Makefile"
\end{code}
</div>


<br>
<br>
<br>
<br>
<br>

<h1 style="border-bottom:none">Refinement Types for Haskell</h1>

<br>

&nbsp; &nbsp; **Ranjit Jhala** *(University of California, San Diego)*

<br>

&nbsp; &nbsp; With: N. Vazou, E. Seidel, D. Vytiniotis, S. Peyton-Jones


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Well-Typed Programs *Can* Go Wrong
==================================


<div class="hidden">

\begin{code}
main = putStrLn "Easter Egg: to force Makefile"
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



Division By Zero
----------------


<div class="fragment">
\begin{spec}
λ> let average xs = sum xs `div` length xs

λ> average [100, 202, 300]
2
\end{spec}
</div>

<br>

<div class="fragment">
\begin{spec}
λ> average []
*** Exception: divide by zero
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



Missing Keys
------------

<div class="fragment">
\begin{spec}
λ> :m +Data.Map
λ> let m = fromList [ ("haskell"    , "lazy")
                    , ("javascript" , "eager")]

λ> m ! "haskell"
"lazy"
\end{spec}
</div>

<br>

<div class="fragment">
\begin{spec}
λ> m ! "racket"
"*** Exception: key is not in the map
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



Segmentation Faults
-------------------

<div class="fragment">
\begin{spec}
λ> :m +Data.Vector
λ> let v = fromList ["haskell", "javascript"]
λ> unsafeIndex v 0
"haskell"
\end{spec}
</div>

<div class="fragment">
<br>
\begin{spec}
λ> V.unsafeIndex v 3

'ghci' terminated by signal SIGSEGV ...
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



"HeartBleeds"
-------------


\begin{spec}
λ> :m + Data.Text Data.Text.Unsafe
λ> let t = pack "LambdaConf"
λ> takeWord16 5 t
"Lambda"
\end{spec}

<br>

<div class="fragment">
Memory overflows **leaking secrets**...

<br>

\begin{spec}
λ> takeWord16 20 t
"LambdaConf\1912\3148\NUL\15928\2486\SOH\NUL"
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



Goal: Extend Type System
------------------------

<br>

<br>

+ To prevent **wider class** of errors

+ To enforce **program specific** properties

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







Plan
----

<br>

**Part I: Refinement Types**

+ <div class="fragment"> [**Refinements**](02-refinements.html)</div>
+ <div class="fragment"> [**Data Types**](03-datatypes.html)</div>

<br>

**Part II: Case Studies**

+ <div class="fragment">[**Insertion Sort**](04-case-study-insertsort.html)</div>
+ <div class="fragment">[**Well Scoped Evaluator**](05-case-study-eval.html)</div>
+ <div class="fragment">[**Low-level Memory**](06-case-study-bytestring.html)</div>



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

Conclusion
----------

<br>

**Refinement Types:** Automated Dependent Typing via SMT

<br>

<div class="fragment">

-------------------       ------------------------------------------------
**Properties:**           Predicates  *+ Types*
**Proofs:**               SMT Solvers *+ Subtyping*
**Inference:**            Abstract Interpretation *+ Hindley-Milner*
-------------------       ------------------------------------------------

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



Current & Future Work
---------------------

<br>

**Technology**

<br>

+ GHC
+ Speed
+ Effects
+ *Error Messages*


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



Current & Future Work
---------------------

<br>

**Applications**

<br>

+ Testing
+ Web frameworks
+ Concurrency
+ Code Completion


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



Thank You!
----------

<br>
<br>


[`http://www.refinement-types.org`](http://www.refinement-types.org)

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




<div class="hidden">

\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--diffcheck"     @-}

module Memory where

import Prelude hiding (null)
import Data.Char
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Data.ByteString.Internal (c2w, w2c)
import Language.Haskell.Liquid.Prelude

create :: Int -> (Ptr Word8 -> IO ()) -> ByteString

\end{code}

</div>

<br>
<br>
<br>
<br>
<br>
<br>

Case Study: Low Level Memory
============================

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


"HeartBleed" in Haskell
-----------------------

<br>

**Modern languages are built on top of C**

<br>

<div class="fragment">
Implementation errors could open up vulnerabilities
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



"HeartBleed" in Haskell (1/3)
-----------------------------

<br>

**A String Truncation Function**

<br>

\begin{spec}
import Data.ByteString.Char8  (pack, unpack)
import Data.ByteString.Unsafe (unsafeTake)

chop     :: String -> Int -> String
chop s n = s'
  where
    b    = pack s         -- down to low-level
    b'   = unsafeTake n b -- grab n chars
    s'   = unpack b'      -- up to high-level
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


"HeartBleed" in Haskell (2/3)
-----------------------------

<br>

<img src="img/overflow.png" height=100px>

<br>

Works if you use a **valid prefix** size

\begin{spec}
λ> let ex = "Ranjit Loves Burritos"

λ> chop ex 10
"Ranjit Lov"
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



"HeartBleed" in Haskell (3/3)
-----------------------------

<br>

<img src="img/overflow.png" height=100px>

<br>

Leaks *overflow buffer* if **invalid prefix** size!

\begin{spec}
λ> let ex = "Ranjit Loves Burritos"

λ> chop ex 30
"Ranjit Loves Burritos\NUL\201\&1j\DC3\SOH\NUL"
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

Types Against Overflows
=======================


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
<br>
<br>
<br>



Types Against Overflows
-----------------------

<br>

**Strategy: Specify and Verify Types for**

<br>

1. <div class="fragment">Low-level `Pointer` API</div>
2. <div class="fragment">Lib-level `ByteString` API</div>
3. <div class="fragment">User-level `Application` API</div>

<br>

<div class="fragment">Errors at *each* level are prevented by types at *lower* levels</div>


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



1. Low-Level Pointer API
========================


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



API: Types
----------

<br>

**Low-level Pointers**

\begin{spec}
data Ptr a
\end{spec}

<br>

<div class="fragment">
**Foreign Pointers**

\begin{spec}
data ForeignPtr a
\end{spec}

<br>

`ForeignPtr` wraps around `Ptr`; can be exported to/from C.
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



API: Operations (1/2)
---------------------

<br>

<div class="fragment">
**Read**

\begin{spec}
peek     :: Ptr a -> IO a
\end{spec}
</div>

<br>
<div class="fragment">
**Write**

\begin{spec}
poke     :: Ptr a -> a -> IO ()
\end{spec}
</div>

<br>
<div class="fragment">
**Arithmetic**
\begin{spec}
plusPtr  :: Ptr a -> Int -> Ptr b
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


API: Operations (2/2)
---------------------

<br>

<div class="fragment">
**Create**

\begin{spec}
malloc  :: Int -> ForeignPtr a
\end{spec}
</div>

<br>

<div class="fragment">
**Unwrap and Use**

\begin{spec}
withForeignPtr :: ForeignPtr a     -- pointer
               -> (Ptr a -> IO b)  -- action
               -> IO b             -- result
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


Low-Level API: Example
----------------------

<br>

**Allocate a block and write 4 zeros into it**

<br>

<div class="fragment">
\begin{code}
zero4 = do fp <- malloc 4
           withForeignPtr fp $ \p -> do
             poke (p `plusPtr` 0) zero
             poke (p `plusPtr` 1) zero
             poke (p `plusPtr` 2) zero
             poke (p `plusPtr` 3) zero
           return fp
        where
           zero = 0 :: Word8
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



Low-Level API: Example
----------------------

<br>

**Allocate a block and write `4` zeros into it**

How to *prevent overflows* e.g. writing `5` or `50` zeros?

<br>

<div class="fragment">
**Step 1**

*Refine pointers* with allocated size
</div>

<br>

<div class="fragment">
**Step 2**

*Track sizes* in pointer operations
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



Refined API: Types
------------------

<br>

**Step 1. Refine pointers with allocated size**

\begin{spec} <div/>
measure plen  :: Ptr a -> Int
measure fplen :: ForeignPtr a -> Int
\end{spec}

<br>

**Pointers of a Given Size**

\begin{spec} <div/>
type PtrN a N        = {v:Ptr a         | plen v  == N }
type ForeignPtrN a N = {v:ForeignPtrN a | fplen v == N }
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

Refined API: Ops (1/3)
----------------------

<br>

<div class="fragment">
**Create**

\begin{spec}
malloc  :: n:Nat -> ForeignPtrN a n
\end{spec}
</div>

<br>

<div class="fragment">
**Unwrap and Use**

\begin{spec}
withForeignPtr :: fp:ForeignPtr a
               -> (PtrN a (fplen fp) -> IO b)
               -> IO b
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



Refined API: Ops (2/3)
----------------------

<br>

**Arithmetic**

Refine type to track *remaining* buffer size

<br>

<div class="fragment">
\begin{spec}
plusPtr :: p:Ptr a
        -> o:{Nat|o <= plen p}   -- in bounds
        -> PtrN b {plen b - o}   -- remainder
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



Refined API: Ops (3/3)
----------------------

<br>

**Read & Write require non-empty remaining buffer**

<br>

<div class="fragment">
**Read**

\begin{spec}
peek :: {v:Ptr a | 0 < plen v} -> IO a
\end{spec}
</div>

<br>
<div class="fragment">
**Write**

\begin{spec}
poke :: {v:Ptr a | 0 < plen v} -> a -> IO ()
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

Example: Overflow Prevented
---------------------------

<br>

How to *prevent overflows* e.g. writing 5 or 50 zeros?

<br>

<div class="fragment">

\begin{code}
exBad = do fp <- malloc 4
           withForeignPtr fp $ \p -> do
             poke (p `plusPtr` 0) zero
             poke (p `plusPtr` 1) zero
             poke (p `plusPtr` 2) zero
             poke (p `plusPtr` 5) zero
           return fp
        where
           zero = 0 :: Word8
\end{code}

</div>

Types Against Overflows
-----------------------

<br>

**Strategy: Specify and Verify Types for**

<br>

1. Low-level `Pointer` API
2. **Lib-level `ByteString` API**
3. User-level `Application` API

<br>

<div class="fragment">Errors at *each* level are prevented by types at *lower* levels</div>


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


2. ByteString API
=================

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
<br>
<br>




ByteString Type
---------------

<br>

<img src="img/bytestring.png" height=150px>

<br>

\begin{code}
data ByteString = PS {
    bPtr :: ForeignPtr Word8
  , bOff :: !Int
  , bLen :: !Int
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




Refined ByteString Type
-----------------------

<br>

<img src="img/bytestring.png" height=150px>

<br>

\begin{code}
{-@ data ByteString = PS {
      bPtr :: ForeignPtr Word8
    , bOff :: {v:Nat| v        <= fplen bPtr}
    , bLen :: {v:Nat| v + bOff <= fplen bPtr}
    }                                       @-}
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



Refined ByteString Type
-----------------------

<br>

<img src="img/bytestring.png" height=150px>

<br>

**ByteStrings of a Given Size**

\begin{spec}
type ByteStringN N = {v:ByteString| bLen v = N}
\end{spec}


<div class="hidden">
\begin{code}
{-@ type ByteStringN N = {v:ByteString | bLen v = N} @-}
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


Legal Bytestrings
-----------------


<br>

\begin{code}
{-@ good1 :: IO (ByteStringN 5) @-}
good1 = do fp <- malloc 5
           return (PS fp 0 5)

{-@ good2 :: IO (ByteStringN 3) @-}
good2 = do fp <- malloc 5
           return (PS fp 2 3)
\end{code}

<br>

<div class="fragment">
**Note:** *length* of `good2` is `3` which is *less than* allocated size `5`
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





Illegal Bytestrings
-----------------

<br>

\begin{code}
bad1 = do fp <- malloc 3
          return (PS fp 0 10)

bad2 = do fp <- malloc 3
          return (PS fp 2 2)
\end{code}

<br>

<div class="fragment">
Claimed length *exceeds* allocation ... **rejected** at compile time
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


ByteString API: `create`
------------------------

<br>

**Allocate and fill a `ByteString`**

<br>

\begin{code}
{-@ create :: n:Nat
           -> (PtrN Word8 n -> IO ())
           -> ByteStringN n
  @-}
create n fill = unsafePerformIO $ do
  fp  <- malloc n
  withForeignPtr fp fill
  return (PS fp 0 n)
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



API: `pack`
------------

<br>

**Pack a high-level `String` into a `ByteString`**

<br>

\begin{code}
{-@ pack      :: s:String -> ByteStringN {len s} @-}
pack str      = create n $ \p -> go p xs
  where
  n           = length str
  xs          = map c2w str
  go p (x:xs) = poke p x >> go (plusPtr p 1) xs
  go _ []     = return  ()
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




API: `unsafeTake`
-----------------

Extract *prefix* string of size `n`

<br>

<div class="fragment">
**Specification**

\begin{code}
{-@ unsafeTake :: n:Nat
               -> b:{ByteString | n <= bLen b}
               -> ByteStringN n            @-}
\end{code}
</div>


<br>

<div class="fragment">
**Implementation**

\begin{code}
unsafeTake n (PS x s l) = PS x s n
\end{code}
</div>

API: `unpack`
-------------

**Specification**

\begin{spec}
unpack
 :: b:ByteString -> StringN (bLen b)
\end{spec}

<br>

<div class="fragment">
**Implementation**

\begin{spec}
unpack b = you . get . the . idea -- see source
\end{spec}
</div>

<div class="hidden">
\begin{code}
{-@ qualif Unpack(v:a, acc:b, n:int) : len v = 1 + n + len acc @-}

{-@ unpack :: b:ByteString -> StringN (bLen b) @-}
unpack :: ByteString -> String
unpack (PS _  _ 0)  = []
unpack (PS ps s l)  = unsafePerformIO $ withForeignPtr ps $ \p ->
   go (p `plusPtr` s) (l - 1)  []
  where
   go p 0 acc = peek p >>= \e -> return (w2c e : acc)
   go p n acc = peek (p `plusPtr` n) >>=   \e -> go p (n-1) (w2c e : acc)
\end{code}

</div>

 {#heartbleedredux}
==================


3. Application API
-------------------


<br>

Strategy: Specify and Verify Types for

<br>

1. Low-level `Pointer` API
2. Lib-level `ByteString` API
3. **User-level `Application` API**

<br>

Errors at *each* level are prevented by types at *lower* levels

3. Application API
==================

Revisit "HeartBleed"
--------------------

Lets revisit our potentially "bleeding" `chop`

<br>

<div class="hidden">
\begin{code}
{-@ type StringN N = {v:String | len v = N} @-}
\end{code}
</div>

<div class="fragment">

\begin{code}
{-@ chop :: s:String
         -> n:{Nat | n <= len s}
         -> StringN n
  @-}
chop s n =  s'
  where
    b    = pack s          -- down to low-level
    b'   = unsafeTake n b  -- grab n chars
    s'   = unpack b'       -- up to high-level
\end{code}

</div>

"HeartBleed" no more
--------------------

<br>

\begin{code}
demo     = [ex6, ex30]
  where
    ex   = ['N','o','r','m','a','n']
    ex6  = chop ex 6  -- ok
    ex30 = chop ex 30  -- out of bounds
\end{code}

<br>

"Bleeding" `chop ex 30` *rejected* by compiler

Recap: Types vs Overflows
-------------------------

<br>

**Strategy: Specify and Verify Types for**

<br>

1. Low-level `Pointer` API
2. Lib-level `ByteString` API
3. User-level `Application` API

<br>

**Errors at *each* level are prevented by types at *lower* levels**


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














<div class="hidden">

\begin{code}
{-@ unsafeCreate :: l:Nat -> ((PtrN Word8 l) -> IO ()) -> (ByteStringN l) @-}
unsafeCreate = create -- unsafePerformIO $ create n f

{-@ invariant {v:ByteString   | bLen  v >= 0} @-}

{-@ qualif PLLen(v:a, p:b) : (len v) <= (plen p) @-}
{-@ qualif ForeignPtrN(v:ForeignPtr a, n:int): fplen v = n @-}
{-@ qualif FPLenPLen(v:Ptr a, fp:ForeignPtr a): fplen fp = plen v @-}
{-@ qualif PtrLen(v:Ptr a, xs:List b): plen v = len xs @-}
{-@ qualif PlenEq(v: Ptr a, x: int): x <= (plen v) @-}
{-@ type ForeignPtrN a N = {v:ForeignPtr a | fplen v = N} @-}

{-@ malloc :: n:Nat -> IO (ForeignPtrN a n) @-}
{-@ assume mallocForeignPtrBytes :: n:Nat -> IO (ForeignPtrN a n) @-}
malloc = mallocForeignPtrBytes

{-@ memcpy :: dst:PtrV Word8
           -> src:PtrV Word8
           -> size: {v:CSize | (v <= (plen src) && v <= (plen dst))}
           -> IO ()
  @-}
memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
memcpy p q s = undefined -- c_memcpy p q s >> return ()

\end{code}

</div>

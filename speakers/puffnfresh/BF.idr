module Main

%default total

-- Data type

data BF : Char -> Type where
  Next : BF '>'
  Prev : BF '<'
  Increment : BF '+'
  Decrement : BF '-'
  Output : BF '.'
  Accept : BF ','
  Forward : BF '['
  Backward : BF ']'

-- Interpreter

data Tape a = MkTape (Stream a) a (Stream a)

right : Tape a -> Tape a
right (MkTape ls c (r :: rs)) = MkTape (c :: ls) r rs

left : Tape a -> Tape a
left (MkTape (l :: ls) c rs) = MkTape ls l (c :: rs)

emptyTape : Tape Int
emptyTape = MkTape (repeat 0) 0 (repeat 0)

Instruction : Type
Instruction = Maybe (Sigma Char BF)

mutual
  partial
  nextInstruction : Tape Int -> Tape Instruction -> IO ()
  nextInstruction dat ins = bf' dat (right ins)

  partial
  runInstruction : Tape Int -> Tape Instruction -> BF c -> IO ()

  runInstruction dat ins Next =
    nextInstruction (right dat) ins

  runInstruction dat ins Prev =
    nextInstruction (left  dat) ins

  runInstruction (MkTape ls c rs) ins Increment =
    nextInstruction (MkTape ls (succ c) rs) ins

  runInstruction (MkTape ls c rs) ins Decrement =
    nextInstruction (MkTape ls (pred c) rs) ins

  runInstruction dat@(MkTape _ c _) ins Output = do
    putChar (chr c)
    nextInstruction dat ins

  runInstruction (MkTape ls _ rs) ins Access = do
    c <- getChar
    nextInstruction (MkTape ls (ord c) rs) ins

  runInstruction dat@(MkTape _ n _) ins Forward = do
    if n == 0
    then loopRight Z dat ins
    else nextInstruction dat ins

  runInstruction dat@(MkTape _ n _) ins Backward = do
    if n /= 0
    then loopLeft Z dat ins
    else nextInstruction dat ins

  runInstruction dat ins _ =
    nextInstruction dat ins

  partial
  loopRight' : Nat -> Tape Int -> Tape Instruction -> BF c -> IO ()
  loopRight' (S Z) dat ins Backward =
    nextInstruction dat ins
  loopRight' n dat ins Backward =
    loopRight (pred n) dat (right ins)
  loopRight' n dat ins Forward =
    loopRight (succ n) dat (right ins)
  loopRight' n dat ins _ =
    loopRight n dat (right ins)

  partial
  loopRight : Nat -> Tape Int -> Tape Instruction -> IO ()
  loopRight n dat ins@(MkTape _ (Just p) _) =
    loopRight' n dat ins (getProof p)
  loopRight n dat ins =
    loopRight n dat (right ins)

  partial
  loopLeft' : Nat -> Tape Int -> Tape Instruction -> BF c -> IO ()
  loopLeft' (S Z) dat ins Forward =
    nextInstruction dat ins
  loopLeft' n dat ins Forward =
    loopLeft (pred n) dat (left ins)
  loopLeft' n dat ins Backward =
    loopLeft (succ n) dat (left ins)
  loopLeft' n dat ins _ =
    loopLeft n dat (left ins)

  partial
  loopLeft : Nat -> Tape Int -> Tape Instruction -> IO ()
  loopLeft n dat ins@(MkTape _ (Just p) _) =
    loopLeft' n dat ins (getProof p)
  loopLeft n dat ins =
    loopLeft n dat (left ins)

  partial
  bf' : Tape Int -> Tape Instruction -> IO ()
  bf' _     (MkTape _ Nothing  _) = return ()
  bf' dat ins@(MkTape _ (Just p) _) = runInstruction dat ins (getProof p)

-- Literals

data Every : List Char -> Type where
  Nil : Every []
  (::) : BF c -> Every cs -> Every (c :: cs)

toInstructions : Every cs -> Tape Instruction
toInstructions [] = MkTape (repeat Nothing) Nothing (repeat Nothing)
toInstructions (x :: y) =
  case toInstructions y of
    MkTape ls c rs => MkTape ls (Just (_ ** x)) (c :: rs)

partial
bf : (s : String) -> { auto p : Every (unpack s) } -> IO ()
bf _ {p} = bf' emptyTape (toInstructions p)


partial
main : IO ()
main = do
  bf"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-."
  bf"+++++++++++++++."
  bf""

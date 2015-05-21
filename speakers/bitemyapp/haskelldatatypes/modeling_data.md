% Modeling data in Haskell
% Chris Allen
% May 2015

# Haskell's type system is one of its most famous strengths

- Lets learn a bit about algebraic datatypes and how to use them effectively

- Talk about domain modeling a little bit, design decisions, where you can go to learn more advanced techniques


# Haskell datatype syntax

Nullary constructor:

```haskell
data Trivial = Trivial
--   [1]       [2]
```

1. Type constructor

2. Data constructor - takes no arguments, thus "nullary"


# Constructors?

We use type constructors to refer to types by name in type signatures.

```haskell
f :: String -> String -> String
f = ...doesn't matta...
```

Here we refer to the type `String` three times in our type signature, denoted syntactically by the double colon `::`

We use data constructors to create values. There's some special syntax for built-in types like String, List, tuples, but in most cases you'll use a data constructor explicitly introduced by the datatype.


# So which is which?

```haskell
data TrivialTypeConstructor
  = TrivialDataConstructor
```

Rule of thumb: before the `=` is the type constructor, after the `=` are the data constructors.

If there's a single data constructor, it'll often have the same name as the type constructor because types and values are strictly separated in Haskell.


# How do we use our datatype with a single nullary data constructor?

```haskell
theProofIs :: Trivial
theProofIs = Trivial

trivialityBegets :: Trivial -> Trivial
trivialityBegets Trivial = Trivial

-- alternately
trivialityBegets _ = Trivial

-- or
trivialityBegets x = Trivial
```

# What can this function do?

What can and can't this function do, just from what we know based on the type?

```haskell
trivInt :: Trivial -> Integer
```

# Haskell datatype syntax

Unary constructor (takes one argument):

```haskell
data Identity a = Identity a
--      [1]           [2]
```

1. Type constructor, takes one argument.

2. Data constructor, takes one argument. Thus, "unary". Unary/nullary refers to the data constructor. You'll see examples with multiple data constructors of mixed arity later.


# How do we use Identity?

```haskell
unpack :: Identity a -> a
unpack (Identity a) = a

embed :: a -> Identity a
embed a = Identity a

imap :: (a -> b) -> Identity a -> Identity b
imap f (Identity a) = Identity (f a)
```

Identity doesn't do much, so if this seems pointless, you're not missing anything.


# Type constructor has to agree with data constructor

Why can't we have:

```haskell
data Identity = Identity a
```

Because you'll get the error: `Not in scope: type variable ‘a’`

Without the argument existing for the data *and* the type constructor, we have no means of expressing what we think `Identity` contains. There ways to "hide" the type variables in data constructors from the type constructors but that's for another day.


# Product

What happens if you add another argument to a unary data constructor? Products!

```haskell
data Person = Person String Int
```

Product here can also be read to mean, "record" or "struct", but be careful with assumptions about representation. Here `Person` is a product of a `String` and an `Int`.


# Person with record syntax

```haskell
data Person = Person { name = String
                     , age = Int }
```

`Person` defined using record syntax for the fields.

Using the field accessors:

```haskell
getName :: Person -> String
getName p = name p

-- eta reduce

getName = name

-- redundant
```

# Tuples

Tuples are our "anonymous product", so called because we don't name anything. We could rewrite our `Person` type as:

```haskell
type Person = (String, Int)
```

The `type` keyword only creates type constructors, that is, aliases to other types with their own data constructors. You could refer to this value `("blah", 3)` has having type `Person`.


# Nesting tuples

You can also nest tuples. Given the product of `String` *and* `Int` *and* `Integer` *and* another `String` you could write that as:

```haskell
(String, (Int, (Integer, String)))
```

This is what makes the 2-tuple an anonymous universal product, that we can nest them.


# Exercises

Rewrite the following type into a nested two tuple:

```haskell
data Car = Car {
             make :: CarMake
           , model :: CarModel
           , year :: CarYear
           }
```

turns into:

```haskell
type Car = ???
```


# Exercises

Given the functions:

```haskell
fst :: (a, b) -> a
snd :: (a, b) -> b
```

Add the accessors back for your nested tuple type.

```haskell
make :: Car -> CarMake
make = undefined

model :: Car -> CarModel
model = undefined

year :: Car -> CarYear
year = undefined
```


# Sum type

The Bool datatype is defined as follows:

```haskell
data Bool = False | True
```

What we've done here is made it so two different data constructors are values of type `Bool`. Where `product` ~ *and*, `sum` ~ *or*.


# We have an anonymous sum type too

```haskell
data Either a b = Left a | Right b
```


# Gettin' silly

We could atomise `Bool` like so:

```haskell
data False' = False' deriving Show
data True' = True' deriving Show

type Bool' = Either False' True'
```


# Bonus

It'll even type-check that you're not messing the order up:

```
Prelude> Right False' :: Bool'

<interactive>:57:7:
    Couldn't match expected type ‘True'’
    with actual type ‘False'’
    In the first argument of ‘Right’,
    namely ‘False'’
    In the expression: Right False' :: Bool'
Prelude> Right True' :: Bool'
Right True'
```

# What can this function do?

Based on the type, what can this function do?

```haskell
boolInteger :: Bool -> Integer
```


# What can this function do?

What about `quantInteger`?

```haskell
data Quantum = True | False | Both

quantInteger :: Quantum -> Integer
```

# Making the "algebra" in algebraic data types do work

- There's an actual set of operations here.

```haskell
data Bool = False | True
```

- False = 1
- True = 1
- | = +
- Either also = +

```haskell
data Bool = False + True

data Bool = 1 + 1
Bool = 2 inhabitants
```


# Making the "algebra" in algebraic data types do work

```haskell
type Bool' = Either False' True'

Either = +

data False' = False'

False' = 1
True' = 1

type Bool' = Either 1 1
           = 1 + 1
-- same as ordinary Bool
-- we can say they are equivalent
```

# Making the "algebra" in algebraic data types do work

Knowing how big your domain is important for knowing how comprehensible it is, as well as knowing how it relates

(,) = *

```haskell
type DoesntMatter = (Bool, Bool)

type DoesntMatter = Bool * Bool

type DoesntMatter = 2 * 2

type DoesntMatter = 4 inhabitants
```


# Exercises

How many inhabitants does each type have?

```haskell
-- Word8 = 0-255

import Data.Word

type A = Either Word8 Bool

type B = (Word8, Bool)

type C = Either (Bool, Word8) (Word8, Bool)
```


# Don't do this

```haskell
data CarType = Null |
               Car { carid :: Int
                   , position :: Float
                   , speed :: Float,
                   , carLength :: Float
                   , state :: [Float]
                   } deriving (Show,Eq)
```


# Why?

Because it's redundant, obnoxious, and it introduces partial functions. Don't mix record syntax and sum types!

Partial what? Partial functions are functions that have inputs for which they don't have answers.

```
data Example = Null
             | Example {
               blah :: Int
               } deriving Show
Prelude> blah $ Example 10
10
Prelude> blah $ Null
*** Exception: No match in record selector blah
```


# Pls no.

`Maybe` exists, use it!

```haskell
data Maybe a = Nothing | Just a
```

If you have a function that might not be able to return a sensible `CarType`, return `Maybe CarType`!


# Strings are information Hotel California

- Until parsing, `String` value is effectively `forall a . a` - you don't know anything and you can't do anything with it.

- `String` values have indefinite length (they're `[Char]`) and a ridiculous number of possible inhabitants for each `Char` value.

- The only way you're escaping `String` is with a `Parser`.

- Use a `String` when you're just holding onto something for presentation purposes. Keep structured information out of the `String` value.


# Maps are not products

- `Map k v` doesn't tell you what to expect, only the types of the keys and values.

- If you know upfront what keys will be populated, make a product type.

- Maps are better used for sparsely inhabited collections of values referenced by keys not known until runtime.


# Lets model something

- Human language, in this case.

- This is an exercise.

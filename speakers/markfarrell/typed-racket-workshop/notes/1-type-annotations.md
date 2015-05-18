## Type Annotations

In this section, we will explore the basic features of Typed Racket's type system, type-annotating examples of previously untyped Racket code.

### Type Basic Value Definitions
  
   We'll start looking at some basic types in Typed Racket. First, let's type-annotate a definition for a person's     full name. 

   ```racket
   (define full-name "John Smith")
   ```
   
   We can type-annotate this definition by saying that `full-name` is a `String`.

   ```racket
   (: full-name String)
   (define full-name "John Smith")
   ```

   Exercise: type-annotate the age of the person. 
  
   ```racket
   (define age 28)
   ```

   You might say that ```age``` is an ```Integer```.
  
   ```racket
   (: age Integer)
   (define age 28)
   ```

   However, you could be more precise by saying that ```age``` is
   a ```Positive-Integer```.
  
   ```racket
   (: age Positive-Integer)
   (define age 28)
   ```
   
   There's a diagram of Type Racket's numeric type hierarchy in the paper titled [Typing the Numeric Tower](http://www.ccs.neu.edu/home/stamourv/papers/numeric-tower.pdf), which may be of use to you.
   
### Type Basic Function Definitions
  
  We'll now look at function types in Typed Racket. Let's type a function that takes the `gcd` of two integers.

  ```racket
  (define (gcd a b)
    (cond [(= b 0) a]
          [(gcd b (abs (modulo a b)))]))
  ```
  
  This function can be given the type `(-> Integer Integer Integer)` since it works for negative integers as well, e.g. `(gcd -3 4) => 1`.
  
  ```racket
  (: gcd (-> Integer Integer Integer))
  (define (gcd a b)
    (cond [(= b 0) a]
          [(gcd b (abs (modulo a b)))]))
  ```
  
  However, it is also possible to implement a `gcd` function using only substraction; in this case the function won't terminate if given a negative integer as one of its inputs. 
  
  ```racket
  (define (gcd m n)
    (cond [(< m n) 
           (gcd m (- n m))]
          [(> m n)
           (gcd (- m n) n)]
          [else m]))
  ```
  
  In untyped Racket, we could impose the constraint that this function has to be given non-negative integers as input at run-time with contracts. 
  
  ```racket
  (define/contract (gcd m n)
    (-> exact-nonnegative-integer?
        exact-nonnegative-integer?
        exact-nonnegative-integer?)
    (cond [(< m n) 
           (gcd m (- n m))]
          [(> m n)
           (gcd (- m n) n)]
          [else m]))
  ```
  
  In Typed Racket, we'd like to be able to impose this constraint before our programs that use this function run.    However, we are unable to guarantee with types before our program runs that our function will take two non-negative integers and give as a non-negative integer as a result: we have to cast the result of subtracting our two non-negative integers to a non-negative integer at run-time in order for our programs that use this function to type check. 
  
  
  ```racket
  (: gcd 
     (-> Exact-Nonnegative-Integer
         Exact-Nonnegative-Integer 
         Exact-Nonnegative-Integer))
  (define (gcd m n)
    (cond [(< m n) 
           (gcd m 
                (cast (- n m) Exact-Nonnegative-Integer))]
          [(> m n)
           (gcd (cast (- m n) Exact-Nonnegative-Integer) n)]
          [else m]))
  ```
  
  Exercise: type-annotate the definition of a function that computes the n-th fibonacci number.
  
  ```racket
  (define (fib n)
    (cond [(<= n 2) 1]
          [else (+ (fib (- n 1))
                   (fib (- n 2)))]))
  ```
  
  You could say that the type-signature of this function should be `(-> Integer Integer Integer)`. 
  
  ```racket
  (: fib 
     (-> Integer 
         Integer
         Integer))
    (define (fib n)
    (cond [(<= n 2) 1]
          [else (+ (fib (- n 1))
                   (fib (- n 2)))]))
   ```
   
   However, we want this function to take a positive integer and give us a positive integer: i.e. `(fib 1)` should give us the first fibonacci number. Though, we suffer from the same problem that we say with our `gcd` function: we have to cast the result of subtracting numbers at run-time to a positive integer in order for our programs that use `(fib n)` to type check. 
  
   ```racket
   (: fib 
     (-> Positive-Integer
         Positive-Integer))
   (define (fib n)
     (cond [(<= n 2) 1]
           [else (+ (fib (cast (- n 1) Positive-Integer))
                    (fib (cast (- n 2) Positive-Integer)))]))
   ```
  
### Record Types
  
  Now we'll look at record types in Typed Racket; record types are like product types where each field has a name.
  In Racket and Typed Racket, record types are called ```struct```s. Here's an example of a `struct` in untyped    Racket: a ```struct``` for student's, where each student has field for their name, age, faculty and term. Untyped Racket supports ```struct```s, though it only knows about these ```struct```s at run-time. 
  
  ```racket
  (struct student (id name age faculty term))
  ```
  ```
  > (student "00000999" "John Smith" 20 'mathematics '2A)
  #<student>
  ```
  
  Suppose that we'd want to impose the constraints that each student's:
  
  * `id` is an 8-digit string containing only numeric characters.
  * `name` is a ```String```.
  * `age` is a positive integer between 16 and 80.
  * `faculty` is either `Mathematics`, `Science`, `Engineering`, `Arts` or `Applied Health Sciences`.
  * `term` is either `1A`, `1B`, `2A`, `2B`, `3A`, `3B`, `4A` or `4B`. 
  
  We don't want to be able to create student instances that don't satisfy these constraints; 
  student instances who don't satisfy these constraints would be invalid.

  In untyped Racket, we could impose these constraints at run-time with contracts.
    
  In Typed Racket, we can impose some of these constraints at compile-time with
  types.
  
  However, it's tricky to impose the constraint that a student's `id` is a 8-digit string 
  containing only numeric characters at compile-time. It's also tricky to impose the 
  constraint that their age is a positive integer between 16 and 80. Though, 
  recall that in Typed Racket, all values are types: e.g. `16` is a subtype of `Positive-Integer`.
  We also have untagged union types in Type Racket: e.g. `(U Number String)`, meaning that 
  the inhabitants of this type can either be a `Number` or `String`, such as `3.0` or `"foo"`.
  
  We can quite easily certify that each student's term and faculty are valid at compile-time,
  using untagged union types and the fact that all values are types in Typed Racket: 
  
  ```racket
  (define-type Term (U '1A '1B '2A '2B '3A '3B '4A '4B))
  ```
    
  ```racket
  (define-type Faculty (U 'mathematics 'science 'engineering 'arts 'applied-health-sciences))
  ```
  
  However, it's trickier to certify that each student's `id` and `age` are meet our specification
  for valid `id`s and `ages` at compile-time; to do this, we'd have to enumerate all possible 
  valid `ids` and `ages`, storing these possibilities in a untagged union types that depend on 
  values like `Term` and `Faculty`. 
  
  For the sake of this example, let's simplify our specification for valid `id`s and `ages`:
  
  We'll say that a valid `age` is a positive integer between 16 and 25: 
  
  ```
  (define-type Age (U 16 17 18 19 20 21 22 23 24 25))
  ```
  
  We'll say that a valid `id` is a 1-digit string containing only numeric characters:
  
  ```
  (define-type Id (U "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  ```
  
  Then we can define our student record type in Typed Racket as follows, and impose these
  constraints at compile-time:
  
  ```racket
  (struct: Student 
           ([id : Id] 
            [name : String] 
            [age : Age]
            [faculty : Faculty]
            [term : Term]))
  ```
    
  ```racket
  > (Student "9" "John Smith" 20 'mathematics '2A)
  - : Student
  #<Student>
  ```
  
  This concludes are example of record types in Typed Racket. Now let's look at some more exercises.
  
  Exercise: represent the Peano numbers in Typed Racket using record types.
  
  Hint: you'll need to define three `struct`s - one for `Nat`, one for `Z` and one for successors `S` of some    natural number. You should also impose the constraint that `Z` and `S` are subtypes of `Nat`. If you happen to be familiar with Scala and Scala's type system, think about how you would represent the Peano numbers using case classes to answer this question. 
  
  ```racket
  (struct: Nat ())
  (struct: Z Nat ())
  (struct: S Nat ((n : Nat)))
  ```
  
  Subtyping gets in the way with type-inference in Typed Racket: you have to use ```ann``` to explicitly annotate 
  that certain naturals are indeed instances of `Nat` and not just instances of `Z` or `S` in Typed Racket. 
  
  For instance: 
  
  ```racket
  > (Z)
  - : Z
  #<Z>
  ```
  
  ```racket
  > (S (Z))
  - : S
  #<S>
  ```
  
  Might often have to explicitly annotated as `Nat`, using the `ann` form: 
  
  ```racket
  > (ann (Z) Nat)
  - : Nat
  #<Z>
  ```
  
  ```racket
  > (ann (S (Z)) Nat)
  - : Nat
  #<S>
  ```
  
  This concludes our exercise for defining the Peano numbers in Typed Racket using record types.
  
  Remark: if you were working in a programming language with algebraic data types, such as Haskell 
  or Idris, you might define a representation of the Peano numbers as follows: 
  
  ```haskell
  data Nat = Z | S Nat
  ```
  
  Typed Racket currently lacks support for algebraic data types, and only let's you pattern match 
  on the structure of types for control-flow at run-time, using ```match```. We might wish to 
  have algebraic data types and ensure that we are pattern-matching on their structure for control-flow
  at compile-time. We'll look at how we might add algebraic data types and a compile-time pattern matching
  construct to Typed Racket later on in this workshop. 
  
  So, you've had an exercise where you've had to use record types in Typed Racket to represent the Peano 
  numbers in Typed Racket. I simplified the specification for valid ```id```s and ```age```s of student's 
  in my previous example of record types in Typed Racket. I said that you'd have to enumerate
  all possible valid `id`s and `age`s, storing them in untagged union types that depend on values in 
  order to make types that allow to certain that the `id`s and `age`s of students are correct before 
  our programs run in Typed Racket. It's possible to write functions that enumerate all possible `id`s 
  and `age`s at run-time - you could even write programs to do this and then wrap the lists of results,
  pasting them into types for `Id` and `Age` in Dr. Racket.
  
  However, if Racket we have hygienic macros. Macro expansion takes place before type-checking occurs 
  in Typed Racket. We can e.g. create a `define-range-type` macro in Typed Racket that
  takes a type name, a lower-bound integer & an upper-bound integer as input, and then defines 
  an untagged union type with all integer values in our range as its inhabitants.
  
  Exercise: define such a macro. 
  
  Check that it your ```define-range-type``` macro correct defines our `Age` type correctly.
  
  ```racket
  (define-range-type Age 16 80)
  ```
  
  ```racket 
  => (define-type Age (U 16 ... 80))
  ```
  
 Challenge problem:

 We could also create a macro that takes a type name and an integer ```n```, defining
 an untagged union type with all possible ```n```-digit strings with number characters
 as its inhabitants. This exercise is left to the reader - its solution will 
 not be covered as part of this workshop.
 
### Parametric Polymorphism
 
  Typed Racket supports parametric polymorphism. You can define types (and record types) that
  take types as parameters to construct new types. For instance, you can define an ```Option```
  type, like you might have seen in Scala, that takes a type and gives you a type; option
  types allow you to e.g. create functions where you might get a result of a particular
  type if your input to the function is valid at run-time (```(Some a)```),  or get an empty result (`(None)`)
  otherwise.

  ```racket
  (struct: None ())
  (struct: (a) Some ([v : a]))
 
  (define-type (Option a) (U None (Some a)))
  ```
  
  Remark: you'll have to use ```ann``` in many cases, just like what we saw in our ```Nat```,
  to convince Typed Racket that instances of ```(None)``` or ```(Some a)``` are ```Option```s.
  
  Exercise: define a type for ```Either``` that takes two type parameters ```a b```, and is either a ```(Left a)```
  or a ```(Right b)```.
  
  ```racket
  (struct: (a) Left ([v : a]))
  (struct: (b) Right ([v : a]))
  
  (define-type (Either a b) (U (Left a) (Right b)))
  ```
  
  Remark: in programming languages that support algebraic data types, like Haskell or Idris, you might define 
  polymorphic ```Option``` and ```Either``` data types as follows: 
  
  ```haskell
  data Option a = None | Some a
  ```
  
  ```haskell
  data Either a b = Left a | Right b
  ```
  
  We could make it more convenient to define ```Option``` or ```Either``` data types in Typed Racket by adding  support for algebraic data types to Typed Racket. As mentioned, we'll look at how we might add algebraic data types to Typed Racket later on in this workshop.
  
### Recursive Type Constructors
  
  Typed Racket also supports recursive type constructors. This meanings that you can define 
  types that refer to themselves, e.g. a binary tree is either a leaf of some type or it 
  branches off into two binary trees.
  
  We can define a recursive polymorphic type constructor for a ```BinaryTree``` in Typed Racket as follows:

  ```racket 
  (define-type (BinaryTree A) (Rec BT (U A (Vector BT BT))))
  ```
  
  And, as another example, we can define a recursive polymorphic type constructor for a ```QuadTree``` in 
  Typed Racket as follows: 
  
  ```racket
  (define-type (QuadTree A) (Rec QT (U A (Vector QT QT QT QT))))
  ```
  
  Exercise: define a recursive polymorphic type constructor for an ```OctTree``` in Typed Racket.
  
  ```
  (define-type (OctTree A) (Rec QT (U A (Vector QT QT QT QT QT QT QT QT))))
  ```
  
  Exercise: we can also define a type for the Peano numbers recursively in Typed Racket; though you
  still need to define record types for ```Z``` and a polymorphic ```S``` because Typed Racket 
  doesn't really support tagged union types. Try this.
  
  ```racket
  (struct: Z ())
  (struct: (a) S ((n : a)))
  
  (define-type Nat (Rec Nat (U Z (S Nat))))
  ```

### Occurrence Types

```racket
(: foo 
   (-> (U String Any)
       (U Integer String)))
(define (foo str-or-any)
  (cond [(string? str-or-any) 
         (string-length str-or-any)]
        [else "error"]))
```

### Intersection Types
 
  ```racket
  > (:print-type random)
    (case->
      (->* (Positive-Fixnum) (Pseudo-Random-Generator) Nonnegative-Fixnum)
      (->* (Integer) (Pseudo-Random-Generator) Nonnegative-Integer)
      (->* () (Pseudo-Random-Generator) Flonum))
  ```

### End of Section Exercises

Here are a few end-of-section exercises for you to try: 

* Type a polymorphic `map` function.

  ```racket
  (define (map f xs)
    (cond [(empty? xs) xs]
          [else 
            (cons (f (first xs))
                  (rest xs))]))
   ```
   
* Type a `zip` function.

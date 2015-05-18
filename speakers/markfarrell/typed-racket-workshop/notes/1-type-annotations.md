## Type Annotations

In this section, we will explore the basic features of Typed Racket's type system, type-annotating examples of previously untyped Racket code.

* Type basic value definitions: e.g. the name of a person.
  
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
   
   There's a diagram of Type Racket's numeric type hierarchy in the paper titled [Typing the Number](http://www.ccs.neu.edu/home/stamourv/papers/numeric-tower.pdf), which may be of use to you.
   
* Type basic function definitions.
  
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
  
* Record types.
  
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
  
  ```racket
  (struct: Student 
           ([id : String] 
            [name : String] 
            [age : Positive-Integer]
            [faculty : Faculty]
            [term : Term]))
  ```
    
  ```racket
  > (Student "John Smith" 20 'mathematics '2A)
  - : Student
  #<Student>
  ```
  
  Exercise:
  
  ```racket
  (struct: Nat ())
  (struct: Z Nat ())
  (struct: S Nat ((n : Nat)))
  ```
  
* Parametric polymorphism.
  
  ```racket
  (struct: None ())
  (struct: (a) Some ([v : a]))
 
  (define-type (Opt a) (U None (Some a)))
  ```
  
  Exercise:
  
  ```racket
  (struct: (a) Left ([v : a]))
  (struct: (b) Right ([v : a]))
  
  (define-type (Either a b) (U (Left a) (Right b)))
  ```
  
* Recursive type constructors.

 ```racket
 (define-type IntList (Rec List (Pair Integer (U List Null))))
 ```
 
 ```racket
 (define-type (List A) (Rec List (Pair A (U List Null))))
 ```

  ```racket 
  (define-type (BinaryTree A) (Rec BT (U A (Vector BT BT))))
  ```
  
  ```racket
  (define-type (QuadTree A) (Rec QT (U A (Vector QT QT QT QT))))
  ```
  
  ```
  (define-type (OctTree A) (Rec QT (U A (Vector QT QT QT QT QT QT QT QT))))
  ```
  
  Exercise:
  
  ```racket
  (struct: Z ())
  (struct: (a) S ((n : a)))
  
  (define-type Nat (Rec Nat (U Z (S Nat))))
  ```
 
* Intersection Types.
 
  ```racket
  > (:print-type random)
    (case->
      (->* (Positive-Fixnum) (Pseudo-Random-Generator) Nonnegative-Fixnum)
      (->* (Integer) (Pseudo-Random-Generator) Nonnegative-Integer)
      (->* () (Pseudo-Random-Generator) Flonum))
  ```
  
* Occurrence Types.

Here are a few end-of-section exercises for you to try: 

* Type a `map` function.
  ```racket
  #lang racket
  
  (define (map f xs)
    (cond [(empty? xs) xs]
          [else 
            (cons (f (first xs))
                  (rest xs))]))
   ```
* Type a `zip` function.

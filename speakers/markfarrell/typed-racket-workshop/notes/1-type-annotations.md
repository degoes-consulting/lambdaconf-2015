## Type Annotations

In this section, we will explore the basic features of Typed Racket's type system, type-annotating examples of previously untyped Racket code.

* Type basic value definitions: e.g. the name of a person.

   ```racket
   (define full-name "John Smith")
   ```

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
  
  Exercise:
  
  ```racket
  (define (fib n)
    (cond [(<= n 2) 1]
          [else (+ (fib (- n 1))
                   (fib (- n 2)))]))
  ```
  
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
  
   ```racket
   (: fib 
     (-> Positive-Integer
         Positive-Integer))
   (define (fib n)
     (cond [(<= n 2) 1]
           [else (+ (fib (cast (- n 1) Positive-Integer))
                    (fib (cast (- n 2) Positive-Integer)))]))
   ```
  
* Define untagged union types.
  
  ```racket
  (define-type Term (U '1A '1B '2A '2B '3A '3B '4A '4B))
  ```
    
  ```racket
  (define-type Faculty (U 'mathematics 'science 'engineering 'arts 'applied-health-sciences))
  ```
    
* Type structs. 
  
  ```racket
  (struct student (name age faculty term))
  ```
  ```
  > (student "John Smith" 20 'mathematics '2A)
  #<student>
  ```
    
  ```racket
  (struct: Student ([name : String] [age : Integer] [faculty : Faculty] [term : Term]))
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

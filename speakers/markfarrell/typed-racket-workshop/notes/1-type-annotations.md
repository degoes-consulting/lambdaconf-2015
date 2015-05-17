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
* Type basic function definitions.

  ```racket
  (define (gcd a b)
    (cond [(= b 0) a]
          [(gcd b (abs (modulo a b)))]))
  ```
  
  ```racket
  (: gcd (-> Integer Integer Integer))
  (define (gcd a b)
    (cond [(= b 0) a]
          [(gcd b (abs (modulo a b)))]))
  ```
  
  ```racket
  (define (gcd m n)
    (cond [(< m n) 
           (gcd m (- n m))]
          [(> m n)
           (gcd (- m n) n)]
          [else m]))
  ```
  
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
  
  ```racket
  (define (fib n)
    (cond [(<= n 2) 1]
          [else (+ (fib (- n 1))
                   (fib (- n 2)))]))
  ```
  
  ```racket
  (: fib 
     (-> Exact-Nonnegative-Integer
         Exact-Nonnegative-Integer))
  (define (fib n)
    (cond [(<= n 2) 1]
          [else (+ (fib (cast (- n 1) Exact-Nonnegative-Integer))
                   (fib (cast (- n 2) Exact-Nonnegative-Integer)))]))
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

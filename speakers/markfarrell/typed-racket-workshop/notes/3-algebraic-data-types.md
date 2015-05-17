* Adding algebraic data types to Typed Racket:

   * Create `define-datatype` macro.
   * Create `type-case` macro.

* Algebraic data type examples:
 
  ```racket 
  (require datatype)
  ```
 
  ```racket
  (define-datatype Nat
    [S (Nat)]
    [Z ()])
  ```
  
  ```racket
  (: Zero Nat)
  (define Zero (Z))
  ```
 
  ```racket
  (: One Nat)
  (define One (S (Z)))
  ```
 
  ```
  (: Two Nat)
  (define Two (S (S (Z))))
  ```

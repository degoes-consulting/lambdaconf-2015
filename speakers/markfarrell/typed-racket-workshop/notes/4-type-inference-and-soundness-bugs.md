## Soundness Bugs

There are currently soundness bugs in Typed Racket, where 
the compile-time type of a term differs from its run-time type.

* Example: [call/cc + letrec + occurrence typing can be unsound](https://github.com/racket/typed-racket/issues/128)

  ```racket
  (define-type klk [(List klk Boolean) -> Nothing])
  
  (: foo : [-> False])
  (define (foo)
    (letrec ([x (let/cc k : (List klk Boolean)
                (list k #f))])
    (if (false? (second x))
        (begin
          (let/cc k : (List klk Boolean)
            ((first x) (list k #t)))
          (second x))
        ((first x) x))))
  ```
  
  ```
  > (foo)
  #t
  ```

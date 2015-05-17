## No Type-Annotations Needed

As mentioned, Typed Racket aims to allow you to write programs in a style similar to how you would write programs in untyped Racket. Several of the example programs on the Racket home page type-check in Typed Racket without modification.

* Program 1:

  ```racket
  #lang racket
  ;; Finds Racket sources in all subdirs
  (for ([path (in-directory)]
       #:when (regexp-match? #rx"[.]rkt$" path))
    (printf "source file: ~a\n" path))
  ```
* Program 2:

  ```racket
  #lang racket
  
  (define listener (tcp-listen 12345))
  
  (let echo-server ()
    (define-values (in out) (tcp-accept listener))
    (thread (lambda () (copy-port in out)
                       (close-output-port out)))
    (echo-server))
  ```

  ```racket
  #lang typed/racket

  (define listener (tcp-listen 12345))
  
  (let echo-server ()
    (define-values (in out) (tcp-accept listener))
    (thread (lambda () (copy-port in out)
                       (close-output-port out)))
    (echo-server))
  ```
  
* Program 3:
  
  ```racket
  #lang racket

  ;; Report each unique line from stdin
  (define seen (make-hash empty))
  
  (for ([line (in-lines)])
    (unless (hash-ref seen line #f)
    (displayln line))
  (hash-set! seen line #t))
  ```

  ```racket
  #lang typed/racket

  ;; Report each unique line from stdin
  (define seen (make-hash empty))
  
  (for ([line (in-lines)])
    (unless (hash-ref seen line #f)
    (displayln line))
  (hash-set! seen line #t))
  ```

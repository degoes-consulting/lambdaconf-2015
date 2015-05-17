## Adding Algebraic Data Types to Typed Racket

* Create `define-datatype` macro: 

```racket
(define-syntax (define-datatype stx)
  (syntax-parse stx
    [(_ class-id:id [variants:id fieldss] ...)
     ;; convert class-id to symbol
     (define class-symbol (syntax->datum #'class-id))
     ;; grab variant identifiers and related symbol
     (define var-ids (syntax->list #'(variants ...)))
     (define var-symbols (map syntax->datum var-ids))
     (define fields-list (map syntax->list (syntax->list #'(fieldss ...))))
     
     ;; verify identifiers are correct
     (validate-identifiers #'class-id class-symbol var-ids var-symbols)
     ;; build guard for parent type
     (define class-guard (build-class-guard class-symbol var-symbols))
     
     ;; build parent struct definition
     (define class-struct-def #`(struct: class-id ()
                                  #:transparent
                                  #:guard #,class-guard))
     ;; build defs for each variant's struct
     (define variant-defs
       (build-varient-defs var-ids var-symbols fields-list #'class-id))

     ;; build type-info-hash definition
     (define class-info-hash-def
       (build-class-info-hash-def var-ids var-symbols fields-list))

     ;; define specialize name for this class's type-case
     (define type-case-id (extend-id #'class-id "-specific-ADT-type-case"))

     ;; define specialized type-case constructor
     (define specialized-type-case-def
       (build-specialized-type-case-def
        type-case-id
        #'class-id
        class-info-hash-def))
     
     (with-syntax ([(var-defs ...)
                    variant-defs])
       #`(begin
           #,class-struct-def
           var-defs ...
           #,specialized-type-case-def))]))
```
* Create `type-case` macro:

```racket
(define-syntax (type-case stx)
  (syntax-parse stx
    [(_ type-stx:id arg-stx cases ...)
     (define id (extend-id #'type-stx "-specific-ADT-type-case"))
     (with-syntax ([spec-case (datum->syntax stx id)])
       #`(spec-case #,stx arg-stx cases ...))]))
```

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
  
  Exercise:
  
  ```racket
  (define-datatype (Opt a)
    [Some (a)]
    [None ()])
  ```
  
  Exercise:
  
  ```racket
  (define-datatype (Either a b)
    [Left (a)]
    [Right (b)])
  ```

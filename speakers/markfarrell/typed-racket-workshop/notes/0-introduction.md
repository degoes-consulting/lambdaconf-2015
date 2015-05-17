## Introduction

Today we're going to be talking to about Typed Racket. Typed Racket adds an optional static type system to Racket, formerly known as PLT Scheme. Unlike untyped Racket, Typed Racket can allow you to ensure that certain aspects of your programs' behavior are correct before they run.

Typed Racket is designed to allow you to write programs in a style similar to how you would write programs in untyped Racket. The idea is that you should be able to modify your previously untyped Racket programs as little as possible, and use them within Typed Racket wherever possible, having Typed Racket infer types wherever it can. Typed Racket achieves this by equipping its type system with a number of features, notably: 

 * Occurrence typing: derive types from predicate tests inside the body of a definition.
 * Subtyping: e.g. ```Any``` is the top type, ```Nothing``` is the bottom type and ```Real``` is a subtype of ```Number```.
 * All values are types: e.g. ```3``` is a subtype of ```Integer```.
 * Untagged union types: e.g. ```(U String Number)```.
 * Intersection types: e.g. define functions that when given a ```Number``` produces a ```Boolean```
   and when given a ```String``` produces an ```Integer```.
 
We'll have a closer look at these particular features of Typed Racket's type system later on in this workshop - and look at how previously untyped Racket code can often type check with minimal modification.

Let's first get you using Typed Racket. We'll look at some more familiar features of Typed Racket's type system, having you add type annotations to examples of previously untyped Racket code.

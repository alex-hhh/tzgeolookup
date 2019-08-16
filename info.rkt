#lang info
(define collection "tzgeolookup")
(define deps '("base" "math-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/tzgeolookup.scrbl" ())))
(define pkg-desc "Find the timezone for a location based on GPS coordinates")
(define version "0.0")
(define pkg-authors '(aharsanyi))

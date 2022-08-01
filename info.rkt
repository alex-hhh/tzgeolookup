#lang info
(define collection "tzgeolookup")
(define license 'LGPL-3.0-or-later)
(define pkg-desc "Find the timezone for a location based on GPS coordinates")
(define version "0.0")
(define pkg-authors '(AlexHarsanyi@gmail.com>))

(define scribblings '(("scribblings/tzgeolookup.scrbl" ())))

(define deps '("base" "math-lib" "db-lib" "geoid"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

;; These files are for development only and depend on other packages...
(define compile-omit-paths '("tools/"))
(define test-omit-paths '("tools/"))

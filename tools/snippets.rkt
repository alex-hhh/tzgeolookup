#lang racket
(require tzgeolookup geoid)

(define (test-all)
  (define test-cases (call-with-input-file "../private/test-cases.rktd" read))
  (printf "~%*** Asia/Kathmandu and Asia/Urumqi conflict with Asia/Shanghai~%~%")
  (for ([entry (in-list test-cases)])
    (match-define (list lat lon tzname) entry)
    (define result (lookup-timezone* lat lon))
    (cond
      ((not (member tzname result))
       (printf "Bad result for ~a: ~a, geoid-24: 0x~x~%"
               entry result (let ([g (lat-lng->geoid lat lon)])
                              (enclosing-geoid g 24))))
      ((> (length result) 1)
       (printf "Needs refinement for ~a, got ~a, geoid-24: 0x~x~%"
               entry result (let ([g (lat-lng->geoid lat lon)])
                              (enclosing-geoid g 24))))
      (#t
       #;(printf "Good ~a~%" entry)))))

(define (test-all-1)
  (define test-cases (call-with-input-file "./test-cases.rktd" read))
  (for ([entry (in-list test-cases)])
    (match-define (list lat lon tzname) entry)
    (define result (lookup-timezone lat lon))
    (if (equal? result tzname)
        (printf "Good ~a~%" entry)
        (printf "Bad result for ~a: ~a, geoid-24: 0x~x~%"
                entry result (let ([g (lat-lng->geoid lat lon)])
                               (enclosing-geoid g 24))))))

(define (print-needs-refinement)
  (define test-cases (call-with-input-file "./test-cases.rktd" read))
  (define need-refinement
    (for/set ([entry (in-list test-cases)])
      (match-define (list lat lon tzname) entry)
      (define result (lookup-timezone* lat lon))
      (cond
        ((not (member tzname result))
         ;; Skip for now
         (void))
        ((> (length result) 1)
         (let ([g (lat-lng->geoid lat lon)])
           (enclosing-geoid g 24)))
        (#t
         (void)))))
  (for ([x (in-set need-refinement)] #:unless (void? x))
    (printf "0x~x~%" x))
  (printf "total ~a of ~a~%" (set-count need-refinement) (length test-cases)))

#lang racket/base

;; SPDX-License-Identifier: LGPL-3.0-or-later
;; tzgeolookup.rkt -- lookup timezone based on GPS coordiantes
;;
;; Copyright (c) 2022 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require db
         file/gunzip
         geoid
         racket/math
         racket/port
         racket/runtime-path
         racket/contract)

(define-runtime-path db-file "./tzgeolookup.db")

;; Database connection for the timezone database, opened on demand, the first
;; time it is needed.
(define db #f)

;; Leaf-span for level-24 geoids, kept in memory for faster lookup, this data
;; comes from the LEAF_SPAN database table.
(define leaf-span-cache (make-hash))

;; Hash table mapping timezone ids to time zone names.  Keeps in-memory the
;; data from the TZNAME database table.
(define tzid-cache (make-hash))

;; Clear the features cache to save memory.  They will be automatically
;; reloaded as needed when `lookup-timezone` is called.
(define (clear-timezone-cache)
  (when db
    (disconnect db)
    (set! db #f))
  (hash-clear! leaf-span-cache)
  (hash-clear! tzid-cache))

(define (maybe-open-database)
  (unless db
    (set! db (sqlite3-connect #:database "./tzgeolookup.db" #:mode 'read-only))))

;; Fetch the compressed leaf span data for for the level 24 geoid, GEOID-24
(define (lookup-in-db geoid-24)
  (maybe-open-database)
  (query-maybe-value
   db "select data from leaf_span where geoid = ?"
   (geoid->sqlite-integer geoid-24)))

;; Fetch the time zone name corresponding to the time zone ID from the
;; database.
(define (lookup-timezone-name id)
  (maybe-open-database)
  (query-maybe-value db "select name from tzname where id = ?" id))

;; Return #t if the geoid G is contained in the geoid LEAF-SPAN.  A leaf span
;; is an ordered list of geoid ranges (cons cells) identified by a START and
;; END geoid.  Note that the range is half-closed, that is, the END value is
;; not part of the range.
(define (leaf-span-member? g leaf-span)
  (let loop ([leaf-span leaf-span])
    (if (null? leaf-span)
        #f                              ; not a member
        (let ([span (car leaf-span)])
          (cond ((< g (car span)) #f)    ; not a member
                ((< g (cdr span)) #t)    ; a member
                (#t (loop (cdr leaf-span))))))))

;; Search the LEAF span DATA for GEOID and return the list of timezone IDs
;; which contain this geoid.  The data is a list of time-zone IDs and their
;; corresponding leaf spans.
(define (search-leaf-span-data data geoid)
  (for/fold ([result '()])
            ([d (in-list data)])
    (define tzid (car d))
    (define leaf-span (cdr d))
    (if (leaf-span-member? geoid leaf-span)
        (cons tzid result)
        result)))

;; Convert the list of timezone ids TZIDS into a list of names.
(define (name-tzids tzids)
  (for/list ([id (in-list tzids)])
    (define name (hash-ref tzid-cache id #f))
    (unless name
      (set! name (lookup-timezone-name id))
      (hash-set! tzid-cache id name))
    name))

;; Unpack a leaf span from the byte BUFFER starting at position START.  The
;; encoded binary format is 2 bytes for the time zone id, 2 bytes for the
;; number of pairs in the leaf span followed by that number of 64 bit geoid
;; pairs.  All encoding is big endian.
(define (unpack-leaf-span buffer [start 0])
  (define tzid (integer-bytes->integer buffer #f #t start (+ start 2)))
  (define count (integer-bytes->integer buffer #f #t (+ start 2) (+ start 4)))
  (let loop ([index (+ start 4)]
             [remaining count]
             [span '()])
    (if (> remaining 0)
        (loop (+ index 16)
              (sub1 remaining)
              (cons (cons (integer-bytes->integer buffer #f #t index (+ index 8))
                          (integer-bytes->integer buffer #f #t (+ index 8) (+ index 16)))
                    span))
        (values (cons tzid (reverse span)) index))))

;; Decode multiple leaf spans from the IDATA byte buffer
(define (decode-leaf-span-data idata)
  (define limit (bytes-length idata))
  (let loop ([spans '()]
             [index 0])
    (if (< index limit)
        (let-values ([(span new-index) (unpack-leaf-span idata index)])
          (loop (cons span spans) new-index))
        (reverse spans))))

;; Determine the nautical time zone of the point at LAT, LON.  This is used
;; when no time zone boundaries are found and the time zone is measured as 1
;; hour for every 15 degrees of longitude.
(define (nautical-timezone lat lon)
  (if (or (= lat 90.0) (= lat -90.0))
      "Etc/GMT"       ; north and south poles are GMT, regardless of longitude
      (let ((hour (exact-round (- 12 (/ (+ lon 180.0) 15.0)))))
        (cond ((> hour 0) (format "Etc/GMT+~a" hour))
              ((< hour 0) (format "Etc/GMT-~a" (abs hour)))
              (else "Etc/GMT")))))

;; Look up time zones for LAT LON.  Can return a list of timezones if the time
;; zones overlap or the time zone data is imprecise (which it is along the
;; borders).  The actual time zone for LAT LON will always be in the returned
;; list, but the list might contain other time zones as well.
(define (lookup-timezone* lat lon)
  (define geoid (lat-lng->geoid lat lon))
  (define g24 (enclosing-geoid geoid 24))
  (cond
    ((hash-ref leaf-span-cache g24 #f)  ; look it up in the cache first
     => (lambda (data)
          (if (null? data)
              (list (nautical-timezone lat lon))
              (let ([tzids (search-leaf-span-data data geoid)])
                (if (null? tzids)
                    (list (nautical-timezone lat lon))
                    (name-tzids tzids))))))
    ((lookup-in-db g24)
     => (lambda (zdata)
          (define data
            (call-with-input-bytes
             zdata
             (lambda (in)
               (call-with-output-bytes
                (lambda (out)
                  (gunzip-through-ports in out))))))
          (define idata (decode-leaf-span-data data))
          (hash-set! leaf-span-cache g24 idata)
          (define result (search-leaf-span-data idata geoid))
          (if (null? result)
              (list (nautical-timezone lat lon))
              (name-tzids result))))
    ((nautical-timezone lat lon)
     => list))) ; if all else fails, it is a nautical time zone

;; Lookup and return a single time zone for the position at LAT LON.  When
;; multiple time zones are present, we attempt to resolve known conflicts,
;; otherwise, we sort the timezones and return the first one.
(define (lookup-timezone lat lon)
  (define candidates (lookup-timezone* lat lon))
  (if (= (length candidates) 1)
      (car candidates)
      (let ([s (sort candidates string<?)])
        (cond ((and (equal? (car s) "Asia/Kathmandu")
                    (equal? (cadr s) "Asia/Shanghai"))
               "Asia/Kathmandu")
              ((and (equal? (car s) "Asia/Shanghai")
                    (equal? (cadr s) "Asia/Urumqi"))
               "Asia/Urumqi")
              ((and (equal? (car s) "Asia/Bishkek")
                    (equal? (cadr s) "Asia/Dushanbe"))
               "Asia/Dushanbe")
              (#t
               ;; Just pick the first one...
               (car candidates))))))


;;............................................................. provides ....

(provide/contract
 (lookup-timezone (-> real? real? string?))
 (lookup-timezone* (-> real? real? (listof string?)))
 (clear-timezone-cache (-> void?)))


;;.............................................................. Testing ....

(module+ test
  (require rackunit racket/match)
  (define-runtime-path test-data-file "./test-cases.rktd")
  (define test-data (call-with-input-file test-data-file read))
  (define nitems (length test-data))
  (for ([(test-case num) (in-indexed (in-list test-data))])
    (match-define (list lat lon tzname) test-case)
    (printf "~a/~a checking ~a ~a ~a..." (add1 num) nitems lat lon tzname)(flush-output)
    (check-equal? (lookup-timezone lat lon) tzname)
    (printf "done.~%"))
  (clear-timezone-cache))

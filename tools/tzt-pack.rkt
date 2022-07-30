#lang racket/base

;; SPDX-License-Identifier: LGPL-3.0-or-later
;; tzt-pack.rkt -- pack time zone tiling data into the database
;;
;; This file is part of tzgeolookup -- find the timezone for a location based on GPS coordinates
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

(require geoid
         db
         json
         file/gunzip
         file/gzip
         racket/format
         racket/port
         racket/string
         racket/match)

;; Create the database tables to hold the data.  We use SQLite as a convenient
;; binary format which allows us to look up quickly the leaf spans for a
;; specific level-14 geoid, but the leaf spans themselves are compressed blobs
;; which we manage ourselves.
(define (create-tz-tables db)
  (query-exec
   db
   "CREATE TABLE tzname(
       id INTEGER PRIMARY KEY,
       name TEXT NOT NULL)")
  (query-exec
   db
   "CREATE TABLE leaf_span(
       geoid INTEGER PRIMARY KEY,
       data BLOB NOT NULL)"))

;; Read geoids from a file and return it as a list.  This is in the same
;; format as written by the tzt-coordinator.py:
;;
;; * one item per line
;; * first item is the number of geoids in the file
;; * following items are the geoids as numeric values
(define (read-geoid-list file-name)
  (call-with-input-file
    file-name
    (lambda (in)
      (define count-str (read-line in))
      (when (eof-object? count-str)
        (error "bad file: unexpected EOF"))
      (define count (string->number count-str))
      (unless count
        (error "bad file: expecting geoid count"))
      (let loop ([n count]
                 [geoids '()])
        (if (> n 0)
            (let ([geoid-str (read-line in)])
              (when (eof-object? count-str)
                (error "bad file: expectin a geoid"))
              (define geoid (string->number geoid-str))
              (unless geoid
                (error "bad file: expecting a geoid"))
              (loop (sub1 n) (cons geoid geoids)))
            (let ([test (read-byte in)])
              (unless (eof-object? test)
                (error "bad file: additional data after reading geoids"))
              (reverse geoids)))))))

;; Create a suitable file name from the name of a timezone -- this is in the
;; same format as used by the tzt-coordinator.py: The "/" in time zone names
;; is replaced by a "+".
(define (sanitize time-zone-name)
  (string-replace time-zone-name "/" "+"))

;; Run an SQL INSERT statement against a database connection and return the id
;; (primary key) of the item just inserted.
(define (db-insert connection statement . args)
  (define result (apply query connection statement args))
  ;; This is a problem if STATEMENT is not an insert statement
  (unless (simple-result? result)
    (error "db-insert: expecting simple-result for query"))

  (cond ((assoc 'insert-id (simple-result-info result)) => cdr)
        (#t #f)))

;; Fetch the leaf span blob, if any, for the specified GEOID -- the blob is
;; decompressed and returned as a byte array -- this is used to append a new
;; leaf span to it.
(define (db-fetch-leaf-span-data db geoid)
  (define data (query-maybe-value db "SELECT DATA FROM leaf_span WHERE geoid = ?" geoid))
  (if data
      (call-with-input-bytes
        data
        (lambda (in)
          (call-with-output-bytes
            (lambda (out)
              (gunzip-through-ports in out)))))
      #f))

;; Compress a byte array using gzip utilities from the Racket library and
;; return the result as a new byte array.
(define (compress data)
  (call-with-input-bytes
   data
   (lambda (in)
     (call-with-output-bytes
      (lambda (out)
        (gzip-through-ports in out #f 0))))))

;; Store serialized leaf-span DATA for GEOID into the database -- if there is
;; already a data chunk for this geoid in the database, we append ours.
(define (db-put-leaf-span-data db geoid data)
  (define existing (db-fetch-leaf-span-data db geoid))
  (if existing
      (let ([zdata (compress (bytes-append existing data))])
        (query-exec db "UPDATE leaf_span SET data=? WHERE geoid = ?" zdata geoid))
      (let ([zdata (compress data)])
        (query-exec db "INSERT INTO leaf_span(geoid, data) VALUES (?, ?)" geoid zdata))))

;; Pack the leaf-span SPAN for the timezone-id TZID into a binary byte array.
;; The data format is: 2 bytes for the time zone id, 2 bytes for the number of
;; items in the leaf span, than two 64bit values (Start and end) for each leaf
;; span entry.
(define (pack-leaf-span span tzid)
  ;; 2 geoids for each span entry, a geoid is 8 bytes (64 bits), header is the
  ;; tzid (2 bytes) and the number of entries in the leaf span (2 bytes)
  (define buffer (make-bytes (+ 4 (* 16 (length span)))))
  (integer->integer-bytes tzid 2 #f #t buffer 0)
  (integer->integer-bytes (length span) 2 #f #t buffer 2)
  (let loop ([index 4]
             [span span])
    (unless (null? span)
      (match-define (cons start end) (car span))
      (integer->integer-bytes start 8 #f #t buffer index)
      (integer->integer-bytes end 8 #f #t buffer (+ index 8))
      (loop (+ index 16) (cdr span))))
  buffer)

;; Import timezone data for TIME-ZONE-NAME, looking for geoids in BASE-DIR --
;; BASE-DIR is supposed to contain tiling output from tzt-coordinator.py and
;; tzt-worker.rkt.
(define (import-tzdata db time-zone-name base-dir)
  (call-with-transaction
   db
   (lambda ()
     (define tz-base-dir (format "~a/~a" base-dir (sanitize time-zone-name)))
     (define tzid (db-insert db "INSERT INTO tzname(name) VALUES(?)" time-zone-name))
     (define contains (read-geoid-list (format "~a/contains.dat" tz-base-dir)))
     (for ([g (in-list contains)])
       (define data (pack-leaf-span (leaf-span* (list g)) tzid))
       (db-put-leaf-span-data db (geoid->sqlite-integer g) data))
     (define intersecs (read-geoid-list (format "~a/intersects.dat" tz-base-dir)))
     (for ([g (in-list intersecs)])
       (with-handlers
         ((exn:fail? (lambda (e) (printf "error: ~a~%" e))))
         ;; NOTE: some geoids are not present...
         (define c (read-geoid-list (format "~a/~a.dat" tz-base-dir g)))
         (define data (pack-leaf-span (leaf-span* c) tzid))
         (db-put-leaf-span-data db (geoid->sqlite-integer g) data))))))

;; Create the timezone lookup database by importing geoids for TIME-ZONE-NAMES
;; from the BASE-DIR.  BASE-DIR is supposed to contain tiling output from
;; tzt-coordinator.py and tzt-worker.rkt.
(define (pack-tzdata database-file time-zone-names base-dir)
  (define db (sqlite3-connect #:database database-file #:mode 'create #:use-place #f))
  (query-exec db "pragma foreign_keys = on")
  (query-exec db "pragma cache_size = 8000")
  (create-tz-tables db)
  (for ([tz (in-list time-zone-names)]
        #:unless (regexp-match #rx"^(?i:Etc)/" tz))
    (printf "Importing tiling data for ~a..." tz)
    (define start (current-inexact-monotonic-milliseconds))
    (import-tzdata db tz base-dir)
    (define duration (- (current-inexact-monotonic-milliseconds) start))
    (printf "done (~a seconds).~%" (~r (/ duration 1000.0) #:precision 2)))
  (query-exec db "pragma secure_delete = true")
  (query-exec db "vacuum")
  (disconnect db))

(module+ main
  (require racket/cmdline)

  (define time-zone-names-file (make-parameter "./timezone-names.json"))
  (define tzdata-directory (make-parameter "./tzdata"))
  (define database-file (make-parameter "./tzgeolookup.db"))

  (command-line
   #:program "tzt-pack"
   #:once-each
   [("-n" "--time-zone-names")
    tznames
    "The JSON file containing time zone names (default ./timezone-names.json)"
    (time-zone-names-file tznames)]
   [("-t" "--tzdata-dir")
    tzdata
    "Tiling data directory (default ./tzdata)"
    (tzdata-directory tzdata)]
   [("-d" "--dbfile")
    dbfile
    "The name of the output database file"
    (database-file dbfile)])

   (unless (file-exists? (time-zone-names-file))
     (error (format "~a does not exist or it is not a file" (time-zone-names-file))))

   (define time-zone-names
     (with-handlers
       ((exn? (lambda (e)
                (error (format "failed to read time zone name from ~a" (time-zone-names-file))))))
       (call-with-input-file (time-zone-names-file) read-json)))

   (unless (directory-exists? (tzdata-directory))
     (error (format "~a does not exist or it is not a directory" (tzdata-directory))))

   (when (file-exists? (database-file))
     (error (format "database file ~a already exists, remove it first" (database-file))))

   (pack-tzdata (database-file) time-zone-names (tzdata-directory)))

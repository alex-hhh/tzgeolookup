#lang racket/base

;; SPDX-License-Identifier: LGPL-3.0-or-later
;; tzt-worker.rkt -- worker program for time zone tiling
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

(require geoid/tiling
         json
         racket/match
         racket/math
         racket/os
         racket/place
         racket/port
         racket/tcp
         racket/format)

;; Application key has to match the one in tzt-coordinator.py -- this is not
;; security, but a simple way to ensure outdated workers don't submit results
;; back to the coordinator.  When incompatible changes are made to the
;; coordinator, the application key should change.
(define app-key "tzt-8")

;; Construct a polygon from the POINTS coming from a GeoJSON feature.  The
;; polygon will have the expected WINDING ('cw or 'ccw).
(define (make-polygon points winding)
  (define track
    (for/list ([p (in-list points)])
      (match-define (list x y) p)
      ;; Note: GeoJSON uses "Easting (x), Norhting (y)", we use "Latitude
      ;; (y)/Longitude (x)", so the vector arguments are swapped.
      (vector y x)))
  (make-closed-polyline track #:ccw? (eq? winding 'ccw)))

;; Construct a polygon from the POINTS coming from a GeoJSON feature, guessing
;; the winding order.
(define (make-polygon/guess points)
  (define track
    (for/list ([p (in-list points)])
      (match-define (list x y) p)
      ;; Note: GeoJSON uses "Easting (x), Norhting (y)", we use "Latitude
      ;; (y)/Longitude (x)", so the vector arguments are swapped.
      (vector y x)))
  (define winding (guess-winding-order track))
  (values (make-closed-polyline track #:ccw? (eq? winding 'ccw)) winding))

;; Construct a region from the GEOJSON-SHAPE -- the shape is a list of
;; polygons, the first one being the outer one, the remaining being the holes.
;; Geo-JSON polygons for holes have opposite winding order from the outer
;; polygon, however, we make use of the geoid tiling property that a reverse
;; winding order represents the opposite region, so we keep the winding order
;; and intersect the holes with the outer region (if we used the "correct"
;; winding order for the holes, we would have to subtract the holes from the
;; outer region).
(define (make-shape geojson-shape)
  (define-values (first-region winding)
    (make-polygon/guess (car geojson-shape)))
  (define other-regions (map (lambda (p) (make-polygon p winding)) (cdr geojson-shape)))
  ;; NOTE: a region with holes is defined such that holes are defined
  ;; clockwise, and therefore they define the "outer" part of the hole, so the
  ;; complete shape is the intersection between the main region and the hole
  ;; (not the subtraction).
  (apply intersect-regions first-region other-regions))

;; Construct a region from the geojson-feature, which is either a polygon or a
;; multi-polygon.
(define (make-feature geojson-feature)
  #;(define name
    (let ([properties (hash-ref geojson-feature 'properties)])
      (hash-ref properties 'tzid #f)))
  (define shapes
    (let ([geometry (hash-ref geojson-feature 'geometry (lambda () (hash)))])
      (let ([shapes (hash-ref geometry 'coordinates null)]
            [type (hash-ref geometry 'type #f)])
        (cond ((equal? type "Polygon")
               ;; NOTE: what GeoJSON calls a polygon corresponds to a SHAPE in
               ;; our data structures.
               (make-shape shapes))
              ((equal? type "MultiPolygon")
               (define sh (map make-shape shapes))
               (apply join-regions sh))
              (#t
               (error (format "make-feature: unsupported geometry type: ~a" type)))))))
  shapes)

;; Send REQUEST to HOST at PORT and read a reply (we expect to talk to a
;; tzt-coordinator.py server).  REQUEST must be serializable to JSON using
;; `write-json`
;;
;; NOTE: the server is set up such that it can only handle one message per
;; connection.
(define (request/reply request host port)

  (define retry-count 20)

  (define request-data
    (call-with-output-bytes (lambda (o) (write-json request o))))
  (define request-length (bytes-length request-data))
  (define request-header (integer->integer-bytes request-length 4 #f #t))

  ;; Connection to a busy coordinator might fail, since the coordinator
  ;; processes requests sequentially.  Don't give up easily, retry a few times.
  (let/ec return
    (let loop ([remaining retry-count])
      (with-handlers* ; use * variant so the loop is called in tail position

        ((exn:fail?
          (lambda (e)
            (printf "*** request/reply: ~a~%" e)
            (if (> remaining 0)
                (begin
                  (sleep (+ 5 (random 5))) ; wait a while...
                  (printf "*** request/reply: retry ~a of ~a~%" (- retry-count remaining) retry-count)
                  (loop (sub1 remaining)))
                (begin
                  (printf "*** request/reply: abandoning request" (- retry-count remaining) retry-count)
                  (return #f))))))

        (define-values (in out) (tcp-connect host port))

        (write-bytes request-header out)
        (write-bytes request-data out)
        (flush-output out)

        (define header (read-bytes 4 in))
        (if (eof-object? header)
            (error "eof while reading reply header")
            (let* ([msize (integer-bytes->integer header #f #t)]
                   [data (read-bytes msize in)])
              (begin0
                  (call-with-input-bytes data read-json)
                (close-input-port in)
                (close-output-port out))))))))

;; Request a new task from the tzt-coordinator running at HOST and PORT.
;; CLIENT-ID identifies ourselves and is used for logging and tracking
;; purposes by the tzt-coordinator
(define (request-task client-id host port)
  (request/reply
   (hash 'key app-key
         'client_id client-id
         'operation "request_task")
   host port))

;; Request Geo-JSON data for the region NAME from the tzt-coordinator running
;; at HOST and PORT.  CLIENT-ID identifies ourselves and is used for logging
;; and tracking purposes by the tzt-coordinator
(define (request-region name client-id host port)
  (define region-data
    (request/reply (hash 'key app-key
                         'client-id client-id
                         'operation "request_tzdata"
                         'tzname name)
                   host port))
  (and region-data (make-feature region-data)))

;; Send a reply to the tzt-coordinator for a coarse tiling task for TZNAME.
;; CONTAINS and INTERSECTS are lists of geoids.
(define (complete-coarse-tiling tzname contains intersects client-id host port)
  (request/reply (hash 'key app-key
                       'client_id client-id
                       'operation "coarse_tiling"
                       'tzname tzname
                       'contains (map ~a contains)
                       'intersects (map ~a intersects))
                 host port))

;; Send a reply to the tzt-coordinator for a "refine tiling" tasks for TZNAME.
;; GEOID is the geoid we refined and REFINEMENT is a list of geoids which
;; contain or intersect the region TZNAME inside GEOID.
(define (complete-refinement-tiling tzname geoid refinement client-id host port)
  (request/reply (hash 'key app-key
                       'client_id client-id
                       'operation "refine_tiling"
                       'tzname tzname
                       'geoid (~a geoid)
                       'refinement (map ~a refinement))
                 host port))

;; This is a worker function started as part of a Racket place.  It will
;; connect to the tzt-coordinator and request tasks and send back replies
;; until it is told by the coordinator that there are no more tasks. I/O is a
;; place channel on which log messages are put -- the main application will
;; read and print these messages to avoid mangling log messages from different
;; places.
(define (worker i/o)
  (define cached-regions (make-hash))
  ;; The worker-id is the only info we get on our place channel.  We will send
  ;; log messages to this channel.
  (define worker-id (place-channel-get i/o))
  (define coordinator-host (place-channel-get i/o))
  (define coordinator-port (place-channel-get i/o))
  (define client-id (format "~a/~a" (gethostname) (random 10000)))

  (define (log msg . args)
    (define m (apply format msg args))
    (place-channel-put
     i/o
     (format "~a: ~a" worker-id m)))

  ;; Get a region named NAME -- we keep a small cache of the regions in case
  ;; we have to refine multiple geoids from the same region, but request the
  ;; region GeoJSON from the coordinator.
  (define (get-region name)
    (define region (hash-ref cached-regions name #f))
    (unless region
      (log "Fetching JSON for ~a" name)
      (set! region (request-region name client-id coordinator-host coordinator-port))
      (unless region
        (error (format "Failed to get region:~a~%" name)))
      (when (> (hash-count cached-regions) 10)
        ;; get rid of old regions (we'll get them again, in case we need them)
        (hash-clear! cached-regions))
      (hash-set! cached-regions name region)
      (log "Fetching JSON for ~a completed" name))
    region)

  (with-handlers
    ((exn:fail?
      (lambda (e) (log "Got exception (will exit place): ~a" e))))
    (let/ec return
      (let loop ()
        (let ([task (request-task client-id coordinator-host coordinator-port)])
          (unless task
            (error "failed to get new task"))
          (let ([kind (hash-ref task 'task)])
            (cond
              ;; A coarse tiling will perform initial tiling at high level for
              ;; the entire region, producing two lists of geoids -- geoids
              ;; which are completely inside the region and geoids which
              ;; intersect the region.  The coordinator will than create new
              ;; tasks for refining the geoids which intersect the region.
              ((equal? kind "coarse_tiling")
               (define tzname (hash-ref task 'tzname))
               (define level (hash-ref task 'level))
               (log "Received coarse_tiling for ~a level ~a" tzname level)
               (define region (get-region tzname))
               (define-values (contains intersects)
                 (coarse-geoid-tiling-for-region region level))
               (if (complete-coarse-tiling
                    tzname contains intersects
                    client-id coordinator-host coordinator-port)
                   (log "Completed assignment for ~a level ~a" tzname level)
                   (log "Failed to send completed assignment for ~a level ~a" tzname level)))
              ;; A refine task asks us to refine the tiling for a region
              ;; inside a single GEOID down to a specified level (controlled
              ;; by the coordinator).
              ((equal? kind "refine_tiling")
               (define tzname (hash-ref task 'tzname))
               (define geoid (string->number (hash-ref task 'geoid)))
               (define level (hash-ref task 'level))
               (log "Received refine_tiling for #x~x part of ~a, to level ~a"
                    geoid tzname level)
               (define region (get-region tzname))
               (define refinement
                 (refine-geoid-tiling-for-region region geoid level))
               (if (complete-refinement-tiling
                    tzname geoid refinement
                    client-id coordinator-host coordinator-port)
                   (log "Completed assignment refine_tiling for #x~x part of ~a, to level ~a"
                        geoid tzname level)
                   (log "Failed to send completed assignment refine_tiling for #x~x part of ~a, to level ~a"
                        geoid tzname level)))
              ;; A "complete" task indicates that the coordinator has no more
              ;; task and we can exit.
              ((equal? kind "complete")
               (log "No more tasks from coordinator, exiting place")
               (return))
              ;; A "retry" task is send by the coordinator when all tasks are
              ;; assigned, but some are not completed yet -- the workers delay
              ;; exiting, since the coordinator might decide that the worker
              ;; for an assigned task is dead and re-assign the task.
              ((equal? kind "retry")
               (log "Coordinator asked to retry later")
               (sleep (+ 10 (random 10))))
              (#t
               (log "Received unknown task: ~a" task)
               (sleep (+ 10 (random 10))))))
          (loop))))))

;; Construct NUM-WORKERS tiling workers as separate racket places and return
;; them as a list.
(define (make-workers num-workers coordinator-host coordinator-port)
  (for/list ([worker-id (in-range num-workers)])
    (define pl (place input (worker input)))
    (place-channel-put pl worker-id)
    (place-channel-put pl coordinator-host)
    (place-channel-put pl coordinator-port)
    pl))

;; Output logging from the workers -- workers send their log messages to their
;; place channel, and this function ensures that only one thread prints them
;; out, avoiding mangling them.
(define (output-logging start-time workers)
  (let loop ()
    (define log-message (apply sync workers))
    (printf "[~a] ~a~%"
            (~r (/ (- (current-inexact-milliseconds) start-time) 1000.0)
                #:precision '(= 1) #:min-width 8)
            log-message)
    (loop)))

(module+ main
  (require racket/cmdline)

  (define coordinator-host (make-parameter "localhost"))
  (define coordinator-port (make-parameter 2395))
  (define num-workers (make-parameter (processor-count)))

  (command-line
   #:program "tzt-worker"
   #:once-each
   [("-c" "--coodinator-host")
    host
    "The host name for the tiling coordinator"
    (coordinator-host host)]
   [("-p" "--coodinator-port")
    port
    "The port number for the tiling coordinator"
    (coordinator-port port)]
   [("-w" "--num-workers")
    workers
    "Number of workers (negative means subtract from processor-count)"
    (let ([w (string->number workers)])
      (unless w
        (error (format "number of workers not an integer, got ~a" workers)))
      (if (< w 1)
          (num-workers (exact-truncate (max 1 (+ (processor-count) w))))
          (num-workers (exact-truncate w))))])

  (define start-time (current-inexact-milliseconds))
  (define workers (make-workers (num-workers) (coordinator-host) (coordinator-port)))
  (void (thread (lambda () (output-logging start-time workers))))
  ;; Wait until all places have exited, than exit ourselves.
  (let loop ([remaining (map place-dead-evt workers)])
    (unless (null? remaining)
      (define evt (apply sync remaining))
      (loop (remove evt remaining)))))

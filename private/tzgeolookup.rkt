#lang racket/base

;; tzgeolookup.rkt -- lookup time zones based on GPS locations
;;
;; This file is part of tzgeolookup -- lookup timezones based on GPS coordinates
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require racket/match
         racket/format
         racket/math
         racket/fasl
         racket/port
         racket/string
         racket/runtime-path
         racket/flonum
         racket/contract
         math/flonum
         file/gzip
         file/gunzip
         json)

;; NOTE: these functions are provided for the tzpack utility, and they should
;; not be used outside the package, if you just require this package, the
;; main.rkt ensures that these functions are not exported.
(provide load-geojson
         prepare-features
         save-feature
         save-index
         data-directory
         (struct-out feature)
         (struct-out shape)
         (struct-out polygon))

(provide/contract
 (lookup-timezone (-> real? real? string?))
 (clear-timezone-cache (-> void?)))

;; A BOUNDING BOX contains the min/max coordinates that enclose a polygon.
(struct bbox (min-x min-y max-x max-y) #:prefab)

;; A POLYGON is defined as its bounding box and the list of points.  Note that
;; this DOES NOT correspond to a GeoJSON polygon, see the `shape` definition
;; below.
(struct polygon (bbox points) #:prefab)

;; A SHAPE is defined as a polygon outline and a set of holes, which are also
;; polygons.  Note that this structure corresponds to a GeoJSON Polygon.
(struct shape (outline holes) #:prefab)

;; A FEATURE is a collection of shapes with a name (the timezone name in our
;; case).
(struct feature (name shapes) #:prefab)


;;...................................... construct features from GeoJSON ....

;; Calculate the bounding box of a list of POINTS and return it as a `bbox`
;; struct.  Each point is a list of two numbers, Easting and Northing.
(define (make-bbox points)
  (if (null? points)
      #f
      (let ([p0 (car points)])
        (let loop ([min-x (car p0)] [min-y (cadr p0)]
                   [max-x (car p0)] [max-y (cadr p0)]
                   [remaining (cdr points)])
          (if (null? remaining)
              (bbox min-x min-y max-x max-y)
              (let ([p1 (car remaining)])
                (loop (min min-x (car p1)) (min min-y (cadr p1))
                      (max max-x (car p1)) (max max-y (cadr p1))
                      (cdr remaining))))))))

;; Construct a polygon instance from a list of POINTS.  We calculate the
;; bounding box than store that plus the points themselves (converted to a
;; vector) in a POLYGON instance.
(define (make-polygon points)
  (define num-points (length points))
  (define data (make-vector (* 2 num-points)))
  (for ([(point index) (in-indexed (in-list points))])
    (match-define (list x y) point)
    (vector-set! data (* 2 index) (exact->inexact x))
    (vector-set! data (+ (* 2 index) 1) (exact->inexact y)))
  (polygon (make-bbox points) data))

;; Construct a SHAPE from a set GeoJSON Polygon.  A shape is composed of a
;; polygon outline plus zero or more polygon holes.  The first item in the
;; GEOJSON-SHAPE list are the points for the outline, while the remaining
;; items are points for the holes.  We construct polygon instances for the
;; outline and the holes and construct a SHAPE instance with them.
(define (make-shape geojson-shape)
  (define outline (make-polygon (car geojson-shape)))
  (define holes (map make-polygon (cdr geojson-shape)))
  (shape outline holes))

;; Construct a FEATURE from a GeoJSON feature node.  We extract the time zone
;; name and construct the appropriate SHAPE instances for it.
(define (make-feature geojson-feature)
  (define name
    (let ([properties (hash-ref geojson-feature 'properties)])
      (hash-ref properties 'tzid #f)))
  (define shapes
    (let ([geometry (hash-ref geojson-feature 'geometry (lambda () (hash)))])
      (let ([shapes (hash-ref geometry 'coordinates null)]
            [type (hash-ref geometry 'type #f)])
        (cond ((equal? type "Polygon")
               ;; NOTE: what GeoJSON calls a polygon corresponds to a SHAPE in
               ;; our data structures.
               (list (make-shape shapes)))
              ((equal? type "MultiPolygon")
               (map make-shape shapes))
              (#t
               (error (format "make-feature: unsupported geometry type: ~a" type)))))))
  (feature name shapes))

;; Load a GeoJSON file from PATH.  This is just a wrapper for `read-json`, and
;; when VERBOSE? is #t, it will print out the amount of time it took to read
;; the file.  No other processing is done on the file.
(define (load-geojson path #:verbose (verbose? #f))
  (define start (current-inexact-milliseconds))
  (when verbose?
    (printf "Loading GeoJSON from ~a ..." path)
    (flush-output))
  (define data (call-with-input-file path read-json))
  (when verbose?
    (define duration (- (current-inexact-milliseconds) start))
    (printf " done (~a seconds).~%" (~r (/ duration 1000.0) #:precision 2)))
  data)

;; Return a list of features found in the GeoJSON object, constructing the
;; feature and related structures from the JSON data.
(define (prepare-features geojson)
  (for/list ([feature (hash-ref geojson 'features '())])
    (make-feature feature)))


;;...................................... Serialization / Deserialization ....

;; Amount of precision for converting the floating point coordinates to
;; integers.  A smaller value will make the integers smaller, see
;; `emit-integer`, so the size of the serialized data is smaller; the downside
;; is that there is less precision.
;;
;; Earth circumference is 40'075'000 meters, so 1e-5 corresponds to approx
;; 1.11 meters and 1e-4 corresponds to 11.1 meters, which is probably better
;; than the GeoJSON data we have to work with and better than the GPS
;; coordinates determined by many of the GPS devices.
;;
;; For now, we use 1e-4, as our test suite passes with this epsilon, but this
;; value might create "empty areas" between adjacent time zones.  Our code
;; would return a nautical time zone for that case, if anyone complains, we
;; can increase the epsilon, or add some fuzzy search to `lookup-timezone`,
;; for the nautical time zone case.
(define epsilon 1e-4)

;; Emit VALUE, an integer, as a byte string to OUT, an output port.  The
;; integer is encoded to use a variable number of bytes, but only as many as
;; needed.  This is the same encoding as used for UTF-8: 7 bit values are used
;; for the bytes, and the top bit is used to flag if more bytes need to be
;; written, this values less than 2^7 are written as 1 byte, values less than
;; 2^14 as two bytes, and so on...
(define (write-integer value out)
  (define nbits 7)
  (define mask #x7F)
  (define flag #x80)
  (if (> value mask)
      (begin
        (write-byte (bitwise-ior flag (bitwise-and value mask)) out)
        (write-integer (arithmetic-shift value (- nbits)) out))
      (write-byte (bitwise-and value mask) out)))

;; Read an integer from the input port IN, that was previously encoded using
;; `write-integer`
(define (read-integer in)
  (define nbits 7)
  (define mask #x7F)
  (define flag #x80)
  (let loop ([result 0]
             [shift 0])
    (let* ([item (read-byte in)]
           [nresult
            (let ([item (bitwise-and item mask)]) ; clear the flag
              (bitwise-ior result (arithmetic-shift item shift)))])
      (if (= flag (bitwise-and item flag))
          ;; If the flag is set, there are more items for this integer
          (loop nresult (+ shift nbits))
          nresult))))

;; Serialize the polygon POLY to the output port OUT.  The serialized data is
;; binary, and made as compact as possible.
;;
;; Implementation notes:
;;
;; The bounding box for the polygon is saved as single precision floats (4
;; bytes), while the points themselves are converted to integers, as a number
;; of steps from the bounding box minimum, with the step size being calculated
;; such that the precision is at least `epsilon`.  Duplicate points are also
;; removed (duplicates might occur since we loose precision).
;;
(define (serialize-polygon poly out)
  (match-define (polygon bb points) poly)
  (match-define (bbox min-x min-y max-x max-y) bb)
  (define width (- max-x min-x))
  (define height (- max-y min-y))
  (define point-count (/ (vector-length points) 2))

  ;; We divide the bounding box size by the EPSILON value to determine the
  ;; number of "clicks" or steps that we can split the size, and still have at
  ;; least `epsilon` amount of precision.
  (define clicks (max (exact-ceiling (/ width epsilon))
                      (exact-ceiling (/ height epsilon))
                      1))

  ;; Determine the list of points, the floating point values are converted
  ;; into the number of "epsilon" steps from min-x (or min-y). Since we loose
  ;; precision, we might end up with duplicate points, these are removed.
  (define-values (encoded-points _1 _2)
    (for/fold ([result '()] [px #f] [py #f])
              ([index (in-range point-count)])
      (define x (vector-ref points (* 2 index)))
      (define y (vector-ref points (+ (* 2 index) 1)))
      (define x1 (/ (- x min-x) width))
      (define y1 (/ (- y min-y) height))
      (define x2 (exact-round (* x1 clicks)))
      (define y2 (exact-round (* y1 clicks)))
      (if (and (equal? px x2) (equal? py y2))
          (values result x2 y2)         ; same as the previous point
          (values (cons (list x2 y2) result) x2 y2))))

  (write-integer (length encoded-points) out)
  (write-integer clicks out)

  (write-bytes (real->floating-point-bytes min-x 4 #t) out)
  (write-bytes (real->floating-point-bytes min-y 4 #t) out)
  (write-bytes (real->floating-point-bytes max-x 4 #t) out)
  (write-bytes (real->floating-point-bytes max-y 4 #t) out)

  (for ([p (in-list (reverse encoded-points))])
    (match-define (list x y) p)
    (write-integer x out)
    (write-integer y out)))

;; Read a polygon from the input port IN which was serialized using
;; `serialize-polygon`
(define (deserialize-polygon in)
  (define point-count (read-integer in))
  (define clicks (read-integer in))
  (define min-x
    (let ([b (read-bytes 4 in)])
      (floating-point-bytes->real b #t)))
  (define min-y
    (let ([b (read-bytes 4 in)])
      (floating-point-bytes->real b #t)))
  (define max-x
    (let ([b (read-bytes 4 in)])
      (floating-point-bytes->real b #t)))
  (define max-y
    (let ([b (read-bytes 4 in)])
      (floating-point-bytes->real b #t)))
  (define data (make-vector (* 2 point-count)))
  (for ([index (in-range point-count)])
    (define rawx (read-integer in))
    (define rawy (read-integer in))
    (vector-set! data (* 2 index) (+ min-x (* (/ rawx clicks) (- max-x min-x))))
    (vector-set! data (+ (* 2 index) 1) (+ min-y (* (/ rawy clicks) (- max-y min-y)))))
  (polygon (bbox min-x min-y max-x max-y) data))

;; Serialize a feature, F, to the output port OUT -- we just serialize the
;; containing polygons, but not the feature name.
(define (serialize-feature f out)
  (define shapes (feature-shapes f))
  (write-integer (length shapes) out)
  (for ([shp (in-list shapes)])
    (match-define (shape outline holes) shp)
    (write-integer (+ 1 (length holes)) out)
    (serialize-polygon outline out)
    (for ([hole (in-list holes)])
      (serialize-polygon hole out))))

;; Read a feature from the input port IN.  Since the feature name is not
;; written by `serialize-feature`, it needs to be supplied as the argument
;; name.
(define (deserialize-feature feature-name in)
  (define shape-count (read-integer in))
  (define shapes
    (for/list ((_ (in-range shape-count)))
      (define polygon-count(read-integer in))
      (define outline (deserialize-polygon in))
      (define holes (for/list ([_ (in-range (sub1 polygon-count))])
                      (deserialize-polygon in)))
      (shape outline holes)))
  (feature feature-name shapes))

;; Save FEATURE to the directory DIR.  The file name will be constructed from
;; the feature name and the feature will serialized using `serialize-feature`,
;; than compressed using GZIP.
(define (save-feature feature dir)
  (define buffer (call-with-output-bytes (lambda (out) (serialize-feature feature out))))
  (define file-name (format "~a.dat" (string-replace (feature-name feature) "/" "+")))
  (call-with-output-file (build-path dir file-name)
    (lambda (out)
      (call-with-input-bytes
       buffer
       (lambda (in)
         (gzip-through-ports
          in out file-name (current-milliseconds)))))
    #:exists 'replace))

;; Load a feature by NAME from the data directory DIR.  The file name to load
;; will be constructed from the feature name, the data will be decompressed
;; than de-serialized using `deserialize-feature`
(define (load-feature name dir)
  (define file-name (format "~a.dat" (string-replace name "/" "+")))
  (define data
    (call-with-output-bytes
     (lambda (out)
       (call-with-input-file (build-path dir file-name)
         (lambda (in)
           (gunzip-through-ports in out))))))
  (call-with-input-bytes data (lambda (in) (deserialize-feature name in))))

;; Save the index (which maps bounding boxes to feature names) into the
;; directory DIR.
(define (save-index index dir)
  (call-with-output-file (build-path dir "index.dat")
    (lambda (out) (s-exp->fasl index out))
    #:exists 'replace))

;; Load the index (which maps bounding boxes to feature names) from the
;; directory DIR.
(define (load-index dir)
  (call-with-input-file (build-path dir "index.dat") fasl->s-exp))


;;....................................................... geolookup code ....

;; Return #t if the point at LAT,LON is inside the bounding box BB
(define (inside-bbox? bb lat lon)
  (match-define (bbox min-x min-y max-x max-y) bb)
  (and (>= lon min-x) (<= lon max-x) (>= lat min-y) (<= lat max-y)))

;; Determine the angle seen from P0 between the points P1 and P2, or, more
;; formally, between the vectors P1 - P0 and P2 - P0. Returns a positive value
;; if P1, P2 are in a counter-clockwise direction w.r.t P0 and a negative
;; angle otherwise.
;;
;; See also: https://en.wikipedia.org/wiki/Subtended_angle
;;
(define (subtended-angle x0 y0 x1 y1 x2 y2)

  (define s1x (fl- x1 x0))
  (define s1y (fl- y1 y0))
  (define s1len (flsqrt (fl+ (fl* s1x s1x) (fl* s1y s1y))))
  (define s2x (fl- x2 x0))
  (define s2y (fl- y2 y0))
  (define s2len (flsqrt (fl+ (fl* s2x s2x) (fl* s2y s2y))))

  (define dot-product (fl+ (fl* s1x s2x) (fl* s1y s2y)))

  (if (or (zero? dot-product) (zero? s1len) (zero? s1len))
      0
      (let ([angle (flacos (flmin 1.0 (fl/ dot-product (fl* s1len s2len))))])
        (define cross-magnitude (fl- (fl* (fl- x1 x0) (fl- y2 y0))
                                     (fl* (fl- y1 y0) (fl- x2 x0))))
        (fl* angle (flsgn cross-magnitude)))))

;; Calculate the winding number of POLYGON as seen from the point at LAT,LON.
;; A winding number greater than 1.0 will indicate that the point is inside
;; the polygon.
;;
;; Unlike the official winding number definition which is an integer, we
;; return a partial winding number, mostly to account for floating point
;; errors in our large and irregular polygons.  Also, the official winding
;; number definition has a sign, being negative if the polygon is traversed in
;; clockwise direction, but we don't care about that either, returning the
;; absolute value instead.
;;
;; https://en.wikipedia.org/wiki/Point_in_polygon and
;; https://en.wikipedia.org/wiki/Winding_number
;;
;; NOTE: this is the internal implementation, see `polygon-winding-number`
;; which does a bounding box check first and avoids this expensive
;; computation.  This exists as a separate function so its time can be
;; measured.
(define (polygon-winding-number-internal poly lat lon)
  (define limit (/ (vector-length poly) 2))
  (define winding-angle
    (for/fold ([winding-angle 0])
              ([index (in-range limit)])
      (define x1 (vector-ref poly (* 2 index)))
      (define y1 (vector-ref poly (+ (* 2 index) 1)))
      (define next-index (if (= (add1 index) limit)
                              0
                              (add1 index)))
      (define x2 (vector-ref poly (* 2 next-index)))
      (define y2 (vector-ref poly (+ (* 2 next-index) 1)))
      (define angle (subtended-angle lon lat x1 y1 x2 y2))
      (+ winding-angle angle)))
  (abs (/ winding-angle (* 2 pi))))

(define (polygon-winding-number poly lat lon)
  (if (inside-bbox? (polygon-bbox poly) lat lon)
      (polygon-winding-number-internal (polygon-points poly) lat lon)
      0))

;; Return the winding number of a "shape" with respect to the point at LAT,LON.
;;
;; In GeoJSON documents, a shape is an outline polygon and one or more "hole"
;; polygons.  If the point is inside the holes (the winding number of the hole
;; is close to 1), we return 0, otherwise we return the winding number of the
;; outline polygon, as returned by `polygon-winding-number`
;;
(define (shape-winding-number p lat lon)
  ;; Must be inside the the outline (which is the first polygon) and not
  ;; inside any of the holes (which are the rest of the polygons).
  (let ([outline (shape-outline p)]
        [holes (shape-holes p)])
    (if (for/first ([h (in-list holes)] #:when (> (polygon-winding-number h lat lon) 0.9999)) #t)
        0
        (polygon-winding-number outline lat lon))))

;; Return the winding number of a GeoJSON feature with respect to the point at
;; LAT,LON.
(define (feature-winding-number-internal feature lat lon)
  (for/fold ([wn 0])
            ([shape (in-list (feature-shapes feature))])
    (max wn (shape-winding-number shape lat lon))))

;; This the directory where we store the serialized timezone data files.
(define-runtime-path data-directory "./tzgeodata")

;; A hash table mapping feature names to loaded features, they are loaded on
;; demand when the index indicates that a LAT/LON coordinate may be inside a
;; certain feature.
(define features (make-hash))

;; Clear the features cache to save memory.  They will be automatically
;; reloaded as needed when `lookup-timezone` is called.
(define (clear-timezone-cache)
  (set! features (make-hash)))

;; Determine the winding number of a feature referenced by name.  Will load
;; the feature from disk if needed, than call
;; `feature-winding-number-internal` to obtain the winding number.
(define (feature-winding-number feature-name lat lon)
  (define feature (hash-ref features feature-name #f))
  (unless feature
    (set! feature (load-feature feature-name data-directory))
    (hash-set! features feature-name feature))
  (feature-winding-number-internal feature (exact->inexact lat) (exact->inexact lon)))

;; Select a timezone from several CANDIDATES.  We return the candidate with
;; the biggest winding number, or resolve some close ties between the top two.
;;
;; See also the tz-lookup project for how some timezone conflicts are
;; resolved.  Since we have a completely different implementation, our
;; conflict sets are slightly different.
;;
;; https://github.com/darkskyapp/tz-lookup/blob/master/pack.js
;;
(define (select-candidate candidates)
  (define sorted (sort candidates > #:key cadr))
  (match sorted
    ;; NOTE candidates are sorted by their winding number, W1 >= W2!
    ((list (list n1 w1) (list n2 w2) other ...)
     (if (> (- w1 w2) 1e-2)
         n1                           ; w1 is definitely greater than w2
         (cond ((or (and (equal? n1 "Asia/Shanghai") (equal? n2 "Asia/Urumqi"))
                    (and (equal? n1 "Asia/Urumqi") (equal? n2 "Asia/Shanghai")))
                "Asia/Urumqi")          ; conflict zone
               (#t
                n1))))
    ((list (list n1 w1) other ...)
     n1)))

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

;; Hold the timezone bounding box index, a list of time-zone bounding boxes
;; and their corresponding names.
(define index #f)

;; Lookup the timezone name for the point at LAT/LON.
(define (lookup-timezone lat lon)
  (unless index
    (set! index (load-index data-directory)))

  (define candidates
    (for/fold ([result '()])
              ([index-entry (in-list index)])
      (match-define (cons name bbox) index-entry)
      (if (and (inside-bbox? bbox lat lon)
               (not (member name result)))
          (let ((wn (feature-winding-number name lat lon)))
            (if (> wn 0.9)
                (cons (list name wn) result)
                result))
          result)))
  (cond ((null? candidates) (nautical-timezone lat lon))
        ((= (length candidates) 1) (caar candidates))
        (#t (select-candidate candidates))))

(module+ test
  (require rackunit)
  (define-runtime-path test-data-file "./test-cases.rktd")
  (define test-data (call-with-input-file test-data-file read))
  (define nitems (length test-data))
  (for ([(test-case num) (in-indexed (in-list test-data))])
    (match-define (list lat lon tzname) test-case)
    (printf "~a/~a checking ~a ~a ~a..." num nitems lat lon tzname)(flush-output)
    (check-equal? (lookup-timezone lat lon) tzname)
    (printf "done.~%")))

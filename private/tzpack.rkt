#lang racket

;; tzpack.rkt -- create the timezone data files
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

(require "tzgeolookup.rkt")

;; This is a standalone program which is used to create the timezone data
;; files in the tzgeodata folder from the combined.json file which is
;; published by the timezone-boundary-builder project at:
;;
;;    https://github.com/evansiroky/timezone-boundary-builder
;;
;; To use it, download the combined.json file form the above link, place it in
;; the same folder as this script, than run
;;
;;    racket tzpack.rkt
;;

(module+ main

  ;; The timezone data JSON contains a 'features list, each feature being a
  ;; time zone definition
  (define features
    (let ([tzdata (load-geojson "./combined.json" #:verbose #t)])
      (prepare-features tzdata)))

  (make-directory* data-directory)

  ;; Save each feature in a separate file, the file name is constructed from
  ;; the feature name.
  (for ([feature (in-list features)])
    ;; Save the data to a buffer, so we can compress it when writing it out.
    (save-feature feature data-directory))

  ;; Prepare and save an index mapping a bounding box to the feature name.
  (define index '())
  (for ([feature (in-list features)])
    (define name (feature-name feature))
    (for ([shape (in-list (feature-shapes feature))])
      (define outline (shape-outline shape))
      (set! index (cons (cons name (polygon-bbox outline)) index))))

  (save-index index data-directory))

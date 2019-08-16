#lang racket/base

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

(require "private/tzgeolookup.rkt")
(provide lookup-timezone
         clear-timezone-cache)

;; raco setup --check-pkg-deps --pkgs tzgeolookup
;; raco test --no-run-if-absent --package tzgeolookup

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install tzgeolookup
;; To uninstall:
;;   $ raco pkg remove tzgeolookup
;; To view documentation:
;;   $ raco docs tzgeolookup
;;
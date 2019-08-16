#lang scribble/manual
@require[@for-label[tzgeolookup
                    racket/base]]

@title{tzgeolookup -- Lookup timezone names for geographic coordinates}
@author{Alex Harsányi}

@defmodule[tzgeolookup]

This package allows determining the time zone name for any geographic
coordinate on earth.  It works entirely off-line, without having to contact
any web service for this information.

This package only returns the time zone name, but it can be used in
conjunction with the @racket[tzinfo] package, which provides functions to
determine the time offset for a time zone.

@defproc[(lookup-timezone (latitude real?) (longitude real?)) string?]{

  Return the IANA timezone name corresponding to the location at
  @racket[latitude] and @racket[longitude].  The coordinates are expressed in
  degrees, with the range of -180 to 180 for @racket[latitude] and -90 to 90
  for @racket[longitude].  For example, calling @racket[(lookup-timezone
  40.7092 -74.0151)] will return the string "America/New_York".

  The function will always return a time zone string.  If the location is not
  inside a time zone defined by a geopolitical area, the
  @hyperlink["https://en.wikipedia.org/wiki/Time_zone#Nautical_time_zones"]{nautical
  time zone} for that location is returned instead. This is a string in the
  format "Etc/UTC+number".

  @bold{Note:} sometimes location data is provided in "easting" and "northing"
  coordinates, which follow the mathematical X, Y notation.  with "easting"
  being the longitude and "northing" being the latitude.

}

@defproc[(clear-timezone-cache) void?]{

  Clear cached data that was loaded from disk during calls to
  @racket[lookup-timezone].  Depending on the number and variety of time zone
  lookups, the loaded data may take several megabytes of memory.  If the
  program has finished the timezone lookup functionality, it may call this
  function to release the memory.  Data will be loaded again, as needed, when
  @racket[lookup-timezone] is called again.

  Lookups are faster when the data is cached, this function should only be
  called if the program knows it won't do timezone lookups for a while.

}

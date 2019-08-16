# tzgeolookup

This package can be used to find the timezone name for a location based on
latitude/longitude GPS coordinates.  The package works entirely off-line, that
is, it does not query any web service for this information.  Usage:

```racket
> (require tzgeolookup)
> (lookup-timezone 40.7092 -74.0151)
"America/New_York"
```

This package can be used in conjunction with the [tzinfo] package to determine
the time zone offset and daylight saving information for a given location:

```racket
> (require tzinfo tzgeolookup)
> (utc-seconds->tzoffset (lookup-timezone 40.7092 -74.0151) (current-seconds))
(tzoffset -14400 #t "EDT")
```

You can install the package using a "raco pkg" command:

```racket
raco pkg install tzgeolookup
```

## Overview

This package provides a single function `lookup-timezone` which takes a
latitude and longitude coordinate and returns a string, representing the IANA
time zone name for that location.

For implementation details see [this][bp1] and [this][bp2] blog post.  In
addition to what is described there, this implementation uses a custom
serialization format, which saves another 10Mb over the compressed FASL files
described in the blog posts.

## Updating

The package contains the timezone boundary information inside, which means it
has no external dependencies.  This data rarely changes, but when it does, the
data set can be regenerated by:

* retrieving the "combined.json" file from the
  [timezone-boundary-builder][tbb] project and placing it in the "private"
  directory.

* running the "tzpack.rkt" script to rebuild the data files in the "tzgeodata"
  directory.  This can be run using "racket tzpack.rkt".

[bp1]: https://alex-hhh.github.io/2019/08/timezone-lookup.html
[bp2]: https://alex-hhh.github.io/2019/08/timezone-lookup-2.html
[tzinfo]: https://pkgs.racket-lang.org/package/tzinfo
[tbb] https://github.com/evansiroky/timezone-boundary-builder
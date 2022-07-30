# Tools to generate the tzgeolookup.db

The `tzgeolookup` package works by looking up locations in a database of
geoids and this database is generated using scripts in this directory.  The
process of generating the geoids covering each region is called "tiling" and
it takes a long time (many hours) to speed things up, this tiling process is
designed around a **coordinator** process which manages the work and stores
the data and **workers** which do the actual work in small chunks.  A large
number of workers can be distributed on multiple computers and each worker
process starts a number of threads corresponding to the number of cores on
each computer, thus parallelizing the work.

The process to generate a new tzgeolookup database is as follows:

* retrieve the "combined.json" and "timezone-names.json" files from the
  [timezone-boundary-builder][tbb] project.

* on one computer start the coordinator process "tzt-coordinator.py" -- this
  can be a low powered computer, such as a Raspberry PI on the local network.
  The process will need the combined.json file and a place where geoid data
  will be written to.  Run "python tzt-coordinator.py --help" for command line
  options.

* start one or more worker processes, "tzt-worker.rkt", run "racket
  tzt-worker.rkt --help" for options, but you'll need to specify the host name
  of the coordinator.  The workers don't use any local disk at all, they
  receive their data from the coordinator and send results back to the
  coordinator.

  If you want to use multiple computers for this task, it is worth creating a
  distribution, so Racket does not need to be installed to the worker
  computers.  You can do that by running the commands (you will need the
  `geoid` package installed):

```
raco exe tzt-worker.rkt
raco dist tzt-worker tzt-worker.exe
```

* Both the coordinator and workers are designed so they can be restarted to
  continue a job, the coordinator keeps track on disk of the progress and can
  be stopped and resumed.

* Once all data has been generated, you can use the "tzt-pack.rkt" program to
  generate the actual SQLite database (run "racket tzt-pack.rkt --help" for
  details.)

[tbb]: https://github.com/evansiroky/timezone-boundary-builder

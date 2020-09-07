
Build
=====

To build, run

    dune build @all

To build with all warnings as errors:

    dune build @all --profile check

To build a statically-linked executable (doesn't work on macOS):

    dune build @all --profile static

utop
====

    dune utop src

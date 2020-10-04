
Build
=====

Requires opam 2.0.

When trying to build for the first time, you may want to create a
local switch. To do so, `cd` into the source directory, and run:

    opam switch create . --empty

Install dependencies:

    opam install . --deps-only --unlock-base --locked -y

To build using dune directly, run

    dune build @all

To build with all warnings as errors:

    dune build @all --profile check

To build a statically-linked executable (doesn't work on macOS):
    
    dune build @all --profile static

Lockfile
========

To re-generate the lockfile, run `opam lock .`.

utop
====

    dune utop src

Links
=====

([How to do local builds](https://opam.ocaml.org/blog/opam-install-dir/))

([Lock files in opam](https://opam.ocaml.org/blog/opam-20-tips/))

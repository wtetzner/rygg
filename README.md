Build
=====

Requires OPAM 2.0

Create local switch:

    opam switch create ./ 4.05.0

Ensure environment is up-to-date:

     eval `opam env --switch=$PWD`

Install dependencies and build:

    opam build

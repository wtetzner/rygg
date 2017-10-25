Build
=====

Create local switch:

    opam switch create ./ 4.04.1

Ensure environment is up-to-date:

     eval `opam env --switch=$PWD`

Install dependencies and build:

    opam build

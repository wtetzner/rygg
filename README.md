Usage
=====

To assemble a file, use the `wombat vmu assemble` command:

    wombat vmu assemble example.s -output example.vms

For help, run `wombat help`:

    % wombat help
    Compiler and build tool for Dreamcast VMU
    
      wombat SUBCOMMAND
    
    === subcommands ===
    
      vmu      Operations for Dreamcast VMU
      version  print version information
      help     explain a given subcommand (perhaps recursively)

The `help` command can be used on subcommands as well:

    % wombat help vmu
    Operations for Dreamcast VMU
    
      wombat vmu SUBCOMMAND
    
    === subcommands ===
    
      assemble  Assembler for Dreamcast VMU
      compile   Compiler for the Wombat programming language
      help      explain a given subcommand (perhaps recursively)

---

    % wombat vmu help assemble
    Assembler for Dreamcast VMU
    
      wombat vmu assemble INPUT-FILE
    
    === flags ===
    
      -output  output-file
               (alias: -o)
      [-help]  print this help text and exit
               (alias: -?)

Build
=====

Requires [OPAM 2.0](https://opam.ocaml.org/blog/opam-2-0-preview/#Let-39-s-go-then-how-to-try-it)

Create local switch:

    opam switch create ./ 4.06.0

Ensure environment is up-to-date:

    eval `opam env --switch=$PWD`

Install dependencies and build:

    opam build

To build directly with jbuilder (after dependencies are installed)

    jbuilder build -p wombat -j 3

If you use `opam build`, then the `wombat` executable will be in your
path. If you use `jbuilder` to do the build, the executable will be at
`./_build/install/default/bin/wombat`. The reason to use `jbuilder`
over `opam build` is that it runs faster, because it doesn't have to
do any dependency resolution, so it gives better turn-around time when
developing.

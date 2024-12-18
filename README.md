Cmdliner — Declarative definition of command line interfaces for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Cmdliner allows the declarative definition of command line interfaces
for OCaml.

It provides a simple and compositional mechanism to convert command
line arguments to OCaml values and pass them to your functions. The
module automatically handles syntax errors, help messages and UNIX man
page generation. It supports programs with single or multiple commands
and respects most of the [POSIX][1] and [GNU][2] conventions.

Cmdliner has no dependencies and is distributed under the ISC license.

[1]: http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap12.html
[2]: http://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html

Home page: http://erratique.ch/software/cmdliner  

## Installation

Cmdliner can be installed with `opam`:

    opam install cmdliner

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc bytesrw`.

Questions are welcome but better asked on the [OCaml forum] than on the
issue tracker.

[online]: http://erratique.ch/software/cmdliner/doc/Cmdliner
[OCaml forum]: https://discuss.ocaml.org/

## Sample programs

If you installed Cmdliner with `opam` sample programs are located in
the directory `opam config var cmdliner:doc`. These programs define
the command line of some classic programs.

In the distribution sample programs are located in the `test`
directory of the distribution. They can be built and run with:

    topkg build --tests true && topkg test

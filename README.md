Cmdliner — Declarative definition of command line interfaces for OCaml
======================================================================

Cmdliner allows the declarative definition of command line interfaces
for OCaml.

It provides a simple and compositional mechanism to convert command
line arguments to OCaml values and pass them to your functions. The
module automatically handles command line completion, syntax errors,
help messages and UNIX man page generation. It supports programs with
single or multiple commands and respects most of the [POSIX] and [GNU]
conventions.

Cmdliner has no dependencies and is distributed under the ISC license.

Homepage: <http://erratique.ch/software/cmdliner>

[POSIX]: http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap12.html
[GNU]: http://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html

## Installation

Cmdliner can be installed with `opam`:

    opam install cmdliner

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc cmdliner`.

Questions are welcome but better asked on the [OCaml forum] than on the
issue tracker.

[online]: http://erratique.ch/software/cmdliner/doc/
[OCaml forum]: https://discuss.ocaml.org/

## Sample programs

A few examples and blueprints can be found in the
[documentation][online] and in the [test](test/) directory.

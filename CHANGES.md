v0.9.8 2015-10-11 Cambridge (UK)
--------------------------------

- Bring back support for OCaml 3.12.0
- Support for pre-formatted paragraphs in man pages. This adds a ``
  `Pre`` case to the `Manpage.block` type which can break existing
  programs. Thanks to Guillaume Bury for suggesting and help.
  
- Support for environment variables. If an argument is absent from the
  command line, its value can be read and parsed from an environment
  variable. This adds an `env` optional argument to the `Arg.info`
  function which can break existing programs.
- Support for new variables in documentation strings. `$(opt)` can be
  used to refer name of the option being documented and `$(env)` for
  option's the environment variable.
- Deprecate `Term.pure` in favor of `Term.const`.
- Man page generation. Keep undefined variables untouched. Previously
  a `$(undef)` would be turned into `undef`.
- Turn a few misterious and spurious `Not_found` exceptions into
  `Invalid_arg`. These can be triggered by client programming errors
  (e.g. an unclosed variable in a documentation string).
- Positional arguments. Invoke the printer on the default (absent)
  value only if needed. See Optional arguments in the release notes of
  v0.9.6.

v0.9.7 2015-02-06 La Forclaz (VS)
---------------------------------

- Build system, don't depend on `ocamlfind`. The package no longer
  depends on ocamlfind. Thanks to Louis Gesbert for the patch. 

v0.9.6 2014-11-18 La Forclaz (VS)
---------------------------------

- Optional arguments. Invoke the printer on the default (absent) value
  only if needed, i.e. if help is shown. Strictly speaking an
  interface breaking change – for example if the absent value was lazy
  it would be forced on each run. This is no longer the case.
- Parsed command line syntax: allow short flags to be specified
  together under a single dash, possibly ending with a short option.
  This allows to specify e.g. `tar -xvzf archive.tgz` or `tar
  -xvzfarchive.tgz`. Previously this resulted in an error, all the
  short flags had to be specified separately. Backward compatible in
  the sense that only more command lines are parsed. Thanks to Hugo
  Heuzard for the patch.
- End user error message improvements using heuristics and edit
  distance search in the optional argument and sub command name
  spaces. Thanks to Hugo Heuzard for the patch.
- Adds `Arg.doc_{quote,alts,alts_enum}`, documentation string
  helpers.
- Adds the `Term.eval_peek_opts` function for advanced usage scenarios.
- The function `Arg.enum` now raises `Invalid_argument` if the
  enumeration is empty.
- Improves help paging behaviour on Windows. Thanks to Romain Bardou
  for the help.


v0.9.5 2014-07-04 Cambridge (UK)
--------------------------------

- Add variance annotation to Term.t. Thanks to Peter Zotov for suggesting.
- Fix section name formatting in plain text output. Thanks to Mikhail
  Sobolev for reporting.


v0.9.4 2014-02-09 La Forclaz (VS)
---------------------------------

- Remove temporary files created for paged help. Thanks to Kaustuv Chaudhuri
  for the suggestion.
- Avoid linking against `Oo` (was used to get program uuid).
- Check the environment for `$MANPAGER` aswell. Thanks to Raphaël Proust
  for the patch.
- OPAM friendly workflow and drop OASIS support.


v0.9.3 2013-01-04 La Forclaz (VS)
---------------------------------

- Allow user specified `SYNOPSIS` sections.


v0.9.2 2012-08-05 Lausanne
--------------------------

- OASIS 0.3.0 support.


v0.9.1 2012-03-17 La Forclaz (VS)
---------------------------------

- OASIS support.
- Fixed broken `Arg.pos_right`.
- Variables `$(tname)` and `$(mname)` can be used in a term's man
  page to respectively refer to the term's name and the main term
  name.
- Support for custom variable substitution in `Manpage.print`.
- Adds `Term.man_format`, to facilitate the definition of help commands.
- Rewrote the examples with a better and consistant style.

Incompatible API changes:

- The signature of `Term.eval` and `Term.eval_choice` changed to make
  it more regular: the given term and its info must be tupled together
  even for the main term and the tuple order was swapped to make it
  consistent with the one used for arguments.


v0.9.0 2011-05-27 Lausanne
--------------------------

- First release.

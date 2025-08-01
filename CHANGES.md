
### End-user visible changes

- **IMPORTANT** Cmdliner no longer allows command names, option names,
  and `Arg.enum` values to be specified by a prefix if the prefix is
  unambiguous. See #200 for the rationale. To quickly salvage scripts
  that may be relying on the old behaviour, it can be restored by
  setting the environment variable `CMDLINER_LEGACY_PREFIXES=true`.
  However the scripts should be fixed: this escape hatch will be
  removed in the future.

- Pager. If set, respect the user's `LESS` environment variable
  (otherwise the default `LESS=FRX` is left unchanged).  Note however
  that you likely need at least `R` specified if you define it
  yourself, otherwise the manpage may look garbled (#191). Thanks to
  Yukai Chou for suggesting.

- Fix lack of output whenever `PAGER` or `MANPAGER` is set but empty;
  fallback to pager discovery (#194). For example this prevented to
  see manpages in `emacs`'s compilation mode which unhelpfully
  hardcodes `PAGER=""`.

- Fix synopsis rendering of required optional arguments (#203).

- Output error messages on `stderr` with styled text (#144). Quoted
  and typewriter text is in bold. Variables are written as
  underlines. Key words of error messages are in red.

- Output error messages after the usage line and remove the `Try with
  $(tool) --help for more information` message. Instead we explicitly
  indicate the `--help` option in the usage line. Having the error message
  at the end makes it easier to spot.

- Make `--help` request work in any context, except after `--` or on
  the arguments after an unknown command error in which case that
  error is reported (less confusing). Since the option has an optional
  argument value, one had to be carefull that it would not pickup the
  next argument and try to parse it according to `FMT`. This is no
  longer the case. If the argument fails to parse `--help=auto` is
  assumed. (#201).
  
- Deprecation messages are now prepended to the doc strings in the manpage.

### API changes

- Reserve the `--__complete` option for library use.

- Documentation language, `$(cmd)`, `$(cmd.name)` and `$(tool)` can be
  used and should be prefered over of `$(iname)`, `$(tname)` and
  `$(mname)`. `$(cmd.parent)` is added to refer to a command's parent
  or itself at the root.

- Make `Cmdliner.Arg.conv` abstract.  Thanks to Andrey Popp for
  the patch (#206).

- Thanks to the previous point, use the `docv` parameter of argument
  converters can now be used to define the default value used by `docv` in
  `Arg.info`. See `Arg.Conv.docv`.

- Add `Manpage.section_name` type alias (#202).

- Add `Cmd.make` which should be preferred to `Cmd.v` (The `M.v` notation is
  nice for simulating literals, not for heavy constructor).

- Add `Cmd.Env.info_var`. To get back the environment variable name
  from a variable info.

- Add optional `doc_envs` argument to `Arg.info` for adding the given
  environment variables info to the command in which the argument is used.
  Sometimes more than one variable make sense and the `env` argument is
  not directly used.

- Add `Arg.Completion` a module to define argument completion 
  strategies (#1, #187). 
  
- Add `Arg.Conv` module to define converters. This should be used in
  new code. 

- Add `Arg.{file,dir,}path` string converters equiped with appropriate
  file system completions.

- Add `docv` optional parameter to `Arg.enum`.

- Add `Term.env` which provides access to the environment access 
  function provided to evaluation functions.

- Clarify the semantics of the `deprecated` argument of
  `Cmdliner.Cmd.info`, `Cmdliner.Arg.info` and
  `Cmdliner.Cmd.Env.info`. First, the language markup is now supported
  therein. Second the message is no longer only used to warn about
  usage it is now also prepended to the doc string of the entity.

- Use `Arg.conv`'s `docv` property in the documentation of arguments
  whenever `Arg.info`'s `docv` is unspecified (#207).

- Do not check file existence for `-` in `Arg.file` or
  `Arg.non_dir_file` values. This is supposed to mean `stdin` or
  `stdout` (#208).

- Fix manpage rendering performing direct calls to `Sys.getenv` in
  `Cmd.eval*` functions instead of calling the `env` argument as
  advertised in the docs. Incidentally add an `env` optional argument
  to `Manpage.print` (#209).

- Deprecate. `Arg.{printer,conv_docv,conv_parser,
  conv_printer,parser_of_kind_of_string,conv,conv'}`. These will
  likely never be removed but they should no longer be used for 
  new code. Use `Arg.Conv`. 

- Remove deprecated `Arg.{converter,parser,pconv}` (#206).
- Remove deprecated `Arg.{env,env_var}` (#206).
- Remove deprecated `Term.{pure,man_format}` (#206).
- Remove deprecated `Term` evaluation interface (#206).

### Other

- Install a `cmdliner` tool to help with manpage and completion script
  installation. See the command line interface manual of the library
  for more information (#187, #227, #228).

- Install all source files for `odoc` and goto definition editor
  functionality. Thanks to Emile Trotignon and Paul-Elliot Anglès
  d'Auriac for noticing and suggesting (#225).

- Added a proper test suite to the library to check for regressions.
  Replaces most of the test executables that had to be run and inspected
  manually (#205).

v1.3.0 2024-05-23 La Forclaz (VS)
---------------------------------

- Add let operators in `Cmdliner.Term.Syntax` (#173). Thanks to Benoit
  Montagu for suggesting, Gabriel Scherer for reminding us of language
  punning obscurities and Sebastien Mondet for strengthening the case
  to add them.
- Pager. Support full path command lookups on Windows.
  (#185). Thanks to @kit-ty-kate for the report.
- In manpage specifications use `$(iname)` in the default 
  introduction of the `ENVIRONMENT` section. Follow up to 
  #168.
- Add `Cmd.eval_value'` a variation on `Cmd.eval_value`.

v1.2.0 2023-04-10 La Forclaz (VS)
---------------------------------

- In manpage specification the new variable `$(iname)` substitutes the 
  command invocation (from program name to subcommand) in bold (#168). 
  This variable is now used in the default introduction of the `EXIT STATUS` 
  section. Thanks to Ali Caglayan for suggesting.
- Fix manpage rendering when `PAGER=less` is set (#167).
- Plain text manpage rendering: fix broken handling of `` `Noblank ``.
  Thanks to Michael Richards and Reynir Björnsson for the report (#176).
- Fix install to directory with spaces (#172). Thanks to 
  @ZSFactory for reporting and suggesting the fix.
- Fix manpage paging on Windows (#166). Thanks to Nicolás Ojeda Bär
  for the report and the solution.

v1.1.1 2022-03-23 La Forclaz (VS)
---------------------------------

- General documentation fixes, tweaks and improvements.
- Docgen: suppress trailing whitespace in synopsis rendering.
- Docgen: fix duplicate rendering of standard options when using `Term.ret` (#135).
- Docgen: fix duplicate rendering of command name on ``Term.ret (`Help (fmt, None)`` 
  (#135).

v1.1.0 2022-02-06 La Forclaz (VS)
---------------------------------

- Require OCaml 4.08.

- Support for deprecating commands, arguments and environment variables (#66).
  See the `?deprecated` argument of `Cmd.info`, `Cmd.Env.info` and `Arg.info`.

- Add `Manpage.s_none` a special section name to use whenever you 
  want something not to be listed in a command's manpage.

- Add `Arg.conv'` like `Arg.conv` but with a parser signature that returns 
  untagged string errors.

- Add `Term.{term,cli_parse}_result'` functions.

- Add deprecation alerts on what is already deprecated.

- On unices, use `command -v` rather than `type` to find commands.

- Stop using backticks for left quotes. Use apostrophes everywhere. 
  Thanks to Ryan Moore for reporting a typo that prompted the change (#128).

- Rework documentation structure. Move out tutorial, examples and
  reference doc from the `.mli` to multiple `.mld` pages.

- `Arg.doc_alts` and `Arg.doc_alts_enum`, change the default rendering
  to match the manpage convention which is to render these tokens in
  bold.  If you want to recover the previous rendering or were using
  these functions outside man page rendering use an explicit
  `~quoted:true` (the optional argument is available on earlier
  versions).

- The deprecated `Term.exit` and `Term.exit_status_of_result` now 
  require a `unit` result.  This avoids various errors to go undetected. 
  Thanks to Thomas Leonard for the patch (#124).
  
- Fix absent and default option values (`?none` string argument of `Arg.some`)
  rendering in manpages:
  
  1. They were not escaped, they now are.
  2. They where not rendered in bold, they now are.
  3. The documentation language was interpreted, it is no longer the case.
  
  If you were relying on the third point via `?none` of `Arg.some`, use the new
  `?absent` optional argument of `Arg.info` instead. Besides a new
  `Arg.some'` function is added to specify a value for `?none` instead
  of a string.  Thanks to David Allsopp for the patch (#111).
  
- Documentation generation use: `…` (U+2026) instead of `...` for 
  ellipsis. See also UTF-8 manpage support below.
  
- Documentation generation, improve command synopsis rendering on 
  commands with few options (i.e. mention them).
  
- Documentation generation, drop section heading in the output if the section 
  is empty.

### New `Cmd` module and deprecation of the `Term` evaluation interface

This version of cmdliner deprecates the `Term.eval*` evaluation
functions and `Term.info` information values in favor of the new
`Cmdliner.Cmd` module. 

The `Cmd` module generalizes the existing subcommand support to allow
arbitrarily nested subcommands each with its own man page and command
line syntax represented by a `Term.t` value.

The mapping between the old interface and the new one should be rather
straightforward. In particular `Term.info` and `Cmd.info` have exactly
the same semantics and fields and a command value simply pairs a
command information with a term.

However in this transition the following things are changed or added:

* All default values of `Cmd.info` match those of `Term.info` except
  for:
  * The `?exits` argument which defaults to `Cmd.Exit.defaults`
    rather than the empty list.
  * The `?man_xrefs` which defaults to the list ``[`Main]`` rather
    than the empty list (this means that by default subcommands 
    at any level automatically cross-reference the main command).
  * The `?sdocs` argument which defaults to `Manpage.s_common_options`
    rather than `Manpage.s_options`.
    
* The `Cmd.Exit.some_error` code is added to `Cmd.Exit.defaults`
  (which in turn is the default for `Cmd.info` see above).  This is an
  error code clients can use when they don't want to bother about
  having precise exit codes.  It is high so that low, meaningful,
  codes can later be added without breaking a tool's compatibility. In
  particular the convenience evaluation functions `Cmd.eval_result*`
  use this code when they evaluate to an error.

* If you relied on `?term_err` defaulting to `1` in the various
  `Term.exit*` function, note that the new `Cmd.eval*` function use
  `Exit.cli_error` as a default. You may want to explicitly specify
  `1` instead if you use `Term.ret` with the `` `Error`` case 
  or `Term.term_result`.
  
Finally be aware that if you replace, in an existing tool, an encoding
of subcommands as positional arguments you will effectively break the
command line compatibility of your tool since options can no longer be
specified before the subcommands, i.e. your tool synopsis moves from:

```
tool cmd [OPTION]… SUBCMD [ARG]…
```
to 
```
tool cmd SUBCMD [OPTION]… [ARG]…
```

Thanks to Rudi Grinberg for prototyping the feature in #123.

### UTF-8 manpage support 

It is now possible to write UTF-8 encoded text in your doc strings and
man pages.

The man page renderer used on `--help` defaults to `mandoc` if
available, then uses `groff` and then defaults to `nroff`. Starting
with `mandoc` catches macOS whose `groff` as of 11.6 still doesn't
support UTF-8 input and struggles to render some Unicode characters.

The invocations were also tweaked to remove the `-P-c` option which
entails that the default pager `less` is now invoked with the `-R` option.

If you install UTF-8 encoded man pages output via `--help=groff`, in
`man` directories bear in mind that these pages will look garbled on
stock macOS (at least until 11.6). One way to work around is to
instruct your users to change the `NROFF` definition in
`/private/etc/man.conf` from:

    NROFF       /usr/bin/groff -Wall -mtty-char -Tascii -mandoc -c
    
to:

    NROFF       /usr/bin/mandoc -Tutf8 -c

Thanks to Antonin Décimo for his knowledge and helping with these
`man`gnificent intricacies (#27).

v1.0.4 2019-06-14 Zagreb
------------------------

- Change the way `Error (_, e)` term evaluation results 
  are formatted. Instead of treating `e` as text, treat
  it as formatted lines.
- Fix 4.08 `Pervasives` deprecation.
- Fix 4.03 String deprecations.
- Fix bootstrap build in absence of dynlink.
- Make the `Makefile` bootstrap build reproducible.
  Thanks to Thomas Leonard for the patch.

v1.0.3 2018-11-26 Zagreb
------------------------

- Add `Term.with_used_args`. Thanks to Jeremie Dimino for
  the patch.
- Use `Makefile` bootstrap build in opam file.
- Drop ocamlbuild requirement for `Makefile` bootstrap build.
- Drop support for ocaml < 4.03.0
- Dune build support.

v1.0.2 2017-08-07 Zagreb
------------------------

- Don't remove the `Makefile` from the distribution.

v1.0.1 2017-08-03 Zagreb
------------------------

- Add a `Makefile` to build and install cmdliner without `topkg` and
  opam `.install` files. Helps bootstraping opam in OS package
  managers. Thanks to Hendrik Tews for the patches.

v1.0.0 2017-03-02 La Forclaz (VS)
---------------------------------

**IMPORTANT** The `Arg.converter` type is deprecated in favor of the
`Arg.conv` type. For this release both types are equal but the next
major release will drop the former and make the latter abstract. All
users are kindly requested to migrate to use the new type and **only**
via the new `Arg.[p]conv` and `Arg.conv_{parser,printer}` functions.

- Allow terms to be used more than once in terms without tripping out
  documentation generation (#77). Thanks to François Bobot and Gabriel
  Radanne.
- Disallow defining the same option (resp. command) name twice via two
  different arguments (resp. terms). Raises Invalid_argument, used
  to be undefined behaviour (in practice, an arbitrary one would be
  ignored).
- Improve converter API (see important message above).
- Add `Term.exit[_status]` and `Term.exit_status_of[_status]_result`.
  improves composition with `Pervasives.exit`.
- Add `Term.term_result` and `Term.cli_parse_result` improves composition
  with terms evaluating to `result` types.
- Add `Arg.parser_of_kind_of_string`.
- Change semantics of `Arg.pos_left` (see #76 for details).
- Deprecate `Term.man_format` in favor of `Arg.man_format`.
- Reserve the `--cmdliner` option for library use. This is unused for now
  but will be in the future.
- Relicense from BSD3 to ISC.
- Safe-string support.
- Build depend on topkg.

### End-user visible changes

The following changes affect the end-user behaviour of all binaries using
cmdliner.

- Required positional arguments. All missing required position
  arguments are now reported to the end-user, in the correct
  order (#39). Thanks to Dmitrii Kashin for the report.
- Optional arguments. All unknown and ambiguous optional argument
  arguments are now reported to the end-user (instead of only
  the first one).
- Change default behaviour of `--help[=FMT]` option. `FMT` no longer
  defaults to `pager` if unspecified.  It defaults to the new value
  `auto` which prints the help as `pager` or `plain` whenever the
  `TERM` environment variable is `dumb` or undefined (#43). At the API
  level this changes the signature of the type `Term.ret` and values
  `Term.ret`, `Term.man_format` (deprecated) and `Manpage.print` to add the
  new `` `Auto`` case to manual formats. These are now represented by the
  `Manpage.format` type rather than inlined polyvars.

### Doc specification improvements and fixes

- Add `?envs` optional argument to `Term.info`. Documents environment
  variables that influence a term's evaluation and automatically
  integrate them in the manual.
- Add `?exits` optional argument to `Term.info`. Documents exit statuses of
  the program. Use `Term.default_exits` if you are using the new `Term.exit`
  functions.
- Add `?man_xrefs` optional argument to `Term.info`. Documents
  references to other manpages. Automatically formats a `SEE ALSO` section
  in the manual.
- Add `Manpage.escape` to escape a string from the documentation markup
  language.
- Add `Manpage.s_*` constants for standard man page section names.
- Add a `` `Blocks`` case to `Manpage.blocks` to allow block splicing
  (#69).  This avoids having to concatenate block lists at the
  toplevel of your program.
- `Arg.env_var`, change default environment variable section to the
   standard `ENVIRONMENT` manual section rather than `ENVIRONMENT
   VARIABLES`.  If you previously manually positioned that section in
   your man page you will have to change the name. See also next point.
- Fix automatic placement of default environment variable section (#44)
  whenever unspecified in the man page.
- Better automatic insertions of man page sections (#73). See the API
  docs about manual specification. As a side effect the `NAME` section
  can now also be overridden manually.
- Fix repeated environment variable printing for flags (#64). Thanks to
  Thomas Gazagnaire for the report.
- Fix rendering of env vars in man pages, bold is standard (#71).
- Fix plain help formatting for commands with empty
  description. Thanks to Maciek Starzyk for the patch.
- Fix (implement really) groff man page escaping (#48).
- Request `an` macros directly in the man page via `.mso` this
  makes man pages self-describing and avoids having to call `groff` with
  the `-man` option.
- Document required optional arguments as such (#82). Thanks to Isaac Hodes
  for the report.

### Doc language sanitization

This release tries to bring sanity to the doc language. This may break
the rendering of some of your man pages. Thanks to Gabriel Scherer,
Ivan Gotovchits and Nicolás Ojeda Bär for the feedback.

- It is only allowed to use the variables `$(var)` that are mentioned in
  the docs (`$(docv)`, `$(opt)`, etc.) and the markup directives
  `$({i,b},text)`. Any other unknown `$(var)` will generate errors
  on standard error during documentation generation.
- Markup directives `$({i,b},text)` treat `text` as is, modulo escapes;
  see next point.
- Characters `$`, `(`, `)` and `\` can respectively be escaped by `\$`,
  `\(`, `\)` and `\\`. Escaping `$` and `\` is mandatory everywhere.
  Escaping `)` is mandatory only in markup directives. Escaping `(`
  is only here for your symmetric pleasure. Any other sequence of
  character starting with a `\` is an illegal sequence.
- Variables `$(mname)` and `$(tname)` are now marked up with bold when
  substituted. If you used to write `$(b,$(tname))` this will generate
  an error on standard output, since `$` is not escaped in the markup
  directive. Simply replace these by `$(tname)`.

v0.9.8 2015-10-11 Cambridge (UK)
--------------------------------

- Bring back support for OCaml 3.12.0
- Support for pre-formatted paragraphs in man pages. This adds a
  ```Pre`` case to the `Manpage.block` type which can break existing
  programs. Thanks to Guillaume Bury for suggesting and help.
- Support for environment variables. If an argument is absent from the
  command line, its value can be read and parsed from an environment
  variable. This adds an `env` optional argument to the `Arg.info`
  function which can break existing programs.
- Support for new variables in option documentation strings. `$(opt)`
  can be used to refer to the name of the option being documented and
  `$(env)` for the name of the option's the environment variable.
- Deprecate `Term.pure` in favor of `Term.const`.
- Man page generation. Keep undefined variables untouched. Previously
  a `$(undef)` would be turned into `undef`.
- Turn a few mysterious and spurious `Not_found` exceptions into
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
  distance search in the optional argument and subcommand name
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
- Check the environment for `$MANPAGER` as well. Thanks to Raphaël Proust
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
- Rewrote the examples with a better and consistent style.

Incompatible API changes:

- The signature of `Term.eval` and `Term.eval_choice` changed to make
  it more regular: the given term and its info must be tupled together
  even for the main term and the tuple order was swapped to make it
  consistent with the one used for arguments.


v0.9.0 2011-05-27 Lausanne
--------------------------

- First release.

(*---------------------------------------------------------------------------
   Copyright (c) 2025 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Cmdliner
open Cmdliner.Term.Syntax

let hey =
  let doc = "Equivalent to set $(opt)." in
  let env = Cmd.Env.info "TEST_ENV" ~doc in
  let doc = "Set hey." in
  Arg.(value & flag & info ["hey"; "y"] ~env ~doc)

let repodir =
  let doc = "See option $(opt)." in
  let env = Cmd.Env.info "TEST_REPODDIR" ~doc in
  let doc = "Run the program in repository directory $(docv)." in
  Arg.(value & opt file Filename.current_dir_name & info ["repodir"] ~env
         ~docv:"DIR" ~doc)

let id =
  let doc = "See option $(opt)." in
  let env = Cmd.Env.info "TEST_ID" ~doc in
  let doc = "Whatever $(docv) bla $(env) and $(opt)." in
  Arg.(value & opt int ~vopt:10 0 & info ["id"; "i"] ~env ~docv:"ID)" ~doc)

let miaouw =
  let doc = "See option $(opt). These are term names $(mname) $(tname)" in
  let docs = "MIAOUW SECTION (non-standard unpositioned do not do this)" in
  let env = Cmd.Env.info "TEST_MIAOUW" ~doc ~docs in
  let doc = "Whatever this is the doc var $(docv) this is the env var $(env) \
             this is the opt $(opt) and this is $(i,italic) and this is
             $(b,bold) and this $(b,\\$(opt\\)) is \\$(opt) in bold and this
             \\$ is a dollar. $(mname) is the main term name, $(tname) is the
             term name."
  in
  Arg.(value & opt string "miaouw" & info ["m";] ~env ~docv:"MIAOUW" ~doc)

let test hey repodir id miaouw =
  Format.printf "hey: %B@.repodir: %s@.id: %d@.miaouw: %s@."
    hey repodir id miaouw

let man_test_t = Term.(const test $ hey $ repodir $ id $ miaouw)

let info =
  let doc = "UTF-8 test: \u{1F42B} íöüóőúűéáăîâșț ÍÜÓŐÚŰÉÁĂÎÂȘȚ 雙峰駱駝" in
  let envs = [ Cmd.Env.info "TEST_IT" ~doc:"This is $(env) for $(tname)" ] in
  let exits = (Cmd.Exit.info ~doc:"This is a $(status) for $(tname)" 1 ::
               Cmd.Exit.info ~doc:"Ranges from $(status) to $(status_max)"
                 ~max:10 2 ::
               Cmd.Exit.defaults)
  in
  let man = [
    `S "THIS IS A SECTION FOR $(mname)";
    `P "$(mname) subst at begin and end $(mname)";
    `P "$(i,italic) and $(b,bold)";
    `P "\\$ escaped \\$\\$ escaped \\$";
    `P "This does not fail \\$(a)";
    `P ". this is a paragraph starting with a dot.";
    `P "' this is a paragraph starting with a quote.";
    `P "This: \\\\(rs is a backslash for groff and you should not see a \\\\";
    `P "This: \\\\N'46' is a quote for groff and you should not see a '";
    `P "This: \\\\\"  is a groff comment and it should not be one.";
    `P "This is a non preformatted paragraph, filling will occur. This will
        be properly layout on 80 columns.";
    `Pre "This is a preformatted paragraph for $(mname) no filling will \
          occur do the $(i,ASCII) art $(b,here) this will overflow on 80 \
          columns \n\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\n\n\
          ... Should not break\n\
          a... Should not break\n\
          +---+\n\
          |  /|\n\
          | / | ----> Let's swim to the moon.\n\
          |/  |\n\
          +---+";
    `P "These are escapes escaped \\$ \\( \\) \\\\";
    `P "() does not need to be escaped outside directives.";
    `Blocks [
      `P "The following to paragraphs are spliced in.";
      `P "This dollar needs escape \\$(var) this one as well $(b,\\$(bla\\))";
      `P "This is another paragraph \\$(bla) $(i,\\$(bla\\)) $(b,\\$\\(bla\\))";
    ];
    `Noblank;
    `Pre "This is another preformatted paragraph.\n\
          There should be no blanks before and after it.";
    `Noblank;
    `P "Hey ho";
    `I ("label", "item label");
    `I ("lebal", "item lebal");
    `P "The last paragraph";
    `S Manpage.s_bugs;
    `P "Email bug reports to <hehey at example.org>.";]
  in
  let man_xrefs = [`Page ("ascii", 7); `Main; `Tool "grep";] in
  Cmd.info "man_test" ~version:"%%VERSION%%" ~doc ~envs ~exits ~man ~man_xrefs

let cmd = Cmd.make info man_test_t

let test_plain =
  Test.test "Test plain text manpage" @@ fun () ->
  Testing_cmdliner.snap_man cmd @@ __POS_OF__
    {|NAME
       man_test - UTF-8 test: 🐫 íöüóőúűéáăîâșț
       ÍÜÓŐÚŰÉÁĂÎÂȘȚ 雙峰駱駝

SYNOPSIS
       man_test [OPTION]…

THIS IS A SECTION FOR man_test
       man_test subst at begin and end man_test

       italic and bold

       $ escaped $$ escaped $

       This does not fail $(a)

       . this is a paragraph starting with a dot.

       ' this is a paragraph starting with a quote.

       This: \(rs is a backslash for groff and you should not see a \

       This: \N'46' is a quote for groff and you should not see a '

       This: \" is a groff comment and it should not be one.

       This is a non preformatted paragraph, filling will occur. This will be
       properly layout on 80 columns.

       This is a preformatted paragraph for man_test no filling will occur do the ASCII art here this will overflow on 80 columns 
       0123455678901234556789012345567890123455678901234556789012345567890123455678901234556789
       
       ... Should not break
       a... Should not break
       +---+
       |  /|
       | / | ----> Let's swim to the moon.
       |/  |
       +---+

       These are escapes escaped $ ( ) \

       () does not need to be escaped outside directives.

       The following to paragraphs are spliced in.

       This dollar needs escape $(var) this one as well $(bla)

       This is another paragraph $(bla) $(bla) $(bla)
       This is another preformatted paragraph.
       There should be no blanks before and after it.
       Hey ho

       label
           item label

       lebal
           item lebal

       The last paragraph

MIAOUW SECTION (non-standard unpositioned do not do this)
       TEST_MIAOUW
           See option -m. These are term names man_test man_test

OPTIONS
       -i [ID)], --id[=ID)] (default=10) (absent=0 or TEST_ID env)
           Whatever ID) bla TEST_ID and --id.

       -m MIAOUW (absent=miaouw or TEST_MIAOUW env)
           Whatever this is the doc var MIAOUW this is the env var
           TEST_MIAOUW this is the opt -m and this is italic and this is bold
           and this $(opt) is $(opt) in bold and this $ is a dollar. man_test
           is the main term name, man_test is the term name.

       --repodir=DIR (absent=. or TEST_REPODDIR env)
           Run the program in repository directory DIR.

       -y, --hey (absent TEST_ENV env)
           Set hey.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       man_test exits with:

       0   on success.

       1   This is a 1 for man_test

       2-10
           Ranges from 2 to 10

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

ENVIRONMENT
       These environment variables affect the execution of man_test:

       TEST_ENV
           Equivalent to set --hey.

       TEST_ID
           See option --id.

       TEST_IT
           This is TEST_IT for man_test

       TEST_REPODDIR
           See option --repodir.

BUGS
       Email bug reports to <hehey at example.org>.

SEE ALSO|}

let test_groff =
  Test.test "Test groff manpage" @@ fun () ->
  Testing_cmdliner.snap_man ~args:["--help=groff"] cmd @@ __POS_OF__
    {|.\" Pipe this output to groff -m man -K utf8 -T utf8 | less -R
.\"
.mso an.tmac
.TH "MAN_TEST" 1 "" "Man_test %%VERSION%%" "Man_test Manual"
.\" Disable hyphenation and ragged-right
.nh
.ad l
.SH NAME
.P
man_test \N'45' UTF\N'45'8 test: 🐫 íöüóőúűéáăîâșț ÍÜÓŐÚŰÉÁĂÎÂȘȚ 雙峰駱駝
.SH SYNOPSIS
.P
\fBman_test\fR [\fIOPTION\fR]…
.SH THIS IS A SECTION FOR \fBman_test\fR
.P
\fBman_test\fR subst at begin and end \fBman_test\fR
.P
\fIitalic\fR and \fBbold\fR
.P
$ escaped $$ escaped $
.P
This does not fail $(a)
.P
\N'46' this is a paragraph starting with a dot\N'46'
.P
\N'39' this is a paragraph starting with a quote\N'46'
.P
This: \N'92'(rs is a backslash for groff and you should not see a \N'92'
.P
This: \N'92'N\N'39'46\N'39' is a quote for groff and you should not see a \N'39'
.P
This: \N'92'" is a groff comment and it should not be one\N'46'
.P
This is a non preformatted paragraph, filling will occur\N'46' This will be properly layout on 80 columns\N'46'
.P
.nf
This is a preformatted paragraph for \fBman_test\fR no filling will occur do the \fIASCII\fR art \fBhere\fR this will overflow on 80 columns 
0123455678901234556789012345567890123455678901234556789012345567890123455678901234556789

\N'46'\N'46'\N'46' Should not break
a\N'46'\N'46'\N'46' Should not break
+\N'45'\N'45'\N'45'+
|  /|
| / | \N'45'\N'45'\N'45'\N'45'> Let\N'39's swim to the moon\N'46'
|/  |
+\N'45'\N'45'\N'45'+
.fi
.P
These are escapes escaped $ ( ) \N'92'
.P
() does not need to be escaped outside directives\N'46'
.P
The following to paragraphs are spliced in\N'46'
.P
This dollar needs escape $(var) this one as well \fB$(bla)\fR
.P
This is another paragraph $(bla) \fI$(bla)\fR \fB$(bla)\fR
.sp -1
.P
.nf
This is another preformatted paragraph\N'46'
There should be no blanks before and after it\N'46'
.fi
.sp -1
.P
Hey ho
.TP 4
label
item label
.TP 4
lebal
item lebal
.P
The last paragraph
.SH MIAOUW SECTION (non\N'45'standard unpositioned do not do this)
.TP 4
\fBTEST_MIAOUW\fR
See option \fB\N'45'm\fR\N'46' These are term names \fBman_test\fR \fBman_test\fR
.SH OPTIONS
.TP 4
\fB\N'45'i\fR [\fIID)\fR], \fB\N'45'\N'45'id\fR[=\fIID)\fR] (default=\fB10\fR) (absent=\fB0\fR or \fBTEST_ID\fR env)
Whatever \fIID)\fR bla \fBTEST_ID\fR and \fB\N'45'\N'45'id\fR\N'46'
.TP 4
\fB\N'45'm\fR \fIMIAOUW\fR (absent=\fBmiaouw\fR or \fBTEST_MIAOUW\fR env)
Whatever this is the doc var \fIMIAOUW\fR this is the env var \fBTEST_MIAOUW\fR this is the opt \fB\N'45'm\fR and this is \fIitalic\fR and this is \fBbold\fR and this \fB$(opt)\fR is $(opt) in bold and this $ is a dollar\N'46' \fBman_test\fR is the main term name, \fBman_test\fR is the term name\N'46'
.TP 4
\fB\N'45'\N'45'repodir\fR=\fIDIR\fR (absent=\fB\N'46'\fR or \fBTEST_REPODDIR\fR env)
Run the program in repository directory \fIDIR\fR\N'46'
.TP 4
\fB\N'45'y\fR, \fB\N'45'\N'45'hey\fR (absent \fBTEST_ENV\fR env)
Set hey\N'46'
.SH COMMON OPTIONS
.TP 4
\fB\N'45'\N'45'help\fR[=\fIFMT\fR] (default=\fBauto\fR)
Show this help in format \fIFMT\fR\N'46' The value \fIFMT\fR must be one of \fBauto\fR, \fBpager\fR, \fBgroff\fR or \fBplain\fR\N'46' With \fBauto\fR, the format is \fBpager\fR or \fBplain\fR whenever the \fBTERM\fR env var is \fBdumb\fR or undefined\N'46'
.TP 4
\fB\N'45'\N'45'version\fR
Show version information\N'46'
.SH EXIT STATUS
.P
\fBman_test\fR exits with:
.TP 4
0
on success\N'46'
.TP 4
1
This is a 1 for \fBman_test\fR
.TP 4
2\N'45'10
Ranges from 2 to 10
.TP 4
123
on indiscriminate errors reported on standard error\N'46'
.TP 4
124
on command line parsing errors\N'46'
.TP 4
125
on unexpected internal errors (bugs)\N'46'
.SH ENVIRONMENT
.P
These environment variables affect the execution of \fBman_test\fR:
.TP 4
\fBTEST_ENV\fR
Equivalent to set \fB\N'45'\N'45'hey\fR\N'46'
.TP 4
\fBTEST_ID\fR
See option \fB\N'45'\N'45'id\fR\N'46'
.TP 4
\fBTEST_IT\fR
This is \fBTEST_IT\fR for \fBman_test\fR
.TP 4
\fBTEST_REPODDIR\fR
See option \fB\N'45'\N'45'repodir\fR\N'46'
.SH BUGS
.P
Email bug reports to <hehey at example\N'46'org>\N'46'
.SH SEE ALSO
.P
ascii(7), grep(1)|}

let main () =
  let doc = "Test manpage specifications" in
  let test_help =
    let doc = "Test manpage interactively as if --help[$(docv)] is invoked" in
    let help_fmts =
      ["auto", "=auto"; "pager", "=pager"; "groff", "=groff";
       "plain", "=plain"; "", ""]
    in
    let help_enum = Cmdliner.Arg.enum help_fmts and docv = "FMT" in
    Arg.(value & opt ~vopt:(Some "") (some help_enum) None &
         info ["test-help"] ~docv ~doc)
  in
  Test.main' test_help ~doc @@ function
  | None ->
      Test.log "Invoke with %a[=FMT] to test %a[=FMT] interactively"
        Fmt.code "--test-help" Fmt.code "--help";
      Test.autorun ()
  | Some fmt ->
      Test.set_main_exit @@ fun () ->
      let argv = Array.of_list (Cmd.name cmd :: ["--help" ^ fmt ]) in
      Cmd.eval ~argv (Cmd.v info man_test_t)

let () = if !Sys.interactive then () else exit (main ())

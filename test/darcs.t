  $ ./darcs_ex.exe --invalid opt
  darcs: unknown option `--invalid'.
  Usage: darcs COMMAND ...
  Try `darcs --help' for more information.
  [1]

  $ ./darcs_ex.exe initialize --invalid
  darcs initialize: unknown option `--invalid'.
  Usage: darcs initialize [OPTION]... 
  Try `darcs initialize --help' or `darcs initialize --help' for more information.
  [1]

  $ ./darcs_ex.exe initialize --help
  NAME
         darcs-initialize - make the current directory a repository
  
  SYNOPSIS
         darcs initialize [OPTION]... 
  
  DESCRIPTION
         Turns the current directory into a Darcs repository. Any existing
         files and subdirectories become ...
  
  OPTIONS
         --repodir=DIR (absent=.)
             Run the program in repository directory DIR.
  
  COMMON OPTIONS
         These options are common to all commands.
  
         --debug
             Give only debug output.
  
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of `auto',
             `pager', `groff' or `plain'. With `auto', the format is `pager` or
             `plain' whenever the TERM env var is `dumb' or undefined.
  
         --prehook=VAL
             Specify command to run before this darcs command.
  
         -q, --quiet
             Suppress informational output.
  
         -v, --verbose
             Give verbose output.
  
         --version
             Show version information.
  
  MORE HELP
         Use `darcs COMMAND --help' for help on a single command.
         Use `darcs help patterns' for help on patch matching.
         Use `darcs help environment' for help on environment variables.
  
  EXIT STATUS
         initialize exits with the following status:
  
         0   on success.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  BUGS
         Check bug reports at http://bugs.example.org.
  

  $ TERM=dumb ./darcs_ex.exe
  DARCS(1)                         Darcs Manual                         DARCS(1)
  
  
  
  NNAAMMEE
         darcs - a revision control system
  
  SSYYNNOOPPSSIISS
         ddaarrccss _C_O_M_M_A_N_D ...
  
  CCOOMMMMAANNDDSS
         hheellpp
             display help about darcs and darcs commands
  
         iinniittiiaalliizzee
             make the current directory a repository
  
         rreeccoorrdd
             create a patch from unrecorded changes
  
  CCOOMMMMOONN OOPPTTIIOONNSS
         These options are common to all commands.
  
         ----ddeebbuugg
             Give only debug output.
  
         ----hheellpp[=_F_M_T] (default=auto)
             Show this help in format _F_M_T. The value _F_M_T must be one of `auto',
             `pager', `groff' or `plain'. With `auto', the format is `pager` or
             `plain' whenever the TTEERRMM env var is `dumb' or undefined.
  
         ----pprreehhooookk=_V_A_L
             Specify command to run before this ddaarrccss command.
  
         --qq, ----qquuiieett
             Suppress informational output.
  
         --vv, ----vveerrbboossee
             Give verbose output.
  
         ----vveerrssiioonn
             Show version information.
  
  MMOORREE HHEELLPP
         Use `ddaarrccss _C_O_M_M_A_N_D --help' for help on a single command.
         Use `ddaarrccss help patterns' for help on patch matching.
         Use `ddaarrccss help environment' for help on environment variables.
  
  EEXXIITT SSTTAATTUUSS
         ddaarrccss exits with the following status:
  
         0   on success.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  BBUUGGSS
         Check bug reports at http://bugs.example.org.
  
  
  
  Darcs 11VERSION11                                                     DARCS(1)

  $ ./darcs_ex.exe --help
  NAME
         darcs - a revision control system
  
  SYNOPSIS
         darcs COMMAND ...
  
  COMMANDS
         help
             display help about darcs and darcs commands
  
         initialize
             make the current directory a repository
  
         record
             create a patch from unrecorded changes
  
  COMMON OPTIONS
         These options are common to all commands.
  
         --debug
             Give only debug output.
  
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of `auto',
             `pager', `groff' or `plain'. With `auto', the format is `pager` or
             `plain' whenever the TERM env var is `dumb' or undefined.
  
         --prehook=VAL
             Specify command to run before this darcs command.
  
         -q, --quiet
             Suppress informational output.
  
         -v, --verbose
             Give verbose output.
  
         --version
             Show version information.
  
  MORE HELP
         Use `darcs COMMAND --help' for help on a single command.
         Use `darcs help patterns' for help on patch matching.
         Use `darcs help environment' for help on environment variables.
  
  EXIT STATUS
         darcs exits with the following status:
  
         0   on success.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  BUGS
         Check bug reports at http://bugs.example.org.
  

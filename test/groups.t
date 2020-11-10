  $ ./groups.exe things list
  listing things

  $ ./groups.exe things
  Fatal error: exception File "src/cmdliner.ml", line 336, characters 42-48: Assertion failed
  [2]

  $ ./groups.exe things show foo
  showing foo

  $ ./groups.exe things list --help
  NAME
         groups-list
  
  SYNOPSIS
         groups list [OPTION]... 
  
  OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of `auto',
             `pager', `groff' or `plain'. With `auto', the format is `pager` or
             `plain' whenever the TERM env var is `dumb' or undefined.
  
         --version
             Show version information.
  

  $ ./groups.exe things --help
  Fatal error: exception File "src/cmdliner.ml", line 336, characters 42-48: Assertion failed
  [2]

  $ ./groups.exe --help
  NAME
         groups
  
  SYNOPSIS
         groups COMMAND ...
  
  COMMANDS
         things
  
  
  OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of `auto',
             `pager', `groff' or `plain'. With `auto', the format is `pager` or
             `plain' whenever the TERM env var is `dumb' or undefined.
  
         --version
             Show version information.
  

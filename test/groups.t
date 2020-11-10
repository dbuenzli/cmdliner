  $ ./groups.exe things list
  listing things

  $ ./groups.exe things
  groups: this command has subcommands
  Usage: groups COMMAND ...
  Try `groups --help' for more information.
  [124]

  $ ./groups.exe things show foo
  showing foo

  $ ./groups.exe things list --help
  NAME
         groups-things-list
  
  SYNOPSIS
         groups things list [OPTION]... 
  
  OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of `auto',
             `pager', `groff' or `plain'. With `auto', the format is `pager` or
             `plain' whenever the TERM env var is `dumb' or undefined.
  
         --version
             Show version information.
  

  $ ./groups.exe things --help
  groups: this command has subcommands
  Usage: groups COMMAND ...
  Try `groups --help' for more information.
  [124]

  $ ./groups.exe --help
  NAME
         groups
  
  SYNOPSIS
         groups COMMAND ...
  
  COMMANDS
         things
  
         widgets
  
  
  OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of `auto',
             `pager', `groff' or `plain'. With `auto', the format is `pager` or
             `plain' whenever the TERM env var is `dumb' or undefined.
  
         --version
             Show version information.
  

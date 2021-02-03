  $ ./groups.exe things list
  listing things

  $ ./groups.exe things
  groups things: is a command group and requires a command argument.
  Usage: groups COMMAND ...
  Try `groups things --help' for more information.
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
  groups things: is a command group and requires a command argument.
  Usage: groups COMMAND ...
  Try `groups things --help' for more information.
  [124]

  $ ./groups.exe --help
  NAME
         groups
  
  SYNOPSIS
         groups COMMAND ...
  
  COMMANDS
         things
  
         widg
  
         widgets
  
  
  OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of `auto',
             `pager', `groff' or `plain'. With `auto', the format is `pager` or
             `plain' whenever the TERM env var is `dumb' or undefined.
  
         --version
             Show version information.
  

  $ ./groups.exe foobar
  groups: unknown command `foobar'.
  Usage: groups COMMAND ...
  Try `groups --help' for more information.
  [124]

  $ ./groups.exe widgets baz
  groups widgets: unknown command `baz'.
  Usage: groups COMMAND ...
  Try `groups widgets --help' for more information.
  [124]

Prefixes

  $ ./groups.exe th show foo
  showing foo

  $ ./groups.exe wid show foo
  groups: command `wid' ambiguous and could be either `widg' or `widgets'
  Usage: groups COMMAND ...
  Try `groups --help' for more information.
  [124]

Default cmd

  $ ./groups.exe
  default cmd

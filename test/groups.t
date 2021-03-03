  $ ./groups.exe things list
  listing things

  $ ./groups.exe things
  groups things: is a command group and requires a command argument.
  Usage: groups things [OPTION]... 
  Try `groups things --help' or `groups --help' for more information.
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
  NAME
         groups-things - doc for "things"
  
  SYNOPSIS
         groups things [OPTION]... 
  
  DESCRIPTION
         description of "things"
  
  OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of `auto',
             `pager', `groff' or `plain'. With `auto', the format is `pager` or
             `plain' whenever the TERM env var is `dumb' or undefined.
  
         --version
             Show version information.
  

  $ ./groups.exe things --invalid-arg
  groups things: is a command group and requires a command argument.
  Usage: groups things [OPTION]... 
  Try `groups things --help' or `groups --help' for more information.
  [124]

  $ ./groups.exe --help
  NAME
         groups - default term doc
  
  SYNOPSIS
         groups COMMAND ...
  
  DESCRIPTION
         description of default term
  
  COMMANDS
         things
             doc for "things"
  
         widg
             doc for "widg"
  
         widgets
             doc for "widgets"
  
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
  Usage: groups widgets [OPTION]... 
  Try `groups widgets --help' or `groups --help' for more information.
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

Version

  $ ./groups.exe things --version
  %%VERSION%%
  $ ./groups.exe things list --version
  %%VERSION%%

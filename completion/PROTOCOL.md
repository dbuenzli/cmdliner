# Shell completion protocol

This document describes the protocol between cmdliner based programs (*the
program* going further) and shell completion scripts (*the script* going
further) which drive the completion.

The script, when completion is requested, invokes the program with a modified
argv line, replacing a token `TOKEN` where completion is requested with a
prefixed token `+cmdliner-complete:TOKEN`.

The program when invoked, produces a list of completions commands which are then
interpreted by the script. There the following commands (but please note that
the script might ignore some if the corresponding shell lacks support for
certain features).

#### `group`

Define a completion group:

    group
    NAME

Completions followed by this command will be presented to user as a single
group. There could be multiple groups.

#### `item`

Define a completion item:

    item
    COMPLETION
    DESCRIPTION

#### `file`

Present filenames as completion items:

    file

#### `dir`

Present dirnames as completion items:

    dir



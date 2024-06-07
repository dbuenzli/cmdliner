let zsh_completion name = Printf.sprintf {|# This is a template zsh script which defines a completion function for a
# cmdliner based program.
#
# The %s is the program name, should be replaced when eval'ing this script,
# for example:
#
#   eval "$(cat set-zsh-completion.sh | sed 's/%s/myprog/g')"
# 
# The script:
#
# 1. Invokes the program with no arguments but COMP_CWORD, COMP_NWORD and
# COMP_WORD# envrironment variables set, where # is the number from 1 to
# COMP_NWORD, the COMP_CWORD designates the position where completion is
# requested.
# 
# 2. It expects the output on stdout from the program where each three lines
# are:
#
#   item
#   COMPLETION
#   DESCRIPTION
#
# or a single line (to activate filename completion):
#
#   file
#
# or a single line (to activate directory completion):
#
#   dir

function _%s {
  words[CURRENT]="+cmdliner_complete:${words[CURRENT]}"
  local line="env COMP_RUN=1 ${(@)words}"
  local -a completions
  local type
  local item
  local item_doc
  local group
  eval $line | while IFS= read -r type; do
    if [[ "$type" == "group" ]]; then
      if [ -n "$completions" ]; then
        _describe -V unsorted completions -U
        completions=()
      fi
      read -r group
    elif [[ "$type" == "item" ]]; then
      read -r item;
      read -r item_doc;
      completions+=("$item":"$item_doc")
    elif [[ "$type" == "dir" ]]; then
      _path_files -/
    elif [[ "$type" == "file" ]]; then
      _path_files -f
    fi
  done
  if [ -n "$completions" ]; then
    _describe -V unsorted completions -U
  fi
}

compdef _%s %s
|} name name name name name;;let bash_completion name = Printf.sprintf {|_%s() {
  local prefix="${COMP_WORDS[COMP_CWORD]}"
  COMP_WORDS[COMP_CWORD]="+cmdliner_complete:${COMP_WORDS[COMP_CWORD]}"
  local line="env COMP_RUN=1 ${COMP_WORDS[@]}"
  local type
  local item
  local item_doc
  local group
  while read type; do
    if [[ $type == 'group' ]]; then
      read group
    elif [[ $type == 'dir' ]] && (type compopt &> /dev/null); then
      if [[ $prefix != -* ]]; then
        COMPREPLY+=( $(compgen -d "$prefix") )
      fi
    elif [[ $type == 'file' ]] && (type compopt &> /dev/null); then
      if [[ $prefix != -* ]]; then
        COMPREPLY+=( $(compgen -f "$prefix") )
      fi
    elif [[ $type == 'item' ]]; then
      read item;
      read item_doc;
      COMPREPLY+=($item)
    fi
  done < <(eval $line)
  return 0
}
_%s_setup() {
  complete -F _%s %s
}
_%s_setup;
|} name name name name name;;
# This is a template zsh script which defines a completion function for a
# cmdliner based program.
#
# The NAME is the program name, should be replaced when eval'ing this script,
# for example:
#
#   eval "$(cat set-zsh-completion.sh | sed 's/NAME/myprog/g')"
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

function _NAME {
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

compdef _NAME NAME

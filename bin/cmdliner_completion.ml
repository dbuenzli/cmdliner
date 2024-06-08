let zsh_completion name = Printf.sprintf {|function _%s {
  words[CURRENT]="+cmdliner_complete:${words[CURRENT]}"
  local line="env COMP_RUN=1 ${(@)words}"
  local -a completions
  local type group item item_doc
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
|} name name name;;let bash_completion name = Printf.sprintf {|_%s() {
  local prefix="${COMP_WORDS[COMP_CWORD]}"
  COMP_WORDS[COMP_CWORD]="+cmdliner_complete:${COMP_WORDS[COMP_CWORD]}"
  local line="env COMP_RUN=1 ${COMP_WORDS[@]}"
  local type group item item_doc
  eval $line | while read type; do
    if [[ $type == "group" ]]; then
      read group
    elif [[ $type == "dir" ]] && (type compopt &> /dev/null); then
      if [[ $prefix != -* ]]; then
        COMPREPLY+=( $(compgen -d "$prefix") )
      fi
    elif [[ $type == "file" ]] && (type compopt &> /dev/null); then
      if [[ $prefix != -* ]]; then
        COMPREPLY+=( $(compgen -f "$prefix") )
      fi
    elif [[ $type == "item" ]]; then
      read item;
      read item_doc;
      COMPREPLY+=($item)
    fi
  done
  return 0
}
complete -F _%s %s
|} name name name;;
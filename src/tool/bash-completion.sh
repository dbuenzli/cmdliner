_cmdliner_generic() {
  local prefix="${COMP_WORDS[COMP_CWORD]}"
  COMP_WORDS[COMP_CWORD]="+cmdliner_complete:${COMP_WORDS[COMP_CWORD]}"
  local line="${COMP_WORDS[@]}"
  local type group item item_doc
  while read type; do
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
  done < <(eval $line)
  return 0
}

_cmdliner_generic() {
  local prefix="${COMP_WORDS[COMP_CWORD]}"
  COMP_WORDS[COMP_CWORD]="--__complete=${COMP_WORDS[COMP_CWORD]}"
  COMP_WORDS=("${COMP_WORDS[@]:0:1}" "--__complete" "${COMP_WORDS[@]:1}")
  local line="${COMP_WORDS[@]}"
  local version type group item item_line item_doc
  {
    read version
    if [[ $version != "1" ]]; then
      printf "\nUnsupported Cmdliner completion protocol version: $version" >&2
      return 1
    fi
    while read type; do
      if [[ $type == "group" ]]; then
        read group
      elif [[ $type == "dirs" ]] && (type compopt &> /dev/null); then
        if [[ $prefix != -* ]]; then
          COMPREPLY+=( $(compgen -d "$prefix") )
        fi
      elif [[ $type == "files" ]] && (type compopt &> /dev/null); then
        if [[ $prefix != -* ]]; then
          COMPREPLY+=( $(compgen -f "$prefix") )
        fi
      elif [[ $type == "item" ]]; then
        read item;
        item_doc="";
        while read item_line; do
            if [[ "$item_line" == "item-end" ]]; then
                break
            fi
            if [[ -n "$item_doc" ]]; then
                item_doc+=$'\n'"$item_line"
            else
                item_doc=$item_line
            fi
        done
        # Sadly it seems bash does not support doc strings. If you now
        # any better get in touch.
        COMPREPLY+=($item)
      fi
    done } < <(eval $line)
  return 0
}

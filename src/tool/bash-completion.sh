_cmdliner_generic() {
  local prefix="${COMP_WORDS[COMP_CWORD]}"
  local w=("${COMP_WORDS[@]}") # Keep COMP_WORDS intact for restart completion
  w[COMP_CWORD]="--__complete=${COMP_WORDS[COMP_CWORD]}"
  local line="${w[@]:0:1} --__complete ${w[@]:1}"
  local version type group item item_line item_doc
  {
    read version
    if [[ $version != "1" ]]; then
      printf "\nUnsupported cmdliner completion protocol: $version" >&2
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
        # Sadly it seems bash does not support doc strings, so we only
        # add item to to the reply. If you know any better get in touch.
        # Handle glued forms, the completion item is the full option
        if [[ $group == "Values" ]]; then
           if [[ $prefix == --* ]]; then
              item="${prefix%%=*}=$item"
           fi
           if [[ $prefix == -* ]]; then
              item="${prefix:0:2}$item"
           fi
        fi
        COMPREPLY+=($item)
      elif [[ $type == "restart" ]]; then
          # N.B. only emitted if there is a -- token
          for ((i = 0; i < ${#COMP_WORDS[@]}; i++)); do
              if [[ "${COMP_WORDS[i]}" == "--" ]]; then
                  _comp_command_offset $((i+1))
                  return
              fi
          done
      fi
    done } < <(eval $line)
  return 0
}

_cmdliner_generic() {
  local words cword
  if (type _get_comp_words_by_ref &> /dev/null); then
    # Equivalent of COMP_WORDS, COMP_CWORD but allow us to  exclude '=' as a word separator
    _get_comp_words_by_ref -n = words cword
  else
    words=("${COMP_WORDS[@]}")
    cword=$COMP_CWORD
  fi

  local prefix="${words[cword]}"
  local w=("${words[@]}") # Keep words intact for restart completion
  w[cword]="--__complete=${words[cword]}"
  local line="${w[@]:0:1} --__complete ${w[@]:1}"
  local version type group item text_line item_doc msg
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
          compopt -o filenames
          COMPREPLY+=( $(compgen -d "$prefix") )
        fi
      elif [[ $type == "files" ]] && (type compopt &> /dev/null); then
        if [[ $prefix != -* ]]; then
          compopt -o filenames
          COMPREPLY+=( $(compgen -f "$prefix") )
        fi
      elif [[ $type == "message" ]]; then
          msg="";
          while read text_line; do
              if [[ "$text_line" == "message-end" ]]; then
                  msg=${msg#?} # remove first newline
                  break
              fi
              msg+=$'\n'"$text_line"
          done
          printf "$msg" >&2
      elif [[ $type == "item" ]]; then
        read item;
        item_doc="";
        while read text_line; do
            if [[ "$text_line" == "item-end" ]]; then
                item_doc=${item_doc#?} # remove first newline
                break
            fi
            item_doc+=$'\n'"$text_line"
        done
        # Sadly it seems bash does not support doc strings, so we only
        # add item to to the reply. If you know any better get in touch.
        if [[ $group == "Values" ]] && [[ $prefix == -* ]] && [[ $prefix != --* ]]; then
          # properly complete short options
          item="${prefix:0:2}$item"
        fi
        COMPREPLY+=($item)
      elif [[ $type == "restart" ]]; then
          # N.B. only emitted if there is a -- token
          for ((i = 0; i < ${#words[@]}; i++)); do
              if [[ "${words[i]}" == "--" ]]; then
                  _comp_command_offset $((i+1))
                  return
              fi
          done
      fi
    done } < <(eval $line)
  return 0
}

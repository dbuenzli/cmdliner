let bash_generic_completion =
{|_cmdliner_generic() {
  local prefix="${COMP_WORDS[COMP_CWORD]}"
  local w=("${COMP_WORDS[@]}") # Keep COMP_WORDS intact for restart completion
  w[COMP_CWORD]="--__complete=${COMP_WORDS[COMP_CWORD]}"
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
          COMPREPLY+=( $(compgen -d "$prefix") )
        fi
      elif [[ $type == "files" ]] && (type compopt &> /dev/null); then
        if [[ $prefix != -* ]]; then
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
        # Handle glued forms, the completion item is the full option
        if [[ $group == "Values" ]]; then
           if [[ $prefix == --* ]]; then
              item="${prefix%%=*}=$item"
           elif [[ $prefix == -* ]]; then
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
|}

let zsh_generic_completion =
{|function _cmdliner_generic {
  local w=("${words[@]}") # Keep words intact for restart completion
  local prefix="${words[CURRENT]}"
  w[CURRENT]="--__complete=${words[CURRENT]}"
  local line="${w[@]:0:1} --__complete ${w[@]:1}"
  local -a completions
  local version type group item text_line item_doc msg
  eval $line | {
    read -r version
    if [[ $version != "1" ]]; then
      _message -r "Unsupported cmdliner completion protocol: $version"
      return 1
    fi
    while IFS= read -r type; do
      if [[ "$type" == "group" ]]; then
        if [ -n "$completions" ]; then
          _describe -V unsorted completions -U
          completions=()
        fi
        read -r group
      elif [[ "$type" == "message" ]]; then
          msg="";
          while read text_line; do
              if [[ "$text_line" == "message-end" ]]; then
                  msg=${msg#?} # remove first newline
                  break
              fi
              msg+=$'\n'"$text_line"
          done
          _message -r "$msg"
      elif [[ "$type" == "item" ]]; then
        read -r item;
        item_doc="";
        while read -r text_line; do
            if [[ "$text_line" == "item-end" ]]; then
                item_doc=${item_doc#?} # remove first space
                break
            fi
            # Sadly it seems impossible to make multiline
            # doc strings. Get in touch if you know any better.
            item_doc+=" $text_line"
        done
        # Handle glued forms, the completion item is the full option
        if [[ "$group" == "Values" ]]; then
            if [[ "$prefix" == --* ]]; then
                item="${prefix%%=*}=${item}"
            elif [[ "$prefix" == -* ]]; then
                item="${prefix:0:2}${item}"
            fi
        fi
        # item_doc="${item_doc//$'\e'\[(01m|04m|m)/}"
        completions+=("${item}":"${item_doc}")
      elif [[ "$type" == "dirs" ]]; then
        _path_files -/
      elif [[ "$type" == "files" ]]; then
        _path_files -f
      elif [[ "$type" == "restart" ]]; then
        # N.B. only emitted if there is a -- token
        while [[ $words[1] != "--" ]]; do
          shift words
          (( CURRENT-- ))
        done
        shift words
        (( CURRENT-- ))
        _normal
      fi
    done
  }
  if [ -n "$completions" ]; then
    _describe -V unsorted completions -U
  fi
}
|}
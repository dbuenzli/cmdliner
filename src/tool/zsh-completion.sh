function _cmdliner_generic {
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
            fi
            if [[ "$prefix" == -* ]]; then
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

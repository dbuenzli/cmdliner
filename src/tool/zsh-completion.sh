function _cmdliner_generic {
  words[CURRENT]="--__complete=${words[CURRENT]}"
  words=("${words[@]:0:1}" "--__complete" "${words[@]:1}")
  local line="${(@)words}"
  local -a completions
  local version type group item item_doc
  eval $line | {
    read -r version
    if [[ $version != "1" ]]; then
      _message -r "Unsupported Cmdliner completion protocol version: $version"
      return 1
    fi
    while IFS= read -r type; do
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

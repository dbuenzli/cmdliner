function _NAME {
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
compdef _NAME NAME

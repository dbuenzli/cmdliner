let bash_generic_completion =
{|_cmdliner_generic() {
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
|}

let zsh_generic_completion =
{|function _cmdliner_generic {
  words[CURRENT]="--__complete=${words[CURRENT]}"
  words=("${words[@]:0:1}" "--__complete" "${words[@]:1}")
  local line="${(@)words}"
  local -a completions
  local version type group item item_line item_doc
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
        item_doc="";
        while read -r item_line; do
            if [[ "$item_line" == "item-end" ]]; then
                break
            fi
            if [[ -n "$item_doc" ]]; then
                # Sadly it seems impossible to make multiline
                # doc strings if you know any better get in touch.
                item_doc+=" $item_line"
            else
                item_doc=$item_line
            fi
        done
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
|}
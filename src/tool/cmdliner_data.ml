let bash_generic_completion =
{|_cmdliner_generic() {
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
  w[CURRENT]="--__complete=${words[CURRENT]}"
  local line="${w[@]:0:1} --__complete ${w[@]:1}"
  local -a completions
  local version type group item item_line item_doc
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
      elif [[ "$type" == "item" ]]; then
        read -r item;
        item_doc="";
        while read -r item_line; do
            if [[ "$item_line" == "item-end" ]]; then
                break
            fi
            if [[ -n "$item_doc" ]]; then
                # Sadly it seems impossible to make multiline
                # doc strings. Get in touch if you know any better.
                item_doc+=" $item_line"
            else
                item_doc=$item_line
            fi
        done
        # Sadly trying to use ANSI styling sequences breaks in all sorts
        # of ways. Get in touch if you know any better. Note this is
        # not general the protocol allows any escape but these are
        # the ones cmdliner currently outputs.
        item_doc="${item_doc//$'\e'\[(01m|04m|m)/}"
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
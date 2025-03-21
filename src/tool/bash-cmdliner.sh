if ! declare -F _cmdliner_generic > /dev/null; then
  _comp_load _cmdliner_generic
fi
complete -F _cmdliner_generic cmdliner

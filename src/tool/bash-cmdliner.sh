if ! declare -F _cmdliner_generic > /dev/null; then
  _completion_loader _cmdliner_generic
fi
complete -F _cmdliner_generic cmdliner

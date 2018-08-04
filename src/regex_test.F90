program regex_test

  use regex_state_mod
  use regex_parser_mod

  implicit none

  type(regex_state_type), pointer :: state_tree
  character(100) regex

  ! regex = '(a*(b(12)c)+f?\w*\d|e)'
  regex = 'datacode=(\w+)'
  write(*, "('Test regex: ', A)") trim(regex)
  state_tree => regex_parser_create_state_tree(regex)

  call state_tree%output_graph('regex_graph.dot')

end program regex_test

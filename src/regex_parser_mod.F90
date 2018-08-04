module regex_parser_mod

  use regex_state_mod

  implicit none

contains

  function regex_parser_create_state_tree(regex) result(state_tree)

    character(*), intent(in) :: regex
    type(regex_state_type), pointer :: state_tree

    type(regex_state_type), pointer :: state => null()
    integer cursor

    call regex_state_create_tree(state_tree)
    state => state_tree
    cursor = 1
    do while (cursor <= len_trim(regex))
      call parse_next(regex, cursor, state)
    end do

  end function regex_parser_create_state_tree

  subroutine parse_next(regex, cursor, state)

    character(*), intent(in) :: regex
    integer, intent(inout) :: cursor
    type(regex_state_type), intent(inout), pointer :: state

    select case (regex(cursor:cursor))
    case ('.')
      call create_simple_state(regex, cursor, any_state_type, state)
    case ('*')
      call create_simple_state(regex, cursor, zero_or_more_state_type, state)
    case ('?')
      call create_simple_state(regex, cursor, zero_or_one_state_type, state)
    case ('+')
      call create_simple_state(regex, cursor, one_or_more_state_type, state)
    case ('(')
      call create_open_group_state(regex, cursor, state)
    case (')')
      call create_simple_state(regex, cursor, close_group_state_type, state)
    case ('\')
      call create_char_class_state(regex, cursor, state)
    case ('|')
      call create_or_state(regex, cursor, state)
    case default
      call create_simple_state(regex, cursor, literal_state_type, state)
    end select

  end subroutine parse_next

  subroutine create_simple_state(regex, cursor, state_type, state)

    character(*), intent(in) :: regex
    integer, intent(inout) :: cursor
    integer, intent(in) :: state_type
    type(regex_state_type), intent(inout), pointer :: state

    call regex_state_create_node_after(state)
    state%type = state_type
    state%value = regex(cursor:cursor)
    cursor = cursor + 1

  end subroutine create_simple_state

  subroutine create_open_group_state(regex, cursor, state)

    character(*), intent(in) :: regex
    integer, intent(inout) :: cursor
    type(regex_state_type), intent(inout), pointer :: state

    type(regex_state_type), pointer :: open_group_state

    call regex_state_create_node_after(state)
    state%type = open_group_state_type
    state%value = regex(cursor:cursor)
    cursor = cursor + 1
    ! Search for ending character.
    open_group_state => state
    do while (cursor <= len_trim(regex))
      call parse_next(regex, cursor, state)
      if (state%type == close_group_state_type .and. .not. associated(state%pair_state)) then
        open_group_state%pair_state => state
        state%pair_state => open_group_state
        exit
      end if
    end do

  end subroutine create_open_group_state

  subroutine create_char_class_state(regex, cursor, state)

    character(*), intent(in) :: regex
    integer, intent(inout) :: cursor
    type(regex_state_type), intent(inout), pointer :: state

    call regex_state_create_node_after(state)
    select case (regex(cursor+1:cursor+1))
    case ('w')
      state%type = word_state_type
    case ('d')
      state%type = digit_state_type
    end select
    state%value = regex(cursor:cursor+1)
    cursor = cursor + 2

  end subroutine create_char_class_state

  subroutine create_or_state(regex, cursor, state)

    character(*), intent(in) :: regex
    integer, intent(inout) :: cursor
    type(regex_state_type), intent(inout), pointer :: state

    type(regex_state_type), pointer :: or_state, start_state, end_state, state_
    type(regex_state_ptr_type), allocatable :: states_(:)

    call regex_state_create_node_after(state)
    or_state => state
    state%type = or_state_type
    state%value = regex(cursor:cursor)
    cursor = cursor + 1
    ! Find out if there is any open group state.
    state_ => state%prev_states(1)%ptr
    do while (associated(state_))
      select case (state_%type)
      case (open_group_state_type)
        if (.not. associated(state_%pair_state)) then
          start_state => state_
          ! Parse until the close group state.
          do while (cursor <= len_trim(regex))
            call parse_next(regex, cursor, state)
            if (state%type == close_group_state_type .and. .not. associated(state%pair_state)) then
              start_state%pair_state => state
              state%pair_state => start_state
              exit
            end if
          end do
          end_state => state
          exit
        end if
      case (initial_state_type)
        start_state => state_
        ! Parse the rest regex.
        do while (cursor <= len_trim(regex))
          call parse_next(regex, cursor, state)
        end do
        end_state => state
        exit
      end select
      state_ => state_%prev_states(1)%ptr
    end do
    ! Adjust state tree.
    ! - Increate start_state next_states array size.
    allocate(states_(size(start_state%next_states) + 1))
    states_(:size(start_state%next_states)) = start_state%next_states
    deallocate(start_state%next_states)
    call move_alloc(states_, start_state%next_states)
    start_state%next_states(size(start_state%next_states))%ptr => or_state%next_states(1)%ptr
    ! - Increate or_state prev_states array size and append right branch to it.
    allocate(states_(size(or_state%prev_states) + 1))
    states_(:size(or_state%prev_states)) = or_state%prev_states
    deallocate(or_state%prev_states)
    call move_alloc(states_, or_state%prev_states)
    or_state%prev_states(size(or_state%prev_states))%ptr => end_state%prev_states(1)%ptr
    ! - Put end_state after or_state.
    or_state%next_states(1)%ptr => end_state
    end_state%prev_states(1)%ptr%next_states(1)%ptr => or_state
    end_state%prev_states(1)%ptr => or_state

  end subroutine create_or_state

end module regex_parser_mod

module regex_state_mod

  implicit none

  integer, parameter :: initial_state_type = 0
  integer, parameter :: literal_state_type = 1
  integer, parameter :: any_state_type = 2
  integer, parameter :: zero_or_more_state_type = 3
  integer, parameter :: zero_or_one_state_type = 4
  integer, parameter :: one_or_more_state_type = 5
  integer, parameter :: or_state_type = 6
  integer, parameter :: open_group_state_type = 7
  integer, parameter :: close_group_state_type = 8
  integer, parameter :: open_list_state_type = 9
  integer, parameter :: close_list_state_type = 10
  integer, parameter :: word_state_type = 11
  integer, parameter :: digit_state_type = 12

  type regex_state_ptr_type
    type(regex_state_type), pointer :: ptr => null()
  end type regex_state_ptr_type

  type regex_state_type
    integer id
    integer type
    character(10) value
    type(regex_state_type), pointer :: pair_state => null()
    type(regex_state_ptr_type), allocatable :: prev_states(:)
    type(regex_state_ptr_type), allocatable :: next_states(:)
  contains
    procedure :: clear => regex_state_clear
    procedure :: output_graph => regex_state_output_graph
  end type regex_state_type

contains

  subroutine regex_state_create_tree(state_tree)

    type(regex_state_type), intent(out), pointer :: state_tree

    allocate(state_tree)
    state_tree%id = 0
    state_tree%type = initial_state_type
    state_tree%value = ''

  end subroutine regex_state_create_tree

  subroutine regex_state_create_node_after(state)

    type(regex_state_type), intent(inout), pointer :: state

    type(regex_state_type), pointer :: state_

    allocate(state%next_states(1))
    allocate(state%next_states(1)%ptr)
    state_ => state%next_states(1)%ptr
    state_%id = state%id + 1
    allocate(state_%prev_states(1))
    state_%prev_states(1)%ptr => state
    state => state_

  end subroutine regex_state_create_node_after

  subroutine regex_state_clear(this)

    class(regex_state_type), intent(inout) :: this

    integer i

    if (allocated(this%next_states)) then
      do i = 1, size(this%next_states)
        if (associated(this%next_states(i)%ptr)) then
          call this%next_states(i)%ptr%clear()
          deallocate(this%next_states(i)%ptr)
        end if
      end do
    end if
    deallocate(this%prev_states)
    deallocate(this%next_states)

  end subroutine regex_state_clear

  function graph_node(state)

    type(regex_state_type), intent(in) :: state
    character(100) graph_node

    select case (state%type)
    case (initial_state_type)
      write(graph_node, "('initial_state')")
    case (literal_state_type)
      write(graph_node, "('literal', I5.5)") state%id
    case (any_state_type)
      write(graph_node, "('any', I5.5)") state%id
    case (zero_or_more_state_type)
      write(graph_node, "('zero_or_more', I5.5)") state%id
    case (zero_or_one_state_type)
      write(graph_node, "('zero_or_one', I5.5)") state%id
    case (one_or_more_state_type)
      write(graph_node, "('one_or_more', I5.5)") state%id
    case (or_state_type)
      write(graph_node, "('or', I5.5)") state%id
    case (open_group_state_type)
      write(graph_node, "('open_group', I5.5)") state%id
    case (close_group_state_type)
      write(graph_node, "('close_group', I5.5)") state%id
    case (word_state_type)
      write(graph_node, "('word', I5.5)") state%id
    case (digit_state_type)
      write(graph_node, "('digit', I5.5)") state%id
    end select

  end function graph_node

  subroutine regex_state_output_graph(this, file_name, file_unit)

    class(regex_state_type), intent(in) :: this
    character(*), intent(in), optional :: file_name
    integer, intent(in), optional :: file_unit

    integer fu, i

    if (present(file_name)) then
      fu = 10
      open(fu, file=file_name)
      write(fu, "('digraph regex {')")
      write(fu, "('rankdir = LR;')")
    else if (present(file_unit)) then
      fu = file_unit
    end if
    if (allocated(this%next_states)) then
      do i = 1, size(this%next_states)
        write(fu, "(A)") trim(graph_node(this)) // ' -> ' // trim(graph_node(this%next_states(i)%ptr)) // ';'
        if (size(this%next_states(i)%ptr%prev_states) > 1 .and. this%id /= this%next_states(i)%ptr%prev_states(1)%ptr%id) cycle
        call this%next_states(i)%ptr%output_graph(file_unit=fu)
      end do
    end if
    if (this%value /= '') then
      if (this%type == word_state_type .or. this%type == digit_state_type) then
        write(fu, "(A)") trim(graph_node(this)) // '[label="\\' // trim(this%value) // '",shape=box];'
      else
        write(fu, "(A)") trim(graph_node(this)) // '[label="' // trim(this%value) // '",shape=box];'
      end if
    end if
    if (present(file_name)) then
      write(fu, "('}')")
      close(fu)
    end if

  end subroutine regex_state_output_graph

end module regex_state_mod

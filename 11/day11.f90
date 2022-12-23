
program day11
    use iso_fortran_env, ip => int64
    implicit none

    integer, parameter :: read_unit = 99
    character(len=200) :: dummy, buffer
    integer :: ii, k, ios, nmonkeys, nitems, max_mod

    type :: monkey
    integer(ip), allocatable :: items(:)
    character :: operation = ""
    character(3) :: opval = ""
    integer :: part = 1
    integer(ip) :: max_mod = 1
    integer :: test_value = 0
    integer :: if_true = 0
    integer :: if_false = 0
    integer :: inspections_counter = 0
    end type

    type (monkey), allocatable ::  monkeys(:)

    open(unit=read_unit, file='data.dat', iostat=ios)
    if ( ios /= 0 ) stop "Error opening file data.dat"

    monkeys = [monkey ::]
    call get_monkeys(monkeys) ! read input puzzle
    nmonkeys = size(monkeys)

    ! part 1
    do k= 1, 20 ! 20 rounds
      call round(monkeys)
    enddo
    call monkey_business(monkeys)
    
    ! part 2
    rewind(read_unit)
    monkeys = [monkey ::]
    call get_monkeys(monkeys) ! read input puzzle

    max_mod=1
    do ii =1, nmonkeys
      max_mod = max_mod*monkeys(ii)%test_value
    enddo
    monkeys(:)%max_mod = max_mod
    monkeys(:)%part = 2

    do k= 1, 10000 ! 10k rounds
      call round(monkeys)
    enddo
    call monkey_business(monkeys)

    contains

    subroutine get_monkeys(monkeys)
      type(monkey), allocatable :: monkeys(:)

      do
        monkeys = [monkeys, monkey()]
        associate (mdata => monkeys(size(monkeys)))
          read(read_unit, *)
          read(read_unit, "(a18, a)") dummy, buffer
          nitems = count(transfer(trim(buffer), ['a']) == ",") + 1
          allocate(mdata%items(nitems))
          read(buffer, *) mdata%items
          read(read_unit, "(a23, a1, 1x, a)") dummy, mdata%operation, mdata%opval
          read(read_unit, *) dummy, dummy, dummy, mdata%test_value
          read(read_unit, *) dummy, dummy, dummy, dummy, dummy, mdata%if_true
          read(read_unit, *) dummy, dummy, dummy, dummy, dummy, mdata%if_false
          read(read_unit, "(a)", iostat=ios) dummy
          if (ios /= 0) exit
        end associate
      end do
    end subroutine get_monkeys

    subroutine monkey_business(monkeys)
      type(monkey) :: monkeys(:)
      integer(ip) :: topcount(2), i

      topcount(:) = 0
      do i=1, nmonkeys
        if (monkeys(i)%inspections_counter > topcount(1)) then
          topcount(2) = topcount(1)
          topcount(1) = monkeys(i)%inspections_counter
        else if (monkeys(i)%inspections_counter > topcount(2)) then
          topcount(2) = monkeys(i)%inspections_counter
        end if
      end do
      print *, "Monkey business = ",topcount(1)*topcount(2)
    end subroutine monkey_business

    subroutine round(monkeys)
      type(monkey) :: monkeys(:)
      integer(ip) :: item, opval
      integer :: i,j

      do i= 1, nmonkeys
        do j=1, size(monkeys(i)%items)
          monkeys(i)%inspections_counter = monkeys(i)%inspections_counter +1
          ! Inspect item
          item = monkeys(i)%items(j)
          ! Operation
          ! check for `old`
          if (monkeys(i)%opval == 'old') then
            opval = item
          else
            read(monkeys(i)%opval, *) opval
          endif
          ! check for `*` or `+`
          if (monkeys(i)%operation == "*") then
            item = item * opval
          else
            item = item + opval
          end if
          ! check which of the two parts shall be solved
          if (monkeys(i)%part == 1) then
            item = item / 3
          else
            item = modulo(item, monkeys(i)%max_mod)
          endif
          ! Test item
          if (modulo(item,monkeys(i)%test_value) == 0) then
            monkeys(monkeys(i)%if_true +1)%items = [monkeys(monkeys(i)%if_true +1)%items, item]
          else
            monkeys(monkeys(i)%if_false +1)%items = [monkeys(monkeys(i)%if_false +1)%items, item]
          endif
        enddo
        monkeys(i)%items = [ integer ::] ! empty the items of ith monkey
      enddo
    end subroutine round

end program day11

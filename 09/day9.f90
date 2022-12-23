
program day9
    implicit none

    integer, allocatable :: grid(:,:)
    integer, parameter :: read_unit = 99
    character(len=1) :: direction
    integer :: i, k, n, visited, ios
    integer,allocatable :: head(:), tail(:), rope(:,:)

    open(unit=read_unit, file='data.dat', iostat=ios)
    if ( ios /= 0 ) stop "Error opening file data.dat"

    ! set a grid size large enough to map the positions of the rope knots
    allocate(grid(-300:300,-300:300)) ! 2d cartesian grid)
    allocate (head(2),tail(2))
    head(:) = 0; tail(:) = 0; grid(:,:) = 0
    grid(0,0) = 1 ! head and tail both start at 0,0 position
    visited = 1 ! tail have begun visiting the start position
    do
      read (read_unit, *, iostat=ios) direction, n
      if (ios /= 0) exit
      ! move head `n` steps in the direction indicated
      select case (direction)
      case ('U')
        head = head + [0, n]
      case ('D')
        head = head + [0, -n]
      case ('L')
        head = head + [-n, 0]
      case ('R')
        head = head + [n, 0]
      end select
      ! move tail one step at time towards head until they are in contact
      do while (any(abs(head(:) - tail(:)) > 1))
       if (head(1) /= tail(1)) tail(1) = tail(1) + sign(1,head(1)-tail(1))
       if (head(2) /= tail(2)) tail(2) = tail(2) + sign(1,head(2)-tail(2))
       ! check if grid position has already been visited
       if (grid(tail(1),tail(2)) == 0) then
          grid(tail(1),tail(2)) = 1
          visited = visited+1
       endif
      end do
    end do

    print *, visited
    rewind(read_unit)
    allocate(rope(0:9,2), source=0) ! head -> rope(0,:) -- tail -> rope(9,:)
    grid(:,:) = 0 ! reset grid
    grid(0,0) = 1 ! head and tail both start at 0,0 position
    visited = 1 ! tail have begun visiting the start position
    do
      read (read_unit, *, iostat=ios) direction, n
      if (ios /= 0) exit
      do i=1,n
        ! move head one step at time in the direction indicated
        select case (direction)
        case('U')
          rope(0,2)=rope(0,2)+1
        case('D')
          rope(0,2)=rope(0,2)-1
        case('L')
          rope(0,1)=rope(0,1)-1
        case('R')
          rope(0,1)=rope(0,1)+1
        end select
        do k=1,9
          do while (any(abs(rope(k,:) - rope(k-1,:)) > 1))
            if (rope(k,1) /= rope(k-1,1)) rope(k,1) = rope(k,1) + sign(1,rope(k-1,1)-rope(k,1))
            if (rope(k,2) /= rope(k-1,2)) rope(k,2) = rope(k,2) + sign(1,rope(k-1,2)-rope(k,2))
            if ((k == 9) .and. grid(rope(9,1),rope(9,2)) == 0) then
               grid(rope(9,1),rope(9,2)) = 1
               visited = visited+1
            end if
          end do
        end do       
      end do
    end do
    print *, visited

end program day9

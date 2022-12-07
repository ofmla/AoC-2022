program day5

    implicit none
    integer :: i, ios, j, stack_line, ilen, nstacks, moves, istart, iend
    integer, parameter :: read_unit = 99, max_stacks = 50
    character(len=200) :: line
    character(len=4) :: str1, str2, str3
    character, allocatable :: stacks(:,:), stacks2(:,:)
    integer, allocatable :: top(:), top2(:)

    open(unit=read_unit, file='data.dat', iostat=ios)
    if ( ios /= 0 ) stop "Error opening file data.dat"

    stack_line = 0
    do
      read(read_unit, '(A)', iostat=ios) line
      if (line(1:1) .ne. "[") then
        ilen = len_trim(line)
        read (line(ilen:ilen), *) nstacks
        exit
      endif
      stack_line = stack_line +1
    end do

    allocate(stacks(-max_stacks:max_stacks, nstacks))
    allocate(stacks2(-max_stacks:max_stacks, nstacks))
    allocate(top (nstacks), top2(nstacks))
    
    top(:) = 0
    rewind(read_unit)
    do i = 1, stack_line
      read(read_unit, '(A)', iostat=ios) line
        do j=1, len_trim(line), 4
            if (line(j+1:j+1) /= " ") then
              stacks(i,j/4 + 1)= line(j+1:j+1)
              if (top(j/4 + 1) == 0) top(j/4 + 1) = i
            endif
        enddo
    enddo
    stacks2(:,:) = stacks(:,:); top2(:) = top(:)
    read(read_unit, *)
    read(read_unit, *)
    do
      read (read_unit,*,iostat=ios) str1, moves ,str2, istart, str3, iend
      if (ios /= 0) exit
      ! part 1
      do i=1,moves
        stacks(top(iend)-1,iend) = stacks(top(istart),istart)
        top(iend) = top(iend) - 1
        top(istart) = top(istart) + 1
      end do   
      ! part 2    
      stacks2(top2(iend)-moves:top2(iend)-1,iend) = stacks2(top2(istart):top2(istart)+(moves-1),istart)
      top2(iend) = top2(iend) - moves
      top2(istart) = top2(istart) + moves
    end do
    
    do i=1,nstacks
      write (*,'(A)',advance='no') stacks(top(i),i)
    end do
    write (*, '(A)') " "
    
    do i=1,nstacks
      write (*,'(A)',advance='no') stacks2(top2(i),i)
    end do
    write (*, '(A)') " "
    close(read_unit)

end program day5

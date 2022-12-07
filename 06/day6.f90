program day6

  implicit none
  integer :: i, ios, j, k, n
  integer, parameter :: read_unit = 99
  character(len=:), allocatable :: line

  open(unit=read_unit, file='data.dat', iostat=ios, form="unformatted", access='stream')
  if ( ios /= 0 ) stop "Error opening file data.dat"

  inquire (read_unit, size=n)
  allocate (character(len=n) :: line)
  read (read_unit) line
  close(read_unit)

  main : do i = 4, n
    do j = i-3, i
      do k = i-3, i
        if (j==k) cycle ! skip this
        if (line(j:j)==line(k:k)) cycle main !  compare with each other
      end do
    end do
    exit ! both loops exhausted means that the four characters are all different
  end do main
  
  print*, "Number characters processed: ", i
  
  main2 : do i = 14, n
    do j = i-13, i
      do k = i-13, i
        if (j==k) cycle
        if (line(j:j)==line(k:k)) cycle main2 
      end do
    end do
    exit
  end do main2
  
  print*, "Number characters processed: ", i

end program day6

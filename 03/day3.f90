program day3

  implicit none
  integer :: i, ilen, ios, iwhere, j, total, preference
  integer :: counts(3,52) ! priority ranges from 1 to 52
  integer, parameter :: read_unit = 99
  character(len=256) :: line
  character(len=1) :: item

  open(unit=read_unit, file='data.dat', iostat=ios)
  if ( ios .ne. 0 ) stop "Error opening file data.dat"

  total = 0 ! part 1
  do
    read(read_unit, '(A)', iostat=ios) line
    if (ios .ne. 0) exit
    ilen=len_trim(line)
    associate (left=>line(:ilen/2), right=>line(ilen/2+1:ilen))
    do i=1,ilen/2
      if(verify(left(i:i),right).eq.0)then
        total=total+get_priority(left(i:i))
        exit
      endif
    enddo
    end associate
  end do

  print*, "Sum of priorities of common items: ", total
  
  rewind(read_unit)
  
  total=0 ! part 2
  outer: do ! label the `do` statment this time
   counts(:,:) = 0
   do i=1,3
      read(read_unit, '(A)', iostat=ios) line
      if(ios .ne. 0) exit outer
      ilen=len_trim(line)
      do j=1,ilen
         iwhere= get_priority(line(j:j)) ! priority acts as index
         counts(i,iwhere)=min(1,counts(i,iwhere)+1) ! maintain a minimum count of 1
      enddo
   enddo
   ! determine the location (=priority) of 3 in the array returned by `sum` intrinsic
   total=total+ findloc(sum(counts,dim=1),3,dim=1)
  enddo outer
  
  print*, "Sum of priorities of items carried by groups of three elves: ", total
  
contains 

elemental function get_priority(item) result (priority)
  character(len=1), intent(in) :: item
  integer :: priority

! ASCII Printable Characters (128 characters, see https://www.w3schools.com/charsets/ref_html_ascii.asp)
! char - A number - 65, char - B number - 66 and so on
! char - a number - 97, char - b number - 98 and so on
! Set priorities as requested:
! Lowercase item types a through z have priorities 1 through 26.
! Uppercase item types A through Z have priorities 27 through 52.
  
  select case (item)
    case ('A':'Z')
	    priority = iachar(item)-65+27
    case ('a':'z')
      priority = iachar(item)-96
  end select
end function

end program day3















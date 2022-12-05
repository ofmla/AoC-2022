program day4

  implicit none
  integer :: ilen, ios, fieldsep, seppos, full_contained, pairs
  integer :: leftpair(2), rightpair(2)
  integer, parameter :: read_unit = 99
  character(len=256) :: line
  logical :: left_contained(2), right_contained(2)

  open(unit=read_unit, file='data.dat', iostat=ios)
  if ( ios .ne. 0 ) stop "Error opening file data.dat"

  full_contained = 0 
  pairs = 0 
  do
    read(read_unit, '(A)', iostat=ios) line
    if (ios .ne. 0) exit
    right_contained(:) = .false.; left_contained(:) = .false.
    ilen=len_trim(line)
    fieldsep = index(line, ",")
    associate (left=>line(:fieldsep), right=>line(fieldsep+1:ilen))
      seppos = index(left, "-")
      read (left(:seppos-1),*) leftpair(1)
      read (left(seppos+1:),*) leftpair(2)
      seppos = index(right, "-")
      read (right(:seppos-1),*) rightpair(1)
      read (right(seppos+1:),*) rightpair(2)
    
      left_contained(1) = rightpair(1) <= leftpair(1) .and. leftpair(1) <= rightpair(2)
      left_contained(2) = rightpair(1) <= leftpair(2) .and. leftpair(2) <= rightpair(2)
      right_contained(1) = leftpair(1) <= rightpair(1) .and. rightpair(1) <= leftpair(2)
      right_contained(2) = leftpair(1) <= rightpair(2) .and. rightpair(2) <= leftpair(2)
    
      if (all(left_contained) .or. all(right_contained)) then ! Check full containment (part 1)
        full_contained = full_contained + 1
      endif
      if (any(left_contained) .or. any(right_contained)) then ! Check partial overlap (part2)
        pairs = pairs +1
      endif
    end associate
  end do

  print*, "In ", full_contained, "pairs, one range is within the other" 
  print*, "In ", pairs, "pairs, there is an overlapping"

end program day4















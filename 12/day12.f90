
program day12
    implicit none

    integer, parameter :: read_unit = 99
    character(len=200) :: line
    integer, allocatable :: heightbuff(:), heights(:,:), dist(:,:)
    integer, allocatable :: startpos(:), endpos(:), candidate(:)
    logical, allocatable :: visited(:,:)
    integer :: i, ii, j, k, ios, ncols, nrows, newcost
    integer, parameter :: adjy(4) = [-1,1,0,0], adjx(4)=[0,0,1,-1]

    ! read input puzzle
    open(unit=read_unit, file='data.dat', iostat=ios)
    if ( ios /= 0 ) stop "Error opening file data.dat"

    read(read_unit, "(a)") line ! read first line as a string 
    rewind(read_unit)
    ncols = len_trim(line) ! use `len_trim` to get the size of grid in x direction
    nrows = 0 ! counter used to find the grid size in y direction
    heightbuff = [integer ::]
    do
      read(read_unit, "(a)", iostat=ios) line
      if (ios /= 0) exit
      nrows = nrows + 1
      ! use the array growing trick and `iachar` as in day 03 to get
      ! the code (integer) for the ASCII character read
      heightbuff = [heightbuff, [(iachar(line(ii:ii)) - iachar('a'), ii = 1, ncols)]]
    end do
    close(read_unit)

    heights = reshape(heightbuff, [nrows, ncols], order=[2, 1]) ! change the shape of heightbuff array
    startpos = findloc(heights, iachar('S') - iachar('a')) 
    endpos = findloc(heights, iachar('E') - iachar('a'))
    heights(startpos(1), startpos(2)) = 0 ! Start position (S) has elevation a (lowercase a - lowercase a =0)
    heights(endpos(1), endpos(2)) = 25 ! final position (E) has elevation z (lowercase z - lowercase a =122-97)

    allocate (dist(nrows,ncols), visited(nrows,ncols))
    dist(:,:) = huge(1) ! set entire grid with the highest possible integer value
    visited (:,:) = .true. ! set entire grid to .true. for convenience
    dist(startpos(1), startpos(2)) = 0 ! start position has 0 dist

    do
      candidate = minloc(dist,mask=visited)
      if (all(candidate == endpos)) exit
      i = candidate(1)
      j = candidate(2)
      visited(i,j) = .false.
      do k=1,4 ! move left, right, up and down
          ! check if new position is within the limits allowed
          if ((i+adjy(k) < 1) .or. (i+adjy(k) > nrows)) cycle
          if ((j+adjx(k) < 1) .or. (j+adjx(k) > ncols)) cycle
          if (visited(i+adjy(k),j+adjx(k))) then 
            ! new position has not yet been visited
            if (heights(i+adjy(k),j+adjx(k))-heights(i,j) <= 1) then
              ! the difference in elevation between the two positions can not be greater than one
              newcost = dist(i,j) + 1
              if (newcost < dist(i+adjy(k),j+adjx(k))) then
                dist(i+adjy(k),j+adjx(k)) = newcost
              end if
            end if
          end if       
      end do
    end do
    print *, dist(endpos(1),endpos(2))

    ! part 2
    dist(:,:) = huge(1) ! set entire grid with the highest possible integer value again
    visited (:,:) = .true. ! set entire grid to .true. for convenience

    ! made the opposite path, reach the closest `a` from `E`
    dist(endpos(1), endpos(2)) = 0 ! end position has 0 dist and became the starting point
    do
      candidate = minloc(dist,mask=visited)
      if (heights(candidate(1),candidate(2)) == 0) then
        endpos(:) = candidate(:)
        exit
      end if
      i = candidate(1)
      j = candidate(2)
      visited(i,j) = .false.
      do k=1,4 ! move left, right, up and down
          ! check if new position is within the limits allowed
          if ((i+adjy(k) < 1) .or. (i+adjy(k) > nrows)) cycle
          if ((j+adjx(k) < 1) .or. (j+adjx(k) > ncols)) cycle
          if (visited(i+adjy(k),j+adjx(k))) then
            ! new position has not yet been visited
            if (heights(i,j) - heights(i+adjy(k),j+adjx(k)) <= 1) then ! reverse comparison
              ! the difference in elevation between the two positions can not be greater than one
              newcost = dist(i,j) + 1
              if (newcost < dist(i+adjy(k),j+adjx(k))) then
                dist(i+adjy(k),j+adjx(k)) = newcost
              end if
            end if
          end if
      end do
    end do
    print *, dist(endpos(1),endpos(2))

end program day12


program day8
    implicit none

    ! a quick inspection of the file allows us to deduce the size of the grid of integers
    integer, parameter :: nrows=99,ncols=99
    integer, parameter :: read_unit = 99
    integer :: trees(nrows,ncols),icol, irow, i, ii, k, i_reverse, ios
    integer :: max_height_col(ncols), max_height_row(nrows), maxscore, score
    integer :: up, down, left, right
    logical :: visible(nrows,ncols), reverse

    open(unit=read_unit, file='data.dat', iostat=ios)
    if ( ios /= 0 ) stop "Error opening file data.dat"

    visible(:,:) = .false. ! set full array as .false.
    do i=1,nrows
        read (read_unit,'(*(I1))') trees(i,:) ! read each line of file as an array of integers
    end do

    reverse = .false.
    do i_reverse=1,2
        if (i_reverse == 2) reverse = .true.
        max_height_row(:) = -1
        max_height_col(:) = -1
        do i = 1, nrows
            do ii =1, ncols
                irow = i; icol = ii
                if (reverse) then ! new indexes, right-->left and bottom-->top
                    irow = nrows - i + 1; icol = ncols - ii + 1
                endif
                if (trees(irow,icol) > max_height_col(icol)) then
                    visible(irow,icol) = .true.
                    max_height_col(icol) = trees(irow,icol)
                endif
                if (trees(irow,icol) > max_height_row(irow)) then
                    visible(irow,icol) = .true.
                    max_height_row(irow) = trees(irow,icol)
                endif            
            enddo
        enddo
    enddo

    ! Count occurrences of .true. in visible array
    print*, "trees visible: ", count(visible)

    maxscore = 0 ! initialize maximum score
    ! scenic scores at edges is zero as at least one of its viewing distances is zero
    do i = 2, nrows-1
        do ii = 2, ncols-1
            up = 0; down = 0; left = 0; right = 0 ! initialize viewing distances for each interior point
            do k = i-1,1,-1  !up
                up = up + 1
                if (trees(k,ii)>=trees(i,ii)) exit
            end do
            do k = i+1,nrows !down
                down = down + 1
                if (trees(k,ii)>=trees(i,ii)) exit
            end do
            do k = ii-1,1,-1 !left
                left = left + 1
                if (trees(i,k)>=trees(i,ii)) exit
            end do
            do k = ii+1,ncols !right
                right = right + 1
                if (trees(i,k)>=trees(i,ii)) exit
            end do
            score = down*up*left*right ! compute score
            ! if greater than the maximum until then found, update maximum
            if (score>maxscore) maxscore = score 
        end do
    end do
    print*, "highest scenic score: ", maxscore

end program day8
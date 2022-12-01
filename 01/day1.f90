program day1

    implicit none
    integer :: ios
    integer, parameter :: read_unit = 99
    character(len=200) :: line
    integer :: count, number, top(3)

    open(unit=read_unit, file='data.dat', iostat=ios)
    if ( ios /= 0 ) stop "Error opening file data.dat"

    top(:) = 0
    do
        count = 0
        do
            read(read_unit, '(A)', iostat=ios) line
            if (len_trim(line) == 0 .or. ios /= 0 ) exit
            read (line,'(I10)') number
            count = count + number
        end do
        if (count > top(1)) then
            if (count > top(3)) then
                top(1) = top(2)
                top(2) = top(3)
                top(3) = count
            else if (count > top(2)) then
                top(1) = top(2)
                top(2) = count
            else
                top(1) = count
            end if     
        end if
        if (ios /= 0 ) exit
    end do

    print*, "highest calories value carried by an elf is: ", top(3)
    print*, "total number of calories carried by top three Elves: ", sum(top)

    close(read_unit)

end program day1

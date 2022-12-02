program day2

    implicit none
    integer :: ios, score1, score2
    integer, parameter :: read_unit = 99
    character(len=3) :: line

    open(unit=read_unit, file='data.dat', iostat=ios)
    if ( ios /= 0 ) stop "Error opening file data.dat"

    score1 = 0; score2 =0
    do
        read(read_unit, '(A)', iostat=ios) line
        if (ios /= 0 ) exit
        score1 = score1 + original_strategy(line)
        score2 = score2 + modified_strategy(line)
    end do

    print*, "the score according to original strategy guide is: ", score1
    print*, "the score according to the modified strategy guide is: ", score2

contains

elemental function original_strategy(line) result(score)
character(3), intent(in) :: line
integer :: score

! A = X = `rock`; B = Y = `paper`; C = Z = `scissors`
! shape scores => rock = 1, paper = 2, scissors = 3
! round score => win = 6, draw = 3, lose = 0

    select case (line)
    case ('A Y', 'B Z', 'C X')
        score = 6
        if (line(3:3) == 'X') then
            score = score + 1
        else if (line(3:3) == 'Y') then
            score = score + 2
        else
            score = score + 3
        endif
    case ('A X', 'B Y', 'C Z')
        score = 3
        if (line(3:3) == 'X') then
            score = score + 1
        else if (line(3:3) == 'Y') then
            score = score + 2
        else
            score = score + 3
        endif
    case default
        score = 0
        if (line(3:3) == 'X') then
            score = score + 1
        else if (line(3:3) == 'Y') then
            score = score + 2
        else
            score = score + 3
        endif
    end select
end function original_strategy

elemental function modified_strategy(line) result(score)
character(3), intent(in) :: line
integer :: score

! A = X = `rock`; B = Y = `paper`; C = Z = `scissors`
! shape scores => rock = 1, paper = 2, scissors = 3
! round score => win = 6, draw = 3, lose = 0

    select case (line(3:3))
    case ('Y') ! draw
        score = 3
        if (line(1:1) == 'A') then
            score = score + 1
        else if (line(1:1) == 'B') then
            score = score + 2
        else
            score = score + 3
        endif
    case ('Z') ! win
        score = 6
        if (line(1:1) == 'A') then
            score = score + 2
        else if (line(1:1) == 'B') then
            score = score + 3
        else
            score = score + 1
        endif
    case default ! lose
        score = 0
        if (line(1:1) == 'A') then
            score = score + 3
        else if (line(1:1) == 'B') then
            score = score + 1
        else
            score = score + 2
        endif
    end select
end function modified_strategy

end program day2
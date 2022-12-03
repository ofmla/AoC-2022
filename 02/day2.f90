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
        score = score + conditional1(line(3:3))
    case ('A X', 'B Y', 'C Z')
        score = 3
        score = score + conditional1(line(3:3))
    case default
        score = 0
        score = score + conditional1(line(3:3))
    end select

end function original_strategy

elemental function modified_strategy(line) result(score)
character(3), intent(in) :: line
integer :: score

! A = X = `rock`; B = Y = `paper`; C = Z = `scissors`
! shape scores => rock = 1, paper = 2, scissors = 3
! round score => win = 6, draw = 3, lose = 0

    select case (line)
    case ('A X', 'C Y', 'B Z')
        score = 3
        score = score + conditional2(line(3:3))
    case ('C X', 'B Y', 'A Z')
        score = 2
        score = score + conditional2(line(3:3))
    case default
        score = 1
        score = score + conditional2(line(3:3))
    end select

end function modified_strategy

elemental function conditional1(line) result(score)
character(1), intent(in) :: line
integer ::  score

if (line == 'X') then
    score = 1
else if (line == 'Y') then
    score = 2
else
    score = 3
endif

end function conditional1

elemental function conditional2(line) result(score)
character(1), intent(in) :: line
integer ::  score

if (line == 'X') then ! X => lose
    score = 0
else if (line == 'Y') then ! Y => draw
    score = 3
else ! Z => win
    score = 6 
endif

end function conditional2

end program day2
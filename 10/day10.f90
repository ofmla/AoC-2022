
program day10
    implicit none

    integer, parameter :: read_unit = 99
    character(len=200) :: line
    integer :: i, ios, totalcycles, sumsix_ss, addx, x, ipixel
    character :: rowpixels(0:39)

    open(unit=read_unit, file='data.dat', iostat=ios)
    if ( ios /= 0 ) stop "Error opening file data.dat"

    sumsix_ss = 0
    totalcycles = 0
    x = 1 ! register starts with the value 1
    do
      read (read_unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      ! Check if substring is "addx" or "noop"
      if (line(1:4) == "addx") then
        read(line(5:), *) addx ! read value to be added to `x` register
        do i = 1, 2 ! 2 cycles to complete
         totalcycles = totalcycles + 1
         if (modulo(totalcycles, 40) == 20) sumsix_ss = sumsix_ss + totalcycles * x
        end do
        x = x + addx ! add value after last cycle
      else
         totalcycles = totalcycles + 1
         if (modulo(totalcycles, 40) == 20) sumsix_ss = sumsix_ss + totalcycles * x
      end if
    end do

    print *, sumsix_ss

    rewind(read_unit)
    sumsix_ss = 0
    totalcycles = 0
    ipixel = 0
    x = 1 ! register starts with the value 1
    rowpixels = '.' ! ........................................
    do
      read (read_unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      ! Check if substring is "addx" or "noop"
      if (line(1:4) == "addx") then
        read(line(5:), *) addx ! read value to be added to `x` register
        do i = 1, 2 ! 2 cycles to complete
         totalcycles = totalcycles + 1
         if (any(ipixel==[x-1,x,x+1])) rowpixels(ipixel) = '#'
         ipixel = ipixel + 1
         if (ipixel>39) then 
          write (*,'(40A1)') rowpixels
          ipixel = 0; rowpixels = '.' ! reset location of pixel and rowpixels
         end if
        end do
        x = x + addx ! add value after last cycle
      else
         totalcycles = totalcycles + 1
         if (any(ipixel==[x-1,x,x+1])) rowpixels(ipixel) = '#'
         ipixel = ipixel + 1
         if (ipixel>39) then
          write (*,'(40A1)') rowpixels
          ipixel = 0; rowpixels = '.'
         end if
      end if
    end do

end program day10

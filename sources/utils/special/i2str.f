      function i2str(i) result(str)
      ! Original author: Y. Audouin

      implicit none

      integer, intent(in) :: i
      character(len=10) :: str

      str = repeat(' ',20)
      if (i.lt.10) then
        write(str,'(I1)') i
      else if (i.lt.100) then
        write(str,'(I2)') i
      else if (i.lt.1000) then
        write(str,'(I3)') i
      else if (i.lt.10000) then
        write(str,'(I4)') i
      else if (i.lt.100000) then
        write(str,'(I5)') i
      else if (i.lt.1000000) then
        write(str,'(I6)') i
      else if (i.lt.10000000) then
        write(str,'(I7)') i
      else if (i.lt.100000000) then
        write(str,'(I8)') i
      else if (i.lt.1000000000) then
        write(str,'(I9)') i
      else
        write(str,'(I10)') i
      endif

      end function i2str


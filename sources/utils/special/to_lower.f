      function to_lower(strIn) result(strOut)
      ! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May
      ! 2012)
      ! Original author: Clive Page

      implicit none

      character(len=*), intent(in) :: strIn
      character(len=len(strIn)) :: strOut
      integer :: i,j

      do i = 1, len(strIn)
        j = iachar(strIn(i:i))
        if (j>= iachar("A") .and. j<=iachar("Z") ) then
          strOut(i:i) = achar(iachar(strIn(i:i))+32)
        else
          strOut(i:i) = strIn(i:i)
        end if
      end do

      end function to_lower

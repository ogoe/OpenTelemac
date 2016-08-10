      SUBROUTINE  ErrMsgAndStop                  !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (  ErrMsge, mark, SR, ipid      )

      USE m_TypeDefs_Nestor

      IMPLICIT NONE
      INTEGER  , INTENT(IN)      :: mark              ! position of error e.g.: number of line
      INTEGER  , INTENT(IN)      :: ipid              ! number of parallel thread where the error occured
!      INTEGER  , INTENT(IN)      :: LenA, LenB, LenSR ! length of string
!     CHARACTER            (128) :: ErrMsgA, ErrMsgB  ! message strings
!     CHARACTER            (128) :: SR            ! subroutine where the error occured
      TYPE(t_String_Length) :: SR     ! subroutine where the error occured
      TYPE(t_String_Length) :: ErrMsge! error message
      !------- local variables ---------------
      INTEGER                    ::    LenC
      CHARACTER            (256) :: ErrMsgC

669   FORMAT(' ?>',9(/,' ?> error:'))             ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
668   FORMAT(5(' ?> error:',/))             ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
661   FORMAT(1(' ?> error:'))               ! write 1 pseudo empty lines like " ?> error:         "                                                                  !


670   FORMAT( 1(' ?> error:'),60('='), '+' )
!     671   FORMAT( 1(' ?> error:'),60('-'), '+' )
672   FORMAT( 1(' ?> error:'),60(' '), '|' )
673   FORMAT( 1(' ?> error:'),11(' '),29A,27(' '),'|' )

      WRITE(6,*)'?>-------  SR ErrMsgAndStop ----------------'
      ErrMsgC = "error in dredge module Nestor"
      ErrMsgC = ErrMsgC(1:29)//"                    |"
      LenC = 29 + 24
      WRITE(6,669)
      WRITE(6,670)
      WRITE(6,672)
      WRITE(6,673)   ErrMsgC(1:LenC)
      WRITE(6,672)
      WRITE(6,672)

      ErrMsgC =
     &" ?> error: occured in Subroutine        "//SR%s(1:SR%i)
      LenC = 40 + SR%i
      WRITE(6,'(A,1X)' ) ErrMsgC(1:LenC)

      ErrMsgC = " ?> error: occured in parallel thread   "
      WRITE(6,'(A,1X,I3)' ) ErrMsgC(1:40), ipid
      WRITE(6,661)

      ErrMsgC = " ?> error:  "//ErrMsge%s(1:ErrMsge%i)
      LenC = 13 + ErrMsge%i

      IF( mark == -1 ) THEN
        WRITE(6,'(A,1X)' ) ErrMsgC(1:LenC)
      ELSE
        WRITE(6,'(A,1X,I4)' ) ErrMsgC(1:LenC), mark
      ENDIF
      WRITE(6,661)
      WRITE(6,661)
      WRITE(6,661)
      WRITE(6,672)
      WRITE(6,672)
      WRITE(6,670)
      WRITE(6,668)

      CALL my_FLUSH(6)      ! ACHTUNG     As FLUSH() is non-standard, you may need to add
                         !             a USE statement, or link with a special library
      STOP

      WRITE(6,*)'?>-------  SR ErrMsgAndStop End ------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE ErrMsgAndStop               !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************



!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************

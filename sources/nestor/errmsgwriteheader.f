      SUBROUTINE  ErrMsgWriteHeader              !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( SR, ipid      )

      USE m_TypeDefs_Nestor


      IMPLICIT NONE
      INTEGER  , INTENT(IN)      :: ipid                    ! number of parallel thread where the error occured
      TYPE(t_String_Length) :: SR     ! subroutine where the error occured
      !------- local variables ---------------
      INTEGER         :: LenT
      CHARACTER (256) :: StrT
      CHARACTER   (8) :: char_ipid   ! to store the value of ipid as string

669   FORMAT(' ?>',9(/,' ?> error:'))       ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
!     668   FORMAT(5(' ?> error:',/))             ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
661   FORMAT(1(' ?> error:'))               ! write 1 pseudo empty lines like " ?> error:         "                                                                  !


670   FORMAT( 1(' ?> error:'),60('='), '+' )
!     671   FORMAT( 1(' ?> error:'),60('-'), '+' )
672   FORMAT( 1(' ?> error:'),60(' '), '|' )
673   FORMAT( 1(' ?> error:'),11(' '),29A,27(' '),'|' )


      WRITE( char_ipid, '(I8)') ipid

      WRITE(6,*)'?>-------  SR ErrMsgWriteHeader ------------'
      StrT = "error in dredge module Nestor"
      StrT = StrT(1:29)//"                    |"
      LenT = 29 + 24
      WRITE(6,669)
      WRITE(6,670)
      WRITE(6,672)
      WRITE(6,673)   StrT(1:LenT)
      WRITE(6,672)
      WRITE(6,672)

      StrT = " ?> error:  occured in Subroutine       "//SR%s(1:SR%i)
      LenT = 40 + SR%i
      WRITE(6,'(A,1X)' ) StrT(1:LenT)

      StrT = " ?> error:  occured in parallel thread  "
      WRITE(6,'(A,A8)' ) StrT(1:40), adjustl(char_ipid)
      WRITE(6,661)

!
      CALL my_FLUSH(6)      ! ACHTUNG     As FLUSH() is non-standard, you may need to add
                         !             a USE statement, or link with a special library

      WRITE(6,*)'?>-------  SR ErrMsgWriteHeader END --------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE ErrMsgWriteHeader           !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************


!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************

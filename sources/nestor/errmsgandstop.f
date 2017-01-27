!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ErrMsgAndStop                  !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (  MsgA,LenA, MsgB,LenB, MsgC,LenC, MsgD,LenD
     &  , mark, SR, ipid      ) 

      USE m_TypeDefs_Nestor
      
         
      
#ifndef  NESTOR_INTERFACES                                        
      USE m_Interfaces_Nestor, ONLY :  my_FLUSH 
#endif   NESTOR_INTERFACES                                        

                                                                                                                              
      IMPLICIT NONE                                                                                                        
      INTEGER  , INTENT(IN)      :: mark                    ! position of error e.g.: number of line
      INTEGER  , INTENT(IN)      :: ipid                    ! number of parallel thread where the error occured
      INTEGER  , INTENT(IN)      :: LenA, LenB, LenC, LenD  ! length of string A, of string B, of ... 
      CHARACTER          (len=*) :: MsgA, MsgB, MsgC, MsgD  ! message strings
      TYPE(t_String_Length)      :: SR                      ! subroutine where the error occured 
      
#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------
      INTEGER         :: LenT
      CHARACTER (256) :: StrT 
      CHARACTER   (8) :: char_ipid   ! to store the value of ipid as string
                                         
669   FORMAT(' ?>',9(/,' ?> error:'))       ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
668   FORMAT(5(' ?> error:',/))             ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
661   FORMAT(1(' ?> error:'))               ! write 1 pseudo empty lines like " ?> error:         "                                                                  !


670   FORMAT( 1(' ?> error:'),60('='), '+' ) 
!     671   FORMAT( 1(' ?> error:'),60('-'), '+' ) 
672   FORMAT( 1(' ?> error:'),60(' '), '|' ) 
673   FORMAT( 1(' ?> error:'),11(' '),29A,27(' '),'|' ) 


      WRITE( char_ipid, '(I8)') ipid

!      dbug WRITE(6,*)'?>-------  SR ErrMsgAndStop ----------------' 
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

      StrT = " ?> error:  "//MsgA(1:LenA)
      LenT = 13 + LenA            
      WRITE(6,'(A,1X)' ) StrT(1:LenT)
      StrT = " ?> error:  "//MsgB(1:LenB)
      LenT = 13 + LenB            
      WRITE(6,'(A,1X)' ) StrT(1:LenT)
      StrT = " ?> error:  "//MsgC(1:LenC)
      LenT = 13 + LenC            
      WRITE(6,'(A,1X)' ) StrT(1:LenT)
      StrT = " ?> error:  "//MsgD(1:LenD)
      LenT = 13 + LenD            
      
      IF( mark == -1 ) THEN            
        WRITE(6,'(A,1X)' ) StrT(1:LenT)
      ELSE                               
        WRITE(6,'(A,1X,I5)' ) StrT(1:LenT), mark
      ENDIF
      WRITE(6,661) 
      WRITE(6,661) 
      WRITE(6,661) 
      WRITE(6,672) 
      WRITE(6,672) 
      WRITE(6,670) 
      WRITE(6,668)
      
      CALL my_FLUSH(6)   ! ACHTUNG     As FLUSH() is non-standard, you may need to add 
                         !             a USE statement, or link with a special library 
                         
!      IF( ParallelComputing ) CALL P_SYNC()
                         
      STOP                                                                                        
                                                                                             
!      dbug WRITE(6,*)'?>-------  SR ErrMsgAndStop End ------------'                                
      RETURN                                                                                  
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE ErrMsgAndStop               !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
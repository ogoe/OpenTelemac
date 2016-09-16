      FUNCTION  ThreeDigitsNumeral               !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( checkStr )         

      ! check if the first thee digits in a character String are numerals 
      
      IMPLICIT NONE
      
      CHARACTER (3) :: checkStr
      INTEGER       :: stat, sumStat, idummy 
      LOGICAL       :: ThreeDigitsNumeral
      !WRITE(6,*)'?>-------  SR ThreeDigitsNumeral -----------'

      READ( checkStr(1:1),'(I1)',IOSTAT=stat ) idummy   ! read the 1st string element as integer
      sumStat = ABS(stat)
      READ( checkStr(2:2),'(I1)',IOSTAT=stat ) idummy   ! read the 2nd string element as integer
      sumStat = sumStat + ABS(stat)
      READ( checkStr(3:3),'(I1)',IOSTAT=stat ) idummy   ! read the 3rd string element as integer
      sumStat = sumStat + ABS(stat)
      
      IF( sumStat == 0 ) THEN
        ThreeDigitsNumeral = .TRUE.
      ELSE       
        ThreeDigitsNumeral = .FALSE.
      ENDIF  

      IF(     checkStr(1:1) == " "        !> We need this check because lines above  
     &   .OR. checkStr(2:2) == " "        !  READ(..,'(I1)',IOSTAT=stat) returns stat=0 
     &   .OR. checkStr(3:3) == " " ) THEN !  although there was a space character 
        ThreeDigitsNumeral = .FALSE.
      ENDIF  

      
      !WRITE(6,*)'?>-------  SR ThreeDigitsNumeral End -------'
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
      END FUNCTION   ThreeDigitsNumeral          !********************************************
!     END SUBROUTINE ThreeDigitsNumeral          !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
 
 
!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
!     SUBROUTINE  DateStringToSeconds            !********************************************

      SUBROUTINE  ParseSteerLine                 !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (line, KeyWord, valueStr)


      USE m_TypeDefs_Nestor


      IMPLICIT NONE
      CHARACTER (128), INTENT(IN)  :: line
      CHARACTER (128), INTENT(OUT) :: KeyWord
      CHARACTER (128), INTENT(OUT) :: valueStr
      INTEGER                      :: sepPos  !position of seperator "="
      CHARACTER (128)              :: str
      !INTEGER         :: intKey

      !LOGICAL         :: ThreeDigitsNumeral   ! function

      TYPE(t_String_Length) :: SRname     ! subroutine where the error occured

      !WRITE(6,*)'?>-------  SR ParseSteerLine ---------------'
      SRname%s = "ParseSteerLine"     ! subroutine name
      SRname%i =  LEN_TRIM(SRname%s)  ! length of name string

      !WRITE(6,*) 'line = >',line, '<'
      str    = line
      sepPos = INDEX(line,"/")
      IF( sepPos > 0) str = line(1:sepPos-1) !    "Keywordbalbla   =  valueblabla    / a comment    "
                                             ! -> "Keywordbalbla   =  valueblabla    "
      !WRITE(6,*)'str = >', str,'<'

      sepPos = INDEX(str,"=")

      KeyWord  = str(1:sepPos-1)              ! -> "Keywordbalbla   "
      valueStr = str(sepPos+1:LEN_TRIM(str))  ! -> "  valueblabla"
      valueStr = ADJUSTL(valueStr)            ! -> "valueblabla  "

      !KeyWord  = KeyWord(1:LEN_TRIM(KeyWord))


      !WRITE(6,*) 'KeyWord = >', KeyWord,'<'
      !WRITE(6,*) 'KeyWord = >', KeyWord(1:LEN_TRIM(KeyWord)),'<'
      !WRITE(6,*) 'valueStr = >', valueStr(1:LEN_TRIM(valueStr)),'<'


      CALL my_FLUSH(6)





      !WRITE(6,*)'?>-------  SR ParseSteerLine End -----------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE ParseSteerLine              !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************


!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************

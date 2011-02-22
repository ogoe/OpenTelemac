C                       ***************************
                        CHARACTER*3 FUNCTION EXTEN1
C                       ***************************
C
     *(N)
C
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: N
C
C-----------------------------------------------------------------------
C
      IF(N.GT.0) THEN
C
        EXTEN1='000'
C
        IF(N.LT.10) THEN
          WRITE(EXTEN1(03:03),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTEN1(02:03),'(I2)') N
        ELSE
          WRITE(EXTEN1(01:03),'(I3)') N
        ENDIF
C
      ELSE
C
        EXTEN1='   '
C
      ENDIF
C
C
      RETURN
      END FUNCTION

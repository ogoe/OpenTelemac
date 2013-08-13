!                       ***************************
                        CHARACTER*3 FUNCTION EXTEN1
!                       ***************************
!
     &(N)
!
!**********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: N
!
!-----------------------------------------------------------------------
!
      IF(N.GT.0) THEN
!
        EXTEN1='000'
!
        IF(N.LT.10) THEN
          WRITE(EXTEN1(03:03),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTEN1(02:03),'(I2)') N
        ELSE
          WRITE(EXTEN1(01:03),'(I3)') N
        ENDIF
!
      ELSE
!
        EXTEN1='   '
!
      ENDIF
!
!
      RETURN
      END FUNCTION
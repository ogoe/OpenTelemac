C                       ***************************
                        CHARACTER*7 FUNCTION EXTEN2
C                       ***************************
C
     *(N,IPID)
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: IPID,N
C
C-----------------------------------------------------------------------
C
      IF(N.GT.0) THEN
C
        EXTEN2='000-000'
C
        IF(N.LT.10) THEN
          WRITE(EXTEN2(03:03),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTEN2(02:03),'(I2)') N
        ELSE
          WRITE(EXTEN2(01:03),'(I3)') N
        ENDIF
C
        IF(IPID.LT.10) THEN
          WRITE(EXTEN2(07:07),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(EXTEN2(06:07),'(I2)') IPID
        ELSE
          WRITE(EXTEN2(05:07),'(I3)') IPID
        ENDIF
C
      ELSE
C
        EXTEN2='       '
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END FUNCTION


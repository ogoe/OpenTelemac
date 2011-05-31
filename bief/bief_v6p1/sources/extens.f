!                    *********************************
                     CHARACTER(LEN=11) FUNCTION EXTENS
!                    *********************************
!
     &(N,IIPID)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE EXTENSION FOR NAMING FILES IN PARALLEL
!+
!
!history  J-M HERVOUET (LNHE)
!+        11/07/2008
!+        V5P9
!+  
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| N              |-->| TOTAL NUMBER OF PROCESSORS
!| IIPID          |-->| RANK OF THE PROCESSOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: IIPID,N
!
!-----------------------------------------------------------------------
!
      IF(N.GT.0) THEN
!
        EXTENS='00000-00000'
!
        IF(N.LT.10) THEN
          WRITE(EXTENS(05:05),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTENS(04:05),'(I2)') N
        ELSEIF(N.LT.1000) THEN
          WRITE(EXTENS(03:05),'(I3)') N
        ELSEIF(N.LT.10000) THEN
          WRITE(EXTENS(02:05),'(I4)') N
        ELSE
          WRITE(EXTENS(01:05),'(I5)') N
        ENDIF
!
        IF(IPID.LT.10) THEN
          WRITE(EXTENS(11:11),'(I1)') IIPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(EXTENS(10:11),'(I2)') IIPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(EXTENS(09:11),'(I3)') IIPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(EXTENS(08:11),'(I4)') IIPID
        ELSE
          WRITE(EXTENS(07:11),'(I5)') IIPID
        ENDIF
!
      ELSE
!
        EXTENS='       '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

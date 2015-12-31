!                    ************************
                     SUBROUTINE GET_BND_IPOBO
!                    ************************
!
     &(FFORMAT,FID,NPOIN,NELEBD,TYP_BND_ELEM,IPOBO,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns an array containing
!+        1 if a point is a boundary point 0 otherwise
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| NELEBD         |-->| TOTAL NUMBER OF BOUNDARY ELEMENTS
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENT
!| IPOBO          |<->| AN ARRAY CONTAINING
!|                |   | 1 IF A POINT IS A BOUNDARY POINT 0 OTHERWISE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN
      USE UTILS_MED
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FID, NPOIN, NELEBD, TYP_BND_ELEM
      INTEGER,          INTENT(INOUT) :: IPOBO(NPOIN)
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF((TYP_BND_ELEM.EQ.TYPE_NULL).OR.(NELEBD.EQ.0)) THEN
        IPOBO(:) = 0
        RETURN
      ENDIF
!
      SELECT CASE (FFORMAT)
        CASE ('SERAFIN ','SERAFIND')
          CALL GET_BND_IPOBO_SRF(FID, TYP_BND_ELEM,NPOIN,IPOBO, IERR)
        CASE ('MED     ')
          CALL GET_BND_IPOBO_MED(FID,TYP_BND_ELEM,NPOIN,NELEBD,IPOBO,
     &      IERR)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'GET_BND_IPOBO : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'GET_BND_IPOBO: BAD FILE FORMAT: ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

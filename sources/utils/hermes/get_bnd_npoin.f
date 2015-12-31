!                    ************************
                     SUBROUTINE GET_BND_NPOIN
!                    ************************
!
     &(FFORMAT,FID,TYPE_BND_ELEM,NPTFR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of boundary points
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| TYPE_BND_ELEM  |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NPTFR          |<->| NUMBER OF BOUNDARY POINTS
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
      INTEGER,          INTENT(IN)  :: FID
      INTEGER,          INTENT(IN)  :: TYPE_BND_ELEM
      INTEGER,          INTENT(INOUT) :: NPTFR
      INTEGER,          INTENT(OUT) :: IERR
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(TYPE_BND_ELEM.EQ.TYPE_NULL) THEN
        NPTFR = 0
        IERR = 0
        RETURN
      ENDIF
!
      SELECT CASE (FFORMAT)
        CASE ('SERAFIN ','SERAFIND')
          CALL GET_BND_NPOIN_SRF(FID, TYPE_BND_ELEM, NPTFR,IERR)
        CASE ('MED     ')
          CALL GET_BND_NPOIN_MED(FID,TYPE_BND_ELEM,NPTFR,IERR)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'GET_BND_NPOIN : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'GET_BND_NPOIN: BAD FILE FORMAT: ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

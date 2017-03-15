!                    ************************
                     SUBROUTINE GET_BND_NELEM
!                    ************************
!
     &(FFORMAT,FID,TYPE_BND_ELEM,NELEM,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Reads the number of boundary elements
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
!| NELEM          |<->| NUMBER OF BOUNDARY ELEMENTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN
      USE UTILS_MED
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FID
      INTEGER,          INTENT(IN)  :: TYPE_BND_ELEM
      INTEGER,          INTENT(INOUT) :: NELEM
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(TYPE_BND_ELEM.EQ.TYPE_NULL) THEN
        NELEM = 0
        RETURN
      ENDIF
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_BND_NELEM_SRF(FID, TYPE_BND_ELEM, NELEM, IERR)
        CASE ('MED    ')
          CALL GET_BND_NELEM_MED(FID, TYPE_BND_ELEM, NELEM, IERR)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'GET_BND_NELEM : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'GET_BND_NELEM: BAD FILE FORMAT: ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

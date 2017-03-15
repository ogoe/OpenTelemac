!                    ************************
                     SUBROUTINE GET_DATA_TIME
!                    ************************
!
     &(FFORMAT,FID,RECORD,TIME,IERR)
!
!***********************************************************************
! HERMES   V7P1
!***********************************************************************
!
!brief    Returns the time value of a given time step
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P1
!+   First version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| RECORD         |-->| NUMBER OF THE TIME STEP
!| TIME           |<->| TIME IN SECOND OF THE TIME STEP
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
      INTEGER,          INTENT(IN)  :: RECORD
      DOUBLE PRECISION, INTENT(INOUT) :: TIME
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_DATA_TIME_SRF(FID,RECORD,TIME,IERR)
        CASE ('MED    ')
          CALL GET_DATA_TIME_MED(FID,RECORD,TIME,IERR)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'GET_DATA_TIME : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'GET_DATA_TIME: BAD FILE FORMAT: ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END



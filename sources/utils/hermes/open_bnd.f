!                    ********************
                     SUBROUTINE OPEN_BND
!                    ********************
!
     &(FFORMAT,FILE_NAME,FILE_ID,OPENMODE,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    OPENS A BOUNDARY FILE
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_NAME      |-->| NAME OF THE BOUNDARY FILE
!| FILE_ID        |-->| FILE DESCRIPTOR OF THE "MESH" FILE
!| OPENMODE       |-->| ONE OF THE FOLLOWING VALUE 'READ','WRITE','READWRITE'
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
      CHARACTER(LEN=8), INTENT(IN)   :: FFORMAT
      CHARACTER(LEN=*), INTENT(IN)   :: FILE_NAME
      INTEGER,          INTENT(IN)   :: FILE_ID
      CHARACTER(LEN=9), INTENT(IN)   :: OPENMODE
      INTEGER, INTENT(OUT)           :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL OPEN_BND_SRF(FILE_NAME, FILE_ID, OPENMODE, IERR)
        CASE ('MED    ')
          CALL OPEN_BND_MED(FILE_NAME, FILE_ID, OPENMODE, IERR)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'OPEN_BND : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'OPEN_BND: BAD FILE FORMAT: ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END


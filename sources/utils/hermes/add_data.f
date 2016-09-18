!                    *************************
                     SUBROUTINE ADD_DATA
!                    *************************
!
     &(FFORMAT,FILE_ID,VAR_NAME,TIME,RECORD,
     & FIRST_VAR,VAR_VALUE,N,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Add data information for a given variable and a given time on
!+        all points of the mesh
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| VAR_NAME       |-->| NAME OF THE VARIABLE
!| TIME           |-->| TIME OF THE DATA
!| RECORD         |-->| TIME STEP OF THE DATA
!| FIRST_VAR      |-->| TRUE IF IT IS THE FIRST VARIABLE OF THE DATASET
!| VAR_VALUE      |-->| THE VALUE FOR EACH POINT OF THE MESH
!| N              |-->| SIZE OF VAR_VALUE
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
      CHARACTER(LEN=8),  INTENT(IN)  :: FFORMAT
      INTEGER,           INTENT(IN)  :: FILE_ID,N
      CHARACTER(LEN=32), INTENT(IN)  :: VAR_NAME
      DOUBLE PRECISION,  INTENT(IN)  :: TIME
      INTEGER,           INTENT(IN)  :: RECORD
      LOGICAL,           INTENT(IN)  :: FIRST_VAR
      DOUBLE PRECISION,  INTENT(IN)  :: VAR_VALUE(N)
      INTEGER,           INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL ADD_DATA_SRF(FILE_ID,VAR_NAME,TIME,RECORD,
     &                    FIRST_VAR,VAR_VALUE,N,IERR)
        CASE ('MED    ')
          CALL ADD_DATA_MED(FILE_ID,VAR_NAME,TIME,RECORD,VAR_VALUE,
     &                      N,IERR)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'ADD_DATA : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'ADD_DATA: BAD FILE FORMAT: ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END


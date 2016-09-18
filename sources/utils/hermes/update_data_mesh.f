!                    ***************************
                     SUBROUTINE UPDATE_DATA_MESH
!                    ***************************
!
     &(FFORMAT,FILE_ID,TIME,RECORD,NB_DIM_MESH,NPOIN,COORD,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Update mesh coordinates
!
!history  V STOBIAC (LNHE)
!+        10/12/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| file_id        |-->| File descriptor
!| time           |-->| Time of the data
!| record         |-->| Time step of the data
!| NB_DIM_MESH    |-->| Dimension of the mesh
!| NPOIN          |-->| Number of points in the mesh
!| COORD          |-->| Coordinates table
!| Ierr           |<--| 0 if no error during the opening
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_MED
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FILE_ID
      DOUBLE PRECISION, INTENT(IN)  :: TIME
      INTEGER,          INTENT(IN)  :: RECORD
      INTEGER,          INTENT(IN)  :: NB_DIM_MESH, NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: COORD(NB_DIM_MESH*NPOIN)
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IERR=0
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          WRITE(LU,*)'NO NEED TO UPDATE MESH COORDINATES FOR SLF FORMAT'
        CASE ('MED    ')
! Do nothing for now as the option is not available in Paravis
!          CALL UPDATE_DATA_MESH_MED (FILE_ID,TIME,RECORD,NB_DIM_MESH,
!     &                               NPOIN,COORD,IERR)
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

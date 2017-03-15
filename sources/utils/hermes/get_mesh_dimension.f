!                    *****************************
                     SUBROUTINE GET_MESH_DIMENSION
!                    *****************************
!
     &(FFORMAT,FID,NDIM,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of dimensions of the space
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| NDIM           |<->| NUMBER OF DIMENSION
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
      INTEGER,          INTENT(INOUT) :: NDIM
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_MESH_DIMENSION_SRF(FID, NDIM, IERR)
        CASE ('MED    ')
          CALL GET_MESH_DIMENSION_MED(FID, NDIM, IERR)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'GET_MESH_DIMENSION : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'GET_MESH_DIMENSION: BAD FILE FORMAT: ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

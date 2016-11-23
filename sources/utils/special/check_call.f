!                    *********************
                     SUBROUTINE CHECK_CALL
!                    *********************
!
     &(IERR, CHFILE)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    Check the value of ierr and if different from 0 calls plante
!+
!
!history  Y AUDOUIN (LNHE)
!+        11/07/2008
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| N              |-->| TOTAL NUMBER OF PROCESSORS
!| I              |-->| RANK OF THE PROCESSOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

      INTEGER,       INTENT(IN) :: IERR
      CHARACTER(LEN=*), INTENT(IN) :: CHFILE
!
      CHARACTER(LEN=200) WRITE_ERROR
      EXTERNAL WRITE_ERROR
!
!-----------------------------------------------------------------------
!
      IF(IERR.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'ERREUR ',IERR,'LORS DE L''APPEL A : ',
     &                          CHFILE
          WRITE(LU,*) 'TEXTE DE L''ERREUR : ',WRITE_ERROR(IERR)
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'ERROR ',IERR,'DURING CALL OF ',CHFILE
          WRITE(LU,*) 'ERROR TEXT: ',WRITE_ERROR(IERR)
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CHECK_CALL

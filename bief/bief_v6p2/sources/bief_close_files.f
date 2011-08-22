!                    ***************************
                     SUBROUTINE BIEF_CLOSE_FILES
!                    ***************************
!
     &(CODE,FILES,NFILES,PEXIT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  J-M HERVOUET (LNHE)
!+        01/04/2009
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CODE           |-->| THE CALLING PROGRAM
!| FILES          |-->| ARRAY OF BIEF_FILE STRUCTURES
!| NFILES         |-->| TOTAL NUMBER OF FILES
!| PEXIT          |-->| LOGICAL, IF YES, P_EXIT WILL BE CALLED
!|                |   | TO STOP PARALLELISM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_CLOSE_FILES => BIEF_CLOSE_FILES
!
      USE DECLARATIONS_TELEMAC
      USE M_MED
!
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)     :: NFILES
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      LOGICAL, INTENT(IN)               :: PEXIT
      TYPE(BIEF_FILE)   , INTENT(INOUT) :: FILES(NFILES)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      DO I=1,NFILES
!
        IF(FILES(I)%NAME(1:1).NE.' ') THEN
!
!         CLOSES THE FILE
!
          IF(FILES(I)%FMT.EQ.'MED     ') THEN
            CALL CLOSE_FILE_MED(FILES(I)%LU)
          ELSE
            CLOSE(FILES(I)%LU)
          ENDIF
!
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     PARALLEL MODE: STOPS IF PEXIT
!
      IF(NCSIZE.GT.0.AND.PEXIT) CALL P_EXIT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

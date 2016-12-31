!                    *****************
                     SUBROUTINE SMTRAC
!                    *****************
!
     &(NPOIN,DIMT,AT,DT,SMTR,SMH,NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,ITRAC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE SECOND MEMBER FOR THE TRACER.
!
!history  INRIA
!+
!+        V5P8
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
!| AT             |-->| TIME
!| DIMT           |-->| DIMENSION OF THE TRACER
!| DT             |-->| HYDRO TIME STEP
!| ISCE           |-->| GLOBAL INDICES OF SOURCE POINTS
!| ITRAC          |-->| TRCER INDEX
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMER OF TRACERS
!| NPOIN          |-->| TOTAL NUMBER OF NODES IN THE MESH
!| NREJET         |-->| NUMBER OF SOURCE/SINK
!| SMH            |-->| SOURCE TERMS FOR CONTINUITY EQUATION
!| SMTR           |-->| SOURCE TERMS FOR TRACER
!| TSCE2          |-->| VALUES OF TRACERS AT SOURCES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NREJET,ISCE(*),DIMT,ITRAC
      INTEGER, INTENT(IN) :: MAXSCE,MAXTRA
      DOUBLE PRECISION, INTENT(IN)    :: AT,DT,SMH(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: TSCE2(MAXSCE,MAXTRA)
      DOUBLE PRECISION, INTENT(INOUT) :: SMTR(DIMT)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IS
!
!-----------------------------------------------------------------------
!
      IF(NREJET.NE.0) THEN
        DO I=1,NREJET
          IS =ISCE(I)
          IF(IS.GT.0) THEN
            SMTR(IS) = SMTR(IS) + DT*SMH(IS) * TSCE2(I,ITRAC)
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

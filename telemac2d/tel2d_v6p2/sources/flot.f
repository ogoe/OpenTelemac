!                    ***************
                     SUBROUTINE FLOT
!                    ***************
!
     &(XFLOT,YFLOT,NFLOT,NITFLO,FLOPRD,X,Y,NPOIN,DEBFLO,FINFLO,NIT)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    THE USER MUST GIVE :
!+
!+
!+   1) THE TIMESTEP WHEN THE FLOATING BODY IS RELEASED.
!+
!+
!+   2) THE TIME WHEN THE COMPUTATION IS STOPPED FOR THIS FLOATING BODY.
!+
!+
!+   3) THE INITIAL POSITION OF THE FLOATING BODY AT THE TIME OF RELEASE.
!
!history  J-M JANIN (LNH)
!+        17/08/1994
!+        V5P2
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
!| DEBFLO         |<--| TIME STEP OF INITIAL RELEASE
!| FINFLO         |<--| TIME STEP FOR END OF FOLLOW UP
!| FLOPRD         |-->| NUMBER OF TIME-STEPS BETWEEN 2 RECORDS OF
!|                |   | SUCCESSIVE POSITIONS OF FLOATING BODIES.
!| NFLOT          |-->| NOMBRE DE FLOTTEURS.
!| NIT            |-->| NUMBER OF TIME STEPS
!| NITFLO         |-->| MAXIMUM NUMBER OF RECORDS OF SUCCESIVE
!|                |   | POSITIONS OF FLOATING BODIES.
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| X,Y            |-->| COORDINATES OF POINTS IN THE MESH
!| XFLOT,YFLOT    |<--| POSITIONS OF FLOATING BODIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NIT,NFLOT,NITFLO,FLOPRD
      INTEGER, INTENT(INOUT)          :: DEBFLO(NFLOT),FINFLO(NFLOT)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NITFLO,NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NITFLO,NFLOT)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT
!
!-----------------------------------------------------------------------
!
!  1) STEP FOR THE BEGINNING OF RELEASE (DEBFLO)
!  2) STEP FOR THE END OF RELEASE (FINFLO)
!
      DO 10 IFLOT=1,NFLOT
         DEBFLO(IFLOT) = 1
         FINFLO(IFLOT) = NIT
10    CONTINUE
!
!-----------------------------------------------------------------------
!
!  3) COORDINATES OF FLOATING BODIES AT THE BEGINNING
!
!     INITIAL POSITION OF FLOATING BODIES WHEN RELEASED.
!     DEFAULT VALUE NUMBER "IFLOT" IS RELEASED AT POINT "IFLOT"
!
!-----------------------------------------------------------------------
!
!     XFLOT(1,1)=-300000.D0
!     YFLOT(1,1)= 300000.D0
!
!     XFLOT(1,2)= 0.D0
!     YFLOT(1,2)= 300000.D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END

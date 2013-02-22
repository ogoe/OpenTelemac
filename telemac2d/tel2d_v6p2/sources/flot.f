!                    ***************
                     SUBROUTINE FLOT
!                    ***************
!
     &(XFLOT,YFLOT,NFLOT,NFLOT_MAX,X,Y,IKLE,NELEM,NELMAX,NPOIN,
     & DEBFLO,FINFLO,TAGFLO,NIT)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/08/2010
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
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NIT            |-->| NUMBER OF TIME STEPS
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
      INTEGER, INTENT(IN)             :: NPOIN,NIT,NFLOT_MAX
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT)          :: NFLOT
      INTEGER, INTENT(INOUT)          :: DEBFLO(NFLOT_MAX)
      INTEGER, INTENT(INOUT)          :: FINFLO(NFLOT_MAX)
      INTEGER, INTENT(INOUT)          :: TAGFLO(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NFLOT_MAX)
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
      DO IFLOT=1,NFLOT
        DEBFLO(IFLOT) = 1
        FINFLO(IFLOT) = NIT
      ENDDO
!
!     DEBFLO(1) = 1
!     FINFLO(1) = NIT
!     DEBFLO(2) = 100
!     FINFLO(2) = 600
!
!-----------------------------------------------------------------------
!
!  3) COORDINATES OF ALL FLOATING BODIES AT THE BEGINNING
!
!     INITIAL POSITION OF FLOATING BODIES WHEN RELEASED.
!
!-----------------------------------------------------------------------
!
!     XFLOT(1)= -14.D0
!     YFLOT(1)= 418.D0
!
!     XFLOT(2)= 636.D0
!     YFLOT(2)= 368.D0
!
!  4) REAL NUMBER OF FLOATS IN THE DOMAIN
!
      NFLOT=0
!
!-----------------------------------------------------------------------
!
      RETURN
      END

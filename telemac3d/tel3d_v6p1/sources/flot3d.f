!                    *****************
                     SUBROUTINE FLOT3D
!                    *****************
!
     &(XFLOT,YFLOT,ZFLOT,NFLOT,NITFLO,FLOPRD,X,Y,Z,NPOIN,DEBFLO,FINFLO,
     & NIT)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    THE USER MUST SPECIFY:
!+
!+            1) WHEN THE FLOATING BODY IS RELEASED (IN TERMS OF
!+                   TIMESTEP NUMBER),
!+
!+            2) WHEN THE COMPUTATION IS STOPPED FOR THIS FLOATING BODY,
!+
!+            3) THE INITIAL POSITION OF THE FLOATING BODY.
!
!history  J-M JANIN   (LNH)
!+        25/11/97
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| DEBFLO         |<->| TIME STEP OF INITIAL RELEASE
!| FINFLO         |<->| TIME STEP FOR END OF FOLLOW UP
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEN 2 RECORDINGS
!|                |   | OF SUCCESSIVE POSITIONS OF DROGUES
!| NFLOT          |-->| NUMBER OF DROGUES
!| NIT            |-->| NUMBER OF TIME STEPS
!| NITFLO         |-->| MAXIMUM NUMBER OF RECORDINGS OF SUCCESSIVE
!|                |   | POSITIONS OF DROGUES
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| X              |-->| COORDINATE
!| XFLOT          |<->| SUCCESSIVE X POSITIONS OF DROGUES
!| Y              |-->| COORDINATE
!| YFLOT          |<->| SUCCESSIVE Y POSITIONS OF DROGUES
!| Z              |-->| COORDINATE
!| ZFLOT          |<->| SUCCESSIVE Z POSITIONS OF DROGUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NFLOT, NITFLO, FLOPRD, NPOIN, NIT
      INTEGER, INTENT(INOUT) :: DEBFLO(NFLOT), FINFLO(NFLOT)
!
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NITFLO,NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NITFLO,NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFLOT(NITFLO,NFLOT)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN), Y(NPOIN), Z(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT
!
!-----------------------------------------------------------------------
!
!  1) STEP FOR THE START OF RELEASE (DEBFLO)
!  2) STEP FOR THE END OF RELEASE (FINFLO)
!
      DO IFLOT=1,NFLOT
         DEBFLO(IFLOT) = 1
         FINFLO(IFLOT) = NIT
      END DO
!
!-----------------------------------------------------------------------
!
!  3) COORDINATES OF FLOATING BODIES AT THE BEGINNING
!
!     INITIAL POSITION OF FLOATING BODIES WHEN RELEASED
!     DEFAULT VALUE NUMBER "IFLOT" IS RELEASED AT POINT "IFLOT"
!
!-----------------------------------------------------------------------
!
!     XFLOT(1,1)=-300000.D0
!     YFLOT(1,1)= 300000.D0
!     ZFLOT(1,1)= -10.D0
!
!     XFLOT(1,2)= 0.D0
!     YFLOT(1,2)= 300000.D0
!     ZFLOT(1,2)= 0.D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE FLOT3D

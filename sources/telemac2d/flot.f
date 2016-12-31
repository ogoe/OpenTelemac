!                    ***************
                     SUBROUTINE FLOT
!                    ***************
!
     &(XFLOT,YFLOT,NFLOT,NFLOT_MAX,X,Y,IKLE,NELEM,NELMAX,NPOIN,
     & TAGFLO,SHPFLO,ELTFLO,MESH,LT,NIT,AT)
!
!***********************************************************************
! TELEMAC2D   V7P0
!***********************************************************************
!
!brief    releasing and removing particles in the mesh.
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        22/02/2013
!+        V6P3
!+   New version called at every time step, compatible with //.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| LT             |-->| CURRENT TIME STEP
!| MESH           |<->| MESH STRUCTURE
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NIT            |-->| NUMBER OF TIME STEPS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR
!|                |   | ELEMENTS.
!| X,Y            |-->| COORDINATES OF POINTS IN THE MESH
!| XFLOT,YFLOT    |<--| POSITIONS OF FLOATING BODIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE STREAMLINE, ONLY : ADD_PARTICLE,DEL_PARTICLE
      USE ALGAE_TRANSP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NIT,NFLOT_MAX,LT
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT)          :: NFLOT
      INTEGER, INTENT(INOUT)          :: TAGFLO(NFLOT_MAX)
      INTEGER, INTENT(INOUT)          :: ELTFLO(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),AT
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPFLO(3,NFLOT_MAX)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     EXAMPLE : AT ITERATION 1 AND EVERY 10 ITERATIONS AFTER 600
!               A PARTICLE IS RELEASED WITH COORDINATES
!               X=-220.
!               Y=400.D0+LT/3.D0
!               AND TAG NUMBER LT (LT IS THE TIME STEP NUMBER)
!
!     IF(LT.LE.600.AND.(10*(LT/10).EQ.LT.OR.LT.EQ.1)) THEN
!       CALL ADD_PARTICLE(-220.D0,400.D0+LT/3.D0,0.D0,LT,NFLOT,
!    &                    NFLOT_MAX,XFLOT,YFLOT,YFLOT,TAGFLO,
!    &                    SHPFLO,SHPFLO,ELTFLO,ELTFLO,MESH,1,
!    &                    0.D0,0.D0,0.D0,0.D0,0,0)
!     ENDIF
!
!     EXAMPLE : PARTICLE WITH TAG 20 REMOVED AT ITERATION 600
!
!     IF(LT.EQ.600) THEN
!        CALL DEL_PARTICLE(20,NFLOT,NFLOT_MAX,
!    &                     XFLOT,YFLOT,YFLOT,TAGFLO,SHPFLO,SHPFLO,
!    &                     ELTFLO,ELTFLO,MESH%TYPELM)
!     ENDIF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     EXAMPLE : FOR ALGAE PARTICLE TRANSPORT
!               => ALGAE_START NEEDS TO BE DEFINED
!
!       ALGAE_START=2
!
!       IF(LT.EQ.MAX(1,ALGAE_START)) THEN
!         DO I=1,NFLOT_MAX
!           CALL ADD_PARTICLE(0.175D0,0.45D0,0.D0,I,NFLOT,
!      &                    NFLOT_MAX,XFLOT,YFLOT,YFLOT,TAGFLO,
!      &                    SHPFLO,SHPFLO,ELTFLO,ELTFLO,MESH,1,
!      &                    0.D0,0.D0,0.D0,0.D0,0,0)
!         END DO
!       ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

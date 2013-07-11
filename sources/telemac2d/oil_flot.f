!                    *******************
                     SUBROUTINE OIL_FLOT
!                    *******************
!
     &(PARTICULES,NFLOT,NFLOT_MAX,X,Y,IKLE,NELEM,NELMAX,NPOIN,
     & MESH,LT,NIT,AT,VOLDEV,RHO_OIL,NB_COMPO,NB_HAP)
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
!history  CEDRIC GOEURY (LHSV)
!+        28/06/2013
!+        V6P3
!+   First version
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
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
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NIT,NFLOT_MAX,LT
      INTEGER, INTENT(IN)             :: NB_COMPO,NB_HAP
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT)          :: NFLOT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),AT
      DOUBLE PRECISION, INTENT(IN)    :: VOLDEV,RHO_OIL
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(OIL_PART), INTENT(INOUT)   :: PARTICULES(NFLOT_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: K,J,NUM_GLO,NUM_LOC,NUM_MAX,I
      INTEGER ::NFLOT_OIL
      DOUBLE PRECISION :: COORD_X, COORD_Y
      DOUBLE PRECISION :: XFLOT(1), YFLOT(1)
      DOUBLE PRECISION :: SHPFLO(3,1)
      INTEGER          :: TAGFLO(1)
      INTEGER          :: ELTFLO(1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TBCOMPO(6)
      DATA TBCOMPO/528.38D0,615.12D0,
     *             705.11D0,820.13D0,960.12D0,1125.16D0/
      DOUBLE PRECISION TBHAP(1)
      DATA TBHAP/497.05D0/
      DOUBLE PRECISION SOLU(1)
      DATA SOLU/0.018D0/
      DOUBLE PRECISION FVCOMPO(6)
      DATA FVCOMPO/0.06D0,0.06D0,0.20D0,0.17D0,0.17D0,0.32D0/
      DOUBLE PRECISION FVHAP(1)
      DATA FVHAP/0.02D0/
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS IS AN EXAMPLE !!!!!!!!!!!!!!!!!!!!
!
      IF(LT.EQ.100) THEN 
         NUM_GLO=0
         NUM_MAX=0
         NUM_LOC=0
         COORD_X=0.D0
         COORD_Y=0.D0 
         NUM_MAX=INT(SQRT(REAL(NFLOT_MAX)))
         DO K=0,NUM_MAX-1
            DO J=0,NUM_MAX-1
               COORD_X=136939.469D0+J*0.1D0
               COORD_Y=6706174.0D0+K*0.1D0
               NUM_GLO=NUM_GLO+1
               NFLOT_OIL = 0
               CALL ADD_PARTICLE(COORD_X,COORD_Y,0.D0,NUM_GLO,NFLOT_OIL,
     &              1,XFLOT,YFLOT,YFLOT,TAGFLO,SHPFLO,SHPFLO,ELTFLO,
     &              ELTFLO,MESH,1,0.D0,0.D0,0.D0,0.D0,0,0)
               IF(NFLOT_OIL.EQ.1)THEN
                  NUM_LOC = NUM_LOC+1
!=========================================================================
!  INITIALISATION PARAMETRES NECESSAIRE AU DEPLACEMENT DES PARTICULES
!=========================================================================
                  PARTICULES(NUM_LOC)%XOIL = XFLOT(1)
                  PARTICULES(NUM_LOC)%YOIL = YFLOT(1)
                  PARTICULES(NUM_LOC)%ID = TAGFLO(1)
                  PARTICULES(NUM_LOC)%SHPOIL(1) = SHPFLO(1,1)
                  PARTICULES(NUM_LOC)%SHPOIL(2) = SHPFLO(2,1)
                  PARTICULES(NUM_LOC)%SHPOIL(3) = SHPFLO(3,1)
                  PARTICULES(NUM_LOC)%ELTOIL = ELTFLO(1)
!=========================================================================
!  INITIALISATION PARAMETRES NECESSAIRE AU PROCESSUS PHYSICO-CHIMIQUES 
!                             DES HYDROCARBURES
!=========================================================================
                  PARTICULES(NUM_LOC)%STATE=1
                  PARTICULES(NUM_LOC)%TPSECH=0
                  PARTICULES(NUM_LOC)%SURFACE = 0.D0
                  PARTICULES(NUM_LOC)%MASS0 = (VOLDEV*RHO_OIL)/
     &                 REAL(NFLOT_MAX)
                  PARTICULES(NUM_LOC)%MASS_EVAP=0.D0
                  PARTICULES(NUM_LOC)%MASS_DISS=0.D0
                  DO I=1,NB_COMPO
                     PARTICULES(NUM_LOC)%COMPO(I)%MASS=
     &                    PARTICULES(NUM_LOC)%MASS0*FVCOMPO(I)
                     PARTICULES(NUM_LOC)%COMPO(I)%TB=TBCOMPO(I)
                     PARTICULES(NUM_LOC)%COMPO(I)%SOL=0.D0
                     PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &                    PARTICULES(NUM_LOC)%COMPO(I)%MASS
                  END DO
                  DO I=1,NB_HAP
                     PARTICULES(NUM_LOC)%HAP(I)%MASS=
     &                    PARTICULES(NUM_LOC)%MASS0*FVHAP(I)
                     PARTICULES(NUM_LOC)%HAP(I)%TB=TBHAP(I)
                      PARTICULES(NUM_LOC)%HAP(I)%SOL=SOLU(I)
                     PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &                    PARTICULES(NUM_LOC)%HAP(I)%MASS
                  END DO
                  NFLOT = NUM_LOC
               END IF
            END DO
         END DO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

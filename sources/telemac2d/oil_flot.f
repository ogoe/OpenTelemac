!                    *******************
                     SUBROUTINE OIL_FLOT
!                    *******************
!
     &(PARTICULES,NFLOT,NFLOT_MAX,MESH,LT,VOLDEV,RHO_OIL,
     &NB_COMPO,NB_HAP,FMCOMPO,TBCOMPO,FMHAP,TBHAP,SOLU,ETAL,AREA)
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
!| AT             |-->| TIME
!| ELTFLO         |-->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| LT             |-->| CURRENT TIME STEP
!| MESH           |<->| MESH STRUCTURE
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NIT            |-->| NUMBER OF TIME STEPS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PARTICULES     |<->| OIL STRUCTURE DEFINED IN BIEF DEF
!| SHPFLO         |-->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR 
!|                |   | ELEMENTS.
!| X,Y            |-->| COORDINATES OF POINTS IN THE MESH
!| XFLOT,YFLOT    |-->| POSITIONS OF FLOATING BODIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : GRAV
      USE STREAMLINE, ONLY : ADD_PARTICLE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NFLOT_MAX,LT
      INTEGER, INTENT(IN)             :: NB_COMPO,NB_HAP
      INTEGER, INTENT(IN)             :: ETAL
      INTEGER, INTENT(INOUT)          :: NFLOT
      DOUBLE PRECISION, INTENT(IN)    :: VOLDEV,RHO_OIL,AREA
      DOUBLE PRECISION, INTENT(IN)    :: FMCOMPO(NB_COMPO)
      DOUBLE PRECISION, INTENT(IN)    :: TBCOMPO(NB_COMPO)
      DOUBLE PRECISION, INTENT(IN)    :: FMHAP(NB_HAP)
      DOUBLE PRECISION, INTENT(IN)    :: TBHAP(NB_HAP)
      DOUBLE PRECISION, INTENT(IN)    :: SOLU(NB_HAP)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(OIL_PART), INTENT(INOUT)   :: PARTICULES(NFLOT_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                         :: K,J,NUM_GLO,NUM_LOC,NUM_MAX,I
      INTEGER                         :: NFLOT_OIL
      DOUBLE PRECISION                :: RHO_EAU,PI,COEF1
      DOUBLE PRECISION                :: COEF2,DELTA,NU,NU2
      DOUBLE PRECISION                :: COORD_X, COORD_Y
      DOUBLE PRECISION                :: XFLOT(1), YFLOT(1)
      DOUBLE PRECISION                :: SHPFLO(3,1)
      INTEGER                         :: TAGFLO(1)
      INTEGER                         :: ELTFLO(1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS IS AN EXAMPLE !!!!!!!!!!!!!!!!!!!!
!
         RHO_EAU=1000.D0
         PI=ACOS(-1.D0)
!        HARDCODED WATER MOLECULAR VISCOSITY
         NU=1.D-6
         NU2=NU**2
!
         COEF1=1.21D0**4
         COEF2=COEF1/1.53**2
         DELTA=(RHO_EAU-RHO_OIL)/(RHO_EAU)
!
      IF(LT.EQ.10000) THEN 
         NUM_GLO=0
         NUM_MAX=0
         NUM_LOC=0
         COORD_X=0.D0
         COORD_Y=0.D0 
         NUM_MAX=INT(SQRT(REAL(NFLOT_MAX)))
         DO K=0,NUM_MAX-1
            DO J=0,NUM_MAX-1
               COORD_X=336000.D0+REAL(j)
               COORD_Y=371000.D0+REAL(k)
               NUM_GLO=NUM_GLO+1
               NFLOT_OIL = 0
               CALL ADD_PARTICLE(COORD_X,COORD_Y,0.D0,NUM_GLO,NFLOT_OIL,
     &              1,XFLOT,YFLOT,YFLOT,TAGFLO,SHPFLO,SHPFLO,ELTFLO,
     &              ELTFLO,MESH,1,0.D0,0.D0,0.D0,0.D0,0,0)
               IF(NFLOT_OIL.EQ.1)THEN
                  NUM_LOC = NUM_LOC+1
!=========================================================================
!----INITIALIZATION PARAMETERS FOR THE CALCULATION OF PARTICULE MOTION----
!=========================================================================
                  PARTICULES(NUM_LOC)%XOIL = XFLOT(1)
                  PARTICULES(NUM_LOC)%YOIL = YFLOT(1)
                  PARTICULES(NUM_LOC)%ID = TAGFLO(1)
                  PARTICULES(NUM_LOC)%SHPOIL(1) = SHPFLO(1,1)
                  PARTICULES(NUM_LOC)%SHPOIL(2) = SHPFLO(2,1)
                  PARTICULES(NUM_LOC)%SHPOIL(3) = SHPFLO(3,1)
                  PARTICULES(NUM_LOC)%ELTOIL = ELTFLO(1)
!=========================================================================
!-----------INITIALIZATION PARAMETERS FOR THE CALCULATION OF OIL----------
!---------------------------WEATHERING PROCESSES--------------------------
!=========================================================================
                  PARTICULES(NUM_LOC)%STATE=1
                  PARTICULES(NUM_LOC)%TPSECH=0
                  IF(ETAL.EQ.1)THEN
                     PARTICULES(NUM_LOC)%SURFACE=PI*COEF2*
     &                    (DELTA*GRAV/(VOLDEV*NU2))**(1.D0/6.D0)
     &                    *VOLDEV/NFLOT_MAX 
                  ELSEIF(ETAL.EQ.3)THEN
                     PARTICULES(NUM_LOC)%SURFACE = AREA
                  ELSEIF(ETAL.EQ.2) THEN
                     PARTICULES(NUM_LOC)%SURFACE = 0.D0
                  ELSE
                    IF(LNG.EQ.1) THEN
                      WRITE(LU,*) 'ETAL=',ETAL,' INCONNU DANS OIL_FLOT'
                    ENDIF
                    IF(LNG.EQ.1) THEN
                      WRITE(LU,*) 'ETAL=',ETAL,' UNKNOWN IN OIL_FLOT'
                    ENDIF
                    CALL PLANTE(1)
                    STOP
                  END IF
                  PARTICULES(NUM_LOC)%MASS0 = (VOLDEV*RHO_OIL)/NFLOT_MAX
                  PARTICULES(NUM_LOC)%MASS_EVAP=0.D0
                  PARTICULES(NUM_LOC)%MASS_DISS=0.D0
                  DO I=1,NB_COMPO
                     PARTICULES(NUM_LOC)%COMPO(I)%MASS=
     &                    PARTICULES(NUM_LOC)%MASS0*FMCOMPO(I)
                     PARTICULES(NUM_LOC)%COMPO(I)%TB=TBCOMPO(I)
                     PARTICULES(NUM_LOC)%COMPO(I)%SOL=0.D0
                     PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &                    PARTICULES(NUM_LOC)%COMPO(I)%MASS
                  END DO
                  DO I=1,NB_HAP
                     PARTICULES(NUM_LOC)%HAP(I)%MASS=
     &                    PARTICULES(NUM_LOC)%MASS0*FMHAP(I)
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

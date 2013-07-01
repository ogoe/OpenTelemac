!
!==========================================================================
!                            MODULE OILSPILL
!==========================================================================
!
!***********************************************************************
! TELEMAC2D   V6P3                                  21/08/2010
!***********************************************************************
!
!brief    OIL SPILL MODEL.
!+
!+
!+            CALLED IF KEYWORD 'OIL SPILL MODEL' IS SET TO YES
!+                AND USES 'MIGRHYCAR STEERING FILE'.
!
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
      MODULE OILSPILL
      USE BIEF
      USE DECLARATIONS_TELEMAC2D

      IMPLICIT NONE

      TYPE(BIEF_OBJ):: UCONV_OIL,VCONV_OIL
      TYPE(OIL_PART),DIMENSION(:),ALLOCATABLE::PARTICULES

      CONTAINS

!                    ***********************
                     SUBROUTINE OIL_SPILL_2D
!                    ***********************
!
     &     (YASMI)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OIL SPILL MODEL.
!+
!+
!+            CALLED IF KEYWORD 'OIL SPILL MODEL' IS SET TO YES
!+                AND USES 'MIGRHYCAR STEERING FILE'.
!
!note     LOGICAL UNIT OF MIGRHYCAR STEERING FILE IS: T2D_FILES(T2DMIG)%LU
!
!warning  DOES NOTHING BY DEFAULT
!
!history  CEDRIC GOEURY (LHSV)
!+        20/04/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE STREAMLINE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(INOUT)          :: YASMI(*) 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION,PARAMETER            ::KARMAN=0.41D0
      LOGICAL INIT
      DATA INIT /.FALSE./
      INTEGER                               ::I,K,IFLOT
      INTEGER                               ::NB_COMPO,NB_HAP
      DOUBLE PRECISION                      ::ETA_OIL,RHO_OIL,VOLDEV
      INTEGER P_IMAX 
      EXTERNAL P_IMAX

!     SAVE
      SAVE RHO_OIL,VOLDEV,NB_COMPO
      SAVE ETA_OIL,NB_HAP
!-----------------------------------------------------------------------
      IF(.NOT.INIT)THEN
         CALL BIEF_ALLVEC(1,UCONV_OIL,'UCONVO',11,1,2,MESH)
         CALL BIEF_ALLVEC(1,VCONV_OIL,'VCONVO',11,1,2,MESH)
         WRITE(0,*) NFLOT_MAX
         ALLOCATE(PARTICULES(NFLOT_MAX))

         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) NB_COMPO
         WRITE(0,*) NB_COMPO
         DO I=1,NFLOT_MAX
            ALLOCATE(PARTICULES(I)%COMPO(NB_COMPO))
         END DO

         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) NB_HAP
         WRITE(0,*) NB_HAP
         DO I=1,NFLOT_MAX
            ALLOCATE(PARTICULES(I)%HAP(NB_HAP))
         END DO
         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) RHO_OIL

         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) ETA_OIL
         WRITE(0,*) ETA_OIL
         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) VOLDEV
         WRITE(0,*) VOLDEV
         WRITE(0,*) (VOLDEV*RHO_OIL)/NFLOT_MAX

         DO I=1,NFLOT_MAX
            PARTICULES(I)%STATE=0
            PARTICULES(I)%ID=0
            PARTICULES(I)%TPSECH=0
            PARTICULES(I)%MASS0=0.D0
            PARTICULES(I)%MASS=0.D0
            PARTICULES(I)%MASS_EVAP=0.D0
            PARTICULES(I)%MASS_DISS=0.D0
            PARTICULES(I)%SURFACE=0.D0
            DO K=1,NB_COMPO
               PARTICULES(I)%COMPO(K)%MASS=0.D0
               PARTICULES(I)%COMPO(K)%TB=0.D0
               PARTICULES(I)%COMPO(K)%SOL=0.D0
            END DO
            DO K=1,NB_HAP
               PARTICULES(I)%HAP(K)%MASS=0.D0
               PARTICULES(I)%HAP(K)%TB=0.D0
               PARTICULES(I)%HAP(K)%SOL=0.D0
            END DO
         END DO
      IF(NCSIZE.GT.1) CALL OIL_ORGANISE_CHARS(NFLOT_MAX)
         INIT=.TRUE.
      ENDIF
!======================================================================
!          AJOUT DU VENT DANS LE TRANSPORT DE L HYDROCARBURE
!======================================================================
           DO I=1,UCONV%DIM1
              UCONV_OIL%R(I)=UCONV%R(I)*(1.D0+(1/KARMAN)*
     *             SQRT(CF%R(I)/2.D0))+0.036D0*WINDX%R(I)
           END DO
           DO I=1,VCONV%DIM1
              VCONV_OIL%R(I)=VCONV%R(I)*(1.D0+(1/KARMAN)*
     *            SQRT(CF%R(I)/2.D0))+0.036D0*WINDY%R(I)
           END DO
!======================================================================
!     APPEL A OIL_FLOT (CONTIENT L'INITIALISATION DES PARTICULES)
!======================================================================
           CALL OIL_FLOT(NFLOT,NFLOT_MAX,MESH%X%R,
     &          MESH%Y%R,MESH%IKLE%I,NELEM,NELMAX,NPOIN,MESH,LT,
     &          NIT,AT,PARTICULES,VOLDEV,RHO_OIL,NB_COMPO,NB_HAP)
!======================================================================
!          APPEL A OIL_SPREADING (ETALEMENT DES PARTICULES)
!======================================================================   
           CALL OIL_SPREADING(PARTICULES,VOLDEV,ETA_OIL,RHO_OIL,NFLOT
     &          ,NFLOT_MAX,LT,DT)
!======================================================================
!          APPEL A OIL_REFLOATING (RELARGAGE DES PARTICULES)
!======================================================================   
           CALL OIL_REFLOATING(LT,DT,NPOIN,NELMAX,3,MESH%IKLE%I,H%R,
     &          HN%R,PARTICULES,RHO_OIL,NFLOT_MAX,NFLOT,CF%R)
!======================================================================
!        APPEL A DERIVE (CONVECTION ET DIFFUSION DES PARTICULES)
!======================================================================         
           CALL OIL_DERIVE(UCONV_OIL%R,VCONV_OIL%R,VCONV_OIL%R,DT,AT,
     &          MESH%X%R,MESH%Y%R,MESH%Y%R,MESH%IKLE%I,MESH%IFABOR%I,LT,
     &          11,UCONV%ELM,3,NPOIN,NPOIN,NELEM,NELMAX,MESH%SURDET%R,
     &          XFLOT%R,YFLOT%R,YFLOT%R,SHPFLO%R,SHPFLO%R,TAGFLO%I,
     &          ELTFLO%I,ELTFLO%I,NFLOT,NFLOT_MAX,FLOPRD,MESH,
     &          T2D_FILES(T2DFLO)%LU,IT1%I,T1%R,T2%R,T2%R,IT2%I,W1%R,
     &          W1%R,NPOIN,1,VISC,PARTICULES,NB_COMPO,NB_HAP)   
!======================================================================
!          APPEL A OIL_BEACHING (ECHOUAGE DES PARTICULES)
!======================================================================   
           CALL OIL_BEACHING(MESH%IKLE%I,NPOIN,NELMAX,3,H%R,HN%R,
     &          PARTICULES,NFLOT_MAX,NFLOT,RHO_OIL,
     &          MESH%SURFAC%R,CF%R,ETA_OIL,LT)
!======================================================================
!          APPEL A OIL_EVAP (EVAPORATION DES PARTICULES)
!======================================================================   
           CALL OIL_EVAP(NB_COMPO,NB_HAP,PARTICULES,NFLOT,NFLOT_MAX,LT
     &          ,DT)
!======================================================================
!          APPEL A OIL_DISSO (DISSOLUTION DES PARTICULES)
!======================================================================  
           CALL OIL_DISSO(PARTICULES,NB_COMPO,NB_HAP,NFLOT,NFLOT_MAX,LT,
     &          DT,NELMAX,MESH%IKLE%I,HN%R,NPOIN,UNSV2D%R,
     &          TN,NTRAC,TB,MESH)
!======================================================================
!             GESTION DES PARTICULE PERDUES SI LEURS  MASS = 0
!====================================================================== 
!
           DO IFLOT=1,NFLOT
              IF(PARTICULES(IFLOT)%MASS.EQ.0.D0) THEN 
                 CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT,
     &           NFLOT_MAX,MESH%TYPELM,IT1%I,PARTICULES,NB_COMPO,NB_HAP)
              END IF
           END DO
!======================================================================
!         APPEL A OIL_VOLATI (VOLATILISATION DU PÉTROLE DISSOUS
!                  DANS LA COLONNE D'EAU DES PARTICULES)
!======================================================================  
           CALL OIL_VOLATI(T3,TIMP,YASMI,HPROP,NTRAC,PARTICULES,
     &          NFLOT_MAX,NFLOT,NELMAX,MESH%IKLE%I,NPOIN,MESH)
!----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_SPILL_2D
!                    *******************
                     SUBROUTINE OIL_FLOT
!                    *******************
!
     &(NFLOT,NFLOT_MAX,X,Y,IKLE,NELEM,NELMAX,NPOIN,
     & MESH,LT,NIT,AT,PARTICULES,VOLDEV,RHO_OIL,NB_COMPO,NB_HAP)
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
!history  J-M HERVOUETIN (EDF R&D, LNHE)
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
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(OIL_PART), DIMENSION(NFLOT_MAX)::PARTICULES
!
!
      INTEGER :: K,J,NUM_GLO,NUM_LOC,NUM_MAX,I
      INTEGER ::NFLOT_OIL
      DOUBLE PRECISION :: COORD_X, COORD_Y
      DOUBLE PRECISION :: XFLOT(1), YFLOT(1)
      DOUBLE PRECISION :: SHPFLO(3,1)
      INTEGER          :: TAGFLO(1)
      INTEGER          :: ELTFLO(1)
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      DOUBLE PRECISION TBCOMPO(6)
      DATA TBCOMPO/525.09D0,527.65D0,
     *     702.704D0,823.16D0,967.48D0,1123.16D0/
      DOUBLE PRECISION TBHAP(4)
      DATA TBHAP/491.11D0,609.15D0,611.454D0,
     *     734.65D0/
      DOUBLE PRECISION SOLU(4)
      DATA SOLU/0.032D0,5.D-04,1.2D-03,2.55D-06/
      DOUBLE PRECISION FVCOMPO(6)
      DATA FVCOMPO/0.03456D0,0.0649D0,
     *     0.193054D0,0.1773D0,0.1814D0,0.3129D0/
      DOUBLE PRECISION FVHAP(4)
      DATA FVHAP/0.01964D0,0.0069D0,0.008646D0,
     *     0.0007D0/
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
!     EXAMPLE : AT ITERATION 1 AND EVERY 10 ITERATIONS AFTER 600
!               A PARTICLE IS RELEASED WITH COORDINATES
!               X=-220.
!               Y=400.D0+LT/3.D0
!               AND TAG NUMBER LT
!     IF(LT.LE.600.AND.(10*(LT/10).EQ.LT.OR.LT.EQ.1)) THEN
!       CALL ADD_PARTICLE(-220.D0,400.D0+LT/3.D0,0.D0,LT,NFLOT,
!    &                    NFLOT_MAX,XFLOT,YFLOT,YFLOT,TAGFLO,
!    &                    SHPFLO,SHPFLO,ELTFLO,ELTFLO,MESH,1,
!    &                    0.D0,0.D0,0.D0,0.D0,0,0)
!     ENDIF     
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
     *                    PARTICULES(NUM_LOC)%MASS0*FVCOMPO(I)
                     PARTICULES(NUM_LOC)%COMPO(I)%TB=TBCOMPO(I)
                     PARTICULES(NUM_LOC)%COMPO(I)%SOL=0.D0
                     PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     *                    PARTICULES(NUM_LOC)%COMPO(I)%MASS
                  END DO
                  DO I=1,NB_HAP
                     PARTICULES(NUM_LOC)%HAP(I)%MASS=
     *                    PARTICULES(NUM_LOC)%MASS0*FVHAP(I)
                     PARTICULES(NUM_LOC)%HAP(I)%TB=TBHAP(I)
                      PARTICULES(NUM_LOC)%HAP(I)%SOL=SOLU(I)
                     PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     *                    PARTICULES(NUM_LOC)%HAP(I)%MASS
                  END DO
                  NFLOT = NUM_LOC
               END IF
            END DO
         END DO
      ENDIF
!
!     EXAMPLE : PARTICLE WITH TAG 20 REMOVED AT ITERATION 600
!
!     IF(LT.EQ.600) THEN
!        CALL DEL_PARTICLE(20,NFLOT,NFLOT_MAX,
!    &                     XFLOT,YFLOT,YFLOT,TAGFLO,SHPFLO,SHPFLO,
!    &                     ELTFLO,ELTFLO,MESH%TYPELM)
!     ENDIF
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_FLOT

!                    **********************
                     SUBROUTINE OIL_DERIVE
!                    **********************
!
     &(U,V,W,DT,AT,X,Y,Z,IKLE,IFABOR,LT,IELM,IELMU,NDP,NPOIN,NPOIN2,
     & NELEM,NELMAX,SURDET,XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,TAGFLO,
     & ELTFLO,ETAFLO,NFLOT,NFLOT_MAX,FLOPRD,MESH,UL,ISUB,DX,DY,DZ,
     & ELTBUF,SHPBUF,SHZBUF,SIZEBUF,STOCHA,VISC,
     & PARTICULES,NB_COMPO,NB_HAP)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    - COMPUTES THE BARYCENTRIC COORDINATES OF A FLOAT
!+                  IN THE MESH AT THE TIME OF RELEASE.
!+
!+            - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!+                  WHICH IS CARRIED WITHOUT FRICTION BY THE CURRENT
!+                 (SUBSEQUENT TIMESTEPS).
!
!history  J-M JANIN (LNH)
!+        18/08/94
!+        V5P1
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
!history  J-M HERVOUET (LNHE)
!+        19/06/2012
!+        V6P2
!+   Adapted for calling SCARACT instead of CHAR11. However parallelism
!+   will require further modifications.
!
!history  J-M HERVOUET (LNHE)
!+        12/03/2013
!+        V6P3
!+   New file format for Tecplot. Works in parallel. Works in 3D.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| DX             |<->| WORK ARRAY (DISPLACEMENTS ALONG X)
!| DY             |<->| WORK ARRAY (DISPLACEMENTS ALONG Y)
!| DZ             |<->| WORK ARRAY (DISPLACEMENTS ALONG Z)
!| ELTBUF         |<->| WORK ARRAY
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| ETAFLO         |<->| LEVELS WHERE ARE THE FLOATS
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEB TWO RECORDS
!|                |   | FOR FLOATS POSITIONS.
!| IELM           |-->| TYPE OF ELEMENT.
!| IELMU          |-->| TYPE OF ELEMENT FOR VELOCITIES.
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF ANOTHER ELEMENT
!|                |   | IF IFABOR NEGATIVE OR 0, THE EDGE IS A
!|                |   | LIQUID OR PERIODIC BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| ISUB           |<->| ARRIVAL SUB-DOMAIN OF PARTICLES.
!| LT             |-->| TIME STEP NUMBER.
!| MESH           |<->| MESH STRUCTURE.
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |<->| NUMBER OF FLOATS.
!| NFLOT_MAX      |<->| MAXIMUM NUMBER OF FLOATS.
!| NPOIN          |-->| NUMBER OF POINTS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SHPBUF         |<->| WORK ARRAY 
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR 
!|                |   | ELEMENTS.
!| SHZBUF         |<->| WORK ARRAY 
!| SHZFLO         |<->| BARYCENTRIC COORDINATE ON VERTICAL
!| SIZEBUF        |-->| DILMENSION OF SOME WORK ARRAYS
!| SURDET         |-->| 1/DETERMINANT, USED IN ISOPARAMETRIC
!|                |   | TRANSFORMATION.
!| TAGFLO         |-->| TAGS OF FLOATS  
!| U              |-->| X-COMPONENT OF VELOCITY
!| UL             |-->| LOGICAL UNIT OF OUTPUT FILE
!| V              |-->| Y-COMPONENT OF VELOCITY
!| W              |-->| Z-COMPONENT OF VELOCITY
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!| XFLOT          |<->| ABSCISSAE OF FLOATS
!| YFLOT          |<->| ORDINATES OF FLOATS
!| ZFLOT          |<->| ELEVATIONS OF FLOATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DERIVE => DERIVE
      USE STREAMLINE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,LT,IELM,IELMU,NDP,NELEM
      INTEGER         , INTENT(IN)    :: FLOPRD,NELMAX,UL,SIZEBUF,NPOIN2
      INTEGER         , INTENT(IN)    :: NFLOT_MAX,STOCHA
      INTEGER         , INTENT(IN)    :: NB_COMPO,NB_HAP
      INTEGER         , INTENT(INOUT) :: NFLOT
      DOUBLE PRECISION, INTENT(IN)    :: DT,AT
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),W(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),Z(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      INTEGER         , INTENT(IN)    :: IFABOR(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NFLOT_MAX),DX(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NFLOT_MAX),DY(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFLOT(NFLOT_MAX),DZ(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: TAGFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: ELTFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: ETAFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: ELTBUF(SIZEBUF)
      INTEGER         , INTENT(INOUT) :: ISUB(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPFLO(NDP,NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZFLO(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPBUF(NDP,SIZEBUF)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZBUF(SIZEBUF)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: VISC
      TYPE(OIL_PART), DIMENSION(NFLOT_MAX)::PARTICULES
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,FRE(1),FREBUF(1),IPROC,NFLOTG,NPLAN,ELT
      INTEGER N1,N2,N3,N4,N5,N6,NOMB,SENS,I,J,IR
!
      DOUBLE PRECISION ZSTAR(1)
!
      CHARACTER(LEN=32) TEXTE(3)
      CHARACTER(LEN=72) LIGNE
!
      LOGICAL YESITIS
!
      TYPE(BIEF_OBJ) :: SVOID
!
      INTEGER  P_ISUM
      EXTERNAL P_ISUM
!
      CHARACTER(LEN=11) EXTENS
      EXTERNAL          EXTENS
!
      LOGICAL DEJA
      DATA    DEJA/.FALSE./
!
      SAVE
!
!-----------------------------------------------------------------------
!
!     PARAMETERISING THE CALL TO SCARACT
!
!     NUMBER OF PLANES
      NPLAN=NPOIN/NPOIN2
!     NO VARIABLE TO INTERPOLATE AT THE FOOT OF CHARACTERISTICS
      NOMB=0
!     FORWARD TRACKING
      SENS=1
!
      IF(IELM.NE.11.AND.IELM.NE.41) THEN
        IF(LNG.EQ.1) WRITE(LU,123) IELM
        IF(LNG.EQ.2) WRITE(LU,124) IELM
123     FORMAT(1X,'DERIVE : TYPE D''ELEMENT NON PREVU : ',1I6)
124     FORMAT(1X,'DERIVE : UNEXPECTED TYPE OF ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISING SVOID AND HEADER OF A TECPLOT FILE
!
      IF(.NOT.DEJA) THEN
!
!       THOUGH NOMB = 0, THESE COMPONENTS WILL BE USED IN SCARACT
!
        SVOID%TYPE=2
        SVOID%DIM1=1
        ALLOCATE(SVOID%R(1))
!
!       HEADER OF TECPLOT FILE
!
        IF(IPID.EQ.0) THEN
          TEXTE(1)='X                               '
          TEXTE(2)='Y                               '
          IF(LNG.EQ.1) THEN
            TEXTE(3)='COTE Z          M               '
          ELSE
            TEXTE(3)='ELEVATION Z     M               '
          ENDIF
          IF(LNG.EQ.1) THEN
            WRITE(UL,100) 'TITLE = "FICHIER DES FLOTTEURS"'
          ELSE
            WRITE(UL,100) 'TITLE = "DROGUES FILE"'
          ENDIF
          IF(IELM.EQ.11) THEN
            WRITE(UL,100) 'VARIABLES = "LABELS","'//
     &                     TEXTE(1)//'","'//TEXTE(2)//'","COLOUR"'
          ELSEIF(IELM.EQ.41) THEN
            WRITE(UL,100) 'VARIABLES = "LABELS","'//
     &      TEXTE(1)//'","'//TEXTE(2)//'","'//TEXTE(3)//'","COLOUR"'
          ENDIF
        ENDIF
        DEJA=.TRUE.
100     FORMAT(A)
      ENDIF
!
      SVOID%ELM=IELM
!
!-----------------------------------------------------------------------
!
!
!========================================================================
!                           OILSPILL
!========================================================================
      IR = 0
      DO IFLOT = 1,NFLOT
         IF(PARTICULES(IFLOT)%STATE.EQ.1)THEN
            IR = IR +1
            XFLOT(IR) = PARTICULES(IFLOT)%XOIL
            YFLOT(IR) = PARTICULES(IFLOT)%YOIL
            TAGFLO(IR) = PARTICULES(IFLOT)%ID
            SHPFLO(1,IR) = PARTICULES(IFLOT)%SHPOIL(1)
            SHPFLO(2,IR) = PARTICULES(IFLOT)%SHPOIL(2)
            SHPFLO(3,IR) = PARTICULES(IFLOT)%SHPOIL(3)
            ELTFLO(IR) = PARTICULES(IFLOT)%ELTOIL
         END IF
      END DO
!========================================================================
!                           OILSPILL
!========================================================================
      CALL SCARACT(SVOID,SVOID,U,V,W,W,X,Y,ZSTAR,ZSTAR,
     &             XFLOT,YFLOT,ZFLOT,ZFLOT,
     &             DX,DY,DZ,DZ,Z,SHPFLO,SHZFLO,SHZFLO,SURDET,DT,
     &             IKLE,IFABOR,ELTFLO,ETAFLO,
     &             FRE,ELTBUF,ISUB,IELM,IELMU,NELEM,NELMAX,            
     &             NOMB,NPOIN,NPOIN2,NDP,NPLAN,1,MESH,IR,NPOIN2,SENS,        
     &             SHPBUF,SHZBUF,SHZBUF,FREBUF,SIZEBUF,
     &             APOST=.TRUE.,ASTOCHA=1,AVISC=VISC)
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1.AND.NFLOT.GT.0) THEN
!
!       IN // XFLOT AND YFLOT MAY HAVE BEEN DESTROYED BY SCARACT
!       BECAUSE RE-USED FOR GENERATIONS OF LOST PARTICLES
!       THEY ARE REDONE HERE FOR PARTICLES WHICH ARE STILL IN THE
!       SUB-DOMAIN
!
!========================================================================
!                           OILSPILL
!========================================================================
        IF(IELM.EQ.11) THEN
           DO IFLOT=NFLOT,1,-1
              IF(PARTICULES(IFLOT)%STATE.EQ.2)THEN 
                 ISUB(IFLOT)=IPID
                 ELTFLO(IFLOT)=PARTICULES(IFLOT)%ELTOIL
              ELSE
                 ISUB(IFLOT)=ISUB(IR)
                 ELTFLO(IFLOT)=ELTFLO(IR)
                 TAGFLO(IFLOT)=TAGFLO(IR)
                 SHPFLO(1,IFLOT)=SHPFLO(1,IR)
                 SHPFLO(2,IFLOT)=SHPFLO(2,IR)
                 SHPFLO(3,IFLOT)=SHPFLO(3,IR)
                 IF(ISUB(IFLOT).EQ.IPID) THEN
                    ELT=ELTFLO(IR)
                    IF(ELT.GT.0) THEN
                       N1=IKLE(ELT,1)
                       N2=IKLE(ELT,2)
                       N3=IKLE(ELT,3)
                       PARTICULES(IFLOT)%XOIL=SHPFLO(1,IR)*X(N1)
     &                      +SHPFLO(2,IR)*X(N2)
     &                      +SHPFLO(3,IR)*X(N3)     
                       PARTICULES(IFLOT)%YOIL=SHPFLO(1,IR)*Y(N1)
     &                      +SHPFLO(2,IR)*Y(N2)
     &                      +SHPFLO(3,IR)*Y(N3)
                       PARTICULES(IFLOT)%SHPOIL(1) = SHPFLO(1,IR)
                       PARTICULES(IFLOT)%SHPOIL(2) = SHPFLO(2,IR)
                       PARTICULES(IFLOT)%SHPOIL(3) = SHPFLO(3,IR)
                       PARTICULES(IFLOT)%ID = TAGFLO(IR)
                       PARTICULES(IFLOT)%ELTOIL = ELTFLO(IR)
                    ENDIF
                 END IF
                 IR = IR - 1
              END IF
           END DO
          IF(IR.NE.0)THEN
              WRITE(LU,*) 'PROBLE DANS DERIVE:: RESTITUTION ISUB OIL', 
     &             'IR: ',IR
              CALL PLANTE(1) 
              STOP  
           END IF
        ELSEIF(IELM.EQ.41) THEN
          DO IFLOT=1,NFLOT
            IF(ISUB(IFLOT).EQ.IPID) THEN
              ELT=ELTFLO(IFLOT)
              IF(ELT.GT.0) THEN     
                N1=IKLE(ELT,1)+NPOIN2*(ETAFLO(IFLOT)-1) 
                N2=IKLE(ELT,2)+NPOIN2*(ETAFLO(IFLOT)-1)
                N3=IKLE(ELT,3)+NPOIN2*(ETAFLO(IFLOT)-1)
                N4=IKLE(ELT,1)+NPOIN2* ETAFLO(IFLOT) 
                N5=IKLE(ELT,2)+NPOIN2* ETAFLO(IFLOT)
                N6=IKLE(ELT,3)+NPOIN2* ETAFLO(IFLOT)
                XFLOT(IFLOT)=SHPFLO(1,IFLOT)*X(N1)
     &                      +SHPFLO(2,IFLOT)*X(N2)
     &                      +SHPFLO(3,IFLOT)*X(N3)     
                YFLOT(IFLOT)=SHPFLO(1,IFLOT)*Y(N1)
     &                      +SHPFLO(2,IFLOT)*Y(N2)
     &                      +SHPFLO(3,IFLOT)*Y(N3)
                ZFLOT(IFLOT)=(Z(N1)*SHPFLO(1,IFLOT) 
     &                      +Z(N2)*SHPFLO(2,IFLOT)
     &                      +Z(N3)*SHPFLO(3,IFLOT))*(1.D0-SHZFLO(IFLOT))
     &                      +(Z(N4)*SHPFLO(1,IFLOT) 
     &                      +Z(N5)*SHPFLO(2,IFLOT) 
     &                      +Z(N6)*SHPFLO(3,IFLOT))*SHZFLO(IFLOT)
              ENDIF
            ENDIF   
          ENDDO 
        ENDIF        
!
      ENDIF
!
!     SENDING THE PARTICLES THAT MIGRATED TO ANOTHER SUB-DOMAIN
!
!========================================================================
!                           OILSPILL
!========================================================================  
      IF(NCSIZE.GT.1) THEN  
         CALL OIL_SEND_INFO(XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,ELTFLO,
     &        ETAFLO,ISUB,TAGFLO,NDP,NFLOT,NFLOT_MAX,
     &        MESH,NPLAN,PARTICULES,NB_COMPO,NB_HAP)
         CALL OIL_SEND_PARTICLES(XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,ELTFLO,
     &        ETAFLO,ISUB,TAGFLO,NDP,NFLOT,NFLOT_MAX,
     &        MESH,NPLAN,PARTICULES)
      ENDIF
!========================================================================
!                           OILSPILL
!========================================================================  
!
!-----------------------------------------------------------------------
!
!     CASE OF LOST FLOATS (EXITED OR NOW REMOVED AFTER BEING SENT TO
!                          ANOTHER SUB-DOMAIN) 
!
      IFLOT=1
      IF(NCSIZE.GT.1) THEN
!
!       IN // MODE
!
11      CONTINUE
!       LOST OR MIGRATED FLOATS
        IF(NFLOT.GT.0.AND.NCSIZE.GT.1) THEN
          IF(ELTFLO(IFLOT).LE.0.OR.ISUB(IFLOT).NE.IPID) THEN 
!===========================================================================
!                                     OILSPILL
!===========================================================================
             CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT,NFLOT_MAX,
     &            MESH%TYPELM,ISUB,PARTICULES,NB_COMPO,NB_HAP)
!===========================================================================
!                                     OILSPILL
!===========================================================================
            IF(IFLOT.LE.NFLOT) GO TO 11
         ENDIF
         IFLOT=IFLOT+1
         IF(IFLOT.LE.NFLOT) GO TO 11
      ENDIF

      ELSE
!
!       IN SCALAR MODE
!
10      CONTINUE
!       LOST FLOATS ONLY
        IF(NFLOT.GT.0) THEN
          IF(ELTFLO(IFLOT).LE.0) THEN 
!===========================================================================
!                                     OILSPILL
!===========================================================================
            CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT,NFLOT_MAX,
     &           MESH%TYPELM,ISUB,PARTICULES,NB_COMPO,NB_HAP)
!===========================================================================
!                                     OILSPILL
!===========================================================================
!           THE SAME IFLOT IS NOW A NEW PARTICLE AND MUST BE CHECKED AGAIN!
            IF(IFLOT.LE.NFLOT) GO TO 10
          ENDIF
          IFLOT=IFLOT+1
          IF(IFLOT.LE.NFLOT) GO TO 10
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TECPLOT FILE
!
      IF(NCSIZE.GT.1) THEN
!
!       WAITING ALL PROCESSORS (SO THAT NFLOT IS UPDATED FOR ALL
!                               BEFORE CALLING P_ISUM)
! 
        CALL P_SYNC
!
!       PARALLEL VERSION
!
        NFLOTG=P_ISUM(NFLOT)
        IF(NFLOTG.GT.0.AND.(LT.EQ.1.OR.(LT/FLOPRD)*FLOPRD.EQ.LT)) THEN
!
!         1) EVERY PROCESSOR WRITES ITS OWN DATA IN A FILE WITH EXTENSION
!
          IF(NFLOT.GT.0) THEN
            OPEN(99,FILE=EXTENS(NCSIZE,IPID+1),
     &           FORM='FORMATTED',STATUS='NEW')
            IF(IELM.EQ.11) THEN
              DO IFLOT=1,NFLOT
!========================================================================
!                           OILSPILL
!========================================================================
!                WRITE(99,300) TAGFLO(IFLOT),XFLOT(IFLOT),YFLOT(IFLOT),1
                 WRITE(99,300) PARTICULES(IFLOT)%ID,
     &                PARTICULES(IFLOT)%XOIL,PARTICULES(IFLOT)%YOIL,
     &                PARTICULES(IFLOT)%STATE
!========================================================================
!                           OILSPILL
!========================================================================
              ENDDO
            ELSE
              DO IFLOT=1,NFLOT
                WRITE(99,301) TAGFLO(IFLOT),XFLOT(IFLOT),YFLOT(IFLOT),
     &                        ZFLOT(IFLOT),1
              ENDDO
            ENDIF
            CLOSE(99)
          ENDIF
!
!         2) WAITING ALL PROCESSORS
!
          CALL P_SYNC
!
!         3) PROCESSOR 0 READS ALL EXISTING FILES AND MERGES 
!            THEM IN THE FINAL FILE
!
          IF(IPID.EQ.0) THEN      
            WRITE(UL,200) 'ZONE DATAPACKING=POINT, T="G_',AT,
     &      ' seconds"',', I=',NFLOTG,', SOLUTIONTIME=',AT
            DO IPROC=1,NCSIZE
              INQUIRE(FILE=EXTENS(NCSIZE,IPROC),EXIST=YESITIS)
              IF(YESITIS) THEN
                OPEN(99,FILE=EXTENS(NCSIZE,IPROC),
     &               FORM='FORMATTED',STATUS='OLD')
20              CONTINUE
                READ(99,100,ERR=21,END=21) LIGNE
                WRITE(UL,*) LIGNE
                GO TO 20
21              CONTINUE
                CLOSE(99,STATUS='DELETE')
              ENDIF
            ENDDO
          ENDIF
!
        ENDIF
!
      ELSE
!
!       SCALAR VERSION
!
        IF(NFLOT.GT.0.AND.(LT.EQ.1.OR.(LT/FLOPRD)*FLOPRD.EQ.LT)) THEN
          WRITE(UL,200) 'ZONE DATAPACKING=POINT, T="G_',AT,
     &                  ' seconds"',', I=',NFLOT,', SOLUTIONTIME=',AT
          IF(IELM.EQ.11) THEN
            DO IFLOT=1,NFLOT
               WRITE(UL,304) PARTICULES(IFLOT)%ID,
     &                PARTICULES(IFLOT)%XOIL,PARTICULES(IFLOT)%YOIL,
     &                PARTICULES(IFLOT)%SURFACE
!!              WRITE(UL,300) TAGFLO(IFLOT),XFLOT(IFLOT),YFLOT(IFLOT),1
            ENDDO
          ELSE
            DO IFLOT=1,NFLOT
              WRITE(UL,301) TAGFLO(IFLOT),XFLOT(IFLOT),
     &                      YFLOT(IFLOT),ZFLOT(IFLOT),1
            ENDDO
          ENDIF
200       FORMAT(A,F12.4,A,A,I4,A,F12.4)
300       FORMAT(I6,',',F16.8,',',F16.8,',',I2)
303       FORMAT(I6,',',F16.8,',',F16.8,',',I4)   
304       FORMAT(I6,',',F16.8,',',F16.8,',',F16.8)          
301       FORMAT(I6,',',F16.8,',',F16.8,',',F16.8,',',I2)
        ENDIF
!
      ENDIF      
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_DERIVE
!                       ************************
                        SUBROUTINE OIL_SPREADING
!                       ************************
!
     * (PARTICULES,VOLDEV,ETA_OIL,RHO_OIL,NFLOT,NFLOT_MAX,LT,DT)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   06/06/2013
!***********************************************************************
!
!brief
!+CETTE ROUTINE CALCULE L'ETALEMENT DE CHACUNE DES PARTICULES SELON LA 
!+FORMULATION PROPOSEE PAR FAY(1971). LA FORMULATION A ETE MODIFIEE
!+POUR QUE LE PHENOMENE D''ETALEMENT DE LA NAPPE NE DEPENDE PAS DU 
!+NOMBRE DE PARTICULES INJECTEES POUR LA MODELISATION DE LA POLLUTION
!+
!+
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU      
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: LT,NFLOT,NFLOT_MAX
      DOUBLE PRECISION, INTENT(IN) :: DT,RHO_OIL,ETA_OIL,VOLDEV        
!
       TYPE(OIL_PART),DIMENSION(NFLOT_MAX)::PARTICULES
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
       INTEGER::IFLOT
       DOUBLE PRECISION::PI,GRAVITE,RHO_EAU
       DOUBLE PRECISION::DELTA,VOL
       LOGICAL DEJA
       DATA DEJA/.FALSE./
       SAVE
!-----------------------------------------------------------------------

       IF(NFLOT.GT.0) THEN
          RHO_EAU=1000.D0
          PI=ACOS(-1.D0)
          GRAVITE=9.81D0
          DELTA=(RHO_EAU-RHO_OIL)/(RHO_EAU)
          DO IFLOT = 1,NFLOT
             IF(PARTICULES(IFLOT)%STATE.EQ.1)THEN
             IF(PARTICULES(IFLOT)%SURFACE.EQ.0.D0)THEN
                PARTICULES(IFLOT)%SURFACE=PI*((1.21D0)**4/(1.53)**2)*
     &               ((DELTA*GRAVITE)/(VOLDEV*(1.D-06)**2))**(1.D0/6.D0)
     &               *(VOLDEV/REAL(NFLOT_MAX))
                
                
             ELSE
              VOL=(PARTICULES(IFLOT)%MASS0/RHO_OIL)
              PARTICULES(IFLOT)%SURFACE=SQRT((PARTICULES(IFLOT)%SURFACE-
     &             PI*((1.21D0)**4/(1.53)**2)*
     &             ((DELTA*GRAVITE)/(VOLDEV*(1.D-06)**2))**(1.D0/6.D0)
     &             *(VOLDEV/REAL(NFLOT_MAX)))**2+
     &             (PI*1.21D0**2*((GRAVITE*DELTA)
     &             /(VOLDEV*SQRT(1.D-06)))**(1.D0/3.D0)
     &             *VOL)**2*DT)
     &             +PI*((1.21D0)**4/(1.53)**2)*
     &             ((DELTA*GRAVITE)/(VOLDEV*(1.D-06)**2))**(1.D0/6.D0)
     &             *(VOLDEV/REAL(NFLOT_MAX))
             END IF
             END IF
          END DO
       END IF
       
       RETURN
       END SUBROUTINE OIL_SPREADING
!                       *******************
                        SUBROUTINE OIL_EVAP
!                       *******************
!
     & (NB_COMPO,NB_HAP,PARTICULES,NFLOT,NFLOT_MAX,LT,DT)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+CETTE ROUTINE CALCULE L'EVAPORATION DE CHACUN DES COMPOSÉS DES PARTICULES 
!+SELON LA FORMULATION BASEE SUR LA THEORIE DU DOUBLE FILM DE WHITMAN (1923)
!+AINSI QUE LA FORMULATION DE CLAYPERON POUR LA DETERMINATION DE LA PRESSION
!+EN VAPEUR SATURANTE. POUR LIMITER LE NOMBRE DE VARIABLE LORS DU CALCUL DE 
!+LA PRESSION EN VAPEUR SATURANTE DE CHACUN DES PRODUITS CONSIDERES DANS L'
!+HYDROCARBURE, LE CALCUL DE L'ENTHALPIE MOLAIRE EST EFFECTUEE EN SE BASANT
!+SUR LA FORMULATION DE GRAY WATSON (2000) QUI NECESSITE UNIQUEMENT LA TEMPERATURE 
!+D'EBULLITION DU PRODUIT CONSIDÉRÉ.
!+LE COEFFICIENT DE TRANSFERT DE MASSE EVAPOREE DE L'HYDROCARBURE DEVERSÉ EST
!+CALCULÉ À PARTIR DE LA FORMULATION DE MACKAY ET MATSUGU (1973).
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          ::NB_COMPO,NB_HAP
      INTEGER, INTENT(IN)          :: LT,NFLOT,NFLOT_MAX
      DOUBLE PRECISION, INTENT(IN) :: DT         
      TYPE(OIL_PART),DIMENSION(NFLOT_MAX)::PARTICULES
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER::IFLOT,K
      DOUBLE PRECISION::VENT,TAMB,TOTALE
      DOUBLE PRECISION::Ke,MW
      DOUBLE PRECISION:: MASSE,BILAN
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::MASSE_EVAP_COMPO,C,D
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::MASSE_EVAP_HAP
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::MW_HAP,MW_COMPO
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      SAVE
!-----------------------------------------------------------------------

      IF(.NOT.DEJA)THEN
         ALLOCATE(MASSE_EVAP_COMPO(NB_COMPO))
         ALLOCATE(MW_COMPO(NB_COMPO))
         ALLOCATE(C(NB_COMPO))
         ALLOCATE(MASSE_EVAP_HAP(NB_HAP))
         ALLOCATE(MW_HAP(NB_HAP))
         ALLOCATE(D(NB_HAP)) 
         DEJA=.TRUE.
      END IF
!-----------------------------------------------------------------------
!--------------------PARAMETRE METEO------------------------------------
!-----------------------------------------------------------------------
      VENT=8.D0
      TAMB=273.15D0+18.D0
!-----------------------------------------------------------------------
!-----------------------INITIALISATION----------------------------------
!-----------------------------------------------------------------------
      IF(NFLOT.GT.0) THEN
         DO IFLOT = 1,NFLOT
            DO K=1,NB_COMPO
               MASSE_EVAP_COMPO(K)=0.D0
               C(K)=0.D0
               MW_COMPO(K)=0.D0
            END DO
            DO K=1,NB_HAP
               MASSE_EVAP_HAP(K)=0.D0
               D(K)=0.D0
               MW_HAP(K)=0.D0
            END DO
            MASSE=0.D0
!-----------------------------------------------------------------------
!---------------------CALCUL MASSE MOLAIRE COMPO------------------------
!-----------------------------------------------------------------------  
            DO K=1,NB_COMPO
               MW_COMPO(K)=
     *              0.04132D0-(1.985D-04*PARTICULES(IFLOT)%COMPO(K)%TB)
     *              +(9.494D-07*PARTICULES(IFLOT)%COMPO(K)%TB**2)
            END DO
!-----------------------------------------------------------------------
!---------------------CALCUL MASSE MOLAIRE HAP--------------------------
!-----------------------------------------------------------------------  
            DO K=1,NB_HAP
               MW_HAP(K)=
     *              0.04132D0-(1.985D-04*PARTICULES(IFLOT)%HAP(K)%TB)
     *              +(9.494D-07*PARTICULES(IFLOT)%HAP(K)%TB**2)
            END DO
!-----------------------------------------------------------------------
!----------CALCUL MASSE MOLAIRE DE L'HYDROCARBURE DEVERSE---------------
!-----------------------------------------------------------------------  
            MW=0.D0
            DO K=1,NB_COMPO
               MW=MW+(PARTICULES(IFLOT)%COMPO(K)%MASS/
     *              PARTICULES(IFLOT)%MASS)*MW_COMPO(K)
            END DO
            DO K=1,NB_HAP
               MW=MW+(PARTICULES(IFLOT)%HAP(K)%MASS/
     *              PARTICULES(IFLOT)%MASS)*MW_HAP(K)
            END DO
!-----------------------------------------------------------------------
!-------------CALCUL DU COEFFICIENT DE TRANSFERT DE MASSE---------------
!-----------------------------------------------------------------------  
            Ke=0.0048D0*VENT**0.78D0*1.3676D0*(0.018D0/MW)**(1.D0/3.D0)
!-----------------------------------------------------------------------
!-------------CALCUL DU NOMBRE DE MOL TOTAL DE LA NAPPE-----------------
!-----------------------------------------------------------------------
            TOTALE=0.D0
            DO K=1,NB_COMPO
               TOTALE=TOTALE+PARTICULES(IFLOT)%COMPO(K)%MASS/MW_COMPO(K)
            END DO
            DO K=1,NB_HAP
               TOTALE=TOTALE+PARTICULES(IFLOT)%HAP(K)%MASS/MW_HAP(K)
            END DO
!------------------------------------------------------------------------------
!--------CALCUL DE LA MASSE  DE CHAQUE COMPO DE LHYDROCARBURE------------------
!--------------------------APRES EVAPORATION-----------------------------------
!------------------------------------------------------------------------------            
            DO K=1,NB_COMPO
               MASSE_EVAP_COMPO(K)=0.D0
               C(K)=PARTICULES(IFLOT)%COMPO(K)%MASS
               IF(PARTICULES(IFLOT)%COMPO(K)%MASS.GT.0.D0)THEN
                  PARTICULES(IFLOT)%COMPO(K)%MASS= 
     *                 PARTICULES(IFLOT)%COMPO(K)%MASS*
     *                 EXP(-((Ke*PARTICULES(IFLOT)%SURFACE*DT*
     *                 EXP(DLOG(82.06D0*PARTICULES(IFLOT)%COMPO(K)%TB)*
     *                (3.D0-2.D0*(TAMB/PARTICULES(IFLOT)%COMPO(K)%TB))**
     *                 (0.4133D0-0.2575D0*(TAMB/
     *                 PARTICULES(IFLOT)%COMPO(K)%TB))
     *                 *(1.D0-(PARTICULES(IFLOT)%COMPO(K)%TB/TAMB))))/
     *                 (8.206D-5*TAMB*TOTALE)))

                  MASSE_EVAP_COMPO(K)=C(K)-
     *                 PARTICULES(IFLOT)%COMPO(K)%MASS

                  IF(PARTICULES(IFLOT)%COMPO(K)%MASS.LE.0.D0) THEN 
                     PARTICULES(IFLOT)%COMPO(K)%MASS=0.D0
                     MASSE_EVAP_COMPO(K)=C(K)
                  END IF
                     
               ELSE
                  PARTICULES(IFLOT)%COMPO(K)%MASS=0.D0
                  MASSE_EVAP_COMPO(K)=0.D0
               END IF
            END DO
!------------------------------------------------------------------------------
!----------CALCUL DE LA MASSE  DE CHAQUE HAP DE LHYDROCARBURE------------------
!--------------------------APRES EVAPORATION-----------------------------------
!------------------------------------------------------------------------------  
            DO K=1,NB_HAP
               MASSE_EVAP_HAP(K)=0.D0
               D(K)=PARTICULES(IFLOT)%HAP(K)%MASS
               IF(PARTICULES(IFLOT)%HAP(K)%MASS.GT.0.D0)THEN
                  PARTICULES(IFLOT)%HAP(K)%MASS= 
     *                 PARTICULES(IFLOT)%HAP(K)%MASS*
     *                 EXP(-((Ke*PARTICULES(IFLOT)%SURFACE*DT*
     *                 EXP(DLOG(82.06D0*PARTICULES(IFLOT)%HAP(K)%TB)*
     *                 (3.D0-2.D0*(TAMB/PARTICULES(IFLOT)%HAP(K)%TB))**
     *                 (0.4133D0-0.2575D0*(TAMB/
     *                 PARTICULES(IFLOT)%HAP(K)%TB))
     *                 *(1.D0-(PARTICULES(IFLOT)%HAP(K)%TB/TAMB))))/
     *                 (8.206D-5*TAMB*TOTALE)))

                  MASSE_EVAP_HAP(K)=D(K)-PARTICULES(IFLOT)%HAP(K)%MASS

                  IF(PARTICULES(IFLOT)%HAP(K)%MASS.LE.0.D0) THEN 
                     PARTICULES(IFLOT)%HAP(K)%MASS=0.D0
                     MASSE_EVAP_HAP(K)=D(K)
                  END IF  
               ELSE
                  PARTICULES(IFLOT)%HAP(K)%MASS=0.D0
                  MASSE_EVAP_HAP(K)=0.D0
               END IF
            END DO 
            MASSE=0.D0
            BILAN=0.D0 
            DO K=1,NB_COMPO
               MASSE=MASSE+PARTICULES(IFLOT)%COMPO(K)%MASS
               BILAN=BILAN+MASSE_EVAP_COMPO(K)
            END DO
            DO K=1,NB_HAP
               MASSE=MASSE+PARTICULES(IFLOT)%HAP(K)%MASS
               BILAN=BILAN+MASSE_EVAP_HAP(K)
            END DO
            PARTICULES(IFLOT)%MASS=MASSE
           PARTICULES(IFLOT)%MASS_EVAP=PARTICULES(IFLOT)%MASS_EVAP+BILAN                  
         END DO
      END IF      
      RETURN
      END SUBROUTINE OIL_EVAP
!                       ********************
                        SUBROUTINE OIL_DISSO
!                       ********************
!
     * (PARTICULES,NB_COMPO,NB_HAP,NFLOT,NFLOT_MAX,LT,DT,
     * NELMAX,IKLE,HN,NPOIN,UNSV2D,TN,NTRAC,TB,MESH)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+CETTE ROUTINE CALCULE LA DISSOLUTION DE CHACUN DES HAP COMPOSANT L'HYDROCARBURE 
!+SELON LA FORMULATION BASEE SUR LA THEORIE DU DOUBLE FILM DE WHITMAN (1923)
!+LE COEFFICIENT DE TRANSFERT DE MASSE DISSOUTE EST À RENSEIGNER PAR L'UTILISATEUR.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      USE BIEF                  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: LT,NFLOT_MAX,NB_COMPO,NTRAC
      INTEGER, INTENT(IN)          :: NB_HAP,NFLOT
      DOUBLE PRECISION, INTENT(IN) :: DT
      INTEGER         , INTENT(IN) :: NELMAX,NPOIN
      INTEGER         , INTENT(IN) :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(INOUT) :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: UNSV2D(NPOIN)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TN    
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TB  
      TYPE(OIL_PART),DIMENSION(NFLOT_MAX)::PARTICULES
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER::NDP
      INTEGER::I,K,J,IFLOT,IEL,I1,I2,I3,N
      DOUBLE PRECISION::X0,MASSE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::VOL,HAUT
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::C
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::ALPHA
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::M
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::MW_HAP,MW_COMPO
      DOUBLE PRECISION::TOT1,TOT2,TOT3,TOTALE
      DOUBLE PRECISION KDISS(4)
      DATA KDISS/5.55555555D-06,5.55555555D-06,
     *    5.55555555D-06,5.55555555D-06/
      LOGICAL DEJA
      DATA DEJA /.FALSE./
      SAVE
!-----------------------------------------------------------------------
! ----------------ALLOCATION DES TABLEAUX-------------------------------
!-----------------------------------------------------------------------
      NDP = 3
      IF(.NOT.DEJA)THEN
         ALLOCATE(VOL(NDP))
         ALLOCATE(HAUT(NDP))
         ALLOCATE(C(NB_HAP,NDP))
         ALLOCATE(M(NDP,NB_HAP))
         ALLOCATE(ALPHA(NB_HAP,NDP))
         ALLOCATE(MW_HAP(NB_HAP))
         ALLOCATE(MW_COMPO(NB_COMPO))
         DEJA=.TRUE.
      END IF
      DO K= 1,NB_HAP
         CALL CPSTVC(TN%ADR(K)%P,TB%ADR(K)%P)
      END DO
      CALL OS('X=0     ',TB)
      IF(NFLOT.GT.0) THEN
         DO IFLOT = 1,NFLOT
            IF(PARTICULES(IFLOT)%STATE.EQ.1)THEN
!--------------------------------------------------------------
! -------------------INITIALISATION----------------------------
!--------------------------------------------------------------
            DO K=1,NB_HAP
               C(K,1)=0.D0
               C(K,2)=0.D0
               C(K,3)=0.D0
               MW_HAP(K)=0.D0
               M(1,K)=0.D0
               M(2,K)=0.D0
               M(3,K)=0.D0
               ALPHA(K,1)=0.D0
               ALPHA(K,2)=0.D0
               ALPHA(K,3)=0.D0 
            END DO
            DO K=1,NB_COMPO
               MW_COMPO(K)=0.D0
            END DO
               HAUT(1)=0.D0
               HAUT(2)=0.D0
               HAUT(3)=0.D0 

            I1=IKLE(PARTICULES(IFLOT)%ELTOIL,1)
            I2=IKLE(PARTICULES(IFLOT)%ELTOIL,2)
            I3=IKLE(PARTICULES(IFLOT)%ELTOIL,3)
!-----------------------------------------------------------------------
!---------------------CALCUL MASSE MOLAIRE COMPO------------------------
!-----------------------------------------------------------------------  
            DO K=1,NB_COMPO
               MW_COMPO(K)=
     *              0.04132D0-(1.985D-04*PARTICULES(IFLOT)%COMPO(K)%TB)
     *              +(9.494D-07*PARTICULES(IFLOT)%COMPO(K)%TB**2.D0)
            END DO
!-----------------------------------------------------------------------
!---------------------CALCUL MASSE MOLAIRE HAP--------------------------
!-----------------------------------------------------------------------  
            DO K=1,NB_HAP
               MW_HAP(K)=
     *              0.04132D0-(1.985D-04*PARTICULES(IFLOT)%HAP(K)%TB)
     *              +(9.494D-07*PARTICULES(IFLOT)%HAP(K)%TB**2.D0)
            END DO
!-----------------------------------------------------------------------
!-------------CALCUL DU NOMBRE DE MOL TOTAL DE LA NAPPE-----------------
!-----------------------------------------------------------------------
            TOTALE=0.D0
            DO K=1,NB_COMPO
               TOTALE=TOTALE+PARTICULES(IFLOT)%COMPO(K)%MASS/MW_COMPO(K)
            END DO
            DO K=1,NB_HAP
               TOTALE=TOTALE+PARTICULES(IFLOT)%HAP(K)%MASS/MW_HAP(K)
            END DO
            HAUT(1)=HN(I1)
            HAUT(2)=HN(I2)
            HAUT(3)=HN(I3)
            VOL(1)=HN(I1)/UNSV2D(I1)
            VOL(2)=HN(I2)/UNSV2D(I2)
            VOL(3)=HN(I3)/UNSV2D(I3)
            DO K=1,NB_HAP
               IF(HN(I1).GE.1.D-04)THEN
                  ALPHA(K,1)=((KDISS(K)*(PARTICULES(IFLOT)%SURFACE))
     *                 *PARTICULES(IFLOT)%SHPOIL(1))*(UNSV2D(I1)/HN(I1))
               END IF
               IF(HN(I2).GE.1.D-04)THEN
                  ALPHA(K,2)=((KDISS(K)*(PARTICULES(IFLOT)%SURFACE))
     *                 *PARTICULES(IFLOT)%SHPOIL(2))*(UNSV2D(I2)/HN(I2))
               END IF
               IF(HN(I3).GE.1.D-04)THEN
                  ALPHA(K,3)=((KDISS(K)*(PARTICULES(IFLOT)%SURFACE))
     *                 *PARTICULES(IFLOT)%SHPOIL(3))*(UNSV2D(I3)/HN(I3))
               END IF
            END DO 

            DO K=1,NB_HAP
               IF(PARTICULES(IFLOT)%HAP(K)%MASS.GT.0.D0)THEN
                  IF(TOTALE.GT.0.D0)THEN
                     X0=(PARTICULES(IFLOT)%HAP(K)%MASS/MW_HAP(K))/TOTALE
                  ELSE
                     X0=0.D0
                  END IF
                  C(K,1)=TN%ADR(K)%P%R(I1)
                  C(K,2)=TN%ADR(K)%P%R(I2)
                  C(K,3)=TN%ADR(K)%P%R(I3)
                  DO J=1,NDP
                   IF(HAUT(J).GE.1.D-04.AND.ALPHA(K,J)*DT.LT.1.D-10)THEN
                      M(J,K)=0.D0 
                      M(J,K)=ALPHA(K,J)*DT*VOL(J)*
     *                     (PARTICULES(IFLOT)%HAP(K)%SOL*X0-C(K,J))
                      M(J,K)=MAX(M(J,K),0.D0)
                   ELSE IF(HAUT(J).GE.1.D-04.AND.
     *                     ALPHA(K,J)*DT.GE.1.D-10)THEN
                      M(J,K)=0.D0
                      M(J,K)=VOL(J)*(PARTICULES(IFLOT)%HAP(K)%SOL
     *                     *X0-C(K,J))*(1.D0-EXP(-ALPHA(K,J)*DT))
                      M(J,K)=MAX(M(J,K),0.D0)
                   ELSE
                      M(J,K)=0.D0
                   END IF
                  END DO
               ELSE
                  M(1,K)=0.D0
                  M(2,K)=0.D0
                  M(3,K)=0.D0
               END IF
            END DO
            DO K=1,NB_HAP
               IF((M(1,K)+M(2,K)+M(3,K)).GT.
     *              PARTICULES(IFLOT)%HAP(K)%MASS)THEN
                  DO J=1,NDP
                     M(J,K)=(M(J,K)*
     *                    PARTICULES(IFLOT)%HAP(K)%MASS)/(
     *                    M(1,K)+M(2,K)+M(3,K))
                  END DO
               ELSE
                  M(1,K)=M(1,K)
                  M(2,K)=M(2,K)
                  M(3,K)=M(3,K)
               END IF
            END DO
            TOT1=0.D0
            TOT2=0.D0
            TOT3=0.D0
            MASSE=0.D0
            DO K=1,NB_COMPO
               MASSE=MASSE+PARTICULES(IFLOT)%COMPO(K)%MASS
            END DO
            
            DO K=1,NB_HAP
             PARTICULES(IFLOT)%HAP(K)%MASS=PARTICULES(IFLOT)%HAP(K)%MASS
     *              -(M(1,K)+M(2,K)+M(3,K))
             PARTICULES(IFLOT)%HAP(K)%MASS=
     *            MAX(0.D0,PARTICULES(IFLOT)%HAP(K)%MASS)
             TOT1=TOT1+M(1,K)
             TOT2=TOT2+M(2,K)
             TOT3=TOT3+M(3,K)
             MASSE=MASSE+PARTICULES(IFLOT)%HAP(K)%MASS
            END DO  
            PARTICULES(IFLOT)%MASS_DISS=PARTICULES(IFLOT)%MASS_DISS+
     *           TOT1+TOT2+TOT3
            PARTICULES(IFLOT)%MASS=MASSE
!-----------------------------------------------------------------------
!---------------QUANTITE DE TRACEUR LACHEE------------------------------
!-----------------------------------------------------------------------
            IF(HN(I1).GE.0.0001D0)THEN
               DO K=1,NB_HAP
                  TB%ADR(K)%P%R(I1)=TB%ADR(K)%P%R(I1)+M(1,K)
     *                 *(UNSV2D(I1)/HN(I1))
               END DO
            END IF
            IF(HN(I2).GE.0.0001D0)THEN
               DO K=1,NB_HAP
                  TB%ADR(K)%P%R(I2)=TB%ADR(K)%P%R(I2)+M(2,K)
     *                 *(UNSV2D(I2)/HN(I2))
               END DO
            END IF
            IF(HN(I3).GE.0.0001D0)THEN
               DO K=1,NB_HAP
                  TB%ADR(K)%P%R(I3)=TB%ADR(K)%P%R(I3)+M(3,K)
     *                 *(UNSV2D(I3)/HN(I3))
               END DO
            END IF 
            END IF
         END DO
      END IF
      DO K=1,NB_HAP
         IF(NCSIZE.GT.1) CALL PARCOM(TB%ADR(K)%P,2,MESH)
         CALL OS('X=X+Y   ',X=TN%ADR(K)%P ,Y=TB%ADR(K)%P)
      END DO
      RETURN
      END SUBROUTINE OIL_DISSO
!                       *********************
                        SUBROUTINE OIL_VOLATI
!                       *********************
!
     *(T3,TIMP,YASMI,HPROP,NTRAC,PARTICULES,NFLOT_MAX,NFLOT,
     *NELMAX,IKLE,NPOIN,MESH)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+CETTE ROUTINE CALCULE LA VOLATILISATION DE CHACUN DES HAP COMPOSANT L'HYDROCARBURE 
!+GRACE À UN TERME SOURCE IMPLICITE À RENSEIGNER PAR L'UTILISATEUR. LES TERMES
!+SOURCES CORRESPONDENT AUX COEFFICIENTS DE TRANSFERT DE MASSE.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      USE BIEF                  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NTRAC,NFLOT_MAX
      INTEGER         , INTENT(IN)    :: NELMAX,NPOIN,NFLOT
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,3)
      LOGICAL, INTENT(INOUT)          :: YASMI(*) 
      TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3,TIMP
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(OIL_PART),DIMENSION(NFLOT_MAX)::PARTICULES
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER I1,I2,I3,IFLOT,ITRAC,I
      INTEGER P_IMAX 
      EXTERNAL P_IMAX
      DOUBLE PRECISION KVOL(4)
      DATA KVOL/1.2D-05,1.2D-05,1.2D-05,1.2D-05/
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     TERMES SOURCES EXPLICITES (ICI MIS A ZERO)
!
      DO ITRAC=1,NTRAC
         CALL OS('X=0     ',X=TIMP%ADR(ITRAC)%P)
      ENDDO
!
!-----------------------------------------------------------------------
      CALL CPSTVC(TIMP%ADR(1)%P,T3)
      CALL OS('X=0     ',T3)
      IF(NFLOT.GT.0) THEN
         DO IFLOT = 1,NFLOT
            I1=IKLE(PARTICULES(IFLOT)%ELTOIL,1)
            I2=IKLE(PARTICULES(IFLOT)%ELTOIL,2)
            I3=IKLE(PARTICULES(IFLOT)%ELTOIL,3)

            T3%R(I1)=T3%R(I1)+1.D0
            T3%R(I2)=T3%R(I2)+1.D0
            T3%R(I3)=T3%R(I3)+1.D0
         END DO
      END IF
      IF(NCSIZE.GT.1) CALL PARCOM(T3,2,MESH)
      IF(P_IMAX(NFLOT).GT.0)THEN
         DO ITRAC=1,NTRAC
            DO I=1,NPOIN
               IF(T3%R(I).LT.0.5D0.AND.HPROP%R(I).GE.1.D-2)THEN
                  TIMP%ADR(ITRAC)%P%R(I)=-KVOL(ITRAC)	
               ELSE
                  TIMP%ADR(ITRAC)%P%R(I)=0.D0
               END IF
            END DO
            YASMI(ITRAC)=.TRUE.
         END DO
      END IF

      RETURN  
      END SUBROUTINE OIL_VOLATI
!                       ***********************
                        SUBROUTINE OIL_BEACHING
!                       ***********************
!
     &(IKLE,NPOIN,NELMAX,NDP,H,HN,PARTICULES,NFLOT_MAX,NFLOT,RHO_OIL,
     &     SURFAC,CF,ETA_OIL,LT)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+CETTE ROUTINE CALCULE L'ECHOUAGE DES PARTICULES D'HYDROCARBURES EN 
!FONCTION DU TYPE DE BERGE ESTIME PAR LA TAILLE DE GRAIN.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      USE BIEF                  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER         , INTENT(IN)    :: NELMAX,NDP,NPOIN
      INTEGER         , INTENT(IN)    :: NFLOT_MAX,NFLOT,LT
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: RHO_OIL,ETA_OIL
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      TYPE(OIL_PART),DIMENSION(NFLOT_MAX)::PARTICULES 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,N1,N2,N3,K,R,J,NECH
!
      INTEGER IEL
      DOUBLE PRECISION HAUTPART,VOLECH,VOLDIS,L,M
      DOUBLE PRECISION Tm,Cv,Dp,Ks,EPAISSEUR
      DOUBLE PRECISION,DIMENSION(NPOIN)::HH
!
      Tm=0.D0
      Cv=0.D0
      Dp=0.D0

      DO  IFLOT=1,NFLOT
         IF(PARTICULES(IFLOT)%STATE.EQ.1)THEN
            HAUTPART=0.D0
            L=0.D0
            Ks=0.D0
            EPAISSEUR=0.D0
            IEL=PARTICULES(IFLOT)%ELTOIL
            N1=IKLE(IEL,1)
            N2=IKLE(IEL,2)
            N3=IKLE(IEL,3)
            HAUTPART=H(N1)*PARTICULES(IFLOT)%SHPOIL(1)+
     &           H(N2)*PARTICULES(IFLOT)%SHPOIL(2)+
     &           H(N3)*PARTICULES(IFLOT)%SHPOIL(3)

            EPAISSEUR=(PARTICULES(IFLOT)%MASS/RHO_OIL)/
     &           PARTICULES(IFLOT)%SURFACE

           L=MAX(PARTICULES(IFLOT)%SHPOIL(1),PARTICULES(IFLOT)%SHPOIL(2)
     *           ,PARTICULES(IFLOT)%SHPOIL(3))
           IF(L.EQ.PARTICULES(IFLOT)%SHPOIL(1)) J=N1
           IF(L.EQ.PARTICULES(IFLOT)%SHPOIL(2)) J=N2
           IF(L.EQ.PARTICULES(IFLOT)%SHPOIL(3)) J=N3

           HH(J)=MAX(HN(J),1.D-04)
           Ks=(CF(J)**3*(8.2D0)**6*HH(J))/8.D0
           Ks=MAX(Ks,6.5D-05)

           IF(HAUTPART.LE.EPAISSEUR.OR.Ks.GE.HAUTPART)THEN
              IF(Ks.GE.6.5D-05.AND.Ks.LT.1.D-03)THEN
                 Dp=5.D-02
                 IF(ETA_OIL.LT.3.D-05)THEN
                    Tm=4.D-03
                    Cv=0.004D0
                 ELSE IF(ETA_OIL.GE.3.D-05.AND.ETA_OIL.LE.2.D-03)THEN
                    Tm=17.D-03
                    Cv=0.017D0
                 ELSEIF(ETA_OIL.GT.2.D-03)THEN
                    Tm=25.D-03
                    Cv=0.025D0
                 END IF
              ELSE IF(Ks.GE.1.D-03.AND.Ks.LT.256.D-03)THEN
                 Dp=18.D-02
                 IF(ETA_OIL.LT.3.D-05)THEN
                    Tm=2.D-03
                    Cv=0.0021D0
                 ELSE IF(ETA_OIL.GE.3.D-05.AND.ETA_OIL.LE.2.D-03)THEN
                    Tm=9.D-03
                    Cv=0.0091D0
                 ELSEIF(ETA_OIL.GT.2.D-03)THEN
                    Tm=15.D-03
                    Cv=0.0151D0
                 END IF
              ELSEIF(Ks.GE.256.D-03)THEN
                 Dp=0.D0
                 IF(ETA_OIL.LT.3.D-05)THEN
                    Tm=1.D-03
                    Cv=0.001D0
                 ELSE IF(ETA_OIL.GE.3.D-05.AND.ETA_OIL.LE.2.D-03)THEN
                    Tm=5.D-03
                    Cv=0.005D0
                  ELSEIF(ETA_OIL.GT.2.D-03)THEN
                     Tm=10.D-03
                     Cv=0.01D0
                  END IF
               END IF

               VOLECH=0.D0
               VOLDIS=0.D0

               DO K=1,NFLOT
                  NECH=0
                  R=0
                  IF(K.NE.IFLOT.AND.PARTICULES(K)%STATE.EQ.2.AND.
     *                 PARTICULES(K)%ELTOIL.EQ.IEL)THEN
                     M=0.D0
                     M=MAX(PARTICULES(K)%SHPOIL(1)
     *                    ,PARTICULES(K)%SHPOIL(2)
     *                    ,PARTICULES(K)%SHPOIL(3))
                     IF(M.EQ.PARTICULES(K)%SHPOIL(1)) R=1
                     IF(M.EQ.PARTICULES(K)%SHPOIL(2)) R=2
                     IF(M.EQ.PARTICULES(K)%SHPOIL(3)) R=3
                     
                     NECH=IKLE(PARTICULES(K)%ELTOIL,R)
                     IF(J.EQ.NECH)THEN
                        VOLECH=VOLECH+(PARTICULES(K)%MASS)/RHO_OIL
                     END IF
                  END IF
               END DO
!DANS LE CALCUL DU VOLUME DE BERGE DISPONIBLE L'INCLINAISON DE LA BERGE EST
!NÉGLIGÉ DANS LA FORMULATION CI-DESSOUS
               VOLDIS=(1.D0/3.D0)*SURFAC(IEL)*Tm+0.5D0*Cv*Dp
     &              *SQRT((1.D0/3.D0)*SURFAC(IEL))-VOLECH

               IF((PARTICULES(IFLOT)%MASS/RHO_OIL).LE.VOLDIS)THEN
                  PARTICULES(IFLOT)%STATE=2
                  PARTICULES(IFLOT)%TPSECH=LT
               END IF
            END IF
 
!
         ENDIF
!     
      END DO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_BEACHING
!                       *************************
                        SUBROUTINE OIL_REFLOATING
!                       *************************
!
     *(LT,DT,NPOIN,NELMAX,NDP,IKLE,H,HN,PARTICULES,RHO_OIL,
     &     NFLOT_MAX,NFLOT,CF)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+CETTE ROUTINE CALCULE LE RELARAGE DES PARTICULES ECHOUÉES D'HYDROCARBURES EN 
!FONCTION DU TYPE DE BERGE ESTIME PAR LA TAILLE DE GRAIN.
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      USE BIEF                  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,LT,NDP
      INTEGER         , INTENT(IN)    :: NELMAX
      INTEGER         , INTENT(IN)    :: NFLOT_MAX,NFLOT
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: RHO_OIL,DT
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      TYPE(OIL_PART),DIMENSION(NFLOT_MAX)::PARTICULES 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,N1,N2,N3,J,IEL
!
      DOUBLE PRECISION HAUTPART,Ks,Kf,L
      DOUBLE PRECISION Pref,RAND,EPAISSEUR
      DOUBLE PRECISION,DIMENSION(NPOIN)::HH
!
!-----------------------------------------------------------------------
!
!
      DO  IFLOT=1,NFLOT
         IF(PARTICULES(IFLOT)%STATE.EQ.2)THEN
            HAUTPART=0.D0
            L=0.D0
            Ks=0.D0
            EPAISSEUR=0.D0
            IEL=PARTICULES(IFLOT)%ELTOIL
            N1=IKLE(IEL,1)
            N2=IKLE(IEL,2)
            N3=IKLE(IEL,3)
            
            HAUTPART=H(N1)*PARTICULES(IFLOT)%SHPOIL(1)+
     *           H(N2)*PARTICULES(IFLOT)%SHPOIL(2)+
     *           H(N3)*PARTICULES(IFLOT)%SHPOIL(3)

            EPAISSEUR=(PARTICULES(IFLOT)%MASS/RHO_OIL)/
     *           PARTICULES(IFLOT)%SURFACE
                        
           L=MAX(PARTICULES(IFLOT)%SHPOIL(1),PARTICULES(IFLOT)%SHPOIL(2)
     *           ,PARTICULES(IFLOT)%SHPOIL(3))
            IF(L.EQ.PARTICULES(IFLOT)%SHPOIL(1)) J=N1
            IF(L.EQ.PARTICULES(IFLOT)%SHPOIL(2)) J=N2
            IF(L.EQ.PARTICULES(IFLOT)%SHPOIL(3)) J=N3
            
            HH(J)=MAX(HN(J),1.D-04)
            Ks=((CF(J))**3*(8.2D0)**6*HH(J))/8.D0

            Ks=MAX(Ks,6.5D-05)
            IF(HAUTPART.GT.EPAISSEUR.AND.Ks.LT.HAUTPART)THEN
               PARTICULES(IFLOT)%STATE=1
               PARTICULES(IFLOT)%TPSECH=0

            ELSE
               Pref=0.D0

               IF(Ks.GE.6.5D-05.AND.Ks.LT.1.D-03)THEN
                  Kf=0.25D0/(24.D0*60.D0*60.D0)
               ELSE IF(Ks.GE.1.D-03.AND.Ks.LT.256.D-03)THEN
                  Kf=0.15D0/(24.D0*60.D0*60.D0)   
               ELSEIF(Ks.GE.256.D-03)THEN
                  Kf=0.85D0/(24.D0*60.D0*60.D0)  
               END IF
               
               Pref=1.D0-EXP(-Kf*(LT-PARTICULES(IFLOT)%TPSECH)*DT)
            
               CALL RANDOM_NUMBER(RAND)

               IF(RAND.LT.Pref)THEN
                  PARTICULES(IFLOT)%STATE=1
                  PARTICULES(IFLOT)%TPSECH=0
               END IF
            END IF
          ENDIF
!
       END DO
!     
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_REFLOATING  
!
      END MODULE OILSPILL

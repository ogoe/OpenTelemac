!
!==========================================================================
!                            MODULE OILSPILL
!==========================================================================
!
!***********************************************************************
! TELEMAC2D & TELEMAC3D   V7P0                                22/05/2014
!***********************************************************************
!
!brief    OIL SPILL MODEL.
!+
!+
!+            CALLED IF KEYWORD 'OIL SPILL MODEL' IS SET TO YES
!+                AND USES 'MIGRHYCAR STEERING FILE'.
!
!
!history  CEDRIC GOEURY (LHSV & LNHE)
!+        22/05/2014
!+        V7P0
!+  Second version
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!=======================================================================
!
!  STRUCTURES FOR OIL SPILL PARTICLES
!
!=======================================================================
!
!      TYPE COMPO
!        DOUBLE PRECISION  SOL---> SOLUBILITY OF OIL COMPONENT
!        DOUBLE PRECISION  MASS---> MASS OF OIL COMPONENT
!        DOUBLE PRECISION  TB---> BOILING TEMPERATURE OF OIL COMPONENT
!      END TYPE COMPO
!
!      TYPE OIL_PART
!        INTEGER  STATE---> STATE OF THE OIL PARTICLE (1:DRIFT PARTICLE,2:BEACHING PARTICLE)
!        INTEGER  ELTOIL---> NUMBERS OF ELEMENTS WHERE THE PARTICLES ARE
!        INTEGER  ETAOIL---> LEVELS WHERE ARE THE PARTICLES
!        INTEGER  TPSECH---> BEACHING TIME OF THE PARTICLE
!        INTEGER  ID---> TAG OF THE PARTICLE
!        DOUBLE PRECISION  XOIL,YOIL,ZOIL---> POSITIONS OF FLOATING OIL PARTICLE
!        DOUBLE PRECISION  MASS0---> INITIAL MASS OF THE PARTICLE
!        DOUBLE PRECISION  MASS---> MASS OF THE PARTICLE
!        DOUBLE PRECISION  MASS_DISS---> DISSOLVED MASS OF THE PARTICLE
!        DOUBLE PRECISION  MASS_EVAP---> EVAPORATED MASS OF THE PARTICLE
!        DOUBLE PRECISION  SURFACE---> SURFACE OF THE PARTICLE
!        DOUBLE PRECISION,DIMENSION(3)  SHPOIL---> BARYCENTRIC COORDINATES OF PARTICLE IN THEIR ELEMENTS
!        DOUBLE PRECISION  SHZOIL---> BARYCENTRIC COORDINATE ON VERTICAL
!        TYPE(COMPO),DIMENSION(:),ALLOCATABLE  COMPO---> UNSOLUBLE COMPONENT OF OIL
!        TYPE(COMPO),DIMENSION(:),ALLOCATABLE  HAP---> SOLUBLE COMPONENT OF OIL
!      END TYPE OIL_PART
!
!=======================================================================
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE OILSPILL
      USE BIEF
!
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ) :: UCONV_OIL,VCONV_OIL
      TYPE(OIL_PART),DIMENSION(:),ALLOCATABLE ::PARTICULES
      INTEGER :: NB_HAP
!
      CONTAINS
!
!                    ***********************
                     SUBROUTINE OIL_SPILL_2D
!                    ***********************
!
     &(YASMI)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    OIL SPILL MODEL.
!+
!+
!+            CALLED IF THE KEYWORD 'OIL SPILL MODEL' IS SET TO YES
!+                AND USES 'OIL SPILL STEERING FILE'.
!
!note     LOGICAL UNIT OF OIL SPILL STEERING FILE IS: T2D_FILES(T2DMIG)%LU
!
!warning  DOES NOTHING BY DEFAULT
!
!history  CEDRIC GOEURY (LHSV)
!+        20/04/2010
!+        V6P0
!+
!
!history  CEDRIC GOEURY (LNHE)
!+        28/06/2013
!+        V6P3
!+   First version
!+
!
!history  CEDRIC GOEURY (LNHE)
!+        25/05/2014
!+        V7P0
!+   Second version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| YASMI          |<->| IF YES, THERE ARE IMPLICIT SOURCE TERMS
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
      LOGICAL, INTENT(INOUT) :: YASMI(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL INIT
      DATA INIT /.FALSE./
      INTEGER I,K,IFLOT,ETAL
      INTEGER NB_COMPO,NB_HAP
      DOUBLE PRECISION ETA_OIL,RHO_OIL,VOLDEV
      DOUBLE PRECISION TAMB,VERIF,AREA
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: TB_COMPO,FM_COMPO
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: TB_HAP,FM_HAP,SOLU
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: KDISS,KVOL
      DOUBLE PRECISION,PARAMETER::KARMAN=0.41D0
!
      SAVE RHO_OIL,VOLDEV,NB_COMPO,ETA_OIL,NB_HAP,TAMB,ETAL
      SAVE TB_COMPO,FM_COMPO,TB_HAP,FM_HAP,SOLU,KDISS,KVOL,AREA
!
!-----------------------------------------------------------------------
!
      IF(.NOT.INIT) THEN
!
        CALL BIEF_ALLVEC(1,UCONV_OIL,'UCONVO',11,1,2,MESH)
        CALL BIEF_ALLVEC(1,VCONV_OIL,'VCONVO',11,1,2,MESH)
!
!
        VERIF=0.D0
!
        WRITE(LU,*) 'NFLOT_MAX = ',NFLOT_MAX
        ALLOCATE(PARTICULES(NFLOT_MAX))
!
!======================================================================
!----------------READING THE INPUT STEERING FILE-----------------------
!======================================================================
!
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3)
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3) NB_COMPO
        WRITE(LU,*) 'NB_COMPO = ',NB_COMPO
!
        DO I=1,NFLOT_MAX
          ALLOCATE(PARTICULES(I)%COMPO(MAX(NB_COMPO,1)))
        END DO
!
        ALLOCATE(TB_COMPO(MAX(NB_COMPO,1)))
        ALLOCATE(FM_COMPO(MAX(NB_COMPO,1)))
        IF(NB_COMPO.GT.0)THEN
          READ(T2D_FILES(T2DMIG)%LU,*)
          DO I=1,NB_COMPO
            READ(T2D_FILES(T2DMIG)%LU,*) FM_COMPO(I),
     &            TB_COMPO(I)
            VERIF=VERIF+FM_COMPO(I)
            WRITE(LU,*) 'COMPO',I,FM_COMPO(I),
     &           TB_COMPO(I)
          END DO
        END IF
!
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3)
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3) NB_HAP
        WRITE(LU,*) 'NB_HAP = ',NB_HAP
!
        DO I=1,NFLOT_MAX
          ALLOCATE(PARTICULES(I)%HAP(MAX(NB_HAP,1)))
        END DO
!
        ALLOCATE(TB_HAP(MAX(NB_HAP,1)))
        ALLOCATE(FM_HAP(MAX(NB_HAP,1)))
        ALLOCATE(SOLU(MAX(NB_HAP,1)))
        ALLOCATE(KDISS(MAX(NB_HAP,1)))
        ALLOCATE(KVOL(MAX(NB_HAP,1)))
        IF(NB_HAP.GT.0)THEN
          READ(T2D_FILES(T2DMIG)%LU,*)
            DO I=1,NB_HAP
             READ(T2D_FILES(T2DMIG)%LU,*) FM_HAP(I),
     &              TB_HAP(I),SOLU(I),KDISS(I),KVOL(I)
             VERIF=VERIF+FM_HAP(I)
             WRITE(LU,*) 'HAP',I,FM_HAP(I),
     &            TB_HAP(I),SOLU(I),KDISS(I),KVOL(I)
            END DO
        END IF
!
        IF(NB_HAP.GT.0.OR.NB_COMPO.GT.0)THEN
          IF(1.D0-VERIF.GT.ABS(1.D-10)) THEN
            WRITE(LU,*) 'WARNING::THE SUM OF EACH COMPONENT',
     &            ' MASS FRACTION IS NOT EQUAL TO 1'
            WRITE(LU,*) 'PLEASE, MODIFY THE INPUT STEERING FILE '
            CALL PLANTE(1)
          END IF
        END IF
!
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3)
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3) RHO_OIL
        WRITE(LU,*) 'RHO_OIL = ',RHO_OIL
!
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3)
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3) ETA_OIL
        WRITE(LU,*) 'ETA_OIL = ',ETA_OIL
!
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3)
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3) VOLDEV
        WRITE(LU,*) 'VOLDEV = ',VOLDEV
!
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3)
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3) TAMB
        WRITE(LU,*) 'TAMB = ',TAMB
!
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3)
        READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3) ETAL
        WRITE(LU,*) 'ETAL = ',ETAL
!
        IF(ETAL.EQ.3)THEN
          READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3)
          READ(T2D_FILES(T2DMIG)%LU,*,END=2,ERR=3) AREA
          WRITE(LU,*) 'AREA = ',AREA
        END IF
!
        GOTO 2
!
3       CONTINUE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'OIL_SPILL_2D: ERREUR A LA LECTURE ',
     &          'DU FICHIER DE COMMANDES HYDROCARBURES'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'OIL_SPILL_2D: PROBLEM TO READ ',
     &          'THE INPUT OIL SPILL STEERING FILE'
        ENDIF
        CALL PLANTE(1)
        STOP

2       CONTINUE
!
        DO I=1,NFLOT_MAX
          PARTICULES(I)%STATE=0
          PARTICULES(I)%ID=0
          PARTICULES(I)%TPSECH=0
          PARTICULES(I)%MASS0=0.D0
          PARTICULES(I)%MASS=0.D0
          PARTICULES(I)%MASS_EVAP=0.D0
          PARTICULES(I)%MASS_DISS=0.D0
          PARTICULES(I)%SURFACE=0.D0
          IF(NB_COMPO.GT.0) THEN
            DO K=1,NB_COMPO
              PARTICULES(I)%COMPO(K)%MASS=0.D0
              PARTICULES(I)%COMPO(K)%TB=0.D0
              PARTICULES(I)%COMPO(K)%SOL=0.D0
            END DO
          END IF
          IF(NB_COMPO.GT.0) THEN
            DO K=1,NB_HAP
              PARTICULES(I)%HAP(K)%MASS=0.D0
              PARTICULES(I)%HAP(K)%TB=0.D0
              PARTICULES(I)%HAP(K)%SOL=0.D0
            END DO
          END IF
        END DO
!
!======================================================================
!-------MEMORY ALLOCATION FOR CALCULATIONS ON MULTIPLE PROCESSORS------
!======================================================================
!
        IF(NCSIZE.GT.1) CALL OIL_ORGANISE_CHARS(NFLOT_MAX)
        INIT=.TRUE.
!
      ENDIF
!
!======================================================================
!------------------------OIL SLICK VELOCITY----------------------------
!-------INDUCED BY THE FLOW VELOCITY AND BY THE ACTION OF WIND---------
!======================================================================
!
      IF(VENT)THEN
        DO I=1,UCONV%DIM1
          UCONV_OIL%R(I)=UCONV%R(I)*(1.D0+(1.D0/KARMAN)*
     &          SQRT(CF%R(I)*0.5D0))+0.036D0*WINDX%R(I)
           VCONV_OIL%R(I)=VCONV%R(I)*(1.D0+(1.D0/KARMAN)*
     &          SQRT(CF%R(I)*0.5D0))+0.036D0*WINDY%R(I)
        ENDDO
      ELSE
        DO I=1,UCONV%DIM1
          UCONV_OIL%R(I)=UCONV%R(I)*(1.D0+(1.D0/KARMAN)*
     &          SQRT(CF%R(I)*0.5D0))
          VCONV_OIL%R(I)=VCONV%R(I)*(1.D0+(1.D0/KARMAN)*
     &         SQRT(CF%R(I)*0.5D0))
        ENDDO
      END IF
!
!======================================================================
!------------INITIALIZATION OF PARTICULES STRUCTURE--------------------
!======================================================================
!
      CALL OIL_FLOT(PARTICULES,NFLOT,NFLOT_MAX,MESH,LT,VOLDEV,RHO_OIL,
     &             NB_COMPO,NB_HAP,FM_COMPO,TB_COMPO,FM_HAP,TB_HAP,SOLU,
     &             ETAL,AREA,1,GRAV)
!
!
!======================================================================
!-------------------------PARTICLE SPREADING---------------------------
!======================================================================
!
      CALL OIL_SPREADING(VOLDEV,ETA_OIL,RHO_OIL,NFLOT,NFLOT_MAX,DT,ETAL,
     &                  GRAV)
!
!======================================================================
!-------OIL/SHORELINE INTERACTION (RELEASE OF BEACHING PARTICLE)-------
!======================================================================
!
      CALL OIL_REFLOATING(LT,DT,NPOIN,NELMAX,3,MESH%IKLE%I,H%R,HN%R,
     &                   RHO_OIL,NFLOT,CF%R,1)
!
!======================================================================
!-------------ADVECTION AND DIFFUSION OF THE OIL PARTICLE--------------
!======================================================================
!
      CALL OIL_DERIVE(UCONV_OIL%R,VCONV_OIL%R,VCONV_OIL%R,DT,AT,
     &               MESH%X%R,MESH%Y%R,MESH%Y%R,MESH%IKLE%I,
     &               MESH%IFABOR%I,LT,11,UCONV_OIL%ELM,3,NPOIN,NPOIN,
     &               NELEM,NELMAX,MESH%SURDET%R,XFLOT%R,YFLOT%R,YFLOT%R,
     &               SHPFLO%R,SHPFLO%R,TAGFLO%I,ELTFLO%I,ELTFLO%I,NFLOT,
     &               NFLOT_MAX,FLOPRD,MESH,T2D_FILES(T2DFLO)%LU,IT1%I,
     &               T1%R,T2%R,T2%R,IT2%I,W1%R,W1%R,NPOIN,1,VISC,
     &               NB_COMPO,NB_HAP)
!
!======================================================================
!------------OIL/SHORELINE INTERACTION (BEACHING PARTICLE)-------------
!======================================================================
!
      CALL OIL_BEACHING(MESH%IKLE%I,NPOIN,NELMAX,3,H%R,HN%R,NFLOT,
     &                 RHO_OIL,MESH%SURFAC%R,CF%R,ETA_OIL,LT)
!
!======================================================================
!------------------EVAPORATION OF OIL PARTICLES------------------------
!======================================================================
!
      IF(NB_COMPO.GT.0.OR.NB_HAP.GT.0)THEN
        CALL OIL_EVAP(NB_COMPO,NB_HAP,NFLOT,DT,3,NELMAX,MESH%IKLE%I,
     &               TAMB,WINDX%R,WINDY%R,VENT,NPOIN,UCONV_OIL%R,
     &               VCONV_OIL%R)
      END IF
!
!======================================================================
!-------------------DISSOLUTION OF OIL PARTICLES-----------------------
!======================================================================
!
      IF(NB_HAP.GT.0.AND.NTRAC.EQ.0)THEN
        WRITE(LU,*) 'WARNING::THERE ARE SOME SOLUBLE COMPONENT',
     &        ' BUT NO TRACER IN THE TELEMAC FILE .CAS'
        WRITE(LU,*) 'PLEASE, MODIFIED THE TELEMAC FILE.CAS '
        CALL PLANTE(1)
      END IF
!
      IF(NB_HAP.GT.0.AND.NTRAC.GT.0)THEN
        CALL OIL_DISSO(NB_COMPO,NB_HAP,NFLOT,DT,3,NELMAX,MESH%IKLE%I,
     &                HN%R,NPOIN,UNSV2D,TN,TB,MESH,KDISS,1,NTRAC)
      END IF
!
!======================================================================
!-------MANAGEMENT OF LOST PARTICLES IF THEIR MASS EQUALS TO 0---------
!======================================================================
!
      IF(NB_COMPO.GT.0.OR.NB_HAP.GT.0)THEN
        DO IFLOT=1,NFLOT
          IF(PARTICULES(IFLOT)%MASS.EQ.0.D0) THEN
             CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT,NFLOT_MAX,
     &                            MESH%TYPELM,IT1%I,PARTICULES,NB_COMPO,
     &                            NB_HAP)
          END IF
        END DO
      END IF
!
!======================================================================
!------------------VOLATILIZATION OFDISSOLVED COMPONENTS---------------
!-------------------------IN THE WATER COLUMN--------------------------
!======================================================================
!
      IF(NB_HAP.GT.0)THEN
        CALL OIL_VOLATI(T3,TIMP,HPROP,NFLOT,3,NELMAX,MESH%IKLE%I,NPOIN,
     &                 MESH,NB_HAP,KVOL,NTRAC,AYASMI=YASMI)
      END IF
!
!======================================================================
!--------------------MASS BALANCE OF OIL)------------------------------
!======================================================================
!
      IF(NB_COMPO.GT.0.OR.NB_HAP.GT.0)THEN
        CALL OIL_BILAN(NFLOT,LT,FLOPRD)
      END IF
!
!----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_SPILL_2D
!
!                    ***********************
                     SUBROUTINE OIL_SPILL_3D
!                    ***********************
     &(LT,IELM2H,MESH2D,NFLOT_MAX,T3D_FILES,MAXLU_T3D,NPOIN2,T3DMIG,
     &UCONV,VCONV,WCONV,NFLOT,NPLAN,MESH3D,AT,DT,GRAV,CF,X,Y,Z,H,HN,
     &IELM3,NPOIN3,NELEM2,XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,TAGFLO,ELTFLO,
     &ETAFLO,FLOPRD,T3DFLO,IT1,IT2,T3_01,T3_02,T3_03,MTRA1,MTRA2,VISCVI,
     &WIND,UNSV3D,NTRAC,TRN,TRAV3,ATABOS,T2_17,T2_18,VENT)
!
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    OIL SPILL MODEL.
!+
!+
!+            CALLED IF KEYWORD 'OIL SPILL MODEL' IS SET TO YES
!+                AND USES 'OIL SPILL STEERING FILE'.
!
!note     LOGICAL UNIT OF OIL SPILL STEERING FILE IS: T2D_FILES(T2DMIG)%LU
!
!warning  DOES NOTHING BY DEFAULT
!
!history  CEDRIC GOEURY (LHSV)
!+        20/04/2010
!+        V6P0
!+
!
!history  CEDRIC GOEURY (LNHE)
!+        28/06/2013
!+        V6P3
!+   First version
!+
!
!history  CEDRIC GOEURY (LNHE)
!+        22/05/2014
!+        V6P3
!+   Second version
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LT         |-->| ITERATION NUMBER
!| IELM2H     |-->| TYPE OF 2D DISCRETISATION FOR H
!| MESH2D     |<->| 2D MESH
!| NFLOT_MAX  |-->| MAXIMUM NUMBER OF FLOATS
!| T3D_FILES  |-->| LOGICAL UNIT OF OUTPUT FILE
!| MAXLU_T3D  |-->| MAXIMUM NUMBER OF OUTPUT FILE
!| NPOIN2     |-->| NUMBER OF POINTS IN 2D MESH
!| T3DMIG     |-->| LOGICAL UNIT OF OIL SPILL FILE
!| UCONV      |-->| X-COMPONENT OF VELOCITY
!| VCONV      |-->| Y-COMPONENT OF VELOCITY
!| WCONV      |-->| Z-COMPONENT OF VELOCITY
!| NFLOT      |<->| NUMBER OF FLOATS
!| NPLAN      |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| MESH3D     |<->| 3D MESH
!| AT         |-->| TIME IN SECONDS
!| DT         |-->| TIME STEP
!| GRAV       |-->| GRAVITY
!| CF         |-->| ADIMENSIONAL FRICTION COEFFICIENT
!| X          |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y          |-->| ORDINATES OF POINTS IN THE MESH
!| Z          |-->| ELEVATIONS OF POINTS IN THE MESH
!| H          |<->| WATER DEPTH AT TIME N+1
!| HN         |<->| WATER DEPTH AT TIME N
!| IELM3      |-->| TYPE OF ELEMENT
!| NELMAX     |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NPOIN3     |-->| NUMBER OF POINTS IN 3D MESH
!| XFLOT      |<->| ABSCISSAE OF FLOATS
!| YFLOT      |<->| ORDINATES OF FLOATS
!| ZFLOT      |<->| ELEVATIONS OF FLOATS
!| SHPFLO     |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR
!|            |   | ELEMENTS
!| SHZFLO     |<->| BARYCENTRIC COORDINATE ON VERTICAL
!| TAGFLO     |<->| TAGS OF FLOATS
!| ELTFLO     |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| ETAFLO     |<->| LEVELS WHERE ARE THE FLOATS
!| FLOPRD     |-->| NUMBER OF TIME STEPS BETWEEB TWO RECORDS
!| T3DFLO     |-->| LOGICAL UNIT OF FLOAT FILE
!| IT1        |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| IT2        |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_01      |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_02      |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T3_03      |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T2_18      |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| T2_17      |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| MTRA1      |<->| 3D WORK MATRIX
!| MTRA2      |<->| 3D WORK MATRIX
!| VISCVI     |-->| TURBULENT VISCOSITY COEFFICIENTS FOR VELOCITIES
!| WIND       |-->| COMPONENTS OF WIND VELOCITY
!| UNSV3D     |-->| INVERSE OF VOLUME OF BASIS FUNCTIONS
!| NTRAC      |-->| NUMBER OF TRACERS
!| TRAV3      |<->| BLOCK OF 3D BIEF_OBJ STRUCTURES (AT LEAST 10)
!| TRN        |<->| TRACER CONCENTRATION AT TIME N
!| VENT       |-->| YES IF WIND TAKEN INTO ACCOUNT
!| ATABOS     |<->| FOR BOUNDARY CONDITION (SURFACE)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE STREAMLINE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(IN) :: LT,IELM2H,NFLOT_MAX
      INTEGER,INTENT(IN) :: MAXLU_T3D,T3DMIG
      INTEGER,INTENT(IN) :: NPLAN,IELM3
      INTEGER,INTENT(INOUT) :: NFLOT
      INTEGER,INTENT(IN) :: FLOPRD,T3DFLO,NTRAC
      INTEGER,INTENT(IN) :: NPOIN2,NPOIN3,NELEM2
      LOGICAL,INTENT(IN) ::  VENT
      TYPE(BIEF_MESH),INTENT(INOUT) :: MESH2D
      TYPE(BIEF_MESH),INTENT(INOUT) :: MESH3D
      TYPE(BIEF_FILE),INTENT(IN) :: T3D_FILES(MAXLU_T3D)
      TYPE(BIEF_OBJ),INTENT(IN) :: UCONV,VCONV,WCONV
      TYPE(BIEF_OBJ),INTENT(INOUT) :: H,HN
      TYPE(BIEF_OBJ),INTENT(IN) :: CF
      TYPE(BIEF_OBJ),INTENT(INOUT) :: XFLOT,YFLOT,ZFLOT
      TYPE(BIEF_OBJ),INTENT(INOUT) :: SHPFLO,SHZFLO,TAGFLO
      TYPE(BIEF_OBJ),INTENT(INOUT) :: ELTFLO,ETAFLO
      TYPE(BIEF_OBJ),INTENT(INOUT) :: MTRA1,MTRA2
      TYPE(BIEF_OBJ),INTENT(IN) :: VISCVI
      TYPE(BIEF_OBJ),INTENT(IN) :: WIND
      TYPE(BIEF_OBJ),INTENT(IN) :: UNSV3D
      TYPE(BIEF_OBJ),INTENT(INOUT) :: TRN
      TYPE(BIEF_OBJ),INTENT(INOUT) :: TRAV3
      TYPE(BIEF_OBJ),INTENT(INOUT) :: ATABOS
      TYPE(BIEF_OBJ),INTENT(INOUT) :: IT1,IT2
      TYPE(BIEF_OBJ),INTENT(INOUT) :: T3_01,T3_02,T3_03
      TYPE(BIEF_OBJ),INTENT(INOUT) :: T2_17,T2_18
      DOUBLE PRECISION,INTENT(IN)  :: AT,DT,GRAV
      DOUBLE PRECISION,INTENT(IN), DIMENSION(NPOIN3) :: X,Y,Z
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL INIT
      DATA INIT /.FALSE./
      INTEGER I,K,IFLOT,ETAL
      INTEGER NB_COMPO,NB_HAP
      DOUBLE PRECISION ETA_OIL,RHO_OIL,VOLDEV
      DOUBLE PRECISION TAMB,VERIF,AREA
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: TB_COMPO,FM_COMPO
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: TB_HAP,FM_HAP,SOLU
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: KDISS,KVOL
!
      SAVE RHO_OIL,VOLDEV,NB_COMPO,ETA_OIL,NB_HAP,TAMB,ETAL
      SAVE TB_COMPO,FM_COMPO,TB_HAP,FM_HAP,SOLU,KDISS,KVOL,AREA
!
!-----------------------------------------------------------------------
!
      IF(.NOT.INIT) THEN
!
        CALL BIEF_ALLVEC(1,UCONV_OIL,'UCONVO',IELM2H,1,1,MESH2D)
        CALL BIEF_ALLVEC(1,VCONV_OIL,'VCONVO',IELM2H,1,1,MESH2D)
!
        VERIF=0.D0
        WRITE(LU,*) 'NFLOT_MAX = ',NFLOT_MAX
        ALLOCATE(PARTICULES(NFLOT_MAX))
!
!
!======================================================================
!----------------READING THE INPUT STEERING FILE-----------------------
!======================================================================
!
!
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) NB_COMPO
        WRITE(LU,*) 'NB_COMPO = ',NB_COMPO
!
        DO I=1,NFLOT_MAX
          ALLOCATE(PARTICULES(I)%COMPO(MAX(NB_COMPO,1)))
        END DO
!
        ALLOCATE(TB_COMPO(MAX(NB_COMPO,1)))
        ALLOCATE(FM_COMPO(MAX(NB_COMPO,1)))
        IF(NB_COMPO.GT.0)THEN
          READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
          DO I=1,NB_COMPO
            READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) FM_COMPO(I),
     &            TB_COMPO(I)
            VERIF=VERIF+FM_COMPO(I)
            WRITE(LU,*) 'COMPO',I,FM_COMPO(I),
     &           TB_COMPO(I)
          END DO
        END IF
!
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) NB_HAP
        WRITE(LU,*) 'NB_HAP = ',NB_HAP
!
        DO I=1,NFLOT_MAX
          ALLOCATE(PARTICULES(I)%HAP(MAX(NB_HAP,1)))
        END DO
!
        ALLOCATE(TB_HAP(MAX(NB_HAP,1)))
        ALLOCATE(FM_HAP(MAX(NB_HAP,1)))
        ALLOCATE(SOLU(MAX(NB_HAP,1)))
        ALLOCATE(KDISS(MAX(NB_HAP,1)))
        ALLOCATE(KVOL(MAX(NB_HAP,1)))
        IF(NB_HAP.GT.0)THEN
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
          DO I=1,NB_HAP
            READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) FM_HAP(I),
     &            TB_HAP(I),SOLU(I),KDISS(I),KVOL(I)
            VERIF=VERIF+FM_HAP(I)
            WRITE(LU,*) 'HAP',I,FM_HAP(I),
     &           TB_HAP(I),SOLU(I),KDISS(I),KVOL(I)
          END DO
        END IF
!
        IF(NB_HAP.GT.0.OR.NB_COMPO.GT.0) THEN
          IF(1.D0-VERIF.GT.ABS(1.D-10)) THEN
            WRITE(LU,*) 'WARNING::THE SUM OF EACH COMPONENT',
     &            ' MASS FRACTION IS NOT EQUAL TO 1'
            WRITE(LU,*) 'PLEASE, MODIFY THE INPUT STEERING FILE '
            CALL PLANTE(1)
          END IF
        END IF
!
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) RHO_OIL
        WRITE(LU,*) 'RHO_OIL = ',RHO_OIL
!
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) ETA_OIL
        WRITE(LU,*) 'ETA_OIL = ',ETA_OIL
!
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) VOLDEV
        WRITE(LU,*) 'VOLDEV = ',VOLDEV
!
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) TAMB
        WRITE(LU,*) 'TAMB = ',TAMB
!
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
        READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) ETAL
        WRITE(LU,*) 'ETAL = ',ETAL
!
        IF(ETAL.EQ.3)THEN
          READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3)
          READ(T3D_FILES(T3DMIG)%LU,*,END=2,ERR=3) AREA
        END IF
!
        GOTO 2
!
3       CONTINUE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'OIL_SPILL_2D: ERREUR A LA LECTURE ',
     &          'DU FICHIER DE COMMANDES HYDROCARBURES'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'OIL_SPILL_2D: PROBLEM TO READ ',
     &          'THE INPUT OIL SPILL STEERING FILE'
        ENDIF
        CALL PLANTE(1)
        STOP

2       CONTINUE
!
        DO I=1,NFLOT_MAX
          PARTICULES(I)%STATE=0
          PARTICULES(I)%ID=0
          PARTICULES(I)%TPSECH=0
          PARTICULES(I)%MASS0=0.D0
          PARTICULES(I)%MASS=0.D0
          PARTICULES(I)%MASS_EVAP=0.D0
          PARTICULES(I)%MASS_DISS=0.D0
          PARTICULES(I)%SURFACE=0.D0
          IF(NB_COMPO.GT.0) THEN
            DO K=1,NB_COMPO
              PARTICULES(I)%COMPO(K)%MASS=0.D0
              PARTICULES(I)%COMPO(K)%TB=0.D0
              PARTICULES(I)%COMPO(K)%SOL=0.D0
            END DO
          END IF
          IF(NB_HAP.GT.0) THEN
            DO K=1,NB_HAP
              PARTICULES(I)%HAP(K)%MASS=0.D0
              PARTICULES(I)%HAP(K)%TB=0.D0
              PARTICULES(I)%HAP(K)%SOL=0.D0
            END DO
          END IF
        END DO
!
!======================================================================
!-------MEMORY ALLOCATION FOR CALCULATIONS ON MULTIPLE PROCESSORS------
!======================================================================
!
        IF(NCSIZE.GT.1) CALL OIL_ORGANISE_CHARS(NFLOT_MAX)
        INIT=.TRUE.
!
      ENDIF
!
      DO I=1,NPOIN2
        UCONV_OIL%R(I)=UCONV%R(I+(NPLAN-1)*NPOIN2)
        VCONV_OIL%R(I)=VCONV%R(I+(NPLAN-1)*NPOIN2)
      ENDDO
!======================================================================
!--------------INITIALIZATION OF PARTICULES STRUCTURE------------------
!======================================================================
!
      CALL OIL_FLOT(PARTICULES,NFLOT,NFLOT_MAX,MESH3D,LT,VOLDEV,RHO_OIL,
     &             NB_COMPO,NB_HAP,FM_COMPO,TB_COMPO,FM_HAP,TB_HAP,SOLU,
     &             ETAL,AREA,NPLAN,GRAV)
!
!======================================================================
!----------------------SPREADING OF PARTICLES--------------------------
!======================================================================
!
      CALL OIL_SPREADING(VOLDEV,ETA_OIL,RHO_OIL,NFLOT,NFLOT_MAX,DT,ETAL,
     &                  GRAV)
!
!======================================================================
!-------OIL/SHORELINE INTERACTION (RELEASE OF BEACHING PARTICLE)-------
!======================================================================
!
      CALL OIL_REFLOATING(LT,DT,NPOIN2,MESH2D%NELMAX,3,MESH2D%IKLE%I,
     &                   H%R,HN%R,RHO_OIL,NFLOT,CF%R,NPLAN,Z)
!
!======================================================================
!-------------ADVECTION AND DIFFUSION OF THE OIL PARTICLE--------------
!======================================================================
!
      CALL OIL_DERIVE(UCONV%R,VCONV%R,WCONV%R,DT,AT,X,Y,Z,
     &               MESH2D%IKLE%I,MESH3D%IFABOR%I,LT,IELM3,UCONV%ELM,3,
     &               NPOIN3,NPOIN2,NELEM2,MESH2D%NELMAX,MESH2D%SURDET%R,
     &               XFLOT%R,YFLOT%R,ZFLOT%R,SHPFLO%R,SHZFLO%R,TAGFLO%I,
     &               ELTFLO%I,ETAFLO%I,NFLOT,NFLOT_MAX,FLOPRD,MESH3D,
     &               T3D_FILES(T3DFLO)%LU,IT1%I,T3_01%R,T3_02%R,T3_03%R,
     &               IT2%I,MTRA1%X%R,MTRA2%X%R,NPOIN3,1,VISCVI,NB_COMPO,
     &               NB_HAP)
!
!======================================================================
!-------OIL/SHORELINE (INTERACTION CALLING OIL_BEACHING)---------------
!======================================================================
!
      CALL OIL_BEACHING(MESH2D%IKLE%I,NPOIN2,MESH2D%NELMAX,3,H%R,HN%R,
     &                 NFLOT,RHO_OIL,MESH2D%SURFAC%R,CF%R,ETA_OIL,LT)
!
!
!======================================================================
!---------------------EVAPORATION OF OIL PARTICLES)--------------------
!======================================================================
!
      IF(NB_COMPO.GT.0.OR.NB_HAP.GT.0)THEN
        CALL OIL_EVAP(NB_COMPO,NB_HAP,NFLOT,DT,3,MESH2D%NELMAX,
     &               MESH2D%IKLE%I,TAMB,WIND%ADR(1)%P%R,WIND%ADR(2)%P%R,
     &               VENT,NPOIN2,UCONV_OIL%R,VCONV_OIL%R)
      END IF
!
!======================================================================
!-------------------DISSOLUTION OF OIL PARTICLES)----------------------
!======================================================================
!
!
      IF(NB_HAP.GT.0.AND.NTRAC.EQ.0)THEN
        WRITE(LU,*) 'WARNING::THERE ARE SOME SOLUBLE COMPONENT',
     &        ' BUT NO TRACER IN THE TELEMAC FILE .CAS'
        WRITE(LU,*) 'PLEASE, MODIFIED THE TELEMAC FILE.CAS '
        CALL PLANTE(1)
      END IF
!
      IF(NB_HAP.GT.0.AND.NTRAC.GT.0)THEN
        CALL OIL_DISSO(NB_COMPO,NB_HAP,NFLOT,DT,3,MESH2D%NELMAX,
     &                MESH2D%IKLE%I,HN%R,NPOIN2,UNSV3D,TRN,TRAV3,MESH3D,
     &                KDISS,NPLAN,NTRAC)
      END IF
!
!======================================================================
!-------MANAGEMENT OF LOST PARTICLES IF THEIR MASS EQUALS TO 0---------
!======================================================================
!
      IF(NB_COMPO.GT.0.OR.NB_HAP.GT.0)THEN
        DO IFLOT=1,NFLOT
          IF(PARTICULES(IFLOT)%MASS.EQ.0.D0) THEN
            CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT, NFLOT_MAX,
     &                           MESH3D%TYPELM,IT1%I,PARTICULES,
     &                           NB_COMPO,NB_HAP)
          END IF
        END DO
      END IF
!
!======================================================================
!----------------------VOLATILIZATION OF DISSOLVED---------------------
!--------------------COMPONENTS IN THE WATER COLUMN--------------------
!======================================================================
!
      IF(NB_HAP.GT.0)THEN
        CALL OIL_VOLATI(T2_17,ATABOS,H,NFLOT,3,MESH2D%NELMAX,
     &                 MESH2D%IKLE%I,NPOIN2,MESH2D,NB_HAP,KVOL,NTRAC)
      END IF
!
!======================================================================
!-----------------------MASS BALANCE OF OIL)---------------------------
!======================================================================
!
      IF(NB_COMPO.GT.0.OR.NB_HAP.GT.0)THEN
        CALL OIL_BILAN(NFLOT,LT,FLOPRD)
      END IF
!
!----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_SPILL_3D
!
!                    **********************
                     SUBROUTINE OIL_DERIVE
!                    **********************
!
     &(U,V,W,DT,AT,X,Y,Z,IKLE,IFABOR,LT,IELM,IELMU,NDP,NPOIN,NPOIN2,
     & NELEM,NELMAX,SURDET,XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,TAGFLO,
     & ELTFLO,ETAFLO,NFLOT,NFLOT_MAX,FLOPRD,MESH,UL,ISUB,DX,DY,DZ,
     & ELTBUF,SHPBUF,SHZBUF,SIZEBUF,STOCHA,VISC,NB_COMPO,NB_HAP)
!
!***********************************************************************
! BIEF   V7P0                                                 21/08/2010
!***********************************************************************
!
!brief    - Like DERIVE but for oil spills.
!
!history  CEDRIC GOEURY (LNHE)
!+        28/06/2013
!+        V6P3
!+   First version
!+
!
!history  CEDRIC GOEURY (LNHE)
!+        28/06/2013
!+        V7P0
!+   Second version
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL)
!| AT             |-->| TIME IN SECONDS
!| DX             |<->| WORK ARRAY (DISPLACEMENTS ALONG X)
!| DY             |<->| WORK ARRAY (DISPLACEMENTS ALONG Y)
!| DZ             |<->| WORK ARRAY (DISPLACEMENTS ALONG Z)
!| ELTBUF         |<->| WORK ARRAY
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE THE FLOATS ARE
!| ETAFLO         |<->| LEVELS WHERE THE FLOATS ARE
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEN TWO RECORDS
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
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS.
!| NPOIN          |-->| NUMBER OF POINTS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SHPBUF         |<->| WORK ARRAY
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR
!|                |   | ELEMENTS.
!| SHZBUF         |<->| WORK ARRAY
!| SHZFLO         |<->| BARYCENTRIC COORDINATE ON VERTICAL
!| SIZEBUF        |-->| DIMENSION OF SOME WORK ARRAYS
!| SURDET         |-->| 1/DETERMINANT, USED IN ISOPARAMETRIC
!|                |   | TRANSFORMATION.
!| TAGFLO         |<->| TAGS OF FLOATS
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
!| VISC           |-->| TURBULENT VISCOSITY COEFFICIENTS FOR VELOCITIES
!| NB_HAP         |-->| NUMBER OF SOLUBLE COMPONENTS
!| NB_COMPO       |-->| NUMBER OF UNSOLUBLE COMPONENTS
!| STOCHA         |-->| STOCHASTIC DIFFUSION MODEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
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
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: VISC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,FRE(1),FREBUF(1),IPROC,NFLOTG,NPLAN,ELT
      INTEGER N1,N2,N3,N4,N5,N6,NOMB,SENS,IR,NRK
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
!     RUNGE-KUTTA SUBSTEPS
      NRK=3
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
!     THE CALCULATION OF THE DRIFT IS PERFORMED ONLY FOR
!     NON BEACHING OIL PARTICLES. SO, THE NON BEACHING PARTICLE
!     CHARACTERISTICS ARE TRANSFERRED TO THE WORK FLOAT TABLE
!
      IR = 0
      IF(IELM.EQ.11.AND.NFLOT.GT.0) THEN
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
      ELSEIF(IELM.EQ.41.AND.NFLOT.GT.0) THEN
        DO IFLOT = 1,NFLOT
          IF(PARTICULES(IFLOT)%STATE.EQ.1)THEN
            IR = IR +1
            XFLOT(IR) = PARTICULES(IFLOT)%XOIL
            YFLOT(IR) = PARTICULES(IFLOT)%YOIL
            ZFLOT(IR) = PARTICULES(IFLOT)%ZOIL
            TAGFLO(IR) = PARTICULES(IFLOT)%ID
            SHPFLO(1,IR) = PARTICULES(IFLOT)%SHPOIL(1)
            SHPFLO(2,IR) = PARTICULES(IFLOT)%SHPOIL(2)
            SHPFLO(3,IR) = PARTICULES(IFLOT)%SHPOIL(3)
            SHZFLO(IR) = PARTICULES(IFLOT)%SHZOIL
            ELTFLO(IR) = PARTICULES(IFLOT)%ELTOIL
            ETAFLO(IR) = PARTICULES(IFLOT)%ETAOIL
          END IF
        END DO
      END IF
!
      CALL SCARACT(SVOID,SVOID,U,V,W,W,X,Y,ZSTAR,ZSTAR,
     &             XFLOT,YFLOT,ZFLOT,ZFLOT,
     &             DX,DY,DZ,DZ,Z,SHPFLO,SHZFLO,SHZFLO,SURDET,DT,
     &             IKLE,IFABOR,ELTFLO,ETAFLO,
     &             FRE,ELTBUF,ISUB,IELM,IELMU,NELEM,NELMAX,NOMB,
     &             NPOIN,NPOIN2,NDP,NRK,NPLAN,1,MESH,IR,NPOIN2,SENS,
     &             SHPBUF,SHZBUF,SHZBUF,FREBUF,SIZEBUF,
     &             APOST=.TRUE.,ASTOCHA=STOCHA,AVISC=VISC)
!
!                  APOST=.TRUE. OTHERWISE ISUB IS NOT FILLED*
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1.AND.NFLOT.GT.0) THEN
!
!     IN // XFLOT AND YFLOT MAY HAVE BEEN DESTROYED BY SCARACT
!     BECAUSE RE-USED FOR GENERATIONS OF LOST PARTICLES
!     THEY ARE REDONE HERE FOR PARTICLES WHICH ARE STILL IN THE
!     SUB-DOMAIN
!
!     AT THIS STAGE, WE NEED TO RECOMPUTE THE TABLE ISUB AND ELTFLO
!     FOR ALL PARTICLES IN THE SUB-DOMAIN IN ORDER TO SEND AND DESTROY
!     PARTICLE INFORMATION. MOREOVER, THE TABLE TAGFLO AND SHPFLO ARE
!     RECOMPUTED IN ORDER TO HAVE INFORMATION NECESSARY TO LOCALIZE
!     THE PARTICLES THAT HAVE MIGRATED TO ANOTHER SUB-DOMAIN
!
        IF(IELM.EQ.11) THEN
          DO IFLOT=NFLOT,1,-1
            IF(PARTICULES(IFLOT)%STATE.EQ.2) THEN
              ISUB(IFLOT)=IPID
            ELSE
              ISUB(IFLOT)=ISUB(IR)
              ELTFLO(IFLOT)=ELTFLO(IR)
              TAGFLO(IFLOT)=TAGFLO(IR)
              SHPFLO(1,IFLOT)=SHPFLO(1,IR)
              SHPFLO(2,IFLOT)=SHPFLO(2,IR)
              SHPFLO(3,IFLOT)=SHPFLO(3,IR)
              IF(ISUB(IFLOT).EQ.IPID) THEN
                ELT=ELTFLO(IFLOT)
                PARTICULES(IFLOT)%ELTOIL = ELTFLO(IFLOT)
                IF(ELT.GT.0) THEN
                  N1=IKLE(ELT,1)
                  N2=IKLE(ELT,2)
                  N3=IKLE(ELT,3)
                  PARTICULES(IFLOT)%XOIL = SHPFLO(1,IFLOT)*X(N1)
     &                                   + SHPFLO(2,IFLOT)*X(N2)
     &                                   + SHPFLO(3,IFLOT)*X(N3)
                  PARTICULES(IFLOT)%YOIL = SHPFLO(1,IFLOT)*Y(N1)
     &                                   + SHPFLO(2,IFLOT)*Y(N2)
     &                                   + SHPFLO(3,IFLOT)*Y(N3)
                  PARTICULES(IFLOT)%SHPOIL(1) = SHPFLO(1,IFLOT)
                  PARTICULES(IFLOT)%SHPOIL(2) = SHPFLO(2,IFLOT)
                  PARTICULES(IFLOT)%SHPOIL(3) = SHPFLO(3,IFLOT)
                  PARTICULES(IFLOT)%ID = TAGFLO(IFLOT)
                ENDIF
              END IF
              IR = IR - 1
            END IF
          END DO
          IF(IR.NE.0)THEN
            WRITE(LU,*) 'PROBLEM IN OIL_DERIVE'
            WRITE(LU,*) 'RESTITUTION OF ISUB IR: ',IR
            CALL PLANTE(1)
            STOP
          END IF
        ELSEIF(IELM.EQ.41) THEN
          DO IFLOT=NFLOT,1,-1
            IF(PARTICULES(IFLOT)%STATE.EQ.2) THEN
              ISUB(IFLOT)=IPID
            ELSE
              ISUB(IFLOT)=ISUB(IR)
              ELTFLO(IFLOT)=ELTFLO(IR)
              TAGFLO(IFLOT)=TAGFLO(IR)
              SHPFLO(1,IFLOT)=SHPFLO(1,IR)
              SHPFLO(2,IFLOT)=SHPFLO(2,IR)
              SHPFLO(3,IFLOT)=SHPFLO(3,IR)
!=====================================================================
!---------------------TO BE AT THE FREE SURFACE-----------------------
!=====================================================================
              SHZFLO(IR) = 1.D0
!=====================================================================
              SHZFLO(IFLOT)=SHZFLO(IR)
              ETAFLO(IFLOT)= ETAFLO(IR)
              IF(ISUB(IFLOT).EQ.IPID) THEN
                ELT=ELTFLO(IFLOT)
                PARTICULES(IFLOT)%ELTOIL = ELTFLO(IFLOT)
                IF(ELT.GT.0) THEN
                  N1=IKLE(ELT,1)+NPOIN2*(ETAFLO(IFLOT)-1)
                  N2=IKLE(ELT,2)+NPOIN2*(ETAFLO(IFLOT)-1)
                  N3=IKLE(ELT,3)+NPOIN2*(ETAFLO(IFLOT)-1)
                  N4=IKLE(ELT,1)+NPOIN2* ETAFLO(IFLOT)
                  N5=IKLE(ELT,2)+NPOIN2* ETAFLO(IFLOT)
                  N6=IKLE(ELT,3)+NPOIN2* ETAFLO(IFLOT)

                  PARTICULES(IFLOT)%XOIL = SHPFLO(1,IFLOT)*X(N1)
     &                                   + SHPFLO(2,IFLOT)*X(N2)
     &                                   + SHPFLO(3,IFLOT)*X(N3)
                  PARTICULES(IFLOT)%YOIL = SHPFLO(1,IFLOT)*Y(N1)
     &                                   + SHPFLO(2,IFLOT)*Y(N2)
     &                                   + SHPFLO(3,IFLOT)*Y(N3)
                  PARTICULES(IFLOT)%SHPOIL(1) = SHPFLO(1,IFLOT)
                  PARTICULES(IFLOT)%SHPOIL(2) = SHPFLO(2,IFLOT)
                  PARTICULES(IFLOT)%SHPOIL(3) = SHPFLO(3,IFLOT)
                  PARTICULES(IFLOT)%ID = TAGFLO(IFLOT)
                  PARTICULES(IFLOT)%ETAOIL = ETAFLO(IFLOT)
                  PARTICULES(IFLOT)%SHZOIL = SHZFLO(IFLOT)
                  PARTICULES(IFLOT)%ZOIL = (Z(N1)*SHPFLO(1,IFLOT)
     &                                   + Z(N2)*SHPFLO(2,IFLOT)
     &                                   + Z(N3)*SHPFLO(3,IFLOT))
     &                                   *(1.D0-SHZFLO(IFLOT))
     &                                   +(Z(N4)*SHPFLO(1,IFLOT)
     &                                   +Z(N5)*SHPFLO(2,IFLOT)
     &                                   +Z(N6)*SHPFLO(3,IFLOT))
     &                                   *SHZFLO(IFLOT)
                ENDIF
              END IF
              IR = IR - 1
            END IF
          END DO
          IF(IR.NE.0)THEN
            WRITE(LU,*) 'PROBLEM IN OIL_DERIVE'
            WRITE(LU,*) 'RESTITUTION OF ISUB, IR: ',IR
            CALL PLANTE(1)
            STOP
          END IF
        ENDIF
      ELSE IF(NFLOT.GT.0)THEN
        IF(IELM.EQ.11) THEN
          DO IFLOT=NFLOT,1,-1
            IF(PARTICULES(IFLOT)%STATE.EQ.1)THEN
              ELTFLO(IFLOT)=ELTFLO(IR)
              TAGFLO(IFLOT)=TAGFLO(IR)
              SHPFLO(1,IFLOT)=SHPFLO(1,IR)
              SHPFLO(2,IFLOT)=SHPFLO(2,IR)
              SHPFLO(3,IFLOT)=SHPFLO(3,IR)
              ELT=ELTFLO(IR)
              PARTICULES(IFLOT)%ELTOIL = ELTFLO(IR)
              IF(ELT.GT.0) THEN
                N1=IKLE(ELT,1)
                N2=IKLE(ELT,2)
                N3=IKLE(ELT,3)
                PARTICULES(IFLOT)%XOIL = SHPFLO(1,IFLOT)*X(N1)
     &                                 + SHPFLO(2,IFLOT)*X(N2)
     &                                 + SHPFLO(3,IFLOT)*X(N3)
                PARTICULES(IFLOT)%YOIL = SHPFLO(1,IFLOT)*Y(N1)
     &                                 + SHPFLO(2,IFLOT)*Y(N2)
     &                                 + SHPFLO(3,IFLOT)*Y(N3)
                PARTICULES(IFLOT)%SHPOIL(1) = SHPFLO(1,IFLOT)
                PARTICULES(IFLOT)%SHPOIL(2) = SHPFLO(2,IFLOT)
                PARTICULES(IFLOT)%SHPOIL(3) = SHPFLO(3,IFLOT)
                PARTICULES(IFLOT)%ID = TAGFLO(IFLOT)
              ENDIF
              IR = IR - 1
            END IF
          END DO
          IF(IR.NE.0)THEN
            WRITE(LU,*) 'PROBLEM IN OIL_DERIVE'
            WRITE(LU,*) 'RESTITUTION OF ISUB, IR: ',IR
            CALL PLANTE(1)
            STOP
          END IF
        ELSEIF(IELM.EQ.41) THEN
          DO IFLOT=NFLOT,1,-1
            IF(PARTICULES(IFLOT)%STATE.EQ.1) THEN
              ELTFLO(IFLOT)=ELTFLO(IR)
              TAGFLO(IFLOT)=TAGFLO(IR)
              SHPFLO(1,IFLOT)=SHPFLO(1,IR)
              SHPFLO(2,IFLOT)=SHPFLO(2,IR)
              SHPFLO(3,IFLOT)=SHPFLO(3,IR)
!=====================================================================
!---------------------TO BE AT THE FREE SURFACE------------------
!=====================================================================
              SHZFLO(IR) = 1.D0
!=====================================================================
              SHZFLO(IFLOT)=SHZFLO(IR)
              ETAFLO(IFLOT)= ETAFLO(IR)
              ELT=ELTFLO(IR)
              PARTICULES(IFLOT)%ELTOIL = ELTFLO(IR)
              IF(ELT.GT.0) THEN
                N1=IKLE(ELT,1)+NPOIN2*(ETAFLO(IFLOT)-1)
                N2=IKLE(ELT,2)+NPOIN2*(ETAFLO(IFLOT)-1)
                N3=IKLE(ELT,3)+NPOIN2*(ETAFLO(IFLOT)-1)
                N4=IKLE(ELT,1)+NPOIN2* ETAFLO(IFLOT)
                N5=IKLE(ELT,2)+NPOIN2* ETAFLO(IFLOT)
                N6=IKLE(ELT,3)+NPOIN2* ETAFLO(IFLOT)

                PARTICULES(IFLOT)%XOIL = SHPFLO(1,IFLOT)*X(N1)
     &                                 + SHPFLO(2,IFLOT)*X(N2)
     &                                 + SHPFLO(3,IFLOT)*X(N3)
                PARTICULES(IFLOT)%YOIL = SHPFLO(1,IFLOT)*Y(N1)
     &                                 + SHPFLO(2,IFLOT)*Y(N2)
     &                                 + SHPFLO(3,IFLOT)*Y(N3)
                PARTICULES(IFLOT)%SHPOIL(1) = SHPFLO(1,IFLOT)
                PARTICULES(IFLOT)%SHPOIL(2) = SHPFLO(2,IFLOT)
                PARTICULES(IFLOT)%SHPOIL(3) = SHPFLO(3,IFLOT)
                PARTICULES(IFLOT)%ID = TAGFLO(IFLOT)
                PARTICULES(IFLOT)%ETAOIL = ETAFLO(IFLOT)
                PARTICULES(IFLOT)%SHZOIL = SHZFLO(IFLOT)
                PARTICULES(IFLOT)%ZOIL = (Z(N1)*SHPFLO(1,IFLOT)
     &                                 + Z(N2)*SHPFLO(2,IFLOT)
     &                                 + Z(N3)*SHPFLO(3,IFLOT))
     &                                 * (1.D0-SHZFLO(IFLOT))
     &                                 + (Z(N4)*SHPFLO(1,IFLOT)
     &                                 + Z(N5)*SHPFLO(2,IFLOT)
     &                                 + Z(N6)*SHPFLO(3,IFLOT))
     &                                 * SHZFLO(IFLOT)
              END IF
              IR = IR - 1
            END IF
          END DO
          IF(IR.NE.0)THEN
            WRITE(LU,*) 'PROBLEM IN OIL_DERIVE'
            WRITE(LU,*) 'RESTITUTION OF IR: ',IR
            CALL PLANTE(1)
            STOP
          END IF
        ENDIF
      END IF
!
!     SENDING THE PARTICLES THAT MIGRATED TO ANOTHER SUB-DOMAIN
!
      IF(NCSIZE.GT.1) THEN
        CALL OIL_SEND_INFO(XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,ELTFLO,
     &                    ETAFLO,ISUB,TAGFLO,NDP,NFLOT,NFLOT_MAX,
     &                    MESH,NPLAN,PARTICULES,NB_COMPO,NB_HAP)
        CALL OIL_SEND_PARTICLES(XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,ELTFLO,
     &                         ETAFLO,ISUB,TAGFLO,NDP,NFLOT,NFLOT_MAX,
     &                         MESH,NPLAN,PARTICULES)
      ENDIF
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
        IF(NFLOT.GT.0) THEN
          IF(PARTICULES(IFLOT)%ELTOIL.LE.0.OR.ISUB(IFLOT).NE.IPID) THEN
            CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT,NFLOT_MAX,
     &                           MESH%TYPELM,ISUB,PARTICULES,NB_COMPO,
     &                           NB_HAP)
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
          IF(PARTICULES(IFLOT)%ELTOIL.LE.0) THEN
            CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT,NFLOT_MAX,
     &                           MESH%TYPELM,ISUB,PARTICULES,NB_COMPO,
     &                           NB_HAP)
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
                WRITE(99,300) PARTICULES(IFLOT)%ID,
     &                        PARTICULES(IFLOT)%XOIL,
     &                        PARTICULES(IFLOT)%YOIL,
     &                        PARTICULES(IFLOT)%STATE
              ENDDO
            ELSE
              DO IFLOT=1,NFLOT
                WRITE(99,301) PARTICULES(IFLOT)%ID,
     &                        PARTICULES(IFLOT)%XOIL,
     &                        PARTICULES(IFLOT)%YOIL,
     &                        PARTICULES(IFLOT)%ZOIL,
     &                        PARTICULES(IFLOT)%STATE
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
              WRITE(UL,300) PARTICULES(IFLOT)%ID,
     &                      PARTICULES(IFLOT)%XOIL,
     &                      PARTICULES(IFLOT)%YOIL,
     &                      PARTICULES(IFLOT)%STATE
            ENDDO
          ELSE
            DO IFLOT=1,NFLOT
              WRITE(UL,301) PARTICULES(IFLOT)%ID,
     &                      PARTICULES(IFLOT)%XOIL,
     &                      PARTICULES(IFLOT)%YOIL,
     &                      PARTICULES(IFLOT)%ZOIL,
     &                      PARTICULES(IFLOT)%STATE
            ENDDO
          ENDIF
200       FORMAT(A,F12.4,A,A,I4,A,F12.4)
300       FORMAT(I6,',',F16.8,',',F16.8,',',I2)
301       FORMAT(I6,',',F16.8,',',F16.8,',',F16.8,',',I2)
302       FORMAT(I6,',',F16.8,',',F16.8,',',F16.8,',',F16.8)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_DERIVE
!
!                       ************************
                        SUBROUTINE OIL_SPREADING
!                       ************************
!
     &(VOLDEV,ETA_OIL,RHO_OIL,NFLOT,NFLOT_MAX,DT,ETAL,GRAV)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   06/06/2013
!***********************************************************************
!
!
!brief    COMPUTE THE SLICK EXPANSION
!+
!
!history  CEDRIC GOEURY (NHE)
!+        11/07/2013
!+        V6P3
!+   First version
!
!history  CEDRIC GOEURY (LNHE)
!+        23/05/2014
!+        V7P0
!+   Second version
!
!+   THIS ROUTINE COMPUTES THE SLICK EXPANSION CONTROLLED BY MECHANICAL
!+   FORCES SUCH AS GRAVITY, SURFACE TENSION, INERTIA AND VISCOSITY (FAY,1971).
!+   TWO FORMULATIONS ARE IMPLEMENTED HERE:
!+   --->IF ETAL=1 THE FORMULATION PROPOSED BY FAY IS USED. THIS MODEL DOES NOT
!+       CONSIDER THE OIL VISCOSITY AND WOULD BE MORE CONVENIENT FOR CALM WATER
!+   --->IF ETAL=2 THE FORMULATION USED, HAS BEEN PROPOSED TO MODEL THE SLICK
!+       EXPANSION FOR RELATIVELY SMALL SPILL (20 ML)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| ETAL           |-->| NUMBER OF THE SPREADING MODEL
!| ETA_OIL        |-->| OIL VISCOSITY
!| RHO_OIL        |-->| OIL DENSITY
!| VOLDEV         |-->| VOLUME OF THE OIL SPILL
!| GRAV           |-->| GRAVITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NFLOT,NFLOT_MAX,ETAL
      DOUBLE PRECISION, INTENT(IN) :: DT,RHO_OIL,ETA_OIL,VOLDEV,GRAV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT
      DOUBLE PRECISION PI,RHO_EAU,DELTA,VOL,NU,NU2,COEF1,COEF2
!
      SAVE
!
!-----------------------------------------------------------------------
!
      IF(NFLOT.GT.0) THEN
        RHO_EAU=1000.D0
        PI=ACOS(-1.D0)
!       HARDCODED WATER MOLECULAR VISCOSITY
        NU=1.D-6
        NU2=NU**2
!
        COEF1=1.21D0**4
        COEF2=COEF1/1.53D0**2
        DELTA=(RHO_EAU-RHO_OIL)/(RHO_EAU)
        DO IFLOT = 1,NFLOT
          IF(PARTICULES(IFLOT)%MASS.GT.0.D0)THEN
              VOL=(PARTICULES(IFLOT)%MASS/RHO_OIL)
          ELSE
              VOL=(PARTICULES(IFLOT)%MASS0/RHO_OIL)
          ENDIF
          IF(PARTICULES(IFLOT)%STATE.EQ.1.AND.ETAL.EQ.1) THEN
            PARTICULES(IFLOT)%SURFACE =
     &            SQRT((PARTICULES(IFLOT)%SURFACE-
     &            PI*COEF2*
     &            (DELTA*GRAV/(VOLDEV*NU2))
     &            **(1.D0/6.D0)*(VOLDEV/NFLOT_MAX))**2
     &            +(PI*COEF1*((GRAV*DELTA)
     &            /(VOLDEV*SQRT(NU)))**(1.D0/3.D0)
     &            *VOL)**2*DT)
     &            +PI*COEF2*
     &            (DELTA*GRAV/(VOLDEV*NU2))
     &            **(1.D0/6.D0)*VOLDEV/NFLOT_MAX
          ELSEIF(PARTICULES(IFLOT)%STATE.EQ.1.AND.ETAL.EQ.2)THEN
            PARTICULES(IFLOT)%SURFACE=(PARTICULES(IFLOT)%SURFACE**4
     &           +((13.5D0*PI*GRAV*DELTA*DT)/
     &           (ETA_OIL*VOLDEV))*VOL**4)**0.25D0
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_SPREADING
!
!                       *******************
                        SUBROUTINE OIL_EVAP
!                       *******************
!
     &(NB_COMPO,NB_HAP,NFLOT,DT,NDP,NELMAX,IKLE,TAMB,WINDX,WINDY,VENT,
     & NPOIN,UCONV,VCONV)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief    COMPUTE THE OIL EVAPORATION
!+
!
!history  CEDRIC GOEURY (NHE)
!+        11/07/2013
!+        V6P3
!+   First version
!
!history  CEDRIC GOEURY (LNHE)
!+        23/05/2014
!+        V7P0
!+   Second version
!
!+   THE EVAPORATION MODEL USED IS BASED ON A PSEUDO-COMPONENT APPROACH. THE
!+   CHANGE IN THE MASS OF THE PETROLEUM COMPONENT IS CHARACTERIZED, USING
!+   THE MOLAR FLUX EXPRESSION OF STIVER AND MACKAY (1984) AND THE THERMODYNAMIC
!+   PHASE EQUILIBRIUM EQUATION. IN ORDER TO LIMIT THE VARIABLE NUMBER IN THE MODEL
!+   THE GRAY WATSON METHOD (BOETHLING, 2000) IS USED TO DETERMINE THE MOLAR
!+   ENTHALPY AS A FUNCTION OF THE COMPONENT'S BOILING TEMPERATURE
!+   IN THIS MODEL, THE MASS TRANSFER COEFFICIENT KEVAP IS CALCULATED
!+   ACCORDING TO THE THEORY OF MACKAY AND MATSUGU (1973)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NB_COMPO       |-->| NUMBER OF UNSOLUBLE COMPOUND IN OIL
!| NB_HAP         |-->| NUMBER OF SOLUBLE COMPOUND IN OIL
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| TAMB           |-->| WATER TEMPERATURE
!| VENT           |-->| YES IF WIND TAKEN INTO ACCOUNT
!| WINDX          |-->| X-COMPONENTS OF WIND VELOCITY
!| WINDY          |-->| Y-COMPONENTS OF WIND VELOCITY
!| UCONV          |-->| X-COMPONENT OF VELOCITY
!| VCONV          |-->| Y-COMPONENT OF VELOCITY
!| NPOIN          |-->| NUMBER OF POINTS IN MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN)          :: VENT
      INTEGER, INTENT(IN)          :: NB_COMPO,NB_HAP,NDP,NELMAX
      INTEGER, INTENT(IN)          :: NFLOT,NPOIN
      DOUBLE PRECISION, INTENT(IN) :: DT,TAMB
      INTEGER         , INTENT(IN) :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN) :: WINDX(NPOIN),WINDY(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: UCONV(NPOIN),VCONV(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,K,I1,I2,I3
      DOUBLE PRECISION TOTALE,KE,MW,MASSE,BILAN
      DOUBLE PRECISION VENT_RELAT,VENTX,VENTY
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::MASSE_EVAP_COMPO,C,D
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::MASSE_EVAP_HAP
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::MW_HAP,MW_COMPO
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      SAVE
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA)THEN
        IF(NB_COMPO.GT.0)THEN
          ALLOCATE(MASSE_EVAP_COMPO(NB_COMPO))
          ALLOCATE(MW_COMPO(NB_COMPO))
          ALLOCATE(C(NB_COMPO))
        END IF
        IF(NB_HAP.GT.0)THEN
          ALLOCATE(MASSE_EVAP_HAP(NB_HAP))
          ALLOCATE(MW_HAP(NB_HAP))
          ALLOCATE(D(NB_HAP))
        END IF
        DEJA=.TRUE.
      END IF
!
!-----------------------------------------------------------------------
!-----------------------INITIALIZATION----------------------------------
!-----------------------------------------------------------------------
!
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
!------------MOLAR MASS CALCULATION OF UNSOLUBLE COMPOUND--------------
!-----------------------------------------------------------------------
          DO K=1,NB_COMPO
            MW_COMPO(K) = 0.04132D0-1.985D-04*
     &                  PARTICULES(IFLOT)%COMPO(K)%TB +
     &                  9.494D-07*PARTICULES(IFLOT)%COMPO(K)%TB**2
          END DO
!-----------------------------------------------------------------------
!------------MOLAR MASS CALCULATION OF UNSOLUBLE COMPOUND--------------
!-----------------------------------------------------------------------
          DO K=1,NB_HAP
            MW_HAP(K) = 0.04132D0-1.985D-04*
     &                PARTICULES(IFLOT)%HAP(K)%TB +
     &                9.494D-07*PARTICULES(IFLOT)%HAP(K)%TB**2
          END DO
!-----------------------------------------------------------------------
!--------------------MOLAR MASS CALCULATION OF THE OIL------------------
!-----------------------------------------------------------------------
          MW=0.D0
          DO K=1,NB_COMPO
            MW = MW + (PARTICULES(IFLOT)%COMPO(K)%MASS/
     &                PARTICULES(IFLOT)%MASS)*MW_COMPO(K)
          END DO
          DO K=1,NB_HAP
             MW = MW + (PARTICULES(IFLOT)%HAP(K)%MASS/
     &                 PARTICULES(IFLOT)%MASS)*MW_HAP(K)
          END DO
!-----------------------------------------------------------------------
!-------WIND CALCULATION (DIFFERENCE BETWEEN U SURFACE AND WIND)--------
!-----------------------------------------------------------------------
          I1=IKLE(PARTICULES(IFLOT)%ELTOIL,1)
          I2=IKLE(PARTICULES(IFLOT)%ELTOIL,2)
          I3=IKLE(PARTICULES(IFLOT)%ELTOIL,3)

          IF(VENT)THEN
            VENTX = ((UCONV(I1)-WINDX(I1))*PARTICULES(IFLOT)%SHPOIL(1)
     &              +(UCONV(I2)-WINDX(I2))*PARTICULES(IFLOT)%SHPOIL(2)
     &              +(UCONV(I3)-WINDX(I3))*PARTICULES(IFLOT)%SHPOIL(3))

            VENTY = ((VCONV(I1)-WINDY(I1))*PARTICULES(IFLOT)%SHPOIL(1)
     &              +(VCONV(I2)-WINDY(I2))*PARTICULES(IFLOT)%SHPOIL(2)
     &              +(VCONV(I3)-WINDY(I3))*PARTICULES(IFLOT)%SHPOIL(3))
          ELSE
            VENTX = UCONV(I1)*PARTICULES(IFLOT)%SHPOIL(1)
     &            + UCONV(I2)*PARTICULES(IFLOT)%SHPOIL(2)
     &            + UCONV(I3)*PARTICULES(IFLOT)%SHPOIL(3)

            VENTY = VCONV(I1)*PARTICULES(IFLOT)%SHPOIL(1)
     &            + VCONV(I2)*PARTICULES(IFLOT)%SHPOIL(2)
     &            + VCONV(I3)*PARTICULES(IFLOT)%SHPOIL(3)
          END IF

          VENT_RELAT = SQRT(VENTX**2+VENTY**2)
!-----------------------------------------------------------------------
!------------CALCULATION OF THE MASS TRANSFER COEFFICIENT---------------
!-----------------------------------------------------------------------
          KE = 0.0048D0*VENT_RELAT**0.78D0*1.3676D0*
     &         (0.018D0/MW)**(1.D0/3.D0)
!-----------------------------------------------------------------------
!----------CALCULATION OF THE TOTAL MOL IN THE OIL SLICK----------------
!-----------------------------------------------------------------------
          TOTALE=0.D0
          DO K=1,NB_COMPO
            TOTALE=TOTALE+PARTICULES(IFLOT)%COMPO(K)%MASS/MW_COMPO(K)
          END DO
          DO K=1,NB_HAP
            TOTALE=TOTALE+PARTICULES(IFLOT)%HAP(K)%MASS/MW_HAP(K)
          END DO
!------------------------------------------------------------------------------
!---------DETERMINATION OF EACH OIL COMPONENT MASS AFTER EVAPORATION-----------
!-----------------------------(UNSOLUBLE)-------------------------------------
!------------------------------------------------------------------------------
          DO K=1,NB_COMPO
            MASSE_EVAP_COMPO(K)=0.D0
            C(K)=PARTICULES(IFLOT)%COMPO(K)%MASS
            IF(PARTICULES(IFLOT)%COMPO(K)%MASS.GT.0.D0) THEN
              PARTICULES(IFLOT)%COMPO(K)%MASS=
     &              PARTICULES(IFLOT)%COMPO(K)%MASS*
     &              EXP(-((KE*PARTICULES(IFLOT)%SURFACE*DT*
     &              EXP(LOG(82.06D0*PARTICULES(IFLOT)%COMPO(K)%TB)*
     &              (3.D0-2.D0*(TAMB/PARTICULES(IFLOT)%COMPO(K)%TB))**
     &              (0.4133D0-0.2575D0*(TAMB/
     &              PARTICULES(IFLOT)%COMPO(K)%TB))*
     &              (1.D0-(PARTICULES(IFLOT)%COMPO(K)%TB/TAMB))))/
     &              (8.206D-5*TAMB*TOTALE)))

              MASSE_EVAP_COMPO(K)=C(K)-PARTICULES(IFLOT)%COMPO(K)%MASS

              IF(PARTICULES(IFLOT)%COMPO(K)%MASS.LE.0.D0) THEN
                PARTICULES(IFLOT)%COMPO(K)%MASS=0.D0
                MASSE_EVAP_COMPO(K)=C(K)
              END IF

            ELSE
              PARTICULES(IFLOT)%COMPO(K)%MASS=0.D0
            END IF
          END DO
!------------------------------------------------------------------------------
!---------DETERMINATION OF EACH OIL COMPONENT MASS AFTER EVAPORATION-----------
!-----------------------------(SOLUBLE)---------------------------------------
!------------------------------------------------------------------------------
          DO K=1,NB_HAP
            MASSE_EVAP_HAP(K)=0.D0
            D(K)=PARTICULES(IFLOT)%HAP(K)%MASS
            IF(PARTICULES(IFLOT)%HAP(K)%MASS.GT.0.D0) THEN
              PARTICULES(IFLOT)%HAP(K)%MASS=
     &              PARTICULES(IFLOT)%HAP(K)%MASS*
     &              EXP(-((KE*PARTICULES(IFLOT)%SURFACE*DT*
     &              EXP(LOG(82.06D0*PARTICULES(IFLOT)%HAP(K)%TB)*
     &              (3.D0-2.D0*(TAMB/PARTICULES(IFLOT)%HAP(K)%TB))**
     &              (0.4133D0-0.2575D0*(TAMB/
     &              PARTICULES(IFLOT)%HAP(K)%TB))*
     &              (1.D0-(PARTICULES(IFLOT)%HAP(K)%TB/TAMB))))/
     &              (8.206D-5*TAMB*TOTALE)))

              MASSE_EVAP_HAP(K)=D(K)-PARTICULES(IFLOT)%HAP(K)%MASS

              IF(PARTICULES(IFLOT)%HAP(K)%MASS.LE.0.D0) THEN
                PARTICULES(IFLOT)%HAP(K)%MASS=0.D0
                MASSE_EVAP_HAP(K)=D(K)
              END IF
            ELSE
                PARTICULES(IFLOT)%HAP(K)%MASS=0.D0
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
!
!                       ********************
                        SUBROUTINE OIL_DISSO
!                       ********************
!
     &(NB_COMPO,NB_HAP,NFLOT,DT,NDP,NELMAX,IKLE,HN,NPOIN,UNSVOL,TN,TB,
     &MESH,KDISS,NPLAN,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief    COMPUTE THE OIL DISSOLUTION
!+
!
!history  CEDRIC GOEURY (NHE)
!+        11/07/2013
!+        V6P3
!+   First version
!
!history  CEDRIC GOEURY (LNHE)
!+        23/05/2014
!+        V7P0
!+   Second version
!
!+   THIS ROUTINE COMPUTES THE DISSOLUTION PHENOMENON OF EACH OIL SOLUBLE
!+   COMPONENT BASED ON WHITMAN'S (1923) THEORY, WHICH FORMULATES THE
!+   MASS TRANSFER FLUX FOR MASS TRANSFER PHENOMENA
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| HN             |<->| DEPTH AT TIME T(N)
!| IKLE           |-->| CONNECTIVITY TABLE.
!| KDISS          |-->| DISSOLUTION RATE COEFFICIENT
!| NB_COMPO       |-->| NUMBER OF UNSOLUBLE COMPOUND IN OIL
!| NB_HAP         |-->| NUMBER OF SOLUBLE COMPOUND IN OIL
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NUMBER OF FLOATS
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| NTRAC          |-->| NUMBER OF TRACERS
!| UNSVOL         |-->| 1/(INTEGRAL OF TEST FUNCTIONS),
!|                |   | NOT ASSEMBLED IN PARALLEL
!| TB             |<->| BLOCK WITH T1,T2,...
!| TN             |<->| TRACERS AT TIME N
!| NTRAC          |-->| NUMBER OF TRACERS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| MESH           |<->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NB_COMPO,NTRAC
      INTEGER         , INTENT(IN)    :: NB_HAP,NFLOT,NDP
      DOUBLE PRECISION, INTENT(IN)    :: DT
      INTEGER         , INTENT(IN)    :: NELMAX,NPOIN,NPLAN
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(INOUT) :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: KDISS(NB_HAP)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TN
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TB
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: UNSVOL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,J,IFLOT,I1,I2,I3
      DOUBLE PRECISION X0,MASSE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::INVOL,HAUT
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::C
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::ALPHA
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::M
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::MW_HAP,MW_COMPO
      DOUBLE PRECISION TOT,TOTALE
      LOGICAL DEJA
      DATA DEJA /.FALSE./
!
      SAVE
!
!-----------------------------------------------------------------------
!-----------------------TABLE ALLOCATION--------------------------------
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA) THEN
        ALLOCATE(INVOL(NDP))
        ALLOCATE(HAUT(NDP))
        IF(NB_HAP.GT.0) THEN
          ALLOCATE(C(NB_HAP,NDP))
          ALLOCATE(M(NDP,NB_HAP))
          ALLOCATE(ALPHA(NB_HAP,NDP))
          ALLOCATE(MW_HAP(NB_HAP))
        END IF
        IF(NB_COMPO.GT.0) THEN
          ALLOCATE(MW_COMPO(NB_COMPO))
        END IF
        DEJA=.TRUE.
      ENDIF
      DO K= 1,NB_HAP
        CALL CPSTVC(TN%ADR(NTRAC-NB_HAP+K)%P,TB%ADR(K)%P)
        CALL OS('X=0     ',TB%ADR(K)%P)
      END DO
!
      IF(NFLOT.GT.0) THEN
        DO IFLOT = 1,NFLOT
          IF(PARTICULES(IFLOT)%STATE.EQ.1) THEN
!
!--------------------------------------------------------------
! -------------------INITIALIZATION----------------------------
!--------------------------------------------------------------
!
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
            ENDDO
            DO K=1,NB_COMPO
              MW_COMPO(K)=0.D0
            END DO
!
            I1=IKLE(PARTICULES(IFLOT)%ELTOIL,1)
            I2=IKLE(PARTICULES(IFLOT)%ELTOIL,2)
            I3=IKLE(PARTICULES(IFLOT)%ELTOIL,3)
!
!-----------------------------------------------------------------------
!------------MOLAR MASS CALCULATION OF UNSOLUBLE COMPOUND--------------
!-----------------------------------------------------------------------
!
            DO K=1,NB_COMPO
              MW_COMPO(K) = 0.04132D0-1.985D-04*
     &                    PARTICULES(IFLOT)%COMPO(K)%TB +
     &                    9.494D-7*PARTICULES(IFLOT)%COMPO(K)%TB**2
            ENDDO
!
!-----------------------------------------------------------------------
!-------------MOLAR MASS CALCULATION OF SOLUBLE COMPOUND---------------
!-----------------------------------------------------------------------
!
            DO K=1,NB_HAP
              MW_HAP(K) = 0.04132D0-1.985D-04*
     &                  PARTICULES(IFLOT)%HAP(K)%TB +
     &                  9.494D-07*PARTICULES(IFLOT)%HAP(K)%TB**2
            ENDDO
!
!-----------------------------------------------------------------------
!----------CALCULATION OF THE TOTAL MOL IN THE OIL SLICK----------------
!-----------------------------------------------------------------------
!
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
!
            IF(NPLAN.EQ.1)THEN
              INVOL(1)=UNSVOL%R(I1)/HN(I1)
              INVOL(2)=UNSVOL%R(I2)/HN(I2)
              INVOL(3)=UNSVOL%R(I3)/HN(I3)
            ELSE
              INVOL(1)=UNSVOL%R(I1+NPOIN*(NPLAN-1))
              INVOL(2)=UNSVOL%R(I2+NPOIN*(NPLAN-1))
              INVOL(3)=UNSVOL%R(I3+NPOIN*(NPLAN-1))
            END IF
!
            DO K=1,NB_HAP
              IF(HN(I1).GE.1.D-04)THEN
                ALPHA(K,1) = KDISS(K)*PARTICULES(IFLOT)%SURFACE *
     &                       PARTICULES(IFLOT)%SHPOIL(1)*INVOL(1)
              ENDIF
              IF(HN(I2).GE.1.D-04)THEN
                ALPHA(K,2) = KDISS(K)*PARTICULES(IFLOT)%SURFACE *
     &                       PARTICULES(IFLOT)%SHPOIL(2)*INVOL(2)
              ENDIF
              IF(HN(I3).GE.1.D-04)THEN
                ALPHA(K,3) = KDISS(K)*PARTICULES(IFLOT)%SURFACE *
     &                       PARTICULES(IFLOT)%SHPOIL(3)*INVOL(3)
              ENDIF
            ENDDO
!
            DO K=1,NB_HAP
              M(1,K)=0.D0
              M(2,K)=0.D0
              M(3,K)=0.D0
              IF(PARTICULES(IFLOT)%HAP(K)%MASS.GT.0.D0)THEN
                IF(TOTALE.GT.0.D0) THEN
                  X0=(PARTICULES(IFLOT)%HAP(K)%MASS/MW_HAP(K))/TOTALE
                ELSE
                  X0=0.D0
                ENDIF
                C(K,1)=TN%ADR(NTRAC-NB_HAP+K)%P%R(I1+NPOIN*(NPLAN-1))
                C(K,2)=TN%ADR(NTRAC-NB_HAP+K)%P%R(I2+NPOIN*(NPLAN-1))
                C(K,3)=TN%ADR(NTRAC-NB_HAP+K)%P%R(I3+NPOIN*(NPLAN-1))
                DO J=1,NDP
                  IF(HAUT(J).GE.1.D-04.AND.ALPHA(K,J)*DT.LT.1.D-10)THEN
                    M(J,K)=ALPHA(K,J)*DT*(1.D0/INVOL(J))*
     &                    (PARTICULES(IFLOT)%HAP(K)%SOL*X0-C(K,J))
                    M(J,K)=MAX(M(J,K),0.D0)
                  ELSE IF(HAUT(J).GE.1.D-04.AND.
     &                     ALPHA(K,J)*DT.GE.1.D-10)THEN
                    M(J,K)=(1.D0/INVOL(J))
     &                    *(PARTICULES(IFLOT)%HAP(K)%SOL
     &                    *X0-C(K,J))*(1.D0-EXP(-ALPHA(K,J)*DT))
                    M(J,K)=MAX(M(J,K),0.D0)
                  ENDIF
                ENDDO
              END IF
            END DO
            DO K=1,NB_HAP
              TOT=M(1,K)+M(2,K)+M(3,K)
              IF(TOT.GT.PARTICULES(IFLOT)%HAP(K)%MASS) THEN
                DO J=1,NDP
                  M(J,K)=M(J,K)*PARTICULES(IFLOT)%HAP(K)%MASS/TOT
                END DO
              END IF
            END DO
!
            MASSE=0.D0
            DO K=1,NB_COMPO
              MASSE=MASSE+PARTICULES(IFLOT)%COMPO(K)%MASS
            END DO
!
            TOT=0.D0
            DO K=1,NB_HAP
              PARTICULES(IFLOT)%HAP(K)%MASS=
     &              PARTICULES(IFLOT)%HAP(K)%MASS-(M(1,K)+M(2,K)+M(3,K))
              PARTICULES(IFLOT)%HAP(K)%MASS=
     &              MAX(0.D0,PARTICULES(IFLOT)%HAP(K)%MASS)
              TOT=TOT+M(1,K)+M(2,K)+M(3,K)
              MASSE=MASSE+PARTICULES(IFLOT)%HAP(K)%MASS
            END DO
            PARTICULES(IFLOT)%MASS_DISS=PARTICULES(IFLOT)%MASS_DISS+TOT
            PARTICULES(IFLOT)%MASS=MASSE
!
!-----------------------------------------------------------------------
!----------AMOUNT OF TRACER INJECTED INTO THE WATER COLUMN--------------
!-----------------------------------------------------------------------
!
            IF(HN(I1).GE.0.0001D0)THEN
              DO K=1,NB_HAP
                TB%ADR(K)%P%R(I1+NPOIN*(NPLAN-1))=
     &                TB%ADR(K)%P%R(I1+NPOIN*(NPLAN-1))+M(1,K)*INVOL(1)
              END DO
            END IF
            IF(HN(I2).GE.0.0001D0)THEN
              DO K=1,NB_HAP
                TB%ADR(K)%P%R(I2+NPOIN*(NPLAN-1))=
     &                TB%ADR(K)%P%R(I2+NPOIN*(NPLAN-1))+M(2,K)*INVOL(2)
              END DO
            END IF
            IF(HN(I3).GE.0.0001D0)THEN
              DO K=1,NB_HAP
                TB%ADR(K)%P%R(I3+NPOIN*(NPLAN-1))=
     &                TB%ADR(K)%P%R(I3+NPOIN*(NPLAN-1))+M(3,K)*INVOL(3)
              END DO
            END IF
          END IF
        END DO
      END IF
      DO K=1,NB_HAP
        IF(NCSIZE.GT.1) CALL PARCOM(TB%ADR(K)%P,2,MESH)
!
!-----------------------------------------------------------------------
!-------------UPDATING OF THE TRACER AMOUNT IN THE DOMAIN---------------
!-----------------------------------------------------------------------
!
        CALL OS('X=X+Y   ',X=TN%ADR(NTRAC-NB_HAP+K)%P ,Y=TB%ADR(K)%P)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_DISSO
!
!                       *********************
                        SUBROUTINE OIL_VOLATI
!                       *********************
!
     &(T3,TIMP,HPROP,NFLOT,NDP,NELMAX,IKLE,NPOIN,MESH,NB_HAP,KVOL,
     &NTRAC,AYASMI)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief    COMPUTE THE VOLATILIZATION OF DISSOLVED OIL IN WATER COLUMN
!+
!
!history  CEDRIC GOEURY (NHE)
!+        11/07/2013
!+        V6P3
!+   First version
!
!history  CEDRIC GOEURY (LNHE)
!+        23/05/2014
!+        V7P0
!+   Second version
!
!+   THIS ROUTINE COMPUTES THE VOLATILIZATION FLUX OF EACH DISSOLVED COMPONENT
!+   ACCORDING TO AN IMPLICIT TERM, WHICH MUST BE INFORMED BY THE USER
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HPROP          |-->| PROPAGATION DEPTH
!| IKLE           |-->| CONNECTIVITY TABLE
!| KVOL           |-->| VOLATILIZATION RATE COEFFICIENT
!| NB_HAP         |-->| NUMBER OF SOLUBLE OIL COMPONENT
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NUMBER OF FLOATS
!| NPOIN          |-->| NUMBER OF POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| TIMP           |<->| IMPLICIT SOURCE TERM
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| AYASMI         |<->| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!| MESH           |<->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NB_HAP,NTRAC
      INTEGER, INTENT(IN)             :: NELMAX,NPOIN,NFLOT,NDP
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION,INTENT(IN)     :: KVOL(NB_HAP)
      TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3,TIMP
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      LOGICAL, OPTIONAL, INTENT(INOUT):: AYASMI(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I1,I2,I3,IFLOT,ITRAC,I
!
!-----------------------------------------------------------------------
!
      DO ITRAC=1,NB_HAP
        CALL OS('X=0     ',X=TIMP%ADR(NTRAC-NB_HAP+ITRAC)%P)
        IF(PRESENT(AYASMI)) THEN
          AYASMI(NTRAC-NB_HAP+ITRAC)=.FALSE.
        ELSE
          TIMP%ADR(NTRAC-NB_HAP+ITRAC)%P%TYPR='0'
        END IF
      END DO
!
!-----------------------------------------------------------------------
!
      CALL CPSTVC(TIMP%ADR(1)%P,T3)
      CALL OS('X=0     ',T3)
!
      IF(NFLOT.GT.0) THEN
        DO IFLOT = 1,NFLOT
           I1=IKLE(PARTICULES(IFLOT)%ELTOIL,1)
           I2=IKLE(PARTICULES(IFLOT)%ELTOIL,2)
           I3=IKLE(PARTICULES(IFLOT)%ELTOIL,3)
           T3%R(I1)=T3%R(I1)+1.D0
           T3%R(I2)=T3%R(I2)+1.D0
           T3%R(I3)=T3%R(I3)+1.D0
        ENDDO
      END IF
!
!THE WORK TABLE T3 IS USED TO DEFINED THE AREA NOT COVERED BY THE OIL SLICK
!
      IF(NCSIZE.GT.1) CALL PARCOM(T3,2,MESH)
!
      IF(PRESENT(AYASMI)) THEN
        DO ITRAC=1,NB_HAP
          DO I=1,NPOIN
            IF(T3%R(I).LT.0.5D0.AND.HPROP%R(I).GE.1.D-4)THEN
              TIMP%ADR(NTRAC-NB_HAP+ITRAC)%P%R(I)=-KVOL(ITRAC)
            ELSE
              TIMP%ADR(NTRAC-NB_HAP+ITRAC)%P%R(I)=0.D0
            END IF
          END DO
          AYASMI(NTRAC-NB_HAP+ITRAC)=.TRUE.
        END DO
      ELSE
        DO ITRAC=1,NB_HAP
          DO I=1,NPOIN
            IF(T3%R(I).LT.0.5D0)THEN
              TIMP%ADR(NTRAC-NB_HAP+ITRAC)%P%R(I)=-KVOL(ITRAC)
            ELSE
              TIMP%ADR(NTRAC-NB_HAP+ITRAC)%P%R(I)=0.D0
            END IF
          END DO
          TIMP%ADR(NTRAC-NB_HAP+ITRAC)%P%TYPR='Q'
        END DO
      END IF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_VOLATI
!
!                       ***********************
                        SUBROUTINE OIL_BEACHING
!                       ***********************
!
     &(IKLE,NPOIN,NELMAX,NDP,H,HN,NFLOT,RHO_OIL,SURFAC,CF,ETA_OIL,LT)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief    COMPUTE THE BEACHING OF OIL ON SHORELINE
!+
!
!history  CEDRIC GOEURY (NHE)
!+        11/07/2013
!+        V6P3
!+   First version
!
!history  CEDRIC GOEURY (LNHE)
!+        23/05/2014
!+        V7P0
!+   Second version
!
!+   THIS ROUTINES COMPUTES THE OIL BEACHING OF THE PARTICLES BASED ON
!+   THE BANK TYPE ESTIMATED BY THE GRAIN SIZE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| ADIMENSIONAL FRICTION COEFFICIENT
!| HN             |-->| DEPTH AT TIME T(N)
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LT             |-->| CURRENT TIME STEP
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NUMBER OF FLOATS
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| RHO_OIL        |-->| DENSITY OF OIL
!| SURFAC         |-->| AREA OF ELEMENTS IN 2D
!| ETA_OIL        |-->| OIL VISCOSITY
!| H              |-->| WATER DEPTH AT TIME N+1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER         , INTENT(IN)    :: NELMAX,NDP,NPOIN
      INTEGER         , INTENT(IN)    :: NFLOT,LT
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: RHO_OIL,ETA_OIL
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,N1,N2,N3,K,R,J,NECH
!
      INTEGER IEL
      DOUBLE PRECISION HAUTPART,VOLECH,VOLDIS,L,M
      DOUBLE PRECISION TM,CV,DP,KS,EPAISSEUR
      DOUBLE PRECISION HH
!
!-----------------------------------------------------------------------
!
      TM=0.D0
      CV=0.D0
      DP=0.D0
!
      IF(NFLOT.GT.0) THEN
        DO IFLOT=1,NFLOT
          IF(PARTICULES(IFLOT)%STATE.EQ.1) THEN
            IEL=PARTICULES(IFLOT)%ELTOIL
            N1=IKLE(IEL,1)
            N2=IKLE(IEL,2)
            N3=IKLE(IEL,3)
            HAUTPART=H(N1)*PARTICULES(IFLOT)%SHPOIL(1)+
     &               H(N2)*PARTICULES(IFLOT)%SHPOIL(2)+
     &               H(N3)*PARTICULES(IFLOT)%SHPOIL(3)
!
            IF(PARTICULES(IFLOT)%MASS.GT.0.D0)THEN
              EPAISSEUR=(PARTICULES(IFLOT)%MASS/RHO_OIL)/
     &                 PARTICULES(IFLOT)%SURFACE
            ELSE
              EPAISSEUR=(PARTICULES(IFLOT)%MASS0/RHO_OIL)/
     &                 PARTICULES(IFLOT)%SURFACE
            END IF
!
            L=MAX(PARTICULES(IFLOT)%SHPOIL(1),
     &            PARTICULES(IFLOT)%SHPOIL(2),
     &            PARTICULES(IFLOT)%SHPOIL(3))
            IF(L.EQ.PARTICULES(IFLOT)%SHPOIL(1)) THEN
              J=N1
            ELSEIF(L.EQ.PARTICULES(IFLOT)%SHPOIL(2)) THEN
              J=N2
            ELSE
              J=N3
            ENDIF
!
            HH=MAX(HN(J),1.D-04)
            KS=MAX((CF(J)**3*8.2D0**6*HH)*0.125D0,6.5D-05)
!
            IF(HAUTPART.LE.EPAISSEUR.OR.KS.GE.HAUTPART) THEN
              IF(KS.GE.6.5D-05.AND.KS.LT.1.D-03) THEN
                DP=5.D-02
                IF(ETA_OIL.LT.3.D-05) THEN
                  TM=4.D-03
                  CV=0.004D0
                ELSEIF(ETA_OIL.GE.3.D-05.AND.ETA_OIL.LE.2.D-03) THEN
                  TM=17.D-03
                  CV=0.017D0
                ELSEIF(ETA_OIL.GT.2.D-03) THEN
                  TM=25.D-03
                  CV=0.025D0
                ENDIF
              ELSEIF(KS.GE.1.D-03.AND.KS.LT.256.D-03) THEN
                DP=18.D-02
                IF(ETA_OIL.LT.3.D-05) THEN
                  TM=2.D-03
                  CV=0.0021D0
                ELSE IF(ETA_OIL.GE.3.D-05.AND.ETA_OIL.LE.2.D-03) THEN
                  TM=9.D-03
                  CV=0.0091D0
                ELSEIF(ETA_OIL.GT.2.D-03) THEN
                  TM=15.D-03
                  CV=0.0151D0
                END IF
              ELSEIF(KS.GE.256.D-03) THEN
                DP=0.D0
                IF(ETA_OIL.LT.3.D-05) THEN
                  TM=1.D-03
                  CV=0.001D0
                ELSE IF(ETA_OIL.GE.3.D-05.AND.ETA_OIL.LE.2.D-03) THEN
                  TM=5.D-03
                  CV=0.005D0
                ELSEIF(ETA_OIL.GT.2.D-03) THEN
                  TM=10.D-03
                  CV=0.01D0
                ENDIF
              ENDIF
!
              VOLECH=0.D0
              VOLDIS=0.D0
!
              DO K=1,NFLOT
                IF(K.NE.IFLOT.AND.PARTICULES(K)%STATE.EQ.2.AND.
     &                 PARTICULES(K)%ELTOIL.EQ.IEL)THEN
                  M=MAX(PARTICULES(K)%SHPOIL(1),
     &                  PARTICULES(K)%SHPOIL(2),
     &                  PARTICULES(K)%SHPOIL(3))
                  IF(M.EQ.PARTICULES(K)%SHPOIL(1)) THEN
                    R=1
                  ELSEIF(M.EQ.PARTICULES(K)%SHPOIL(2)) THEN
                    R=2
                  ELSE
                    R=3
                  ENDIF
!
                  NECH=IKLE(PARTICULES(K)%ELTOIL,R)
                  IF(J.EQ.NECH) THEN
                    VOLECH=VOLECH+(PARTICULES(K)%MASS)/RHO_OIL
                  ENDIF
                ENDIF
              ENDDO
!
!IN THE CALCULATION OF THE BANK VOLUME AVAILABLE, THE SLOPE OF THE BANK IS NEGLECTED
!IN THE FOLLOWING FORMULATION
!
              VOLDIS=(1.D0/3.D0)*SURFAC(IEL)*TM+0.5D0*CV*DP*
     &              SQRT((1.D0/3.D0)*SURFAC(IEL))-VOLECH
!
              IF(PARTICULES(IFLOT)%MASS/RHO_OIL.LE.VOLDIS) THEN
                PARTICULES(IFLOT)%STATE=2
                PARTICULES(IFLOT)%TPSECH=LT
              END IF
            END IF

!
          ENDIF
!
        END DO
!
      END IF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_BEACHING
!
!                       *************************
                        SUBROUTINE OIL_REFLOATING
!                       *************************
!
     &(LT,DT,NPOIN,NELMAX,NDP,IKLE,H,HN,RHO_OIL,NFLOT,CF,NPLAN,Z)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief    COMPUTE THE RELEASE OF OIL BEACHED ON SHORELINE
!+
!
!history  CEDRIC GOEURY (NHE)
!+        11/07/2013
!+        V6P3
!+   First version
!
!history  CEDRIC GOEURY (LNHE)
!+        23/05/2014
!+        V7P0
!+   Second version
!
!+   THIS ROUTINE COMPUTES THE RELEASE OF THE OIL BEACHING PARTICLE BASED ON
!+   THE BANK TYPE ESTIMATED BY THE GRAIN SIZE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| ADIMENSIONAL FRICTION COEFFICIENT
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL)
!| HN             |<->| DEPTH AT TIME T(N)
!| IKLE           |-->| CONNECTIVITY TABLE
!| LT             |-->| CURRENT TIME STEP
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NUMBER OF FLOATS
!| NPOIN          |-->| NUMBER OF POINTS IN 2D MESH
!| RHO_OIL        |-->| DENSITY OF OIL
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| H              |<->| WATER DEPTH AT TIME N+1
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)          :: NPOIN,LT,NDP
      INTEGER         , INTENT(IN)          :: NELMAX
      INTEGER         , INTENT(IN)          :: NFLOT
      DOUBLE PRECISION, INTENT(INOUT)       :: H(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)          :: CF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)          :: RHO_OIL,DT
      INTEGER         , INTENT(IN)          :: IKLE(NELMAX,NDP)
      INTEGER, INTENT(IN)                   :: NPLAN
      DOUBLE PRECISION,INTENT(IN), OPTIONAL :: Z(NPOIN*NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,N1,N2,N3,J,IEL
!
      DOUBLE PRECISION HAUTPART,KS,KF,L
      DOUBLE PRECISION PREF,RAND,EPAISSEUR
      DOUBLE PRECISION HH
!
      INTRINSIC RANDOM_NUMBER
!
!-----------------------------------------------------------------------
!
      IF(NFLOT.GT.0)THEN
        DO IFLOT=1,NFLOT
          IF(PARTICULES(IFLOT)%STATE.EQ.2)THEN
            IEL=PARTICULES(IFLOT)%ELTOIL
            N1=IKLE(IEL,1)
            N2=IKLE(IEL,2)
            N3=IKLE(IEL,3)
!
            HAUTPART=H(N1)*PARTICULES(IFLOT)%SHPOIL(1)+
     &               H(N2)*PARTICULES(IFLOT)%SHPOIL(2)+
     &               H(N3)*PARTICULES(IFLOT)%SHPOIL(3)
!
            IF(PARTICULES(IFLOT)%MASS.GT.0.D0)THEN
              EPAISSEUR=(PARTICULES(IFLOT)%MASS/RHO_OIL)/
     &                 PARTICULES(IFLOT)%SURFACE
            ELSE
              EPAISSEUR=(PARTICULES(IFLOT)%MASS0/RHO_OIL)/
     &                 PARTICULES(IFLOT)%SURFACE
            END IF
!
            L=MAX(PARTICULES(IFLOT)%SHPOIL(1),
     &            PARTICULES(IFLOT)%SHPOIL(2),
     &            PARTICULES(IFLOT)%SHPOIL(3))
            IF(L.EQ.PARTICULES(IFLOT)%SHPOIL(1)) THEN
              J=N1
            ELSEIF(L.EQ.PARTICULES(IFLOT)%SHPOIL(2)) THEN
              J=N2
            ELSE
              J=N3
            ENDIF
!
            HH=MAX(HN(J),1.D-04)
            KS=((CF(J))**3*8.2D0**6*HH)*0.125D0
!
            KS=MAX(KS,6.5D-05)
            IF(HAUTPART.GT.EPAISSEUR.AND.KS.LT.HAUTPART)THEN
              PARTICULES(IFLOT)%STATE=1
              PARTICULES(IFLOT)%TPSECH=0
              IF(PRESENT(Z))THEN
                PARTICULES(IFLOT)%ZOIL=
     &                PARTICULES(IFLOT)%SHPOIL(1)*Z(N1+NPOIN*(NPLAN-1))+
     &                PARTICULES(IFLOT)%SHPOIL(2)*Z(N2+NPOIN*(NPLAN-1))+
     &                PARTICULES(IFLOT)%SHPOIL(3)*Z(N3+NPOIN*(NPLAN-1))
                PARTICULES(IFLOT)%SHZOIL=1.D0
              END IF
            ELSE
              PREF=0.D0
              IF(KS.GE.6.5D-05.AND.KS.LT.1.D-03)THEN
                KF=0.25D0/(24.D0*60.D0**2)
              ELSE IF(KS.GE.1.D-03.AND.KS.LT.256.D-03)THEN
                KF=0.15D0/(24.D0*60.D0**2)
              ELSEIF(KS.GE.256.D-03)THEN
                KF=0.85D0/(24.D0*60.D0**2)
              END IF
              PREF=1.D0-EXP(-KF*(LT-PARTICULES(IFLOT)%TPSECH)*DT)
              CALL RANDOM_NUMBER(RAND)
              IF(RAND.LT.PREF)THEN
                PARTICULES(IFLOT)%STATE=1
                PARTICULES(IFLOT)%TPSECH=0
                IF(PRESENT(Z))THEN
                  PARTICULES(IFLOT)%ZOIL=
     &                PARTICULES(IFLOT)%SHPOIL(1)*Z(N1+NPOIN*(NPLAN-1))+
     &                PARTICULES(IFLOT)%SHPOIL(2)*Z(N2+NPOIN*(NPLAN-1))+
     &                PARTICULES(IFLOT)%SHPOIL(3)*Z(N3+NPOIN*(NPLAN-1))
                  PARTICULES(IFLOT)%SHZOIL=1.D0
                END IF
              END IF
            END IF
          ENDIF
!
        END DO
      END IF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_REFLOATING
!
!                       ********************
                        SUBROUTINE OIL_BILAN
!                       ********************
!
     &(NFLOT,LT,FLOPRD)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief    COMPUTE THE OIL MASS BALANCE
!+
!
!history  CEDRIC GOEURY (NHE)
!+        11/07/2013
!+        V6P3
!+   First version
!
!history  CEDRIC GOEURY (LNHE)
!+        23/05/2014
!+        V7P0
!+   Second version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEB TWO RECORDS
!| LT             |-->| CURRENT TIME STEP
!| NFLOT          |-->| NUMBER OF FLOATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: LT,NFLOT,FLOPRD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,TOTAL
      DOUBLE PRECISION MASSE_PART,MASSE_INACT,MASSE_INI,MASSE_EVAP
      DOUBLE PRECISION MASSE_DISS,BILAN_OIL
!
      INTEGER P_IMAX
      DOUBLE PRECISION P_DSUM,P_DMAX
      EXTERNAL P_DSUM,P_IMAX,P_DMAX
!
!-----------------------------------------------------------------------
!--------------------------INITIALIZATION-------------------------------
!-----------------------------------------------------------------------
!
      MASSE_INACT= 0.D0
      MASSE_PART = 0.D0
      MASSE_INI  = 0.D0
      MASSE_EVAP = 0.D0
      MASSE_DISS = 0.D0
      BILAN_OIL  = 0.D0
!
!-----------------------------------------------------------------------
!
      IF(NFLOT.GT.0)THEN
        DO I=1,NFLOT
          IF(PARTICULES(I)%STATE.EQ.1.OR.PARTICULES(I)%STATE.EQ.2)THEN
            MASSE_EVAP=MASSE_EVAP+PARTICULES(I)%MASS_EVAP
            MASSE_DISS=MASSE_DISS+PARTICULES(I)%MASS_DISS
          ENDIF
          IF(PARTICULES(I)%STATE.EQ.2)THEN
            MASSE_INACT=MASSE_INACT+PARTICULES(I)%MASS
          ELSE
            MASSE_PART=MASSE_PART+PARTICULES(I)%MASS
          ENDIF
          MASSE_INI=MASSE_INI+PARTICULES(I)%MASS0
        ENDDO
!
        BILAN_OIL=0.D0
        IF(MASSE_INI.GT.0.D0) THEN
          BILAN_OIL=(MASSE_INI-(MASSE_INACT+MASSE_PART+
     &             MASSE_EVAP+MASSE_DISS))/MASSE_INI
        ENDIF
!
        IF(ABS(BILAN_OIL).GT.1.D-13) THEN
          WRITE(LU,*)  'PROBLEM WITH THE BALANCE OF OIL SPILL',IPID,
     &                 BILAN_OIL
        ENDIF
      END IF
!
!--------------------------------------------------------------------------
!------------------------------ PRINTING-----------------------------------
!--------------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) THEN
        TOTAL=P_IMAX(NFLOT)
      ELSE
        TOTAL=NFLOT
      ENDIF
!
      IF(MODULO(LT,FLOPRD).EQ.0.AND.TOTAL.GT.0) THEN
        IF(NCSIZE.GT.1) THEN
!
!--------------------------------------------------------------------------
!----------------------------PARALLEL VERSION------------------------------
!--------------------------------------------------------------------------
!
          WRITE(LU,*) '---------------------------------------',
     &                '----------------------------------------'
          WRITE(LU,*) '---------------------------------------',
     &                '----------------------------------------'
          WRITE(LU,*) '      BALANCE OF OIL SPILL             ',
     &               '                                        '
          WRITE(LU,*) '    MASS OF SPILL    ',P_DSUM(MASSE_PART)    ,
     &                (P_DSUM(MASSE_PART)/P_DSUM(MASSE_INI))*100.D0,'%'
          WRITE(LU,*) '    INITIAL MASS     ',P_DSUM(MASSE_INI)     ,
     &                100.D0,'%'
          WRITE(LU,*) '    STRANDED MASS    ',P_DSUM(MASSE_INACT)   ,
     &                (P_DSUM(MASSE_INACT)/P_DSUM(MASSE_INI))*100.D0,'%'
          WRITE(LU,*) '    DISSOLVED MASS   ',P_DSUM(MASSE_DISS)    ,
     &                (P_DSUM(MASSE_DISS)/P_DSUM(MASSE_INI))*100.D0,'%'
          WRITE(LU,*) '   EVAPORATED MASS   ',P_DSUM(MASSE_EVAP)    ,
     &                (P_DSUM(MASSE_EVAP)/P_DSUM(MASSE_INI))*100.D0,'%'
          WRITE(LU,*) '---------------------------------------',
     &                '----------------------------------------'
          WRITE(LU,*) '      BALANCE SURFACE SPILL         ',
     &                 P_DMAX(BILAN_OIL)
          WRITE(LU,*) '---------------------------------------',
     &                '----------------------------------------'
        ELSE
!
!--------------------------------------------------------------------------
!--------------------------SCALAR VERSION----------------------------------
!--------------------------------------------------------------------------
!
          WRITE(LU,*) '---------------------------------------',
     &               '----------------------------------------'
          WRITE(LU,*) '---------------------------------------',
     &                '----------------------------------------'
          WRITE(LU,*) '      BALANCE OF OIL SPILL             ',
     &                '                                        '
          WRITE(LU,*) '    MASS OF SPILL    ',MASSE_PART    ,
     &                (MASSE_PART/MASSE_INI)*100.D0,'%'
          WRITE(LU,*) '    INITIAL MASS     ',MASSE_INI     ,
     &                100.D0,'%'
          WRITE(LU,*) '    STRANDED MASS    ',MASSE_INACT   ,
     &                (MASSE_INACT/MASSE_INI)*100.D0,'%'
          WRITE(LU,*) '    DISSOLVED MASS   ',MASSE_DISS    ,
     &                (MASSE_DISS/MASSE_INI)*100.D0,'%'
          WRITE(LU,*) '   EVAPORATED MASS   ',MASSE_EVAP    ,
     &                (MASSE_EVAP/MASSE_INI)*100.D0,'%'
          WRITE(LU,*) '---------------------------------------',
     &                '----------------------------------------'
          WRITE(LU,*) '      BALANCE SURFACE SPILL           ',
     &                BILAN_OIL
          WRITE(LU,*) '---------------------------------------',
     &               '----------------------------------------'
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE OIL_BILAN
!
      END MODULE OILSPILL


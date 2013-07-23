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
!+        11/07/2013
!+        V6P3
!+   First version
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
!        DOUBLE PRECISION::SOL                         ---> SOLUBILITY OF OIL COMPONENT
!        DOUBLE PRECISION::MASS                        ---> MASS OF OIL COMPONENT
!        DOUBLE PRECISION::TB                          ---> BOILING TEMPERATURE OF OIL COMPONENT 
!      END TYPE COMPO
!      
!      TYPE OIL_PART
!        INTEGER::STATE                                ---> STATE OF THE OIL PARTICLE (1:DRIFT PARTICLE,2:BEACHING PARTICLE)
!        INTEGER::ELTOIL                               ---> NUMBERS OF ELEMENTS WHERE ARE THE PARTICLE
!        INTEGER::TPSECH                               ---> BEACHING TIME OF THE PARTICLE
!        INTEGER::ID                                   ---> TAG OF THE PARTICLE
!        DOUBLE PRECISION::XOIL,YOIL                   ---> POSITIONS OF FLOATING OIL PARTICLE
!        DOUBLE PRECISION::MASS0                       ---> INITAL MASS OF THE PARTICLE
!        DOUBLE PRECISION::MASS                        ---> MASS OF THE PARTICLE
!        DOUBLE PRECISION::MASS_DISS                   ---> DISSOLVED MASS OF THE PARTICLE
!        DOUBLE PRECISION::MASS_EVAP                   ---> EVAPORATED MASS OF THE PARTICLE
!        DOUBLE PRECISION::SURFACE                     ---> SURFACE OF THE PARTICLE
!        DOUBLE PRECISION,DIMENSION(3)::SHPOIL         ---> BARYCENTRIC COORDINATES OF PARTICLE IN THEIR ELEMENTS
!        TYPE(COMPO),DIMENSION(:),ALLOCATABLE::COMPO   ---> UNSOLVABLE COMPONENT OF OIL
!        TYPE(COMPO),DIMENSION(:),ALLOCATABLE::HAP     ---> SOLVABLE COMPONENT OF OIL
!      END TYPE OIL_PART
!
!=======================================================================
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      MODULE OILSPILL
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ) :: UCONV_OIL,VCONV_OIL
      TYPE(OIL_PART),DIMENSION(:),ALLOCATABLE::PARTICULES
!
      CONTAINS
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
!history  CEDRIC GOEURY (LHSV)
!+        28/06/2013
!+        V6P3
!+   First version
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEB TWO RECORDS
!|                |   | FOR FLOATS POSITIONS.
!| FM_COMPO       |-->| MASS FRACTION OF UNSOLVABLE COMPOUND IN OIL
!| FM_HAP         |-->| MASS FRACTION TEMPERATURE OF SOLVABLE COMPOUND IN OIL
!| ISUB           |<->| ARRIVAL SUB-DOMAIN OF PARTICLES.
!| KDISS          |-->| DISSOLVED MASS TRANSFER COEFFICIENT
!| KVOL           |-->| OVERALL VOLATILIZATION RATE COEFFICIENT
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS.
!| NB_COMPO       |-->| NUMBER OF UNSOLVABLE COMPOUND IN OIL
!| NB_HAP         |-->| NUMBER OF SOLVABLE COMPOUND IN OIL
!| PARTICULES     |-->| OIL STRUCTURE DEFINED IN BIEF DEF
!| SOLU           |-->| SOLUBILITY OF SOLVABLE COMPOUND
!| TB_COMPO       |-->| BOILING TEMPERATURE OF UNSOLVABLE COMPOUND IN OIL
!| TB_HAP         |-->| BOILING TEMPERATURE OF SOLVABLE COMPOUND IN OIL
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
      DOUBLE PRECISION,PARAMETER               ::KARMAN=0.41D0
      LOGICAL INIT
      DATA INIT /.FALSE./
      INTEGER                                  ::I,K,IFLOT
      INTEGER                                  ::NB_COMPO,NB_HAP
      DOUBLE PRECISION                         ::ETA_OIL,RHO_OIL,VOLDEV
      DOUBLE PRECISION                         ::TAMB,VERIF
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::TB_COMPO,FM_COMPO
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::TB_HAP,FM_HAP,SOLU
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::KDISS,KVOL
!
      SAVE RHO_OIL,VOLDEV,NB_COMPO,ETA_OIL,NB_HAP,TAMB
      SAVE TB_COMPO,FM_COMPO,TB_HAP,FM_HAP,SOLU,KDISS,KVOL
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
         ALLOCATE(PARTICULES(NFLOT_MAX))
!
!======================================================================
!----------------READING THE INPUT STEERING FILE-----------------------
!======================================================================   
!
         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) NB_COMPO
!
         DO I=1,NFLOT_MAX
           ALLOCATE(PARTICULES(I)%COMPO(NB_COMPO))
         END DO
!
         ALLOCATE(TB_COMPO(NB_COMPO))
         ALLOCATE(FM_COMPO(NB_COMPO))
         READ(T2D_FILES(T2DMIG)%LU,*)
         DO I=1,NB_COMPO
            READ(T2D_FILES(T2DMIG)%LU,*) FM_COMPO(I),
     &           TB_COMPO(I)
            VERIF=VERIF+FM_COMPO(I)
         END DO
!
         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) NB_HAP
!
         DO I=1,NFLOT_MAX
           ALLOCATE(PARTICULES(I)%HAP(NB_HAP))
         END DO
!
         ALLOCATE(TB_HAP(NB_HAP))
         ALLOCATE(FM_HAP(NB_HAP))
         ALLOCATE(SOLU(NB_HAP))
         ALLOCATE(KDISS(NB_HAP))
         ALLOCATE(KVOL(NB_HAP)) 
         READ(T2D_FILES(T2DMIG)%LU,*)
         DO I=1,NB_HAP
            READ(T2D_FILES(T2DMIG)%LU,*) FM_HAP(I)
     *           ,TB_HAP(I),SOLU(I),KDISS(I),KVOL(I)
            VERIF=VERIF+FM_HAP(I)
         END DO 
!
         IF(1.D0-VERIF.GT.ABS(1.D-10)) THEN
            WRITE(LU,*) 'WARNING::THE SUM OF EACH COMPONENT', 
     *           ' MASS FRACTION IS NOT EQUALS TO 1',VERIF
            WRITE(LU,*) 'PLEASE, MODIFIED THE INPUT STEERING FILE '
            CALL PLANTE(1)
         END IF
!
         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) RHO_OIL
!
         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) ETA_OIL
!
         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) VOLDEV
!
         READ(T2D_FILES(T2DMIG)%LU,*)
         READ(T2D_FILES(T2DMIG)%LU,*) TAMB
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
     *            SQRT(CF%R(I)*0.5D0))+0.036D0*WINDX%R(I)
             VCONV_OIL%R(I)=VCONV%R(I)*(1.D0+(1.D0/KARMAN)*
     *            SQRT(CF%R(I)*0.5D0))+0.036D0*WINDY%R(I)
           ENDDO
        ELSE
           DO I=1,UCONV%DIM1
             UCONV_OIL%R(I)=UCONV%R(I)*(1.D0+(1.D0/KARMAN)*
     *            SQRT(CF%R(I)*0.5D0))
             VCONV_OIL%R(I)=VCONV%R(I)*(1.D0+(1.D0/KARMAN)*
     *            SQRT(CF%R(I)*0.5D0))
           ENDDO
        END IF
!
!======================================================================
!--CALLING OIL_FLOT (CONTAINED INITIALIZATION OF PARTICULES STRUCTURE)-
!======================================================================
!
           CALL OIL_FLOT(PARTICULES,NFLOT,NFLOT_MAX,
     &                   MESH,LT,VOLDEV,RHO_OIL,NB_COMPO,NB_HAP,
     &                   FM_COMPO,TB_COMPO,FM_HAP,TB_HAP,SOLU)
!
!
!======================================================================
!--------------CALLING OIL_SPREADING (ETALEMENT DES PARTICULES)--------
!====================================================================== 
!
           CALL OIL_SPREADING(VOLDEV,ETA_OIL,RHO_OIL,NFLOT,
     &                        NFLOT_MAX,DT,2)
!
!======================================================================
!--------CALLING OIL_REFLOATING (RELEASE OF BEACHING PARTICLE)---------
!====================================================================== 
! 
           CALL OIL_REFLOATING(LT,DT,NPOIN,NELMAX,3,MESH%IKLE%I,H%R,
     &                         HN%R,RHO_OIL,NFLOT,CF%R)
!
!======================================================================
!-----CALLING DERIVE (ADVECTION AND DIFFUSION OF THE OIL PARTICLE)-----
!====================================================================== 
!
           CALL OIL_DERIVE(UCONV_OIL%R,VCONV_OIL%R,VCONV_OIL%R,DT,AT,
     &                     MESH%X%R,MESH%Y%R,MESH%Y%R,MESH%IKLE%I,
     &                     MESH%IFABOR%I,LT,11,UCONV_OIL%ELM,3,NPOIN,
     &                     NPOIN,NELEM,NELMAX,MESH%SURDET%R,
     &                     XFLOT%R,YFLOT%R,YFLOT%R,SHPFLO%R,SHPFLO%R,
     &                     TAGFLO%I,ELTFLO%I,ELTFLO%I,NFLOT,NFLOT_MAX,
     &                     FLOPRD,MESH,T2D_FILES(T2DFLO)%LU,IT1%I,
     &                     T1%R,T2%R,T2%R,IT2%I,W1%R,
     &                     W1%R,NPOIN,1,VISC,NB_COMPO,NB_HAP)
!
!======================================================================
!----------------------CALLING OIL_BEACHING----------------------------
!======================================================================   
!
           CALL OIL_BEACHING(MESH%IKLE%I,NPOIN,NELMAX,3,H%R,HN%R,NFLOT,
     &                       RHO_OIL,MESH%SURFAC%R,CF%R,ETA_OIL,LT)
!
!======================================================================
!-------------CALLING OIL_EVAP (EVAPORATION OF OIL PARTICLES)-----------
!======================================================================   
!
           CALL OIL_EVAP(NB_COMPO,NB_HAP,NFLOT,
     &          DT,3,NELMAX,MESH%IKLE%I,TAMB)
!
!======================================================================
!-----------CALLING OIL_DISSO (DISSOLUTION OF OIL PARTICLES)------------
!======================================================================  
!
           CALL OIL_DISSO(NB_COMPO,NB_HAP,NFLOT,DT,
     &                    3,NELMAX,MESH%IKLE%I,HN%R,NPOIN,UNSV2D%R,
     &                    TN,TB,MESH,KDISS)
!
!======================================================================
!-------MANAGEMENT OF LOST PARTICLES IF THEIR MASS EQUALS TO 0---------
!====================================================================== 
!
           DO IFLOT=1,NFLOT
              IF(PARTICULES(IFLOT)%MASS.EQ.0.D0) THEN 
                 CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT,
     &           NFLOT_MAX,MESH%TYPELM,IT1%I,PARTICULES,NB_COMPO,NB_HAP)
              END IF
           END DO
!
!======================================================================
!------------------CALLING OIL_VOLATI (VOLATILIZATION OF---------------
!----------------DISSOLVED COMPONENTS IN THE WATER COLUMN--------------
!====================================================================== 
!
           CALL OIL_VOLATI(T3,TIMP,YASMI,HPROP,NFLOT,
     &          3,NELMAX,MESH%IKLE%I,NPOIN,MESH,NB_HAP,KVOL)
!
!======================================================================
!---------------CALLING OIL_BILAN (MASS BALANCE OF OIL)----------------
!====================================================================== 
! 
           CALL OIL_BILAN(NFLOT,LT,FLOPRD)
!
!----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_SPILL_2D
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
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    - Like DERIVE but for oil spills.
!
!history  CEDRIC GOEURY (LHSV)
!+        28/06/2013
!+        V6P3
!+   First version
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      INTEGER N1,N2,N3,NOMB,SENS,IR
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
!     THE CALCULATION OF THE DRIFT IS PERFORMED ONLY FOR 
!     NON BEACHING OIL PARTICLES. SO, THE NON BEACHING PARTICLE
!     CHARACTERISTICS ARE TRANSFERRED TO THE WORK FLOAT TABLE
!
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
!
      CALL SCARACT(SVOID,SVOID,U,V,W,W,X,Y,ZSTAR,ZSTAR,
     &             XFLOT,YFLOT,ZFLOT,ZFLOT,
     &             DX,DY,DZ,DZ,Z,SHPFLO,SHZFLO,SHZFLO,SURDET,DT,
     &             IKLE,IFABOR,ELTFLO,ETAFLO,
     &             FRE,ELTBUF,ISUB,IELM,IELMU,NELEM,NELMAX,            
     &             NOMB,NPOIN,NPOIN2,NDP,NPLAN,1,MESH,IR,NPOIN2,SENS,        
     &             SHPBUF,SHZBUF,SHZBUF,FREBUF,SIZEBUF,
     &             APOST=.TRUE.,ASTOCHA=STOCHA,AVISC=VISC)
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1.AND.NFLOT.GT.0) THEN
!
!       IN // XFLOT AND YFLOT MAY HAVE BEEN DESTROYED BY SCARACT
!       BECAUSE RE-USED FOR GENERATIONS OF LOST PARTICLES
!       THEY ARE REDONE HERE FOR PARTICLES WHICH ARE STILL IN THE
!       SUB-DOMAIN
!
!     AT THIS STAGE, WE NEED TO RECOMPUTE THE TABLE ISUB AND ELTFLO
!     FOR ALL PARTICLES IN THE SUB-DOMAIN IN ORDER TO SEND AND DESTROY 
!     PARTICLE INFORMATION. MOREOVER, THE TABLE TAGFLO AND SHPFLO ARE
!     ARE RECOMPUTED IN ORDER TO HAVE INFORMATION NECESSARY TO LOCALIZED
!     THE PARTICLES THAT MIGRATED TO ANOTHER SUB-DOMAIN  
!
        IF(IELM.EQ.11) THEN
           DO IFLOT=NFLOT,1,-1
              IF(PARTICULES(IFLOT)%STATE.EQ.2) THEN 
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
              WRITE(LU,*) 'PROBLEME DANS OIL_DERIVE'
              WRITE(LU,*) 'RESTITUTION ISUB OIL IR: ',IR
              CALL PLANTE(1) 
              STOP  
           END IF
        ENDIF
      ELSE
         IF(IELM.EQ.11) THEN
            DO IFLOT=NFLOT,1,-1
               IF(PARTICULES(IFLOT)%STATE.EQ.2)THEN 
                  ELTFLO(IFLOT)=PARTICULES(IFLOT)%ELTOIL
               ELSE
                  ELTFLO(IFLOT)=ELTFLO(IR)
                  TAGFLO(IFLOT)=TAGFLO(IR)
                  SHPFLO(1,IFLOT)=SHPFLO(1,IR)
                  SHPFLO(2,IFLOT)=SHPFLO(2,IR)
                  SHPFLO(3,IFLOT)=SHPFLO(3,IR)
                  ELT=ELTFLO(IR)
                  IF(ELT.GT.0) THEN
                     N1=IKLE(ELT,1)
                     N2=IKLE(ELT,2)
                     N3=IKLE(ELT,3)
                     PARTICULES(IFLOT)%XOIL=SHPFLO(1,IR)*X(N1)
     &                    +SHPFLO(2,IR)*X(N2)
     &                    +SHPFLO(3,IR)*X(N3)     
                     PARTICULES(IFLOT)%YOIL=SHPFLO(1,IR)*Y(N1)
     &                    +SHPFLO(2,IR)*Y(N2)
     &                    +SHPFLO(3,IR)*Y(N3)
                     PARTICULES(IFLOT)%SHPOIL(1) = SHPFLO(1,IR)
                     PARTICULES(IFLOT)%SHPOIL(2) = SHPFLO(2,IR)
                     PARTICULES(IFLOT)%SHPOIL(3) = SHPFLO(3,IR)
                     PARTICULES(IFLOT)%ID = TAGFLO(IR)
                     PARTICULES(IFLOT)%ELTOIL = ELTFLO(IR)
                  ENDIF
                  IR = IR - 1
               END IF
            END DO
            IF(IR.NE.0)THEN
               WRITE(LU,*) 'PROBLE DANS OIL_DERIVE' 
               WRITE(LU,*) 'RESTITUTION ISUB OIL IR: ',IR
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
     &        ETAFLO,ISUB,TAGFLO,NDP,NFLOT,NFLOT_MAX,
     &        MESH,NPLAN,PARTICULES,NB_COMPO,NB_HAP)
         CALL OIL_SEND_PARTICLES(XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,ELTFLO,
     &        ETAFLO,ISUB,TAGFLO,NDP,NFLOT,NFLOT_MAX,
     &        MESH,NPLAN,PARTICULES)
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
        IF(NFLOT.GT.0.AND.NCSIZE.GT.1) THEN
          IF(ELTFLO(IFLOT).LE.0.OR.ISUB(IFLOT).NE.IPID) THEN 
             CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT,NFLOT_MAX,
     &            MESH%TYPELM,ISUB,PARTICULES,NB_COMPO,NB_HAP)
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
            CALL OIL_DEL_PARTICLE(PARTICULES(IFLOT)%ID,NFLOT,NFLOT_MAX,
     &           MESH%TYPELM,ISUB,PARTICULES,NB_COMPO,NB_HAP)
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
     &                PARTICULES(IFLOT)%XOIL,PARTICULES(IFLOT)%YOIL,
     &                PARTICULES(IFLOT)%STATE
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
     &                PARTICULES(IFLOT)%XOIL,PARTICULES(IFLOT)%YOIL,
     &                PARTICULES(IFLOT)%STATE
            ENDDO
          ENDIF
200       FORMAT(A,F12.4,A,A,I4,A,F12.4)
300       FORMAT(I6,',',F16.8,',',F16.8,',',I2)        
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
     *(VOLDEV,ETA_OIL,RHO_OIL,NFLOT,NFLOT_MAX,DT,ETAL)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   06/06/2013
!***********************************************************************
!
!brief
!+THIS ROUTINE COMPUTES THE SLICK EXPANSION CONTROLLED BY MACHANICAL 
!+FORCES SUCH AS GRAVITY, SURFACE TENSION, INERTIA AND VISCOSITY (FAY,1971).
!+TWO FORMULATION ARE IMPLEMENTED HERE:
!++++IF ETAL=1 THE FORMULATION PROPOSED BY FAY IS USED. THIS MODEL DOES NOT 
!+CONSIDER THE OIL VISCOSITY AND WOULD BE MORE CONVENIENT FOR CALM WATER
!++++IF ETAL=2 THE FORMULATION USED, HAS BEEN PROPOSED TO MODEL THE SLICK
!+EXPANSION FOR RELATIVELY SMALL SPILL (20 ML)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LT             |-->| CURRENT TIME STEP
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| ETAL           |-->| NUMBER OF THE SPREADING MODEL
!| ETA_OIL        |-->| OIL VISCOSITY
!| RHO_OIL        |-->| OIL DENSITY
!| VOLDEV         |-->| VOLUME OF THE OIL SPILL
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
      DOUBLE PRECISION, INTENT(IN) :: DT,RHO_OIL,ETA_OIL,VOLDEV        
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
       INTEGER IFLOT
       DOUBLE PRECISION PI,GRAVITE,RHO_EAU,DELTA,VOL,NU,NU2,COEF1,COEF2
!
       SAVE
!
!-----------------------------------------------------------------------
!
       IF(NFLOT.GT.0) THEN
         RHO_EAU=1000.D0
         PI=ACOS(-1.D0)
         GRAVITE=9.81D0
!        HARDCODED WATER MOLECULAR VISCOSITY
         NU=1.D-6
         NU2=NU**2
!
         COEF1=1.21D0**4
         COEF2=COEF1/1.53**2
         DELTA=(RHO_EAU-RHO_OIL)/(RHO_EAU)
         DO IFLOT = 1,NFLOT
           IF(PARTICULES(IFLOT)%STATE.EQ.1) THEN
             IF(ETAL.EQ.1)THEN
               IF(PARTICULES(IFLOT)%SURFACE.EQ.0.D0) THEN
                 PARTICULES(IFLOT)%SURFACE=PI*COEF2*
     &     (DELTA*GRAVITE/(VOLDEV*NU2))**(1.D0/6.D0)*VOLDEV/NFLOT_MAX
               ELSE
                 VOL=(PARTICULES(IFLOT)%MASS/RHO_OIL)
                 PARTICULES(IFLOT)%SURFACE=
     &                        SQRT((PARTICULES(IFLOT)%SURFACE-
     &                        PI*COEF2*
     &                        (DELTA*GRAVITE/(VOLDEV*NU2))
     &                        **(1.D0/6.D0)*(VOLDEV/NFLOT_MAX))**2
     &                        +(PI*COEF1*((GRAVITE*DELTA)
     &                        /(VOLDEV*SQRT(NU)))**(1.D0/3.D0)
     &                        *VOL)**2*DT)
     &                        +PI*COEF2*
     &                        (DELTA*GRAVITE/(VOLDEV*NU2))
     &                        **(1.D0/6.D0)*VOLDEV/NFLOT_MAX
               ENDIF
             ELSEIF(ETAL.EQ.2)THEN
               IF(PARTICULES(IFLOT)%SURFACE.EQ.0.D0)THEN
                 PARTICULES(IFLOT)%SURFACE=(27.D0*PI/2.D0)
     &                        **(1.D0/4.D0)*(GRAVITE*DELTA)
     &                        **(1.D0/4.D0)*(ETA_OIL*VOLDEV)
     &                        **(-1.D0/4.D0)*DT**(1.D0/4.D0)
     &                        *(VOLDEV/NFLOT_MAX)
               ELSE
                 VOL=PARTICULES(IFLOT)%MASS/RHO_OIL
                 PARTICULES(IFLOT)%SURFACE=(
     &                       PARTICULES(IFLOT)%SURFACE**4+(27.D0*PI/
     &              2.D0)*(GRAVITE*DELTA)
     &              *(ETA_OIL*VOLDEV)**(-1.D0)*DT*VOL**4)**(1.D0/4.D0) 
               ENDIF
             ENDIF
           ENDIF
         ENDDO
       ENDIF
!
!-----------------------------------------------------------------------
!       
       RETURN
       END SUBROUTINE OIL_SPREADING
!                       *******************
                        SUBROUTINE OIL_EVAP
!                       *******************
!
     &(NB_COMPO,NB_HAP,NFLOT,DT,NDP,NELMAX,IKLE,TAMB)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+THE EVAPORATION MODEL USED IS BASED ON A PSEUDO-COMPONENT APPROACH. THE
!+CHANGE IN THE MASS OF THE PETROLEUM COMPONENT IS CHARACTERIZED, USING
!+THE MOLAR FLUX EXPRESSION OF STIVER AND MACKAY (1984) AND THE THERMODYNAMIC
!+PHASE EQUILIBRIUM EQUATION. IN ORDER TO LIMIT THE VARIABLE NUMBER IN THE MODEL
!+THE GRAY WATSON METHOD (BOETHLING, 2000) IS USED TO DETERMINE THE MOLAR 
!+ENTHALPY AS A FUNCTION OF THE COMPONENT'S BOILING TEMPERATURE
!+IN THIS MODEL, THE MASS TRANSFER COEFFICIENT KEVAP IS CALCULATED ACCORDING TO 
!+THE THEORY OF MACKAY AND MATSUGU (1973)
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LT             |-->| CURRENT TIME STEP
!| NB_COMPO       |-->| NUMBER OF UNSOLVABLE COMPOUND IN OIL
!| NB_HAP         |-->| NUMBER OF SOLVABLE COMPOUND IN OIL
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| TAMB           |-->| WATER TEMPERATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : WINDX,WINDY,VENT
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU   
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NB_COMPO,NB_HAP,NDP,NELMAX
      INTEGER, INTENT(IN)          :: NFLOT
      DOUBLE PRECISION, INTENT(IN) :: DT,TAMB
      INTEGER         , INTENT(IN) :: IKLE(NELMAX,NDP)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,K,I1,I2,I3
      DOUBLE PRECISION::TOTALE,Ke,MW,MASSE,BILAN
      DOUBLE PRECISION::VENT_RELAT,VENTX,VENTY
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
         ALLOCATE(MASSE_EVAP_COMPO(NB_COMPO))
         ALLOCATE(MW_COMPO(NB_COMPO))
         ALLOCATE(C(NB_COMPO))
         ALLOCATE(MASSE_EVAP_HAP(NB_HAP))
         ALLOCATE(MW_HAP(NB_HAP))
         ALLOCATE(D(NB_HAP)) 
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
!------------MOLAR MASS CALCULATION OF UNSOLVABLE COMPOUND--------------
!-----------------------------------------------------------------------  
            DO K=1,NB_COMPO
               MW_COMPO(K)=
     *              0.04132D0-(1.985D-04*PARTICULES(IFLOT)%COMPO(K)%TB)
     *              +(9.494D-07*PARTICULES(IFLOT)%COMPO(K)%TB**2)
            END DO
!-----------------------------------------------------------------------
!------------MOLAR MASS CALCULATION OF UNSOLVABLE COMPOUND--------------
!-----------------------------------------------------------------------  
            DO K=1,NB_HAP
               MW_HAP(K)=
     *              0.04132D0-(1.985D-04*PARTICULES(IFLOT)%HAP(K)%TB)
     *              +(9.494D-07*PARTICULES(IFLOT)%HAP(K)%TB**2)
            END DO
!-----------------------------------------------------------------------
!--------------------MOLAR MASS CALCULATION OF THE OIL------------------
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
!-------WIND CALCULATION (DIFFERENCE BETWEEN U SURFACE AND WIND)--------
!-----------------------------------------------------------------------
            I1=IKLE(PARTICULES(IFLOT)%ELTOIL,1)
            I2=IKLE(PARTICULES(IFLOT)%ELTOIL,2)
            I3=IKLE(PARTICULES(IFLOT)%ELTOIL,3)

            IF(VENT)THEN
               VENTX=((UCONV_OIL%R(I1)-WINDX%R(I1))
     *              *PARTICULES(IFLOT)%SHPOIL(1)
     *              +(UCONV_OIL%R(I2)-WINDX%R(I2))
     *              *PARTICULES(IFLOT)%SHPOIL(2)
     *              +(UCONV_OIL%R(I3)-WINDX%R(I3))
     *              *PARTICULES(IFLOT)%SHPOIL(3))

               VENTY=((VCONV_OIL%R(I1)-WINDY%R(I1))
     *              *PARTICULES(IFLOT)%SHPOIL(1)
     *              +(VCONV_OIL%R(I2)-WINDY%R(I2))
     *              *PARTICULES(IFLOT)%SHPOIL(2)
     *              +(VCONV_OIL%R(I3)-WINDY%R(I3))
     *              *PARTICULES(IFLOT)%SHPOIL(3))
            ELSE
               VENTX=UCONV_OIL%R(I1)*PARTICULES(IFLOT)%SHPOIL(1)
     *              +UCONV_OIL%R(I2)*PARTICULES(IFLOT)%SHPOIL(2)
     *              +UCONV_OIL%R(I3)*PARTICULES(IFLOT)%SHPOIL(3)

               VENTY=VCONV_OIL%R(I1)*PARTICULES(IFLOT)%SHPOIL(1)
     *              +VCONV_OIL%R(I2)*PARTICULES(IFLOT)%SHPOIL(2)
     *              +VCONV_OIL%R(I3)*PARTICULES(IFLOT)%SHPOIL(3)
            END IF
            VENT_RELAT = SQRT(VENTX**2+VENTY**2)
!-----------------------------------------------------------------------
!------------CALCULATION OF THE MASS TRANSFER COEFFICIENT---------------
!-----------------------------------------------------------------------  
            Ke=0.0048D0*VENT_RELAT**0.78D0*1.3676D0
     *           *(0.018D0/MW)**(1.D0/3.D0)
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
!-----------------------------(UNSOLVABLE)-------------------------------------
!------------------------------------------------------------------------------            
            DO K=1,NB_COMPO
               MASSE_EVAP_COMPO(K)=0.D0
               C(K)=PARTICULES(IFLOT)%COMPO(K)%MASS
               IF(PARTICULES(IFLOT)%COMPO(K)%MASS.GT.0.D0) THEN
                  PARTICULES(IFLOT)%COMPO(K)%MASS= 
     *                 PARTICULES(IFLOT)%COMPO(K)%MASS*
     *                 EXP(-((Ke*PARTICULES(IFLOT)%SURFACE*DT*
     *                 EXP(LOG(82.06D0*PARTICULES(IFLOT)%COMPO(K)%TB)*
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
!---------DETERMINATION OF EACH OIL COMPONENT MASS AFTER EVAPORATION-----------
!-----------------------------(SOLVABLE)---------------------------------------
!------------------------------------------------------------------------------  
            DO K=1,NB_HAP
               MASSE_EVAP_HAP(K)=0.D0
               D(K)=PARTICULES(IFLOT)%HAP(K)%MASS
               IF(PARTICULES(IFLOT)%HAP(K)%MASS.GT.0.D0) THEN
                  PARTICULES(IFLOT)%HAP(K)%MASS= 
     *                 PARTICULES(IFLOT)%HAP(K)%MASS*
     *                 EXP(-((Ke*PARTICULES(IFLOT)%SURFACE*DT*
     *                 EXP(LOG(82.06D0*PARTICULES(IFLOT)%HAP(K)%TB)*
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
     *(NB_COMPO,NB_HAP,NFLOT,DT,NDP,
     * NELMAX,IKLE,HN,NPOIN,UNSV2D,TN,TB,MESH,KDISS)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+THIS ROUTINE COMPUTES THE DISSOLUTION PHENOMENON OF EACH OIL SOLVABLE COMPONENT
!+BASED ON WHITMAN'S (1923) THEORY, WHICH FORMULATES THE MASS TRANSFER FLUX 
!+FOR MASS TRANSFER PHENOMENA
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| HN             |<->| DEPTH AT TIME T(N)
!| IKLE           |-->| CONNECTIVITY TABLE.
!| KDISS          |-->| DISSOLUTION RATE COEFFICIENT
!| LT             |-->| CURRENT TIME STEP
!| NB_COMPO       |-->| NUMBER OF UNSOLVABLE COMPOUND IN OIL
!| NB_HAP         |-->| NUMBER OF SOLVABLE COMPOUND IN OIL
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NPOIN          |-->| NUMBER OF POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| UNSV2D         |-->| 1/(INTEGRAL OF TEST FUNCTIONS), NOT ASSEMBLED IN PARALLEL
!| TB             |<->| BLOCK WITH T1,T2,...
!| TN             |<->| TRACERS AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF                  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NB_COMPO
      INTEGER, INTENT(IN)             :: NB_HAP,NFLOT,NDP
      DOUBLE PRECISION, INTENT(IN)    :: DT
      INTEGER         , INTENT(IN)    :: NELMAX,NPOIN
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(INOUT) :: HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UNSV2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: KDISS(NB_HAP)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TN    
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TB  
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,J,IFLOT,I1,I2,I3
      DOUBLE PRECISION::X0,MASSE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::VOL,HAUT
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::C
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::ALPHA
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::M
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::MW_HAP,MW_COMPO
      DOUBLE PRECISION::TOT,TOTALE
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
        ALLOCATE(VOL(NDP))
        ALLOCATE(HAUT(NDP))
        ALLOCATE(C(NB_HAP,NDP))
        ALLOCATE(M(NDP,NB_HAP))
        ALLOCATE(ALPHA(NB_HAP,NDP))
        ALLOCATE(MW_HAP(NB_HAP))
        ALLOCATE(MW_COMPO(NB_COMPO))
        DEJA=.TRUE.
      ENDIF
      DO K= 1,NB_HAP
        CALL CPSTVC(TN%ADR(K)%P,TB%ADR(K)%P)
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
!------------MOLAR MASS CALCULATION OF UNSOLVABLE COMPOUND--------------
!-----------------------------------------------------------------------
! 
             DO K=1,NB_COMPO
               MW_COMPO(K)=
     &              0.04132D0-(1.985D-04*PARTICULES(IFLOT)%COMPO(K)%TB)
     &              +(9.494D-7*PARTICULES(IFLOT)%COMPO(K)%TB**2)
             ENDDO
!
!-----------------------------------------------------------------------
!-------------MOLAR MASS CALCULATION OF SOLVABLE COMPOUND---------------
!-----------------------------------------------------------------------
!  
             DO K=1,NB_HAP
               MW_HAP(K)=
     &              0.04132D0-(1.985D-04*PARTICULES(IFLOT)%HAP(K)%TB)
     &              +(9.494D-07*PARTICULES(IFLOT)%HAP(K)%TB**2)
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
            VOL(1)=HN(I1)/UNSV2D(I1)
            VOL(2)=HN(I2)/UNSV2D(I2)
            VOL(3)=HN(I3)/UNSV2D(I3)
            DO K=1,NB_HAP
               IF(HN(I1).GE.1.D-04)THEN
                 ALPHA(K,1)=((KDISS(K)*(PARTICULES(IFLOT)%SURFACE))
     &                 *PARTICULES(IFLOT)%SHPOIL(1))*(UNSV2D(I1)/HN(I1))
               ENDIF
               IF(HN(I2).GE.1.D-04)THEN
                 ALPHA(K,2)=((KDISS(K)*(PARTICULES(IFLOT)%SURFACE))
     &                 *PARTICULES(IFLOT)%SHPOIL(2))*(UNSV2D(I2)/HN(I2))
               ENDIF
               IF(HN(I3).GE.1.D-04)THEN
                 ALPHA(K,3)=((KDISS(K)*(PARTICULES(IFLOT)%SURFACE))
     &                 *PARTICULES(IFLOT)%SHPOIL(3))*(UNSV2D(I3)/HN(I3))
               ENDIF
            ENDDO 
!
            DO K=1,NB_HAP
               IF(PARTICULES(IFLOT)%HAP(K)%MASS.GT.0.D0)THEN
                  IF(TOTALE.GT.0.D0) THEN
                     X0=(PARTICULES(IFLOT)%HAP(K)%MASS/MW_HAP(K))/TOTALE
                  ELSE
                     X0=0.D0
                  ENDIF
                  C(K,1)=TN%ADR(K)%P%R(I1)
                  C(K,2)=TN%ADR(K)%P%R(I2)
                  C(K,3)=TN%ADR(K)%P%R(I3)
                  DO J=1,NDP
                   IF(HAUT(J).GE.1.D-04.AND.ALPHA(K,J)*DT.LT.1.D-10)THEN
                      M(J,K)=0.D0 
                      M(J,K)=ALPHA(K,J)*DT*VOL(J)*
     &                     (PARTICULES(IFLOT)%HAP(K)%SOL*X0-C(K,J))
                      M(J,K)=MAX(M(J,K),0.D0)
                   ELSE IF(HAUT(J).GE.1.D-04.AND.
     &                     ALPHA(K,J)*DT.GE.1.D-10)THEN
                      M(J,K)=0.D0
                      M(J,K)=VOL(J)*(PARTICULES(IFLOT)%HAP(K)%SOL
     &                     *X0-C(K,J))*(1.D0-EXP(-ALPHA(K,J)*DT))
                      M(J,K)=MAX(M(J,K),0.D0)
                   ELSE
                      M(J,K)=0.D0
                   ENDIF
                  ENDDO
               ELSE
                  M(1,K)=0.D0
                  M(2,K)=0.D0
                  M(3,K)=0.D0
               END IF
            END DO
            DO K=1,NB_HAP
               TOT=M(1,K)+M(2,K)+M(3,K)
               IF(TOT.GT.PARTICULES(IFLOT)%HAP(K)%MASS) THEN
                 DO J=1,NDP
                   M(J,K)=(M(J,K)*PARTICULES(IFLOT)%HAP(K)%MASS)/TOT
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
             PARTICULES(IFLOT)%HAP(K)%MASS=PARTICULES(IFLOT)%HAP(K)%MASS
     &              -(M(1,K)+M(2,K)+M(3,K))
             PARTICULES(IFLOT)%HAP(K)%MASS=
     &            MAX(0.D0,PARTICULES(IFLOT)%HAP(K)%MASS)
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
                  TB%ADR(K)%P%R(I1)=TB%ADR(K)%P%R(I1)+M(1,K)
     &                 *(UNSV2D(I1)/HN(I1))
               END DO
            END IF
            IF(HN(I2).GE.0.0001D0)THEN
               DO K=1,NB_HAP
                  TB%ADR(K)%P%R(I2)=TB%ADR(K)%P%R(I2)+M(2,K)
     &                 *(UNSV2D(I2)/HN(I2))
               END DO
            END IF
            IF(HN(I3).GE.0.0001D0)THEN
               DO K=1,NB_HAP
                  TB%ADR(K)%P%R(I3)=TB%ADR(K)%P%R(I3)+M(3,K)
     &                 *(UNSV2D(I3)/HN(I3))
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
        CALL OS('X=X+Y   ',X=TN%ADR(K)%P ,Y=TB%ADR(K)%P)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_DISSO
!                       *********************
                        SUBROUTINE OIL_VOLATI
!                       *********************
!
     *(T3,TIMP,YASMI,HPROP,NFLOT,NDP,NELMAX,IKLE,NPOIN,MESH,NB_HAP,KVOL)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+THIS ROUTINE COMPUTES THE VOLATILIZATION FLUX OF EACH DISSOLVED COMPONENT
!+ACCORDING TO AN IMPLICIT TERM, WHICH MUST BE INFORMED BY THE USER
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HPROP          |-->| PROPAGATION DEPTH
!| IKLE           |-->| CONNECTIVITY TABLE.
!| KVOL           |-->| VOLATILIZATION RATE COEFFICIENT
!| NB_HAP         |-->| NUMBER OF SOLVABLE OIL COMPONENT
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NPOIN          |-->| NUMBER OF POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| TIMP           |<->| IMPLICIT SOURCE TERM.
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| YASMI          |<->| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF                  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NB_HAP
      INTEGER, INTENT(IN)             :: NELMAX,NPOIN,NFLOT,NDP
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,NDP)
      LOGICAL, INTENT(INOUT)          :: YASMI(*) 
      DOUBLE PRECISION,INTENT(IN)     :: KVOL(NB_HAP)
      TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3,TIMP
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I1,I2,I3,IFLOT,ITRAC,I,NFLOTMAX
      INTEGER P_IMAX 
      EXTERNAL P_IMAX
!
!-----------------------------------------------------------------------
!
      DO ITRAC=1,NB_HAP
        CALL OS('X=0     ',X=TIMP%ADR(ITRAC)%P)
      ENDDO
!
!-----------------------------------------------------------------------
!
      CALL CPSTVC(TIMP%ADR(1)%P,T3)
      CALL OS('X=0     ',T3)
!
      IF(NCSIZE.GT.1) THEN
        NFLOTMAX=P_IMAX(NFLOT)
      ELSE
        NFLOTMAX=NFLOT
      ENDIF
!
      IF(NFLOTMAX.GT.0) THEN
        DO IFLOT = 1,NFLOT
           I1=IKLE(PARTICULES(IFLOT)%ELTOIL,1)
           I2=IKLE(PARTICULES(IFLOT)%ELTOIL,2)
           I3=IKLE(PARTICULES(IFLOT)%ELTOIL,3)
           T3%R(I1)=T3%R(I1)+1.D0
           T3%R(I2)=T3%R(I2)+1.D0
           T3%R(I3)=T3%R(I3)+1.D0
        ENDDO
!
!THE WORK TABLE T3 IS USED TO DEFINED THE AREA NOT COVERED BY THE OIL SLICK
!
        IF(NCSIZE.GT.1) CALL PARCOM(T3,2,MESH)
!
        DO ITRAC=1,NB_HAP
          DO I=1,NPOIN
            IF(T3%R(I).LT.0.5D0.AND.HPROP%R(I).GE.1.D-4)THEN
              TIMP%ADR(ITRAC)%P%R(I)=-KVOL(ITRAC)	
            ELSE
              TIMP%ADR(ITRAC)%P%R(I)=0.D0
            END IF
          END DO
          YASMI(ITRAC)=.TRUE.
        END DO
!
      END IF
!
!-----------------------------------------------------------------------
!
      RETURN  
      END SUBROUTINE OIL_VOLATI
!                       ***********************
                        SUBROUTINE OIL_BEACHING
!                       ***********************
!
     &(IKLE,NPOIN,NELMAX,NDP,H,HN,NFLOT,RHO_OIL,
     & SURFAC,CF,ETA_OIL,LT)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+THIS ROUTINES COMPUTES THE OIL BEACHING OF THE PARTICLES BASED ON
!+THE BANK TYPE ESTIMATED BY THE GRAIN SIZE
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| ADIMENSIONAL FRICTION COEFFICIENT
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEB TWO RECORDS
!| HN             |-->| DEPTH AT TIME T(N)
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LT             |-->| CURRENT TIME STEP
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NPOIN          |-->| NUMBER OF POINTS
!| RHO_OIL        |-->| DENSITY OF OIL
!| SURFAC         |-->| AREA OF ELEMENTS IN 2D
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
      DOUBLE PRECISION Tm,Cv,Dp,Ks,EPAISSEUR
      DOUBLE PRECISION HH
!
!-----------------------------------------------------------------------
!
      Tm=0.D0
      Cv=0.D0
      Dp=0.D0
!
      DO IFLOT=1,NFLOT
        IF(PARTICULES(IFLOT)%STATE.EQ.1) THEN
          IEL=PARTICULES(IFLOT)%ELTOIL
          N1=IKLE(IEL,1)
          N2=IKLE(IEL,2)
          N3=IKLE(IEL,3)
          HAUTPART=H(N1)*PARTICULES(IFLOT)%SHPOIL(1)+
     &             H(N2)*PARTICULES(IFLOT)%SHPOIL(2)+
     &             H(N3)*PARTICULES(IFLOT)%SHPOIL(3)
!
          EPAISSEUR=(PARTICULES(IFLOT)%MASS/RHO_OIL)/
     &               PARTICULES(IFLOT)%SURFACE
!
          L=MAX(PARTICULES(IFLOT)%SHPOIL(1),
     &          PARTICULES(IFLOT)%SHPOIL(2),
     &          PARTICULES(IFLOT)%SHPOIL(3))
          IF(L.EQ.PARTICULES(IFLOT)%SHPOIL(1)) THEN
            J=N1
          ELSEIF(L.EQ.PARTICULES(IFLOT)%SHPOIL(2)) THEN
            J=N2
          ELSE
            J=N3
          ENDIF
!
          HH=MAX(HN(J),1.D-04)
          Ks=MAX((CF(J)**3*8.2D0**6*HH)*0.125D0,6.5D-05)
!
          IF(HAUTPART.LE.EPAISSEUR.OR.Ks.GE.HAUTPART) THEN
            IF(Ks.GE.6.5D-05.AND.Ks.LT.1.D-03) THEN
              Dp=5.D-02
              IF(ETA_OIL.LT.3.D-05) THEN
                Tm=4.D-03
                Cv=0.004D0
              ELSEIF(ETA_OIL.GE.3.D-05.AND.ETA_OIL.LE.2.D-03) THEN
                Tm=17.D-03
                Cv=0.017D0
              ELSEIF(ETA_OIL.GT.2.D-03) THEN
                Tm=25.D-03
                Cv=0.025D0
              ENDIF
            ELSEIF(Ks.GE.1.D-03.AND.Ks.LT.256.D-03) THEN
              Dp=18.D-02
              IF(ETA_OIL.LT.3.D-05) THEN
                Tm=2.D-03
                Cv=0.0021D0
              ELSE IF(ETA_OIL.GE.3.D-05.AND.ETA_OIL.LE.2.D-03) THEN
                Tm=9.D-03
                Cv=0.0091D0
              ELSEIF(ETA_OIL.GT.2.D-03) THEN
                Tm=15.D-03
                Cv=0.0151D0
              END IF
            ELSEIF(Ks.GE.256.D-03) THEN
              Dp=0.D0
              IF(ETA_OIL.LT.3.D-05) THEN
                Tm=1.D-03
                Cv=0.001D0
              ELSE IF(ETA_OIL.GE.3.D-05.AND.ETA_OIL.LE.2.D-03) THEN
                Tm=5.D-03
                Cv=0.005D0
              ELSEIF(ETA_OIL.GT.2.D-03) THEN
                Tm=10.D-03
                Cv=0.01D0
              ENDIF
            ENDIF
!
            VOLECH=0.D0
            VOLDIS=0.D0
!
            DO K=1,NFLOT
              IF(K.NE.IFLOT.AND.PARTICULES(K)%STATE.EQ.2.AND.
     &                 PARTICULES(K)%ELTOIL.EQ.IEL)THEN
                M=MAX(PARTICULES(K)%SHPOIL(1)
     &               ,PARTICULES(K)%SHPOIL(2)
     &               ,PARTICULES(K)%SHPOIL(3))
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
            VOLDIS=(1.D0/3.D0)*SURFAC(IEL)*Tm+0.5D0*Cv*Dp
     &              *SQRT((1.D0/3.D0)*SURFAC(IEL))-VOLECH
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
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE OIL_BEACHING
!                       *************************
                        SUBROUTINE OIL_REFLOATING
!                       *************************
!
     *(LT,DT,NPOIN,NELMAX,NDP,IKLE,H,HN,RHO_OIL,NFLOT,CF)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+THIS ROUTINE COMPUTES THE RELEASE OF THE OIL BEACHING PARTICLE BASED ON
!+THE BANK TYPE ESTIMATED BY THE GRAIN SIZE
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| ADIMENSIONAL FRICTION COEFFICIENT
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEB TWO RECORDS
!| HN             |<->| DEPTH AT TIME T(N)
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LT             |-->| CURRENT TIME STEP
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NPOIN          |-->| NUMBER OF POINTS
!| RHO_OIL        |-->| DENSITY OF OIL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF                  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,LT,NDP
      INTEGER         , INTENT(IN)    :: NELMAX
      INTEGER         , INTENT(IN)    :: NFLOT
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: RHO_OIL,DT
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,N1,N2,N3,J,IEL
!
      DOUBLE PRECISION HAUTPART,Ks,Kf,L
      DOUBLE PRECISION Pref,RAND,EPAISSEUR
      DOUBLE PRECISION HH
!
!-----------------------------------------------------------------------
!
      DO  IFLOT=1,NFLOT
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
            EPAISSEUR=(PARTICULES(IFLOT)%MASS/RHO_OIL)/
     &           PARTICULES(IFLOT)%SURFACE
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
            Ks=((CF(J))**3*8.2D0**6*HH)*0.125D0
!
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
!                       ********************
                        SUBROUTINE OIL_BILAN
!                       ********************
!
     *(NFLOT,LT,FLOPRD)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   16/06/2013
!***********************************************************************
!
!brief
!+THIS ROUTINE COMPUTES THE OIL MASS BALANCE
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEB TWO RECORDS
!| LT             |-->| CURRENT TIME STEP
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF                  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: LT,NFLOT
      INTEGER         , INTENT(IN) :: FLOPRD   
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         ::I,MOD
      DOUBLE PRECISION::MASSE_PART
      DOUBLE PRECISION::MASSE_INACT
      DOUBLE PRECISION::MASSE_INI
      DOUBLE PRECISION::MASSE_EVAP
      DOUBLE PRECISION::MASSE_DISS
      DOUBLE PRECISION::BILAN_OIL
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!-----------------------------------------------------------------------
!--------------------------INITIALIZATION-------------------------------
!-----------------------------------------------------------------------
!
      MASSE_INACT=0.D0
      MASSE_PART=0.D0
      MASSE_INI=0.D0
      MASSE_EVAP=0.D0
      MASSE_DISS=0.D0
      BILAN_OIL=0.D0

      MOD=MODULO(LT,FLOPRD) 
!
!-----------------------------------------------------------------------
!
      DO I=1,NFLOT
        IF(PARTICULES(I)%STATE.EQ.1.OR.PARTICULES(I)%STATE.EQ.2)THEN
          MASSE_PART=MASSE_PART+PARTICULES(I)%MASS
          MASSE_EVAP=MASSE_EVAP+PARTICULES(I)%MASS_EVAP
          MASSE_DISS=MASSE_DISS+PARTICULES(I)%MASS_DISS
        ENDIF
        IF(PARTICULES(I)%STATE.EQ.2)THEN
          MASSE_INACT=MASSE_INACT+PARTICULES(I)%MASS
        ENDIF
        MASSE_INI=MASSE_INI+PARTICULES(I)%MASS0
      ENDDO
!
      BILAN_OIL=0.D0
      IF(MASSE_INI.GT.0.D0) THEN
         BILAN_OIL=(MASSE_INI-(MASSE_INACT+MASSE_PART+
     *        MASSE_EVAP+MASSE_DISS))/MASSE_INI
      END IF

      IF(ABS(BILAN_OIL).GT.1.D-13)THEN
        WRITE(LU,*)  'BILAN',IPID,BILAN_OIL
      END IF
!
!--------------------------------------------------------------------------
!----------------------------PARALLEL VERSION------------------------------
!--------------------------------------------------------------------------
!
      IF(NCSIZE.GT.1)THEN
      IF(MOD.EQ.0) WRITE(LU,*) '======================================='
      IF(MOD.EQ.0) WRITE(LU,*) '========','BILAN HYDROCARBURE','======='
      IF(MOD.EQ.0) WRITE(LU,*) '======================================='
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE NAPPE',P_DSUM(MASSE_PART)
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE INITIALE',P_DSUM(MASSE_INI)
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE ECHOUEE',P_DSUM(MASSE_INACT)
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE DISSOUTE',P_DSUM(MASSE_DISS)
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE EVAPOREE',P_DSUM(MASSE_EVAP)
      IF(MOD.EQ.0) WRITE(LU,*) '======================================='
      IF(MOD.EQ.0) WRITE(LU,*) 'BILAN NAPPE SURFACE',P_DSUM(BILAN_OIL)
      IF(MOD.EQ.0) WRITE(LU,*) '======================================='
      ELSE
!
!--------------------------------------------------------------------------
!--------------------------SCALAR VERSION----------------------------------
!--------------------------------------------------------------------------
!
      IF(MOD.EQ.0) WRITE(LU,*) '======================================='
      IF(MOD.EQ.0) WRITE(LU,*) '========','BILAN HYDROCARBURE','======='
      IF(MOD.EQ.0) WRITE(LU,*) '======================================='
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE NAPPE',MASSE_PART
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE INITIALE',MASSE_INI
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE ECHOUEE',MASSE_INACT
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE DISSOUTE',MASSE_DISS
      IF(MOD.EQ.0) WRITE(LU,*) 'MASSE EVAPOREE',MASSE_EVAP
      IF(MOD.EQ.0) WRITE(LU,*) '======================================='
      IF(MOD.EQ.0) WRITE(LU,*) 'BILAN NAPPE SURFACE',BILAN_OIL
      IF(MOD.EQ.0) WRITE(LU,*) '======================================='
      END IF
      RETURN
      END SUBROUTINE OIL_BILAN
!
      END MODULE OILSPILL

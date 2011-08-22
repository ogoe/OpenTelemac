!                    *****************
                     SUBROUTINE PROSOU
!                    *****************
!
     &(FU,FV,SMH,    UN,VN,HN,GRAV,NORD,
     & FAIR,WINDX,WINDY,VENT,HWIND,CORIOL,FCOR,
     & SPHERI,YASMH,COSLAT,SINLAT,AT,LT,
     & NREJET,NREJEU,DSCE,ISCE,T1,MESH,MSK,MASKEL,
     & MAREE,MARDAT,MARTIM,PHI0,OPTSOU,COUROU,NPTH,VARCL,NVARCL,VARCLA,
     & UNSV2D,FXWAVE,FYWAVE)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE SOURCE TERMS IN THE CONTINUITY EQUATION
!+                AND IN THE DYNAMIC EQUATIONS. ARE TAKEN INTO ACCOUNT :
!+
!+              - WIND
!+
!+              - CORIOLIS FORCE
!+
!+              - TIDAL FORCE
!+
!+              - SOURCES AND SINKS
!code
!+    RESPECTIVE TERMS ARE:
!+    ==========================
!+
!+     * WIND
!+       ---------
!+                                 1                         2      2
!+                FU           =  --- * F    * U    * SQRT( U    + V   )
!+                  VENT           H     AIR    AIR          AIR    AIR
!+
!+                                 1                         2      2
!+                FV           =  --- * F    * V    * SQRT( U    + V   )
!+                  VENT           H     AIR    AIR          AIR    AIR
!+
!+           WHERE :
!+                  UAIR   :  WIND VELOCITY ALONG X
!+                  VAIR   :  WIND VELOCITY ALONG Y
!+                  FAIR   :  AIR FRICTION COEFFICIENT
!+
!+     * CORIOLIS FORCE
!+       ---------------------
!+
!+                FU           =  + FCOR * V
!+                  CORIOLIS
!+
!+                FV           =  - FCOR * U
!+                  CORIOLIS
!+
!+           WHERE :
!+                  U       :  FLOW VELOCITY ALONG X
!+                  V       :  FLOW VELOCITY ALONG Y
!+                  FCOR    :  CORIOLIS PARAMETER
!
!note     BOTTOM FRICTION IS TAKEN INTO ACCOUNT IN THE PROPAGATION
!+         THROUGH CALL TO FROTXY, IT IS SEMI-IMPLICIT.
!note  IF SOURCES OR SINKS TERMS ARE ADDED TO THE CONTINUITY EQUATION,
!+         IT IS IDENTIFIED WITH VARIABLE YASMH (SET TO TRUE).
!note  SOURCE TERMS FU AND FV ARE FIRST COMPUTED IN P1.
!+         THEY ARE THEN EXTENDED TO QUASI-BUBBLE IF REQUIRED.
!
!history  J-M HERVOUET (LNHE)
!+        08/04/2008
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
!| AT             |-->| TIME
!| CORIOL         |-->| IF YES, CORIOLIS FORCE
!| COSLAT         |-->| COSINUS OF LATITUDE (SPHERICAL COORDINATES)
!| COUROU         |-->| IF YES, WAVE DRIVEN CURRENTS TAKEN INTO ACCOUNT
!| DSCE           |-->| DISCHARGE OF POINT SOURCES
!| FAIR           |-->| FRICTION COEFFICIENT FOR WIND
!| FCOR           |-->| CORIOLIS PARAMETER
!| FU             |<->| SOURCE TERMS ON VELOCITY U
!| FV             |<->| SOURCE TERMS ON VELOCITY V
!| FXWAVE         |<->| FORCING OF WAVES ALONG X
!| FYWAVE         |<->| FORCING OF WAVES ALONG Y
!| GRAV           |-->| GRAVITY
!| HN             |-->| DEPTH AT TIME T(N)
!| HWIND          |-->| MINIMUM DEPTH FOR TAKING WIND INTO ACCOUNT
!| ISCE           |-->| NEAREST POINTS TO SOURCES
!| LT             |-->| TIME STEP NUMBER
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MAREE          |-->| IF YES, TAKES THE TIDAL FORCE INTO ACCOUNT
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NORD           |-->| DIRECTION OF NORTH WITH RESPECT TO Y AXIS
!|                |   | (TRIGONOMETRIC SENSE) IN DEGREES.
!| NPTH           |-->| RECORD NUMBER IN THE WAVE CURRENTS FILE
!| NREJET         |-->| NUMBER OF POINT SOURCES
!| NREJEU         |-->| NUMBER OF POINT SOURCES WITH GIVEN VELOCITY
!|                |   | IF NREJEU=0 VELOCITY OF SOURCES IS TAKEN EQUAL
!|                |   | TO VELOCITY.
!| NVARCL         |-->| NUMBER OF CLANDESTINE VARIABLES
!| OPTSOU         |-->| OPTION FOR THE TREATMENT OF SOURCES
!| PHI0           |-->| LATITUDE OF ORIGIN POINT
!| SINLAT         |-->| SINUS OF LATITUDE (SPHERICAL COORDINATES)
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SPHERI         |-->| IF TRUE : SPHERICAL COORDINATES
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| VARCL          |<->| BLOCK OF CLANDESTINE VARIABLES
!| VARCLA         |-->| NAMES OF CLANDESTINE VARIABLES
!| VENT           |-->| IF YES, WIND IS TAKEN INTO ACCOUNT
!| WINDX          |-->| FIRST COMPONENT OF WIND VELOCITY
!| WINDY          |-->| SECOND COMPONENT OF WIND VELOCITY
!| YASMH          |<->| IF TRUE SMH IS TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DBI1
      USE INTERFACE_TELEMAC2D, EX_PROSOU => PROSOU
! --- JP RENAUD START ---
      USE M_COUPLING_ESTEL3D
! --- JP RENAUD END ---
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     WORKING ARRAYS
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1
!
!-----------------------------------------------------------------------
!
!     VECTORS
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FU,FV,SMH,FXWAVE,FYWAVE
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,UN,VN,HN,UNSV2D
      TYPE(BIEF_OBJ), INTENT(IN)    :: WINDX,WINDY,COSLAT,SINLAT
!
!-----------------------------------------------------------------------
!
!     MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)           :: NVARCL,LT,NREJET,NREJEU,OPTSOU
      INTEGER, INTENT(IN)           :: NPTH
      INTEGER, INTENT(IN)           :: MARDAT(3),MARTIM(3),ISCE(NREJET)
      DOUBLE PRECISION, INTENT(IN)  :: HWIND,AT,FAIR,FCOR,DSCE(NREJET)
      DOUBLE PRECISION, INTENT(IN)  :: GRAV,NORD,PHI0
      CHARACTER(LEN=32), INTENT(IN) :: VARCLA(NVARCL)
      LOGICAL, INTENT(IN)           :: VENT,MAREE,CORIOL,SPHERI,MSK
      LOGICAL, INTENT(IN)           :: COUROU
      LOGICAL, INTENT(INOUT)        :: YASMH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARCL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I,IELMU,IELMH,IELM1,NPOIN,IR,ERR,NP
!
      DOUBLE PRECISION PI,WROT,WD,ATH
!
      CHARACTER*16 NOMX,NOMY
      LOGICAL DEJALU,OKX,OKY,OKC
      DATA DEJALU /.FALSE./
      REAL, ALLOCATABLE :: W(:)
      SAVE W
!
      INTRINSIC SQRT,MAX,ACOS
!
!-----------------------------------------------------------------------
!  EXTRACTS X COORDINATES, NUMBER OF POINTS P1
!                          AND P1 ELEMENT OF THE MESH
!-----------------------------------------------------------------------
!
      IELM1 = MESH%X%ELM
      NPOIN = MESH%NPOIN
!
!-----------------------------------------------------------------------
!  INITIALISES
!-----------------------------------------------------------------------
!
      CALL CPSTVC(UN,FU)
      CALL CPSTVC(VN,FV)
      CALL OS( 'X=0     ' , X=FU )
      CALL OS( 'X=0     ' , X=FV )
!
!-----------------------------------------------------------------------
!
!  COMPUTATION WITH WIND
!  ----------------
!
!                               1                         2     2
!              FU           =  --- * F    * U    * SQRT( U   + V    )
!                VENT           H     AIR    AIR          AIR   AIR
!
!
!                               1                         2     2
!              FV           =  --- * F    * V    * SQRT( U   + V    )
!                VENT           H     AIR    AIR          AIR   AIR
!
!
      IF(VENT) THEN
!
!  TEMPORARY TREATMENT OF TIDAL FLATS
!  THE WIND EFFECT IS ONLY CONSIDERED IF THE WATER DEPTH IS
!  GREATER THAN 1 M.
!
!  ASSUMES HERE THAT THE WIND IS GIVEN IN P1
!
        DO 10 N=1,NPOIN
          IF (HN%R(N).GT.HWIND) THEN
            WD = SQRT( WINDX%R(N)**2 + WINDY%R(N)**2 )
            FU%R(N) = FU%R(N) + FAIR * WINDX%R(N) * WD / HN%R(N)
            FV%R(N) = FV%R(N) + FAIR * WINDY%R(N) * WD / HN%R(N)
          ENDIF
10      CONTINUE
!
      ENDIF
!
!***********************************************************************
!
!     * WITH CORIOLIS FORCE
!       --------------------------
!
!                FU           =  + FCOR * V
!                  CORIOLIS
!
!                FV           =  - FCOR * U
!                  CORIOLIS
!
      IF(CORIOL) THEN
!
      PI = ACOS(-1.D0)
!
        IF(SPHERI) THEN
!
          WROT = 2 * PI / 86164.D0
          DO 20 I=1,NPOIN
!           FORMULATION INDEPENDENT OF THE DIRECTION OF NORTH
            FU%R(I) = FU%R(I) + VN%R(I) * 2 * WROT * SINLAT%R(I)
            FV%R(I) = FV%R(I) - UN%R(I) * 2 * WROT * SINLAT%R(I)
20        CONTINUE
!
!         TAKES THE TIDAL FORCE INTO ACCOUNT
!
          IF(MAREE) THEN
            CALL MARAST(MARDAT,MARTIM,PHI0,NPOIN,AT,
     &                  FU%R,FV%R,MESH%X%R,SINLAT%R,COSLAT%R,GRAV)
          ENDIF
!
          IF(LT.EQ.1) THEN
            IF(LNG.EQ.1) WRITE(LU,11)
            IF(LNG.EQ.2) WRITE(LU,12)
          ENDIF
11        FORMAT(1X,'PROSOU : EN COORDONNEES SHERIQUES, LE',/,
     &           1X,'         COEFFICIENT DE CORIOLIS EST',/,
     &           1X,'         CALCULE EN FONCTION DE LA LATITUDE.',/,
     &           1X,'         LE MOT-CLE ''COEFFICIENT DE CORIOLIS''',/,
     &           1X,'         N''EST DONC PAS PRIS EN COMPTE.')
12        FORMAT(1X,'PROSOU : IN SPHERICAL COORDINATES, THE CORIOLIS',/,
     &           1X,'         PARAMETER DEPENDS ON THE LATITUDE.',/,
     &           1X,'         THE KEY WORD ''CORIOLIS COEFFICIENT''',/,
     &           1X,'         IS CONSEQUENTLY IGNORED.')
!
        ELSE
!
          CALL OS( 'X=X+CY  ' , FU , VN , VN ,  FCOR )
          CALL OS( 'X=X+CY  ' , FV , UN , UN , -FCOR )
!
          IF(LT.EQ.1) THEN
            IF(LNG.EQ.1) WRITE(LU,21)
            IF(LNG.EQ.2) WRITE(LU,22)
          ENDIF
21        FORMAT(1X,'PROSOU : EN COORDONNEES CARTESIENNES, LE',/,
     &           1X,'         COEFFICIENT DE CORIOLIS EST LU DANS LE',/,
     &           1X,'         FICHIER DES PARAMETRES ET CORRESPOND',/,
     &           1X,'         AU MOT-CLE ''COEFFICIENT DE CORIOLIS''',/,
     &           1X,'         IL EST ALORS CONSTANT EN ESPACE')
22        FORMAT(1X,'PROSOU : IN CARTESIAN COORDINATES, THE CORIOLIS',/,
     &           1X,'         PARAMETER IS READ IN THE STEERING FILE',/,
     &           1X,'         IT IS THE KEY WORD ''CORIOLIS',/,
     &           1X,'         COEFFICIENT'', IT IS UNIFORM IN SPACE')
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  THE SECOND MEMBERS ARE PROPERLY DISCRETISED
!
      IELMU=UN%ELM
!
      IF(IELMU.NE.IELM1) THEN
        CALL CHGDIS(FU,IELM1,IELMU,MESH)
        CALL CHGDIS(FV,IELM1,IELMU,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IELMH=HN%ELM
      CALL CPSTVC(HN,SMH)
      CALL OS( 'X=0     ' , X=SMH )
!
      YASMH = .FALSE.
!
      IF(NREJET.NE.0) THEN
!
!  YASMH BECOMES TRUE
!
      YASMH = .TRUE.
!
!  SOURCE TERMS IN THE CONTINUITY EQUATION
!           AND IN THE MOMENTUM EQUATION:
!
!  BEWARE, SMH IS ALSO USED FOR TRACER
!
!     COMPUTES THE VOLUME OF THE BASES
!     HN HERE IS A DUMMY STRUCTURE
      CALL VECTOR(T1,'=','MASBAS          ',IELMH,
     &            1.D0,HN,HN,HN,HN,HN,HN,MESH,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
!
      DO I = 1 , NREJET
!
        IR = ISCE(I)
!       THE TEST IS USEFUL IN PARALLEL MODE, WHEN THE POINT SOURCE
!       IS NOT IN THE SUB-DOMAIN
        IF(IR.GT.0) THEN
         IF(OPTSOU.EQ.1) THEN
!          "NORMAL" VERSION
           SMH%R(IR)=SMH%R(IR)+DSCE(I)/T1%R(IR)
         ELSE
!          "DIRAC" VERSION
           SMH%R(IR) = SMH%R(IR)+DSCE(I)
         ENDIF
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
! EXPLICIT TREATMENT OF MOMENTUM CONTRIBUTIONS TO THE SOURCES
!
      IF(NREJEU.GT.0) THEN
!
      DO I = 1 , NREJEU
!
        IR = ISCE(I)
!       THE TEST IS USEFUL IN PARALLEL MODE, WHEN THE POINT SOURCE
!       IS NOT IN THE SUB-DOMAIN
        IF(IR.GT.0) THEN
!       MOMENTUM ADDED BY THE SOURCE
!      -MOMENTUM TAKEN BY THE SOURCE
        FU%R(IR)=FU%R(IR) + (VUSCE(AT,I)-UN%R(IR))*
     &  DSCE(I)/(T1%R(IR)*MAX(HN%R(IR),0.1D0))
        FV%R(IR)=FV%R(IR) + (VVSCE(AT,I)-VN%R(IR))*
     &  DSCE(I)/(T1%R(IR)*MAX(HN%R(IR),0.1D0))
        ENDIF
!
      ENDDO
!
      ENDIF
!
      ENDIF
!
!***********************************************************************
!
!     * WITH WAVE DRIVEN CURRENTS
!       -------------------------------------
!
!                FU        =  FXWAVE
!                  COUROU
!
!                FV        =  FYWAVE
!                  COUROU
!
!       FXWAVE AND FYWAVE ARE TAKEN IN A RESULTS FILE FROM
!       ARTEMIS OR TOMAWAC
!
!       BEWARE   : 1. MESHES MUST BE THE SAME
!       ---------
!
!                  2. STATIONARY FORCING
!
      IF(COUROU) THEN
!
!        WITH NO COUPLING, TAKING THE WAVE STRESSES ONCE FOR ALL
!        IN A BINARY DATA FILE
!
         IF(.NOT.DEJALU.AND..NOT.INCLUS(COUPLING,'TOMAWAC')) THEN
!
            ALLOCATE(W(NPOIN),STAT=ERR)
            IF(ERR.NE.0) THEN
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'ERREUR D''ALLOCATION DE W DANS PROSOU'
              ENDIF
              IF(LNG.EQ.2) THEN
                WRITE(LU,*) 'MEMORY ALLOCATION ERROR OF W IN PROSOU'
              ENDIF
            ENDIF
!
!           NBI1 : BINARY DATA FILE 1
            NOMX='FORCE FX        '
            NOMY='FORCE FY        '
            CALL FIND_IN_SEL(FXWAVE,NOMX,T2D_FILES(T2DBI1)%LU,
     &                       W,OKX,NPTH,NP,ATH)
            CALL FIND_IN_SEL(FYWAVE,NOMY,T2D_FILES(T2DBI1)%LU,
     &                       W,OKY,NPTH,NP,ATH)
            IF(.NOT.OKX.OR..NOT.OKY) THEN
!             SECOND TRY (OLD VERSIONS OF ARTEMIS OR TOMAWAC)
              NOMX='FORCE_FX        '
              NOMY='FORCE_FY        '
              CALL FIND_IN_SEL(FXWAVE,NOMX,T2D_FILES(T2DBI1)%LU,
     &                         W,OKX,NPTH,NP,ATH)
              CALL FIND_IN_SEL(FYWAVE,NOMY,T2D_FILES(T2DBI1)%LU,
     &                         W,OKY,NPTH,NP,ATH)
            ENDIF
!           CLANDESTINE VARIABLES FROM TOMAWAC TO SISYPHE
            IF(NVARCL.GT.0) THEN
              DO I=1,NVARCL
              CALL FIND_IN_SEL(VARCL%ADR(I)%P,VARCLA(I)(1:16),
     &                         T2D_FILES(T2DBI1)%LU,
     &                         W,OKC,NPTH,NP,ATH)
              IF(.NOT.OKC) THEN
                IF(LNG.EQ.1) WRITE(LU,7) VARCLA(I)(1:16)
                IF(LNG.EQ.2) WRITE(LU,8) VARCLA(I)(1:16)
7             FORMAT(1X,'PROSOU : VARIABLE CLANDESTINE :',/,1X,A16,/,1X,
     &                  '         NON TROUVEE',/,1X,
     &                  '         DANS LE FICHIER DE HOULE')
8             FORMAT(1X,'PROSOU : CLANDESTINE VARIABLE:',/,1X,A16,/,1X,
     &                  '         NOT FOUND',/,1X,
     &                  '         IN THE WAVE RESULTS FILE')
              CALL PLANTE(1)
              STOP
              ENDIF
              ENDDO
            ENDIF
!
            IF(.NOT.OKX.OR..NOT.OKY) THEN
              IF(LNG.EQ.1) WRITE(LU,5)
              IF(LNG.EQ.2) WRITE(LU,6)
5             FORMAT(1X,'PROSOU : FORCE FX OU FY NON TROUVES',/,1X,
     &                  '         DANS LE FICHIER DE HOULE')
6             FORMAT(1X,'PROSOU: FORCE FX OR FY NOT FOUND',/,1X,
     &                  '         IN THE WAVE RESULTS FILE')
              CALL PLANTE(1)
              STOP
            ENDIF
            IF(NP.NE.NPOIN) THEN
              IF(LNG.EQ.1) WRITE(LU,95)
              IF(LNG.EQ.2) WRITE(LU,96)
 95           FORMAT(1X,'PROSOU : SIMULATION DES COURANTS DE HOULE.',/,
     &               1X,'LES MAILLAGES HOULE ET COURANTS SONT ',/,
     &               1X,'DIFFERENTS : PAS POSSIBLE POUR LE MOMENT.')
 96           FORMAT(1X,'PROSOU: WAVE DRIVEN CURRENTS MODELLING.',/,
     &               1X,'WAVE AND CURRENT MODELS MESHES ARE ',/,
     &               1X,'DIFFERENT : NOT POSSIBLE AT THE MOMENT.')
!
              CALL PLANTE(1)
              STOP
            ENDIF
!           WRITES OUT TO THE LISTING
            IF(LNG.EQ.1) WRITE(LU,115) ATH
            IF(LNG.EQ.2) WRITE(LU,116) ATH
115         FORMAT(1X,/,1X,'PROSOU : COURANTS DE HOULE',/,
     &                  1X,'         LECTURE AU TEMPS ',F10.3,/)
116         FORMAT(1X,/,1X,'PROSOU: WAVE DRIVEN CURRENTS MODELLING',/,
     &                  1X,'         READING FILE AT TIME ',F10.3,/)
            IF(IELMU.NE.IELM1) THEN
              CALL CHGDIS(FXWAVE,IELM1,IELMU,MESH)
              CALL CHGDIS(FYWAVE,IELM1,IELMU,MESH)
            ENDIF
            DEJALU = .TRUE.
!
         ENDIF
!
!        ADDS INTO FU AND FV
!
         IF(INCLUS(COUPLING,'TOMAWAC')) THEN
           IF(IELMU.NE.IELM1) THEN
             CALL CHGDIS(FXWAVE,IELM1,IELMU,MESH)
             CALL CHGDIS(FYWAVE,IELM1,IELMU,MESH)
           ENDIF
         ENDIF
         CALL OS('X=X+Y   ',X=FU,Y=FXWAVE)
         CALL OS('X=X+Y   ',X=FV,Y=FYWAVE)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!
!  TAKES SEEPAGE IN THE SOIL INTO ACCOUNT
!  COMMUNICATES WITH ESTEL-3D
!
!     GETS SOURCE TERM FROM ESTEL-3D TO ACCOUNT FOR SEEPAGE
!     CALLS THE INFILTRATION ROUTINE
!
      CALL INFILTRATION_GET(SMH%R,UNSV2D%R,YASMH)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

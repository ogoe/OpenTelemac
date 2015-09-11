!                    ***************************
                     MODULE DECLARATIONS_TOMAWAC
!                    ***************************
!
!
!***********************************************************************
! TOMAWAC   V7P0                                  25/06/2012
!***********************************************************************
!
!brief    DECLARES BIEF STRUCTURES IN TOMAWAC.
!
!history  OPTIMER
!+        14/06/00
!+        V5P2
!+   M. BENOIT / J.M HERVOUET, LEADS FOR EDF
!
!history  OPTIMER
!+        25/08/00
!+        V6P0
!+   D. VIOLEAU, LEAD FOR EDF
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
!history  G.MATTAROLO (EDF)
!+        16/05/2011
!+        V6P1
!+   Declaration of new variables defined by
!+       E. GAGNAIRE-RENOU for solving new source terms models.
!
!history  G.MATTAROLO (EDF)
!+        25/06/2012
!+        V6P2
!+   Declaration of new variables for representing diffraction
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        24/12/2013
!+        V7P0
!+   Logical variable RAZTIM added (initial time set to zero).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     DECLARES BIEF STRUCTURES
!
!     variables 4d a convecter
!
      TYPE (BIEF_OBJ), TARGET :: SF
!
!     coefficient b pour convection
!
      TYPE (BIEF_OBJ), TARGET :: SB
!
!     tableaux des frequences de discretisation
!
      TYPE (BIEF_OBJ), TARGET :: SFR
!
!     tableaux des pas de frequence
!
      TYPE (BIEF_OBJ), TARGET :: SDFR
!
!     tableaux pour les interactions non lineaires
!
      TYPE (BIEF_OBJ), TARGET :: SCOEF
!
!     tableau des pieds des caracteristiques
!
      TYPE (BIEF_OBJ), TARGET :: SSHP1
!
!     tableau des pieds des caracteristiques
!
      TYPE (BIEF_OBJ), TARGET :: SSHZ
!
      TYPE (BIEF_OBJ), TARGET ::SCT,SCF,SSHF,SXK,SCG ,
     & SZF,SDEPTH,SUC,SVC,SUC1,SVC1,SUC2,SVC2,SDZHDT,SDZX ,
     & SDZY,SDF_LIM,SUV,SVV,SUV1,SVV1,SUV2,SVV2,SZM1,SZM2,
     & SCOSF,STGF,SDUX,SDUY,SDVX,SDVY,STETA,SCOSTE,SSINTE,SSURDE,
     & SFBOR,AM1,STSDER,STSTOT,ST0,ST1,ST2,ST3,ST4,BST1,VARSOR,STRA40,
     & BOUNDARY_COLOUR
!
      TYPE (BIEF_OBJ), TARGET :: STRA01, STOLD , STNEW,STRA31,
     & STRA32, STRA33, STRA34, STRA35, STRA36, STRA37, STRA38, STRA39,
     & STRA41, STRA42, STRA43, STRA44, STRA51, STRA52, STRA53, STRA54,
     & STRA55, STRA56, STRA57, STRA58, STRA59, STRA60, STRA61, STRA62,
     & STRA63, STRA64, STRA65, STRA66, SPRIVE, SIBOR ,
     & SLIFBR, SLIQ, SELT, SETA, SFRE, SETAP1, SIAGNL, SITR11, SITR12,
     & SITR13, SITR01, SITR31,SITR32, SITR33, SBETA,SISUB
!
!     BLOCK OF BIEF_OBJ STRUCTURES
!
      TYPE(BIEF_OBJ) :: TB
!
!     EXTENSION OF IKLE2 (WITH LARGER NUMBER OF ELEMENTS)
!
      TYPE(BIEF_OBJ) :: IKLE_EXT
!
!     BIEF_OBJ STRUCTURES FOR ARRAYS OF DIMENSION NPOIN3
!
      TYPE(BIEF_OBJ), POINTER :: T3_01,T3_02,T3_03,T3_04,T3_05,T3_06
      TYPE(BIEF_OBJ), POINTER :: T3_07,T3_08,T3_09,T3_10
!
!     MESH STRUCTURES FOR 2D AND 3D
!
      TYPE(BIEF_MESH) :: MESH,MESH3D
!
!     DIFFRACTION
!
      TYPE(BIEF_OBJ), TARGET :: SA, SA_RMSE, SCCG, SDELTA, SDDX, SDDY
      TYPE(BIEF_OBJ), TARGET :: SDIV, SNB_CLOSE, SNEIGB
      TYPE(BIEF_OBJ), TARGET :: SRK, SRX, SRXX, SRY, SRYY, SXKONPT
!
!-----------------------------------------------------------------------
!     KEYWORDS AND PARAMETERS
!-----------------------------------------------------------------------
!
!     NUMBER OF DISCRETISED DIRECTIONS
!
      INTEGER NPLAN
!
!     NUMBER OF DISCRETISED FREQUENCIES
!
      INTEGER NF
!
!     PERIOD FOR LISTING PRINTOUTS
!
      INTEGER LISPRD
!
!     PERIOD FOR GRAPHIC PRINTOUTS
!
      INTEGER GRAPRD
!
!     NUMBER OF TIME STEP
!
      INTEGER NIT
!
!     TIDE REFRESHING PERIOD
!
      INTEGER LAM
!
!     RANK OF THE WATER LEVEL DATA IN THE TELEMAC FILE
!
      INTEGER IDHMA
!
!     NUMBER OF FIRST ITERATION FOR GRAPHICS PRINTOUTS
!
      INTEGER GRADEB
!
!     NUMBER OF BOTTOM SMOOTHINGS
!
      INTEGER LISFON
!
!     WIND GENERATION
!
      INTEGER SVENT
!
!     WHITECAPPING DISSIPATION
!
      INTEGER SMOUT
!
!     BOTTOM FRICTION DISSIPATION
!
      INTEGER SFROT
!
!     NON-LINEAR TRANSFERS BETWEEN FREQUENCIES
!
      INTEGER STRIF
!
!     DEPTH-INDUCED BREAKING DISSIPATION
!
      INTEGER SBREK
!
!     DEPTH-INDUCED BREAKING 1 (BJ) QB COMPUTATION METHOD
!
      INTEGER IQBBJ
!
!     DEPTH-INDUCED BREAKING 1 (BJ) HM COMPUTATION METHOD
!
      INTEGER IHMBJ
!
!     DEPTH-INDUCED BREAKING 1 (BJ) CHARACTERISTIC FREQUENCY
!
      INTEGER IFRBJ
!
!     DEPTH-INDUCED BREAKING 2 (TG) CHARACTERISTIC FREQUENCY
!
      INTEGER IFRTG
!
!     DEPTH-INDUCED BREAKING 2 (TG) WEIGHTING FUNCTION
!
      INTEGER IWHTG
!
!     DEPTH-INDUCED BREAKING 3 (RO) WAVE HEIGHT DISTRIBUTION
!
      INTEGER IDISRO
!
!     DEPTH-INDUCED BREAKING 3 (RO) EXPONENT WEIGHTING FUNCTION
!
      INTEGER IEXPRO
!
!     DEPTH-INDUCED BREAKING 3 (RO) CHARACTERISTIC FREQUENCY
!
      INTEGER IFRRO
!
!     DEPTH-INDUCED BREAKING 4 (IH) CHARACTERISTIC FREQUENCY
!
      INTEGER IFRIH
!
!     NUMBER OF BREAKING TIME STEPS
!
      INTEGER NDTBRK
!
!     WAVE GROWTH LIMITER
!
      INTEGER LIMIT
!
!     CURRENTS FILE FORMAT
!
      INTEGER INDIC
!
!     WINDS FILE FORMAT
!
      INTEGER INDIV
!
!     TIDAL WATER LEVEL FILE FORMAT
!
      INTEGER INDIM
!
!     TYPE OF INITIAL DIRECTIONAL SPECTRUM
!
      INTEGER INISPE
!
!     TYPE OF BOUNDARY DIRECTIONAL SPECTRUM
!
      INTEGER LIMSPE
!
!     TRIAD INTERACTIONS
!
      INTEGER STRIA
!
!     NUMBER OF ITERATIONS FOR THE SOURCE TERMS
!
      INTEGER NSITS
!
!     TIME INCREMENT NUMBER IN TELEMAC FILE
!
      INTEGER NPTT
!
!     VECTOR LENGTH
!
      INTEGER LVMAC
!
!     NUMBER OF PRIVATE ARRAYS, NUMBER OF PRIVATE ARRAYS WITH GIVEN NAME
!
      INTEGER NPRIV,N_NAMES_PRIV
!
!     INITIAL ANGULAR DISTRIBUTION FUNCTION
!
      INTEGER FRABI
!
!     BOUNDARY ANGULAR DISTRIBUTION FUNCTION
!
      INTEGER FRABL
!
!     ??????
!
      INTEGER NPLEO
!
!     DEBUGGER
!
      INTEGER DEBUG
!
!     ?????
!
      INTEGER SORG2D
!
!     COORDINATES OF THE ORIGIN IN (X, Y)
!
      INTEGER I_ORIG,J_ORIG
!
!     LINEAR WAVE GROWTH
!
      INTEGER LVENT
!
!     SETTING FOR INTEGRATION ON OMEGA1
!
      INTEGER IQ_OM1
!
!     SETTING FOR INTEGRATION ON THETA1
!
      INTEGER NQ_TE1
!
!     SETTING FOR INTEGRATION ON OMEGA2
!
      INTEGER NQ_OM2
!
!     DIFFRACTION
!
      INTEGER DIFFRA
!
!     STARTING TIME STEP FOR DIFFRACTION
!
      INTEGER NPTDIF
!
!     DIAGNOSTIC TAIL OPTION
!
      INTEGER DIAGHF
!
!     OPTION FOR SECOND DERIVATIVES
!
      INTEGER OPTDER
!
!     DISSIPATION BY STRONG CURRENT
!
      INTEGER SDSCU
!
!     MODELISATION CBAJ
!
      INTEGER CBAJ
!
!     TIME STEP
!
      DOUBLE PRECISION DT
!
!     MINIMAL FREQUENCY
!
      DOUBLE PRECISION F1
!
!     FREQUENTIAL RATIO
!
      DOUBLE PRECISION RAISF
!
!     DATE OF COMPUTATION BEGINNING
!
      DOUBLE PRECISION DDC
!
!     BOTTOM FRICTION COEFFICIENT
!
      DOUBLE PRECISION CFROT1
!
!     WHITE CAPPING DISSIPATION COEFFICIENT
!
      DOUBLE PRECISION CMOUT1
!
!     WHITE CAPPING WEIGHTING COEFFICIENT
!
      DOUBLE PRECISION CMOUT2
!
!     AIR DENSITY
!
      DOUBLE PRECISION ROAIR
!
!     WATER DENSITY
!
      DOUBLE PRECISION ROEAU
!
!     WIND GENERATION COEFFICIENT
!
      DOUBLE PRECISION BETAM
!
!     VON KARMAN CONSTANT
!
      DOUBLE PRECISION XKAPPA
!
!     CHARNOCK CONSTANT
!
      DOUBLE PRECISION ALPHA
!
!     SHIFT GROWING CURVE DUE TO WIND
!
      DOUBLE PRECISION DECAL
!
!     ELEVATION FOR WIND MEASUREMENTS
!
      DOUBLE PRECISION ZVENT
!
!     WIND DRAG COEFFICIENT
!
      DOUBLE PRECISION CDRAG
!
!     DEPTH-INDUCED BREAKING 1 (BJ) COEFFICIENT ALPHA
!
      DOUBLE PRECISION ALFABJ
!
!     DEPTH-INDUCED BREAKING 1 (BJ) COEFFICIENT GAMMA1
!
      DOUBLE PRECISION GAMBJ1
!
!     DEPTH-INDUCED BREAKING 1 (BJ) COEFFICIENT GAMMA2
!
      DOUBLE PRECISION GAMBJ2
!
!     DEPTH-INDUCED BREAKING 2 (TG) COEFFICIENT B
!
      DOUBLE PRECISION BORETG
!
!     DEPTH-INDUCED BREAKING 2 (TG) COEFFICIENT GAMMA
!
      DOUBLE PRECISION GAMATG
!
!     DEPTH-INDUCED BREAKING 3 (RO) COEFFICIENT ALPHA
!
      DOUBLE PRECISION ALFARO
!
!     DEPTH-INDUCED BREAKING 3 (RO) COEFFICIENT GAMMA
!
      DOUBLE PRECISION GAMARO
!
!     DEPTH-INDUCED BREAKING 3 (RO) COEFFICIENT GAMMA2
!
      DOUBLE PRECISION GAM2RO
!
!     DEPTH-INDUCED BREAKING 4 (IH) COEFFICIENT BETA0
!
      DOUBLE PRECISION BETAIH
!
!     DEPTH-INDUCED BREAKING 4 (IH) COEFFICIENT M2STAR
!
      DOUBLE PRECISION EM2SIH
!
!     MAXIMUM VALUE OF THE RATIO HM0 OVER D
!
      DOUBLE PRECISION COEFHS
!
!     COEFFICIENT OF THE TIME SUB-INCREMENTS FOR BREAKING
!
      DOUBLE PRECISION XDTBRK
!
!     STANDARD CONFIGURATION PARAMETER
!
      DOUBLE PRECISION XLAMD
!
!     IMPLICITATION COEFFICIENT FOR SOURCE TERMS
!
      DOUBLE PRECISION CIMPLI
!
!     INITIAL STILL WATER LEVEL
!
      DOUBLE PRECISION ZREPOS
!
!     TRIADS 1 (LTA) COEFFICIENT ALPHA
!
      DOUBLE PRECISION ALFLTA
!
!     TRIADS 1 (LTA) COEFFICIENT RFMLTA
!
      DOUBLE PRECISION RFMLTA
!
!     TRIADS 2 (SPB) COEFFICIENT K
!
      DOUBLE PRECISION KSPB
!
!     TRIADS 2 (SPB) LOWER DIRECTIONAL BOUNDARY
!
      DOUBLE PRECISION BDISPB
!
!     TRIADS 2 (SPB) UPPER DIRECTIONAL BOUNDARY
!
      DOUBLE PRECISION BDSSPB
!
!     SIGNIFICANT WAVE HEIGHT
!
      DOUBLE PRECISION HM0
!
!     PEAK FREQUENCY
!
      DOUBLE PRECISION FPIC
!
!     PEAK FACTOR
!
      DOUBLE PRECISION GAMMA
!
!     VALUE OF SIGMA-A FOR SPECTRUM
!
      DOUBLE PRECISION SIGMAA
!
!     VALUE OF SIGMA-B FOR SPECTRUM
!
      DOUBLE PRECISION SIGMAB
!
!     PHILLIPS CONSTANT
!
      DOUBLE PRECISION ALPHIL
!
!     MEAN FETCH VALUE
!
      DOUBLE PRECISION FETCH
!
!     MAXIMUM PEAK FREQUENCY
!
      DOUBLE PRECISION FREMAX
!
!     MAIN DIRECTION 1
!
      DOUBLE PRECISION TETA1
!
!     DIRECTIONAL SPREAD 1
!
      DOUBLE PRECISION SPRED1
!
!     MAIN DIRECTION 2
!
      DOUBLE PRECISION TETA2
!
!     DIRECTIONAL SPREAD 2
!
      DOUBLE PRECISION SPRED2
!
!     WEIGHTING FACTOR FOR ADF (DIRECTIONAL SPREADING FUNCTION)
!
      DOUBLE PRECISION XLAMDA
!
!     SPECTRUM TAIL FACTOR
!
      DOUBLE PRECISION TAILF
!
!     SPECTRUM ENERGY THRESHOLD
!
      DOUBLE PRECISION E2FMIN
!
!     BOUNDARY SIGNIFICANT WAVE HEIGHT
!
      DOUBLE PRECISION HM0L
!
!     BOUNDARY PEAK FREQUENCY
!
      DOUBLE PRECISION FPICL
!
!     BOUNDARY SPECTRUM VALUE OF SIGMA-A
!
      DOUBLE PRECISION SIGMAL
!
!     BOUNDARY SPECTRUM VALUE OF SIGMA-B
!
      DOUBLE PRECISION SIGMBL
!
!     BOUNDARY PHILLIPS CONSTANT
!
      DOUBLE PRECISION APHILL
!
!     BOUNDARY MEAN FETCH VALUE
!
      DOUBLE PRECISION FETCHL
!
!     BOUNDARY MAXIMUM PEAK FREQUENCY
!
      DOUBLE PRECISION FPMAXL
!
!     BOUNDARY MAIN DIRECTION 1
!
      DOUBLE PRECISION TETA1L
!
!     BOUNDARY DIRECTIONAL SPREAD 1
!
      DOUBLE PRECISION SPRE1L
!
!     BOUNDARY MAIN DIRECTION 2
!
      DOUBLE PRECISION TETA2L
!
!     BOUNDARY DIRECTIONAL SPREAD 2
!
      DOUBLE PRECISION SPRE2L
!
!     BOUNDARY WEIGHTING FACTOR FOR ADF
!
      DOUBLE PRECISION XLAMDL
!
!     BOUNDARY PEAK FACTOR
!
      DOUBLE PRECISION GAMMAL
!
!     ?????
!
      DOUBLE PRECISION ALF1, GAM1, GAM2
!
!     WIND VELOCITY ALONG X AND Y
!
      DOUBLE PRECISION VX_CTE,VY_CTE
!
!     MINIMUM WATER DEPTH
!
      DOUBLE PRECISION PROMIN
!
!     COORDINATES OF SPECTRUM PRINTOUT POINTS
!
      DOUBLE PRECISION XLEO(99),YLEO(99)
!
!     YAN GENERATION COEFFICIENT D
!
      DOUBLE PRECISION COEFWD
!
!     YAN GENERATION COEFFICIENT E
!
      DOUBLE PRECISION COEFWE
!
!     YAN GENERATION COEFFICIENT F
!
      DOUBLE PRECISION COEFWF
!
!     YAN GENERATION COEFFICIENT H
!
      DOUBLE PRECISION COEFWH
!
!     WESTHUYSEN DISSIPATION COEFFICIENT
!
      DOUBLE PRECISION CMOUT3
!
!     SATURATION THRESHOLD FOR THE DISSIPATION
!
      DOUBLE PRECISION CMOUT4
!
!     WESTHUYSEN WHITE CAPPING DISSIPATION
!
      DOUBLE PRECISION CMOUT5
!
!     WESTHUYSEN WEIGHTING COEFFICIENT
!
      DOUBLE PRECISION CMOUT6
!
!     QNL4 - THRESHOLD0 FOR CONFIGURATIONS ELIMINATION
!
      DOUBLE PRECISION SEUIL
!
!     QNL4 - THRESHOLD1 FOR CONFIGURATIONS ELIMINATION
!
      DOUBLE PRECISION SEUIL1
!
!     QNL4 - THRESHOLD2 FOR CONFIGURATIONS ELIMINATION
!
      DOUBLE PRECISION SEUIL2
!
!     SPECTRUM VARIANCE THRESHOLD FOR DIFFRACTION
!
      DOUBLE PRECISION F2DIFM
!
!     DISSIPATION COEFFICIENT FOR STRONG CURRENT
!
      DOUBLE PRECISION CDSCUR
!
!     CONSIDERATION OF SOURCE TERMS
!
      LOGICAL TSOU
!
!     SPHERICAL COORDINATES
!
      LOGICAL SPHE
!
!     GLOBAL OUTPUT AT THE END
!
      LOGICAL GLOB
!
!     NEXT COMPUTATION
!
      LOGICAL SUIT
!
!     INFINITE DEPTH
!
      LOGICAL PROINF
!
!     CONSIDERATION OF A CURRENT
!
      LOGICAL COURAN
!
!     CONSIDERATION OF A WIND
!
      LOGICAL VENT
!
!     CONSIDERATION OF A STATIONARY CURRENT
!
      LOGICAL COUSTA
!
!     CONSIDERATION OF A STATIONARY WIND
!
      LOGICAL VENSTA
!
!     CONSIDERATION OF TIDE
!
      LOGICAL MAREE
!
!     TRIGONOMETRICAL CONVENTION
!
      LOGICAL TRIGO
!
!     RECOVERY OF TELEMAC DATA ITEM
!
      LOGICAL DONTEL
!
!     CONSIDERATION OF PROPAGATION
!
      LOGICAL PROP
!
!     VALIDATION
!
      LOGICAL VALID
!
!     LIMIT SPECTRUM MODIFIED BY USER
!
      LOGICAL SPEULI
!
!     DIFFRACTION FILTER
!
      LOGICAL FLTDIF
!
!     INITIAL TIME SET TO ZERO
!
      LOGICAL RAZTIM
!
!     VEGETATION TAKEN INTO ACCOUNT
!
      LOGICAL VEGETATION
!
!     TITLE
!
      CHARACTER (LEN=80) :: TITCAS
!
!     VARIABLES FOR 2D GRAPHIC PRINTOUTS
!
      CHARACTER(LEN=72) :: SORT2D
!
!     GEOMETRY FILE BINARY
!
      CHARACTER(LEN=3) BINGEO
!
!     GLOBAL RESULT FILE BINARY
!
      CHARACTER(LEN=3) BINRBI
!
!     2D RESULTS FILE BINARY
!
      CHARACTER(LEN=3) BINRES
!
!     PUNCTUAL RESULTS FILE BINARY
!
      CHARACTER(LEN=3) BINLEO
!
!     PREVIOUS COMPUTATION FILE BINARY
!
      CHARACTER(LEN=3) BINPRE
!
!     CURRENTS FILE BINARY
!
      CHARACTER(LEN=8) BINCOU
!
!     WINDS FILE BINARY
!
      CHARACTER(LEN=8) BINVEN
!
!     TIDAL WATER LEVEL FILE BINARY
!
      CHARACTER(LEN=8) BINMAR
!
!     BINARY FILE 1 BINARY
!
      CHARACTER(LEN=3) BINBI1
!
!     RELEASE
!
      CHARACTER(LEN=4) VERS
!> @brief
      INTEGER NDP
!> @brief
! standard du fichier de geometrie
      INTEGER STDGEO
!
!     EQUATION SOLVED
!
      CHARACTER(LEN=20) EQUA
!
!     NAMES OF PRIVATE ARRAYS (GIVEN BY USER)
!
      CHARACTER(LEN=32) NAMES_PRIVE(4)    
!
!     TYPE OF ELEMENT IN 2D, 3D
!
      INTEGER IELM2,IELM3
!
!     NPOIN2*NPLAN
!
      INTEGER NPOIN3
!
!GM V6P1 - NEW SOURCE TERMS
!> @brief
! declaration for QNL4 - MDIA method
      INTEGER, PARAMETER :: MDIA = 4
!> @brief
! declaration for QNL4 - MDIA method
      INTEGER         , ALLOCATABLE ::  IANMDI(:,:,:)
!> @brief
! declaration for QNL4 - MDIA method
      DOUBLE PRECISION,ALLOCATABLE :: COEMDI(:,:),XMUMDI(:),XLAMDI(:)
!> @brief
! declaration for QNL4 - GQM method
      INTEGER  NCONF , NCONFM , NF1 , NF2 , NT1
!> @brief
! declaration for QNL4 - GQM method
      DOUBLE PRECISION ELIM
!> @brief
! declaration for QNL4 - GQM method
      INTEGER, ALLOCATABLE :: K_IF1(:) , K_1P(:,:) , K_1M(:,:),
     &                        K_IF2 (:,:,:), K_IF3 (:,:,:),
     &                        K_1P2P(:,:,:), K_1P2M(:,:,:),
     &                        K_1P3P(:,:,:), K_1P3M(:,:,:),
     &                        K_1M2P(:,:,:), K_1M2M(:,:,:),
     &                        K_1M3P(:,:,:), K_1M3M(:,:,:),
     &                        IDCONF(:,:)
!> @brief
! declaration for QNL4 - GQM method
      DOUBLE PRECISION, ALLOCATABLE :: TB_V24(:,:,:), TB_V34(:,:,:),
     &                                 TB_TPM(:,:,:), TB_TMP(:,:,:),
     &                                 TB_FAC(:,:,:),
     &                                 TB_V14(:)
!> @brief
! declaration for QNL4 - GQM method
      INTEGER, PARAMETER :: LBUF = 500
!> @brief
! declaration for QNL4 - GQM method
      INTEGER, PARAMETER :: DIMBUF = 2*LBUF+200
!> @brief
! declaration for QNL4 - GQM method
      INTEGER F_POIN(DIMBUF) , T_POIN(DIMBUF)
!> @brief
! declaration for QNL4 - GQM method
      DOUBLE PRECISION F_COEF(DIMBUF), F_PROJ(DIMBUF), TB_SCA(DIMBUF)
!GM Fin
!
!V6P2 Diffraction
!> @brief NAME
! description
      INTEGER, PARAMETER :: MAXNSP = 30
      INTEGER, PARAMETER :: NRD = 30
      INTEGER :: NRK_C
!V6P2 End diffraction
!
!> @brief
      INTEGER, PARAMETER :: MAXVAR = 35
!> @brief
      LOGICAL SORLEO(MAXVAR) , SORIMP(MAXVAR)
!> @brief
      CHARACTER(LEN=32) VARCLA(10)
!> @brief
! nom des variables
      CHARACTER(LEN=32) TEXTE(MAXVAR)
!> @brief
! nom des variables du calcul precedent
      CHARACTER(LEN=32) TEXTPR(MAXVAR)
!     7 VARIABLES HAVE BEEN USED FOR VALIDATION
!          SIGNIFICANT WAVE HEIGHT    HM0       ( 2)
!          MEAN DIRECTION             DMOY      ( 3)
!          DIRECTIONAL SPREADING      SPD       ( 4)
!          DRIVING FORCE ALONG X      FX        (11)
!          DRIVING FORCE ALONG Y      FY        (12)
!          MEAN FREQUENCY FM-10       FMOY      (18)
!          MEAN FREQUENCY FM01        FM01      (19)
!> @brief
!
      DOUBLE PRECISION HIST(1)
!
      INTEGER ALIRE(MAXVAR)
!
      DATA ALIRE /0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0/
      DATA HIST /9999.D0/
!
!-----------------------------------------------------------------------
!   DECLARES POINTERS FOR ALIASES
!   THE TARGETS ARE DEFINED IN POINT
!-----------------------------------------------------------------------
!
!     MESH COORDINATES PER POINT
!
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X,Y
!
!     MESH COORDINATES IN ELEMENTS
!
      DOUBLE PRECISION, DIMENSION(:), POINTER :: XEL,YEL
!> @brief
! inverse du determinant des elements 2d
      DOUBLE PRECISION, DIMENSION(:), POINTER :: SURDET
!> @brief
! correspondance numerotation locale-globale
      INTEGER, DIMENSION(:) , POINTER :: IKLE2
!> @brief
! numeros 2d des elements ayant une face commune avec l'element.
! si ifabor<=0 on a une face liquide,solide,ou periodique
      INTEGER, DIMENSION(:) , POINTER :: IFABOR
!> @brief
! numeros globaux des points frontieres
      INTEGER, DIMENSION(:) , POINTER :: NBOR
!> @brief
! nombre d'elements du maillage 2d
      INTEGER, POINTER:: NELEM2
!> @brief
! nombre de points 'frontiere'
      INTEGER, POINTER:: NPTFR
!> @brief NUMBER OF POINTS IN THE 2D MESH
! nombre de points du maillage 2d
      INTEGER, POINTER:: NPOIN2
!> @brief
! spectre directionnel de variance
      DOUBLE PRECISION, DIMENSION(:) , POINTER :: F
!> @brief
! champ convecteur selon teta
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  CT
!> @brief
! champ convecteur selon freq. relat.
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  CF
!> @brief
! coordonnees barycentriques suivant z des noeuds dans leurs etages "eta" associes
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  SHZ
!> @brief
! coordonnees barycentriques suivant f des noeuds dans leurs frequences "fre" associees
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  SHF
!> @brief
! jacobien passage de n(kx,ky) a f(fr,teta)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  B
!> @brief
! nombres d'onde
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  XK
!> @brief
! vitesses de groupe
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  CG
!> @brief BOTTOM ELEVATION
! cote du fond
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  ZF
!> @brief
! profondeur
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DEPTH
!> @brief
! composantes ouest-est du courant (a t)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  UC
!> @brief
! composantes sud-nord du courant (a t)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  VC
!> @brief
! composantes ouest-est du courant (a t1)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  UC1
!> @brief
! composantes sud-nord du courant (a t1)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  VC1
!> @brief
! composantes ouest-est du courant (a t2)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  UC2
!> @brief
! composantes sud-nord du courant (a t2)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  VC2
!> @brief
! variation temporelle de la profondeur
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DZHDT
!> @brief
! gradient de profondeur par rapport a x
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DZX
!> @brief
! gradient de profondeur par rapport a y
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DZY
!> @brief
! tableau utilise pour le limiteur de croissance
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DF_LIM
!> @brief
! composantes ouest-est du vent (a t)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  UV
!> @brief
! composantes sud-nord du vent (a t)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  VV
!> @brief
! composantes ouest-est du vent (a t1)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  UV1
!> @brief
! composantes sud-nord du vent (a t1)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  VV1
!> @brief
! composantes ouest-est du vent (a t2)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  UV2
!> @brief
! composantes sud-nord du vent (a t2)
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  VV2
!> @brief
! hauteur de la maree par rapport a zrepos a t1
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  ZM1
!> @brief
! hauteur de la maree par rapport a zrepos a t2
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  ZM2
!> @brief
! frequences de discretisation
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  FREQ
!> @brief
! pas de frequence
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DFREQ
!> @brief
! coefficients de calcul pour dia
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  COEFNL
!> @brief
! cosinus des latitudes des points 2d
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  COSF
!> @brief
! tangentes des latitudes des points 2d
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  TGF
!> @brief
! gradient de courant u  par rapport a x
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DUX
!> @brief
! gradient de courant u  par rapport a y
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DUY
!> @brief
! gradient de courant v  par rapport a x
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DVX
!> @brief
! gradient de courant v  par rapport a y
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  DVY
!> @brief
! directions de discretisation
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  TETA
!> @brief
! cosinus des directions
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  COSTET
!> @brief
! sinus des directions
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  SINTET
!> @brief
! densite spectrale au bord
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  FBOR
!> @brief
! contribution terme source - partie derivee
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  TSDER
!> @brief
! contribution terme source - partie totale
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  TSTOT
!> @brief WORKING ARRAY
! tableau de travail structures
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  T0
!> @brief WORKING ARRAY
! tableau de travail structures
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  T1
!> @brief WORKING ARRAY
! tableau de travail structures
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  T2
!> @brief WORKING ARRAY
! tableau de travail structures
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  T3
!> @brief WORKING ARRAY
! tableau de travail structures
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  T4
!> @brief
      DOUBLE PRECISION, DIMENSION(:) , POINTER :: TRA40
!> @brief
! tableau de travail
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  BETA
!> @brief
! tableau de travail
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  TOLD
!> @brief
! tableau de travail
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::  TNEW
!> @brief
!
      DOUBLE PRECISION, DIMENSION(:) ,POINTER :: TRA32,TRA33,
     & TRA34 , TRA35 , TRA36 , TRA37 , TRA38 , TRA39 , TRA41,TRA42,
     & TRA43 , TRA44 , TRA51 , TRA52 , TRA53 , TRA54 , TRA55,TRA56,
     & TRA57 , TRA58 , TRA59 , TRA60 , TRA61 , TRA62 , TRA63,TRA64,
     & TRA65 , TRA66
!> @brief
! tableau de travail
      DOUBLE PRECISION, DIMENSION(:) , POINTER :: TRA01,TRA31
!> @brief
! tableau utilisateur
      DOUBLE PRECISION, DIMENSION(:) , POINTER ::PRIVE
!> @brief
!
      INTEGER, DIMENSION(:), POINTER :: IBOR
!> @brief
! type des conditions a la limite sur f(libre / impose)
      INTEGER, DIMENSION(:), POINTER :: LIFBOR
!> @brief
! numeros des elements 2d choisis pour chaque noeud
      INTEGER, DIMENSION(:), POINTER :: ELT
!> @brief
! numeros des directions/etages choisis pour chaque noeud
      INTEGER, DIMENSION(:), POINTER :: ETA
! numeros des Sous-domaines ou sont les pieds des caracteristiques
      INTEGER, DIMENSION(:), POINTER :: ISUB
!> @brief
! numeros des frequences choisies pour chaque noeud
      INTEGER, DIMENSION(:), POINTER :: FRE
!> @brief
! tableau de travail donnant le numero de l'etage superieur
      INTEGER, DIMENSION(:), POINTER :: ETAP1
!> @brief
! tableau des indices angulaires pour dia
      INTEGER, DIMENSION(:), POINTER :: IANGNL
!> @brief
!
      INTEGER, DIMENSION(:), POINTER :: ITR11 , ITR12 , ITR13
!> @brief
! tableau de travail
      INTEGER, DIMENSION(:), POINTER :: ITR01
!> @brief
!
      INTEGER, DIMENSION(:), POINTER :: ITR31,ITR32,ITR33
!
!     Diffraction
!
      INTEGER, DIMENSION(:), POINTER ::   NB_CLOSE, NEIGB
      DOUBLE PRECISION, DIMENSION(:), POINTER :: A, A_RMSE, CCG, DELTA,
     &                DIV, DDX, DDY, RK, RX, RXX, RY, RYY, XKONPT
!
!     GEOMETRY FILE
!
      INTEGER :: WACGEO
!
!     STEERING FILE
!
      INTEGER :: WACCAS
!
!     BOUNDARY CONDITIONS FILE
!
      INTEGER :: WACCLI
!
!     BOTTOM TOPOGRAPHY FILE
!
      INTEGER :: WACFON
!
!     2D RESULTS FILE
!
      INTEGER :: WACRES
!
!     1D SPECTRA RESULTS FILE
!
      INTEGER :: WACSPE
!
!     PUNCTUAL RESULTS FILE
!
      INTEGER :: WACLEO
!
!     PREVIOUS COMPUTATION FILE
!
      INTEGER :: WACPRE
!
!     GLOBAL RESULT FILE
!
      INTEGER :: WACRBI
!
!     BINARY CURRENTS FILE
!
      INTEGER :: WACCOB
!
!     FORMATTED CURRENTS FILE
!
      INTEGER :: WACCOF
!
!     BINARY FILE 1
!
      INTEGER :: WACBI1
!
!     FORMATTED FILE 1
!
      INTEGER :: WACFO1
!
!     BINARY WINDS FILE
!
      INTEGER :: WACVEB
!
!     FORMATTED WINDS FILE
!
      INTEGER :: WACVEF
!
!     VALIDATION FILE
!
      INTEGER :: WACREF
!
!     BINARY TIDAL WATER LEVEL FILE
!
      INTEGER :: WACMAB
!
!     FORMATTED TIDAL WATER LEVEL FILE
!
      INTEGER :: WACMAF
!
!     MAXIMUM OF LOGICAL UNITS NUMBERS
!
      INTEGER, PARAMETER :: MAXLU_WAC = 44
!
!     BIEF_FILES STRUCTURES
!
      TYPE(BIEF_FILE) :: WAC_FILES(MAXLU_WAC)
!
!     TOMAWAC CONSTANTS (INITIALISED INTO TOMAWAC_CONSTANTS)
!
      DOUBLE PRECISION :: PI,DEUPI,GRAVIT,PISUR2,GRADEG,DEGRAD
      DOUBLE PRECISION :: SR,R2,USDPI
!
!     FOR READING NAMES OF VARIABLES IN SERAFIN DATA FILES
!     30 IS HERE A MAXIMUM OF VARIABLES PER FILE
!
      CHARACTER(LEN=32) :: TEXCOB(30),TEXMAB(30),TEXVEB(30)
!
!     NAMES OF VARIABLES IN SERAFIN DATA FILES (VELOCITY ALONG X AND Y
!                                               WIND ALONG X AND Y
!                                               DEPTH)
!
      CHARACTER(LEN=32) :: NAMEU,NAMEV,NAMEWX,NAMEWY,NAMEH
!
!     UNIT OF TIME IN SERAFIN DATA FILES
!
      DOUBLE PRECISION :: UNITCOB,UNITMAB,UNITVEB
!
!     TIME SHIFT IN SERAFIN DATA FILES
!
      DOUBLE PRECISION :: PHASCOB,PHASMAB,PHASVEB
!
      SAVE
!
      END MODULE DECLARATIONS_TOMAWAC

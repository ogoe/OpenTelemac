!                    ***************************
                     MODULE DECLARATIONS_TOMAWAC
!                    ***************************
!
!
!***********************************************************************
! TOMAWAC   V6P2                                   25/06/2012
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
!
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
     & SITR13, SITR01, SKNI,SKNOGL,SELI, SITR31,
     & SITR32, SITR33, SBETA,SISUB
!
!     BLOCK OF BIEF_OBJ STRUCTURES
!
      TYPE(BIEF_OBJ) :: TB
!
!     EXTENSION OF IKLE2 (WITH LARGER NUMBER OF ELEMENTS)
!
      TYPE(BIEF_OBJ) :: IKLE_EXT
!
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
!> @brief DEPTH-INDUCED BREAKING 1 (BJ) QB COMPUTATION METHOD
! modele deferlement bj : mode calcul de qb
      INTEGER IQBBJ
!> @brief DEPTH-INDUCED BREAKING 1 (BJ) HM COMPUTATION METHOD
! modele deferlement bj : mode calcul de hm
      INTEGER IHMBJ
!> @brief DEPTH-INDUCED BREAKING 1 (BJ) CHARACTERISTIC FREQUENCY
! modele deferlement bj : mode calcul de freq. caracteristique
      INTEGER IFRBJ
!> @brief DEPTH-INDUCED BREAKING 2 (TG) CHARACTERISTIC FREQUENCY
! modele deferlement tg : mode calcul de freq. caracteristique
      INTEGER IFRTG
!> @brief DEPTH-INDUCED BREAKING 2 (TG) WEIGHTING FUNCTION
! modele deferlement tg : mode calcul de w(h)
      INTEGER IWHTG
!> @brief DEPTH-INDUCED BREAKING 3 (RO) WAVE HEIGHT DISTRIBUTION
! modele deferlement ro : distribution houle
      INTEGER IDISRO
!> @brief DEPTH-INDUCED BREAKING 3 (RO) EXPONENT WEIGHTING FUNCTION
! modele deferlement ro : exposant n
      INTEGER IEXPRO
!> @brief DEPTH-INDUCED BREAKING 3 (RO) CHARACTERISTIC FREQUENCY
! modele deferlement ro : mode calcul de freq. caracteristique
      INTEGER IFRRO
!> @brief DEPTH-INDUCED BREAKING 4 (IH) CHARACTERISTIC FREQUENCY
! modele deferlement ih : mode calcul de freq. caracteristique
      INTEGER IFRIH
!> @brief NUMBER OF BREAKING TIME STEPS
! nombre de sous-pas de temps de deferlement
      INTEGER NDTBRK
!> @brief WAVE GROWTH LIMITER
! limiteur de croissance
      INTEGER LIMIT
!> @brief CURRENTS FILE FORMAT
! format du fichier des courants
      INTEGER INDIC
!> @brief WINDS FILE FORMAT
! format du fichier des vents
      INTEGER INDIV
!> @brief TIDAL WATER LEVEL FILE FORMAT
! format du fichier du niveau de la maree
      INTEGER INDIM
!> @brief TYPE OF INITIAL DIRECTIONAL SPECTRUM
! type de spectre directionnel initial
      INTEGER INISPE
!> @brief TYPE OF BOUNDARY DIRECTIONAL SPECTRUM
! type de spectre directionnel aux limites
      INTEGER LIMSPE
!> @brief TRIAD INTERACTIONS
! indicateur de transferts entre triplets de frequences
      INTEGER STRIA
!> @brief NUMBER OF ITERATIONS FOR THE SOURCE TERMS
! nombre de sous-iterations pour les termes sources
      INTEGER NSITS
!> @brief RANK OF THE TELEMAC DATA ITEM TO BE RECOVERED
! rang de la donnee telemac a recuperer
      INTEGER IDTEL
!> @brief TIME INCREMENT NUMBER IN TELEMAC FILE
! numero du pas de temps du fichier telemac
      INTEGER NPTT
!> @brief VECTOR LENGTH
! longueur du vecteur
      INTEGER LVMAC
!> @brief NUMBER OF PRIVATE ARRAYS
! nombre de tableaux prives
      INTEGER NPRIV
!> @brief INITIAL ANGULAR DISTRIBUTION FUNCTION
! fonction de repartition angulaire initiale
      INTEGER FRABI
!> @brief BOUNDARY ANGULAR DISTRIBUTION FUNCTION
! fonction de repartition angulaire aux limites
      INTEGER FRABL
!> @brief
!
      INTEGER NPLEO
!> @brief DEBUGGER
! debugger
      INTEGER DEBUG
!> @brief
! indicateur de sortie des variables 2d
      INTEGER SORG2D
!> @brief COORDINATES OF THE ORIGIN IN (X, Y)
! coordonnes de l'origine
      INTEGER I_ORIG
!> @brief COORDINATES OF THE ORIGIN IN (X, Y)
! coordonnee de l'origine
      INTEGER J_ORIG
!> @brief LINEAR WAVE GROWTH
!
!GM V6P1 - NEW SOURCE TERMS
! croissance lineaire des vagues
      INTEGER LVENT
!> @brief SETTING FOR INTEGRATION ON OMEGA1
! reglage pour integration sur omega1 - QNL4 methode GQM
      INTEGER IQ_OM1
!> @brief SETTING FOR INTEGRATION ON THETA1
! reglage pour integration sur theta1 - QNL4 methode GQM
      INTEGER NQ_TE1
!> @brief SETTING FOR INTEGRATION ON OMEGA2
! Nombre point d integration sur omega2 - QNL4 methode GQM
      INTEGER NQ_OM2
!GM Fin
!V6P2 Diffraction
!> @brief DIFFRACTION
! prise en compte de la diffraction
      INTEGER DIFFRA
!> @brief STARTING TIME STEP FOR DIFFRACTION
! Pas de temps debut diffraction 
      INTEGER NPTDIF
!V6P2 End diffraction
!
!> @brief TIME STEP
! pas de temps
      DOUBLE PRECISION DT
!> @brief MINIMAL FREQUENCY
! frequence minimale (premiere frequence de discretisation)
      DOUBLE PRECISION F1
!> @brief FREQUENTIAL RATIO
! raison frequentielle de discretisation
      DOUBLE PRECISION RAISF
!> @brief DATE OF COMPUTATION BEGINNING
! date de debut du calcul
      DOUBLE PRECISION DDC
!> @brief BOTTOM FRICTION COEFFICIENT
! coefficient pour le terme de frottement sur le fond
      DOUBLE PRECISION CFROT1
!> @brief WHITE CAPPING DISSIPATION COEFFICIENT
! coefficient de dissipation par moutonnement
      DOUBLE PRECISION CMOUT1
!> @brief WHITE CAPPING WEIGHTING COEFFICIENT
! coefficient de ponderation pour le moutonnement
      DOUBLE PRECISION CMOUT2
!> @brief AIR DENSITY
! masse volumique de l'air
      DOUBLE PRECISION ROAIR
!> @brief WATER DENSITY
! masse volumique de l'eau
      DOUBLE PRECISION ROEAU
!> @brief WIND GENERATION COEFFICIENT
! constante betamax de la formule de generation par le vent
      DOUBLE PRECISION BETAM
!> @brief VON KARMAN CONSTANT
! constante de von karman
      DOUBLE PRECISION XKAPPA
!> @brief CHARNOCK CONSTANT
! constante de la loi de charnock
      DOUBLE PRECISION ALPHA
!> @brief SHIFT GROWING CURVE DUE TO WIND
! constante decalage courbe de croissance due au vent
      DOUBLE PRECISION DECAL
!> @brief ELEVATION FOR WIND MEASUREMENTS
! cote a laquelle le vent est mesure (m)
      DOUBLE PRECISION ZVENT
!> @brief WIND DRAG COEFFICIENT
! coefficient de trainee de vent
      DOUBLE PRECISION CDRAG
!> @brief DEPTH-INDUCED BREAKING 1 (BJ) COEFFICIENT ALPHA
! modele deferlement bj : constante alpha
      DOUBLE PRECISION ALFABJ
!> @brief DEPTH-INDUCED BREAKING 1 (BJ) COEFFICIENT GAMMA1
! modele deferlement bj : constante gamma1
      DOUBLE PRECISION GAMBJ1
!> @brief DEPTH-INDUCED BREAKING 1 (BJ) COEFFICIENT GAMMA2
! modele deferlement bj : constante gamma2
      DOUBLE PRECISION GAMBJ2
!> @brief DEPTH-INDUCED BREAKING 2 (TG) COEFFICIENT B
! modele deferlement tg : constante b
      DOUBLE PRECISION BORETG
!> @brief DEPTH-INDUCED BREAKING 2 (TG) COEFFICIENT GAMMA
! modele deferlement tg : constante gamma
      DOUBLE PRECISION GAMATG
!> @brief DEPTH-INDUCED BREAKING 3 (RO) COEFFICIENT ALPHA
! modele deferlement ro : constante alpha
      DOUBLE PRECISION ALFARO
!> @brief DEPTH-INDUCED BREAKING 3 (RO) COEFFICIENT GAMMA
! modele deferlement ro : constante gamma
      DOUBLE PRECISION GAMARO
!> @brief DEPTH-INDUCED BREAKING 3 (RO) COEFFICIENT GAMMA2
! modele deferlement ro : constante gamma2
      DOUBLE PRECISION GAM2RO
!> @brief DEPTH-INDUCED BREAKING 4 (IH) COEFFICIENT BETA0
! modele deferlement ih : constante beta
      DOUBLE PRECISION BETAIH
!> @brief DEPTH-INDUCED BREAKING 4 (IH) COEFFICIENT M2STAR
! modele deferlement ih : constante m2*
      DOUBLE PRECISION EM2SIH
!> @brief MAXIMUM VALUE OF THE RATIO HM0 OVER D
! coefficient limitateur de la hauteur hm0 par rapport a d
      DOUBLE PRECISION COEFHS
!> @brief COEFFICIENT OF THE TIME SUB-INCREMENTS FOR BREAKING
! pas de temps pour le deferlement
      DOUBLE PRECISION XDTBRK
!> @brief STANDARD CONFIGURATION PARAMETER
! coefficient lambda de la configuartion std
      DOUBLE PRECISION XLAMD
!> @brief IMPLICITATION COEFFICIENT FOR SOURCE TERMS
! coefficient implicitation pour termes sources
      DOUBLE PRECISION CIMPLI
!
!     INITIAL STILL WATER LEVEL
! 
      DOUBLE PRECISION ZREPOS
!> @brief TRIADS 1 (LTA) COEFFICIENT ALPHA
! triads 1 (lta) constante alpha
      DOUBLE PRECISION ALFLTA
!> @brief TRIADS 1 (LTA) COEFFICIENT RFMLTA
! triads 1 (lta) constante rfmlta
      DOUBLE PRECISION RFMLTA
!> @brief TRIADS 2 (SPB) COEFFICIENT K
! triads 2 (spb) constante k
      DOUBLE PRECISION KSPB
!> @brief TRIADS 2 (SPB) LOWER DIRECTIONAL BOUNDARY
! triads 2 (spb) borne directionnelle inferieure
      DOUBLE PRECISION BDISPB
!> @brief TRIADS 2 (SPB) UPPER DIRECTIONAL BOUNDARY
! triads 2 (spb) borne directionnelle superieure
      DOUBLE PRECISION BDSSPB
!> @brief SIGNIFICANT WAVE HEIGHT
! hauteur significative jonswap
      DOUBLE PRECISION HM0
!> @brief PEAK FREQUENCY
! frequence de pic jonswap
      DOUBLE PRECISION FPIC
!> @brief PEAK FACTOR
! facteur de forme de pic jonswap
      DOUBLE PRECISION GAMMA
!> @brief VALUE OF SIGMA-A FOR SPECTRUM
! valeur de sigma jonswap pour f < fp
      DOUBLE PRECISION SIGMAA
!> @brief VALUE OF SIGMA-B FOR SPECTRUM
! valeur de sigma jonswap pour f > fp
      DOUBLE PRECISION SIGMAB
!> @brief PHILLIPS CONSTANT
! constante de phillips (alpha)
      DOUBLE PRECISION ALPHIL
!> @brief MEAN FETCH VALUE
! fetch moyen
      DOUBLE PRECISION FETCH
!> @brief MAXIMUM PEAK FREQUENCY
! valeur maximum de la frequence de pic
      DOUBLE PRECISION FREMAX
!> @brief MAIN DIRECTION 1
! direction principale 1 pour fonction de repartition ang.
      DOUBLE PRECISION TETA1
!> @brief DIRECTIONAL SPREAD 1
! etalement directionnel 1 pour fonction de repartition ang.
      DOUBLE PRECISION SPRED1
!> @brief MAIN DIRECTION 2
! direction principale 2 pour fonction de repartition ang.
      DOUBLE PRECISION TETA2
!> @brief DIRECTIONAL SPREAD 2
! etalement directionnel 2 pour fonction de repartition ang.
      DOUBLE PRECISION SPRED2
!> @brief WEIGHTING FACTOR FOR ADF (DIRECTIONAL SPREADING FUNCTION)
! facteur de ponderation pour la fonction de repartition ang.
      DOUBLE PRECISION XLAMDA
!> @brief SPECTRUM TAIL FACTOR
! facteur de queue du spectre
      DOUBLE PRECISION TAILF
!> @brief SPECTRUM ENERGY THRESHOLD
! seuil minimum de variance considere
      DOUBLE PRECISION E2FMIN
!> @brief BOUNDARY SIGNIFICANT WAVE HEIGHT
! hauteur significative aux limites
      DOUBLE PRECISION HM0L
!> @brief BOUNDARY PEAK FREQUENCY
! frequence de pic aux limites
      DOUBLE PRECISION FPICL
!> @brief BOUNDARY SPECTRUM VALUE OF SIGMA-A
! valeur aux limites de sigma-a pour spectre
      DOUBLE PRECISION SIGMAL
!> @brief BOUNDARY SPECTRUM VALUE OF SIGMA-B
! valeur aux limites de sigma-b pour spectre
      DOUBLE PRECISION SIGMBL
!> @brief BOUNDARY PHILLIPS CONSTANT
! constante de phillips aux limites
      DOUBLE PRECISION APHILL
!> @brief BOUNDARY MEAN FETCH VALUE
! valeur moyenne du fetch aux limites
      DOUBLE PRECISION FETCHL
!> @brief BOUNDARY MAXIMUM PEAK FREQUENCY
! frequence de pic maximale aux limites
      DOUBLE PRECISION FPMAXL
!> @brief BOUNDARY MAIN DIRECTION 1
! direction principale 1 aux limites
      DOUBLE PRECISION TETA1L
!> @brief BOUNDARY DIRECTIONAL SPREAD 1
! etalement directionnel 1 aux limites
      DOUBLE PRECISION SPRE1L
!> @brief BOUNDARY MAIN DIRECTION 2
! direction principale 2 aux limites
      DOUBLE PRECISION TETA2L
!> @brief BOUNDARY DIRECTIONAL SPREAD 2
! etalement directionnel 2 aux limites
      DOUBLE PRECISION SPRE2L
!> @brief BOUNDARY WEIGHTING FACTOR FOR ADF
! facteur de ponderation pour fonction de repartition angulaire aux limites
      DOUBLE PRECISION XLAMDL
!> @brief BOUNDARY PEAK FACTOR
! facteur de pic aux limites
      DOUBLE PRECISION GAMMAL
!> @brief
!
      DOUBLE PRECISION ALF1, GAM1, GAM2
!> @brief WIND VELOCITY ALONG X
! vitesse du vent suivant x
      DOUBLE PRECISION VX_CTE
!> @brief WIND VELOCITY ALONG Y
! vitesse du vent suivant y
      DOUBLE PRECISION VY_CTE
!> @brief MINIMUM WATER DEPTH
! valeur minimale de la profondeur d'eau
      DOUBLE PRECISION PROMIN
!> @brief ABSCISSAE OF SPECTRUM PRINTOUT POINTS
! abscisses des points de sortie du spectre
      DOUBLE PRECISION XLEO(99)
!> @brief ORDINATES OF SPECTRUM PRINTOUT POINTS
! ordonnees des points de sortie du spectre
      DOUBLE PRECISION YLEO (99)
!
!GM V6P1 - NEW SOURCE TERMS
!> @brief YAN GENERATION COEFFICIENT D
! constante utilisee pour la generation  par le vent de Yan
      DOUBLE PRECISION COEFWD
!> @brief YAN GENERATION COEFFICIENT E
! constante utilisee pour la generation  par le vent de Yan
      DOUBLE PRECISION COEFWE
!> @brief YAN GENERATION COEFFICIENT F
! constante utilisee pour la generation  par le vent de Yan
      DOUBLE PRECISION COEFWF
!> @brief YAN GENERATION COEFFICIENT H
! constante utilisee pour la generation  par le vent de Yan
      DOUBLE PRECISION COEFWH
!> @brief WESTHUYSEN DISSIPATION COEFFICIENT
! coefficient de dissipation par moutonnement - Westhuysen
      DOUBLE PRECISION CMOUT3
!> @brief SATURATION THRESHOLD FOR THE DISSIPATION
! dissipation par moutonnement, seuil de saturation - Westhuysen
      DOUBLE PRECISION CMOUT4
!> @brief WESTHUYSEN WHITE CAPPING DISSIPATION
! coefficient de dissipation par moutonnement - Westhuysen
      DOUBLE PRECISION CMOUT5
!> @brief WESTHUYSEN WEIGHTING COEFFICIENT
! coefficient de ponderation de Westhuysen
      DOUBLE PRECISION CMOUT6
!> @brief QNL4 - THRESHOLD0 FOR CONFIGURATIONS ELIMINATION
! seuil0 pour elimination des configurations - QNL4, methode GQM
      DOUBLE PRECISION SEUIL
!> @brief QNL4 - THRESHOLD1 FOR CONFIGURATIONS ELIMINATION
! seuil1 pour elimination des configurations - QNL4, methode GQM
      DOUBLE PRECISION SEUIL1
!> @brief QNL4 - THRESHOLD2 FOR CONFIGURATIONS ELIMINATION
! seuil2 pour elimination des configurations - QNL4, methode GQM
      DOUBLE PRECISION SEUIL2
!GM Fin
!V6P2 Diffraction
!> @brief SPECTRUM VARIANCE THRESHOLD FOR DIFFRACTION
! seuil minimum de variance spectrale considere pour diffraction
      DOUBLE PRECISION F2DIFM
!V6P2 End diffraction
!> @brief CONSIDERATION OF SOURCE TERMS
! si oui, prise en compte des termes sources
      LOGICAL TSOU
!> @brief SPHERICAL COORDINATES
! si oui, on est en coord. spher.
      LOGICAL SPHE
!> @brief GLOBAL OUTPUT AT THE END
! si oui, sortie globale a la fin
      LOGICAL GLOB
!> @brief NEXT COMPUTATION
! si oui, suite de calcul
      LOGICAL SUIT
!> @brief INFINITE DEPTH
! si oui, profondeur infinie
      LOGICAL PROINF
!> @brief CONSIDERATION OF A CURRENT
! si oui, prise en compte du courant
      LOGICAL COURAN
!> @brief CONSIDERATION OF A WIND
! si oui, prise en compte du vent
      LOGICAL VENT
!> @brief CONSIDERATION OF A STATIONARY CURRENT
! si oui, prise en compte d'un courant stationnaire
      LOGICAL COUSTA
!> @brief CONSIDERATION OF A STATIONARY WIND
! si oui, prise en compte d'un vent stationnaire
      LOGICAL VENSTA
!> @brief CONSIDERATION OF TIDE
! si oui, prise en compte de la maree
      LOGICAL MAREE
!> @brief TRIGONOMETRICAL CONVENTION
! convention trigonometrique
      LOGICAL TRIGO
!> @brief RECOVERY OF TELEMAC DATA ITEM
! si oui, on recupere une donnee telemac
      LOGICAL DONTEL
!> @brief CONSIDERATION OF PROPAGATION
! si oui, prise en compte de la propagation
      LOGICAL PROP
!> @brief VALIDATION
! validation
      LOGICAL VALID
!> @brief LIMIT SPECTRUM MODIFIED BY USER
! spectre aux limites modifie par l'utilisateur
      LOGICAL SPEULI
!V6P2 Diffraction
!> @brief DIFFRACTION FILTER
! si oui, lissage de l amplitude locale du spectre direct.
      LOGICAL FLTDIF
!V6P2 End Diffraction
!> @brief TITLE!!!!!!!!!!
! titre du cas de calcul
      CHARACTER (LEN=80) :: TITCAS
!> @brief VARIABLES FOR 2D GRAPHIC PRINTOUTS
! variables pour les sorties graphiques 2d
      CHARACTER (72) :: SORT2D
!> @brief GEOMETRY FILE BINARY
! binaire du fichier de geometrie
      CHARACTER*3 BINGEO
!> @brief GLOBAL RESULT FILE BINARY
! binaire du fichier des resultats globaux
      CHARACTER*3 BINRBI
!> @brief 2D RESULTS FILE BINARY
! binaire du fichier des resultats 2d
      CHARACTER*3 BINRES
!> @brief PUNCTUAL RESULTS FILE BINARY
! binaire du fichier des resultats ponctuels
      CHARACTER*3 BINLEO
!> @brief PREVIOUS COMPUTATION FILE BINARY
! binaire du fichier du calcul precedent
      CHARACTER*3 BINPRE
!> @brief CURRENTS FILE BINARY
! binaire du fichier des courants
      CHARACTER*3 BINCOU
!> @brief WINDS FILE BINARY
! binaire du fichier des vents en entree
      CHARACTER*3 BINVEN
!> @brief TIDAL WATER LEVEL FILE BINARY
! binaire du fichier des hauteurs de la maree
      CHARACTER*3 BINMAR
!> @brief BINARY FILE 1 BINARY
! binaire du fichier binaire utilisateur
      CHARACTER*3 BINBI1
!> @brief RELEASE
! numero de version
      CHARACTER*4 VERS
!> @brief
      INTEGER NDP
!> @brief
! standard du fichier de geometrie
      INTEGER STDGEO
!> @brief
      CHARACTER*20 EQUA
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
     *                        K_IF2 (:,:,:), K_IF3 (:,:,:),
     *                        K_1P2P(:,:,:), K_1P2M(:,:,:),
     *                        K_1P3P(:,:,:), K_1P3M(:,:,:),
     *                        K_1M2P(:,:,:), K_1M2M(:,:,:),
     *                        K_1M3P(:,:,:), K_1M3M(:,:,:),
     *                        IDCONF(:,:)
!> @brief
! declaration for QNL4 - GQM method
      DOUBLE PRECISION, ALLOCATABLE :: TB_V24(:,:,:), TB_V34(:,:,:),
     *                                 TB_TPM(:,:,:), TB_TMP(:,:,:),
     *                                 TB_FAC(:,:,:),
     *                                 TB_V14(:)
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
      CHARACTER*32 VARCLA(10)
!> @brief
! nom des variables
      CHARACTER*32 TEXTE(MAXVAR)
!> @brief
! nom des variables du calcul precedent
      CHARACTER*32 TEXTPR(MAXVAR)
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
!> @brief
!
      INTEGER ALIRE(MAXVAR)
!
! JMH20/04/2009, MISSING A 0 VALUE (?)
      DATA ALIRE /0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,
     &0,0,0,0,0,0,0,0/
      DATA HIST /9999.D0/
!
!-----------------------------------------------------------------------
!   DECLARES POINTERS FOR ALIASES
!   THE TARGETS ARE DEFINED IN POINT
!-----------------------------------------------------------------------
!
!> @brief MESH COORDINATES
! abscisses des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X
!> @brief MESH COORDINATES
! ordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Y
!> @brief
! abscisses des noeuds des elements
      DOUBLE PRECISION, DIMENSION(:), POINTER :: XEL
!> @brief
! ordonnees des noeuds des elements
      DOUBLE PRECISION, DIMENSION(:), POINTER :: YEL
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
      INTEGER, DIMENSION(:), POINTER :: KNI , KNOGL  
      INTEGER, DIMENSION(:), POINTER :: ELI,ITR31,ITR32,ITR33
!
!V6P2 Diffraction
      INTEGER, DIMENSION(:), POINTER ::   NB_CLOSE, NEIGB
      DOUBLE PRECISION, DIMENSION(:), POINTER :: A, A_RMSE, CCG, DELTA,
     & DIV, DDX, DDY, RK, RX, RXX, RY, RYY, XKONPT
!V6P2 End diffraction

!> @brief GEOMETRY FILE
! fichier de geometrie
      INTEGER :: WACGEO
!> @brief STEERING FILE
! fichier des parametres
      INTEGER :: WACCAS
!> @brief BOUNDARY CONDITIONS FILE
! fichier des conditions aux limites
      INTEGER :: WACCLI
!> @brief BOTTOM TOPOGRAPHY FILE
! fichier des fonds
      INTEGER :: WACFON
!> @brief 2D RESULTS FILE
! fichier des resultats 2d
      INTEGER :: WACRES
!> @brief PUNCTUAL RESULTS FILE
! fichier des resultats ponctuels
      INTEGER :: WACLEO
!> @brief PREVIOUS COMPUTATION FILE
! fichier du calcul precedent
      INTEGER :: WACPRE
!> @brief GLOBAL RESULT FILE
! fichier des resultats globaux
      INTEGER :: WACRBI
!> @brief BINARY CURRENTS FILE
! fichier des courants binaire
      INTEGER :: WACCOB
!> @brief FORMATTED CURRENTS FILE
! fichier des courants formate
      INTEGER :: WACCOF
!> @brief BINARY FILE 1
! fichier binaire 1
      INTEGER :: WACBI1
!> @brief FORMATTED FILE 1
! fichier formate 1
      INTEGER :: WACFO1
!> @brief BINARY WINDS FILE
! fichier des vents binaire
      INTEGER :: WACVEB
!> @brief FORMATTED WINDS FILE
! fichier des vents formate
      INTEGER :: WACVEF
!> @brief PARALLELISM FILE
! fichier de parallelisme
      INTEGER :: WACPAR
!> @brief VALIDATION FILE
! fichier de reference
      INTEGER :: WACREF
!> @brief BINARY TIDAL WATER LEVEL FILE
! fichier du niveau de la maree binaire
      INTEGER :: WACMAB
!> @brief FORMATTED TIDAL WATER LEVEL FILE
! fichier du niveau de la maree formate
      INTEGER :: WACMAF
!> @brief
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
      SAVE
!
      END MODULE DECLARATIONS_TOMAWAC

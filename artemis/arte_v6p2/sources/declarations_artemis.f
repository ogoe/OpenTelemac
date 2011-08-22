!                    ***************************
                     MODULE DECLARATIONS_ARTEMIS
!                    ***************************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   31/05/2011
!***********************************************************************
!
!brief    DECLARATION OF PRINICIPAL ARTEMIS VARIABLES
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
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
      USE BIEF_DEF
!
!brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
!
      INTEGER NPTFR_TOT
!brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
!
      INTEGER NPOIN_TOT
!brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
!
      INTEGER, ALLOCATABLE :: KP1BOR_TOT(:)
!brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
!
      INTEGER, ALLOCATABLE :: NBOR_TOT(:)
!brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
!
      INTEGER, ALLOCATABLE :: LIHBORT(:)
!brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
!
      INTEGER, ALLOCATABLE :: LIDIRT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: MASK1T(:),MASK2T(:)
      DOUBLE PRECISION, ALLOCATABLE :: MASK3T(:),MASK4T(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: XT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: YT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: CGT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: KT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: CTT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: RPT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: ALFAPT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: HBT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: TETABT(:)
!brief
!
      DOUBLE PRECISION, ALLOCATABLE :: TETAPT(:)
!
!       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
!
!       1) VECTORS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!       2) MATRICES (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!       3) BLOCKS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!       4) INTEGERS
!       5) LOGICAL VALUES
!       6) REALS
!       7) STRINGS
!       8) SLVCFG STRUCTURES
!       9) MESH STRUCTURE
!      10) ALIASES
!
!-----------------------------------------------------------------------
!
!       1) VECTORS
!
!-----------------------------------------------------------------------
!
!brief REAL PART OF WAVE POTENTIAL
! partie reelle du potentiel
      TYPE(BIEF_OBJ), TARGET :: PHIR
!brief IMAGINARY PART OF WAVE POTENTIAL
! partie imaginaire du potentiel
      TYPE(BIEF_OBJ), TARGET :: PHII
!brief WATER DEPTH AT REST
! hauteur d'eau au repos
      TYPE(BIEF_OBJ), TARGET :: H
!brief (MEAN) WAVE NUMBER
! nombre d'onde
      TYPE(BIEF_OBJ), TARGET :: K
!brief (MEAN) PHASE CELERITY
! vitesse de phase
      TYPE(BIEF_OBJ), TARGET :: C
!brief (MEAN) GROUP CELERITY
! vitesse de groupe
      TYPE(BIEF_OBJ), TARGET :: CG
!brief SIGNIFICATIVE WAVE HEIGHT (REGULAR MODE)
! hauteur de la houle
      TYPE(BIEF_OBJ), TARGET :: HHO
!brief WAVE PHASE (REGULAR MODE)
! phase de la houle
      TYPE(BIEF_OBJ), TARGET :: PHAS
!brief SURFACE WAVE VELOCITY COMPONENT
! vitesse en surface (a t=0)              !!!!! really? !!!!!
      TYPE(BIEF_OBJ), TARGET :: U0
!brief SURFACE WAVE VELOCITY COMPONENT
! vitesse en surface (a t=0)              !!!!! really? !!!!!
      TYPE(BIEF_OBJ), TARGET :: V0
!brief MEAN COSINE OF WAVE DIRECTION
! moyennes des cosinus de la direction de houle
      TYPE(BIEF_OBJ), TARGET :: MCOS
!brief MEAN SINE OF WAVE DIRECTION
! moyennes des sinus de la direction de houle
      TYPE(BIEF_OBJ), TARGET :: MSIN
!brief WAVE INCIDENCE (OR DIRECTION)
! incidence de la houle
      TYPE(BIEF_OBJ), TARGET :: INCI
!brief FREE SURFACE ELEVATION
! cote de la surface libre
      TYPE(BIEF_OBJ), TARGET :: S
!brief BOTTOM ELEVATION
! cote du fond
      TYPE(BIEF_OBJ), TARGET :: ZF
!brief FRICTION FACTOR
! coefficient de frottement (variable en espace)
      TYPE(BIEF_OBJ), TARGET :: FW
!brief WAVE HEIGHT (RANDOM WAVE)
! hauteur de la houle aleatoire
      TYPE(BIEF_OBJ), TARGET :: HALE
!brief WAVE PERIODS ARRAY (RANDOM MODE)
! tableau des periodes de discretisation du spectre pour un calcul en houle aleatoire multidirectionnelle
      TYPE(BIEF_OBJ), TARGET :: PALE
!brief REFLEXION COEFFICIENT
! coefficient de reflexion des parois
      TYPE(BIEF_OBJ), TARGET :: RP
!brief ANGLE OF WAVE ATTACK (FROM X AXIS)
! angle d'attaque de la houle sur les limites - pas seulement les parois (compte par rapport a a la normale exterieure dans le sens direct)some sources say (compte par rapport a l'axe des x)
      TYPE(BIEF_OBJ), TARGET :: TETAP
!brief DEPHASING CAUSED BY THE WALLS
! dephasage induit par la paroi entre l'onde reflechie et l'onde incidente (si alfap est positif, l'onde reflechie est en retard)
      TYPE(BIEF_OBJ), TARGET :: ALFAP
!brief INCIDENT WAVE HEIGHT AT THE BOUNDARY
! hauteur de la houle aux frontieres ouvertes
      TYPE(BIEF_OBJ), TARGET :: HB
!brief INCIDENT WAVE DIRECTION AT THE BOUNDARY
! angle d'attaque de la houle aux frontieres ouvertes (compte par rapport a l'axe des x dans le sens direct)
      TYPE(BIEF_OBJ), TARGET :: TETAB
!brief REAL PART OF INCIDENT WAVE AT THE BOUNDARY
! partie reelle du potentiel impose au bord (dirichlet)
      TYPE(BIEF_OBJ), TARGET :: PHIRB
!brief IMAGINARY PART OF INCIDENT WAVE AT THE BOUNDARY
! partie imaginaire du potentiel impose au bord (dirichlet)
      TYPE(BIEF_OBJ), TARGET :: PHIIB
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI1B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI1B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI1B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI1B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI2B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI2B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI2B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI2B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI3B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI3B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI3B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI3B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI4B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI4B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI4B
!brief COEFFICIENT FOR BOUNDARY CONDITIONS
! coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI4B
!brief WORKING ARRAY
! tableau de travail
      TYPE(BIEF_OBJ), TARGET :: W1
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT1
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT2
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT3
!brief VOID STRUCTURE
!
      TYPE(BIEF_OBJ), TARGET :: SBID
!brief RIGHT MEMBER OF SYSTEM TO BE SOLVED
!
      TYPE(BIEF_OBJ), TARGET :: CV1
!brief RIGHT MEMBER OF SYSTEM TO BE SOLVED
!
      TYPE(BIEF_OBJ), TARGET :: CV2
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: MU
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: MU2
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: QB
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: HMU
!brief WAVE DISSIPATION QUANTITY
! tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: HMUANC
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: SXX
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: SXY
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: SYY
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: FX
!brief RADIATION STRESSES QUANTITY
! tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: FY
!brief MEAN WAVE PERIOD
! periode moyenne issue du moment d'ordre 1
      TYPE(BIEF_OBJ), TARGET :: T01
!brief MEAN WAVE PERIOD
! periode moyenne issue du moment d'ordre 2
      TYPE(BIEF_OBJ), TARGET :: T02
!brief MEAN WAVE PERIOD
! periode moyenne issue du moment d'ordre 1
      TYPE(BIEF_OBJ), TARGET :: TM
!brief WAVE DIRECTIONS AT THE BOUNDARY (RANDOM MODE)
!
      TYPE(BIEF_OBJ), TARGET :: DALE
!brief BOUNDARY CONDITION TYPE
! type de conditions aux limites sur u
      TYPE(BIEF_OBJ), TARGET :: LIUBOR
!brief BOUNDARY CONDITION TYPE
! type de conditions aux limites sur v
      TYPE(BIEF_OBJ), TARGET :: LIVBOR
!brief BOUNDARY CONDITION TYPE
! type de conditions aux limites sur h
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!brief
!
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!brief
!
      TYPE(BIEF_OBJ), TARGET :: LIDIR
!brief
!
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO INCIDENT WAVES (KINC)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK1
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO FREE EXIT (KSORT)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK2
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO SOLID BOUNDARY (KLOG)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK3
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO IMPOSED WAVES (KENT)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK4
!brief FLOW
! courant
      TYPE(BIEF_OBJ), TARGET :: UC
!brief FLOW
! courant
      TYPE(BIEF_OBJ), TARGET :: VC
!brief RELATIVE ANGULAR FREQUENCY
! pulsation relative
      TYPE(BIEF_OBJ), TARGET :: WR
!brief WAVE VECTOR
! vecteur d'onde
      TYPE(BIEF_OBJ), TARGET :: KN1
!brief WAVE VECTOR
! vecteur d'onde
      TYPE(BIEF_OBJ), TARGET :: KN2
!brief
!
      TYPE(BIEF_OBJ), TARGET :: KNANC1,KNANC2
!
!-----------------------------------------------------------------------
!
!       2) MATRICES
!
!-----------------------------------------------------------------------
!
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: AM1
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: AM2
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: AM3
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: BM1
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: BM2
!brief MATRICE FOR SYSTEM SOLVING
!
      TYPE(BIEF_OBJ), TARGET :: MBOR
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!
!brief BLOCK OF POTENTIAL VECTORS
!
      TYPE(BIEF_OBJ), TARGET :: PHIB
!brief BLOCK OF WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: TB
!brief BLOCK OF WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: TBBD
!brief BLOCK OF PRIVATE VECTORS
! tableaux reserves a l'utilisateur
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!brief BLOCK OF MATRICES
!
      TYPE(BIEF_OBJ), TARGET :: MAT
!brief BLOCK OF UNKNOWN VECTORS
!
      TYPE(BIEF_OBJ), TARGET :: UNK
!brief BLOCK OF RIGHT HAND SIDE VECTORS IN SOLVING SYSTEM
!
      TYPE(BIEF_OBJ), TARGET :: RHS
!brief BLOCK OF VARIABLES FOR OUTPUT
!
      TYPE(BIEF_OBJ), TARGET :: VARSOR
!
!-----------------------------------------------------------------------
!
!       4) INTEGERS
!
!-----------------------------------------------------------------------
!
!brief
! maximum de variables de sortie
      INTEGER, PARAMETER :: MAXVAR = 100
!brief
! maximum de frontieres liquides
      INTEGER, PARAMETER :: MAXFRO = 100
!brief ORIGIN COORDINATE
! coordonnee de l'origine
      INTEGER I_ORIG
!brief ORIGIN COORDINATE
! coordonnee de l'origine
      INTEGER J_ORIG
!brief GRAPHIC PRINTOUT PERIOD
! periode de sortie graphique
      INTEGER LEOPRD
!brief LISTING PRINTOUT PERIOD
! periode de sortie listing
      INTEGER LISPRD
!brief MAXIMUM NUMBER OF ITERATIONS FOR SOLVER
! maximum d'iterations pour le solveur
      INTEGER NITMAX
!brief GEOMETRY FILE STANDARD
! standard du fichier de geometrie
      INTEGER STDGEO
!brief RESULTS FILE STANDARD
! standard du fichier des resultats
      INTEGER STDRES
!brief SOLVER OPTION
! option du solveur
      INTEGER ISOLVE(2)
!brief BOTTOM TOPOGRAPHY SMOOTHINGS
! nombre de lissages du fond
      INTEGER LISFON
!brief DISCRETISATION IN SPACE
! discretisation en espace
      INTEGER DISESP
!brief NUMBER OF DISCRETISED PERIODS
! nombre de periodes de discretisation du spectre de houle
      INTEGER NPALE
!brief NUMBER OF DISCRETISED DIRECTIONS
! nombre de directions de discretisation du spectre de houle
      INTEGER NDALE
!brief MATRIX STORAGE
! stockage des matrices
      INTEGER OPTASS
!brief BREAKING LAW
! formulation du deferlement
      INTEGER IBREAK
!brief MAXIMUM OF SUB-ITERATIONS
! maximum de sous-iterations
      INTEGER NITDIS
!brief VECTOR LENGTH
! longueur du vecteur
      INTEGER LVMAC
!brief LAW OF BOTTOM FRICTION
! loi de frottement sur le fond
      INTEGER KFROT
!brief BOTTOM FRICTION LAW
! formulation du frottement de fond
      INTEGER FORMFR
!brief HYDRAULIC REGIME TYPE
! type du regime hydraulique
      INTEGER REGIDO
!brief MATRIX-VECTOR PRODUCT
! produit matrice-vecteur
      INTEGER PRODUC
!brief NUMBER OF PRIVATE ARRAYS
! nombre de tableaux prives
      INTEGER NPRIV
!brief
!
      INTEGER PTINIG
!brief
!
      INTEGER PTINIL
!brief
! type d'element
      INTEGER IELM
!brief
!
      INTEGER IELM0
!brief
! type d'element de bord
      INTEGER IELMB
!brief
!
      INTEGER IELMB0
!brief ORIGINAL DATE OF TIME
! date de l'origine des temps
      INTEGER MARDAT(3)
!brief ORIGINAL HOUR OF TIME
! heure de l'origine des temps
      INTEGER MARTIM(3)
!brief
!
      INTEGER NFRLIQ
!brief
!
      INTEGER NFRSOL
!brief
!
      INTEGER DEBLIQ(MAXFRO)
!brief
!
      INTEGER FINLIQ(MAXFRO)
!brief
!
      INTEGER DEBSOL(MAXFRO)
!brief
!
      INTEGER FINSOL(MAXFRO)
!brief
! prise en compte des effets de pente/courbure
      INTEGER IPENTCO
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
!brief LISTING PRINTOUT
! si oui, sortie listing
      LOGICAL LISTIN
!brief
!
      LOGICAL INFOGR
!brief PERIOD SCANNING
! si oui, balayage en periodes
      LOGICAL BALAYE
!brief MONODIRECTIONAL RANDOM WAVE
! si oui, houle aleatoire monodirectionnelle
      LOGICAL ALEMON
!brief MULTIDIRECTIONAL RANDOM WAVE
! si oui, houle aleatoire multidirectionnelle
      LOGICAL ALEMUL
!brief
!
      LOGICAL MSK
!brief
!
      LOGICAL SPHERI
!brief BREAKING
! si oui, deferlement
      LOGICAL DEFERL
!brief FRICTION
! si oui, frottement
      LOGICAL FROTTE
!brief FRICTION FACTOR IMPOSED
! si oui, facteur de frottement impose
      LOGICAL ENTFW
!brief HYDRAULIC REGIME IMPOSED
! si oui, regime hydraulique impose
      LOGICAL ENTREG
!brief SKIN ROUGHNESS ONLY
! si oui, rugosite de peau seule
      LOGICAL ENTRUG
!brief WAVE HEIGHTS SMOOTHING
! si oui, lissage des hauteurs de houle
      LOGICAL LISHOU
!brief
!
      LOGICAL SORLEO(MAXVAR)
!brief
!
      LOGICAL SORIMP(MAXVAR)
!brief VALIDATION
! si oui, validation
      LOGICAL VALID
!brief
!
      LOGICAL COURANT
!
!-----------------------------------------------------------------------
!
!       6) REALS
!
!-----------------------------------------------------------------------
!
!brief GRAVITY ACCELERATION
! acceleration de la pesanteur
      DOUBLE PRECISION GRAV
!brief MINIMUM VALUE FOR H
! valeur minimum de h
      DOUBLE PRECISION HMIN
!brief WAVE PERIOD
! periode de la houle en cours de calcul
      DOUBLE PRECISION PER
!brief ANGULAR FREQUENCY
! pulsation de la houle
      DOUBLE PRECISION OMEGA
!brief DIRECTION OF WAVE PROPAGATION
! direction principale de propagation de la houle
      DOUBLE PRECISION TETAH
!brief INITIAL WATER LEVEL
! cote initiale
      DOUBLE PRECISION COTINI
!brief INITIAL DEPTH
! hauteur initiale
      DOUBLE PRECISION HAUTIN
!brief BEGINNING PERIOD FOR PERIOD SCANNING
! periode de debut pour le balayage en periode
      DOUBLE PRECISION PERDEB
!brief ENDING PERIOD FOR PERIOD SCANNING
! periode de fin pour le balayage en periode
      DOUBLE PRECISION PERFIN
!brief STEP FOR PERIOD SCANNING
! pas pour le balayage en periode
      DOUBLE PRECISION PERPAS
!brief PEAK PERIOD
! periode de pic
      DOUBLE PRECISION PERPIC
!brief GAMMA
! gamma
      DOUBLE PRECISION GAMMA
!brief MINIMUM ANGLE OF PROPAGATION
! valeur minimum de l'angle de propagation
      DOUBLE PRECISION TETMIN
!brief MAXIMUM ANGLE OF PROPAGATION
! valeur maximum de l'angle de propagation
      DOUBLE PRECISION TETMAX
!brief S EXPONENT
! exposant s dans la formule du spectre
      DOUBLE PRECISION EXPOS
!brief
!
      DOUBLE PRECISION RELAX
!brief FRICTION COEFFICIENT
! coefficient de frottement
      DOUBLE PRECISION FFON
!brief SUB-ITERATIONS ACCURACY
! precision sur les sous-iterations
      DOUBLE PRECISION EPSDIS
!brief DISSIPATION RELAXATION
! relaxation sur la dissipation
      DOUBLE PRECISION RELDIS
!brief ALPHA
! alpha
      DOUBLE PRECISION ALFABJ
!brief GAMMAS
! gammas
      DOUBLE PRECISION GAMMAS
!brief
!
      DOUBLE PRECISION KDALLY
!brief
!
      DOUBLE PRECISION GDALLY
!brief FLUID KINEMATIC VISCOSITY
! viscosite cinematique du fluide
      DOUBLE PRECISION VISCO
!brief DIAMETER90
! diametre90
      DOUBLE PRECISION DIAM90
!brief DIAMETER50
! diametre50
      DOUBLE PRECISION DIAM50
!brief SEDIMENT SPECIFIC WEIGHT
! masse volumique du sediment
      DOUBLE PRECISION MVSED
!brief FLUID SPECIFIC MASS
! masse volumique du fluide
      DOUBLE PRECISION MVEAU
!brief FRICTION FACTOR
! coefficient de frottement constant impose
      DOUBLE PRECISION FWCOEF
!brief RIPPLES COEFFICIENT
! coefficient de rides
      DOUBLE PRECISION RICOEF
!brief MINIMUM SPECTRAL PERIOD
! periode minimum du spectre
      DOUBLE PRECISION PMIN
!brief MAXIMUM SPECTRAL PERIOD
! periode maximum du spectre
      DOUBLE PRECISION PMAX
!brief
! courant : valeurs en x
      DOUBLE PRECISION CURRENTX
!brief
! courant : valeurs en y
      DOUBLE PRECISION CURRENTY
!
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!
!brief TITLE
! titre de l'etude
      CHARACTER*72 TITCAS
!brief
!
      CHARACTER*72 VARDES
!brief VARIABLES TO BE PRINTED
! variables a imprimer
      CHARACTER*72 VARIMP
!brief INITIAL CONDITIONS
! conditions initiales
      CHARACTER*72 CDTINI
!brief GEOMETRY FILE BINARY
! binaire du fichier de geometrie
      CHARACTER*3 BINGEO
!brief RESULTS FILE BINARY
! binaire du fichier des resultats
      CHARACTER*3 BINRES
!brief
!
      CHARACTER*20 EQUA
!brief
!
      CHARACTER*32 VARCLA(10)
!brief
!
      CHARACTER*32 TEXTE(MAXVAR)
!brief
!
      CHARACTER*32 TEXTPR(MAXVAR)
!
!-----------------------------------------------------------------------
!
!       8) SLVCFG STRUCTURES
!
!-----------------------------------------------------------------------
!
!brief SLVCFG STRUCTURE
!
      TYPE(SLVCFG) :: SLVART
!
!-----------------------------------------------------------------------
!
!       9) MESH STRUCTURE
!
!-----------------------------------------------------------------------
!
!brief MESH STRUCTURE
!
      TYPE(BIEF_MESH) :: MESH
!
!-----------------------------------------------------------------------
!
!      10) ALIASES
!
!-----------------------------------------------------------------------
!
!       DECLARATION OF POINTERS FOR ALIASES.
!       TARGETS ARE DEFINED IN POINT_ARTEMIS
!
!       ALIASES FOR WORKING VECTORS IN TB AND TBBD
!
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T1
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T2
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T3
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T4
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T5
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T6
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T7
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T8
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T9
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T10
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T11
!brief WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T12
!brief WORKING VECTOR IN TBBD
!
      TYPE(BIEF_OBJ),POINTER :: TBD1
!brief WORKING VECTOR IN TBBD
!
      TYPE(BIEF_OBJ),POINTER :: TBD2
!brief WORKING VECTOR IN TBBD
!
      TYPE(BIEF_OBJ),POINTER :: TBD3
!brief WORKING VECTOR IN TBBD
!
      TYPE(BIEF_OBJ),POINTER :: TBD4
!
!       USEFUL COMPONENTS IN STRUCTURE MESH
!
!brief
! table de connectivite
      TYPE(BIEF_OBJ), POINTER :: IKLE
!brief
! coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X
!brief
! coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Y
!brief
! nombre d'elements du maillage
      INTEGER, POINTER        :: NELEM
!brief
!
      INTEGER, POINTER        :: NELMAX
!brief
! nombre de points frontiere
      INTEGER, POINTER        :: NPTFR
!brief
!
      INTEGER, POINTER        :: NPTFRX
!brief
!
      INTEGER, POINTER        :: DIM
!brief
!
      INTEGER, POINTER        :: TYPELM
!brief
! nombre de points du maillage
      INTEGER, POINTER        :: NPOIN
!brief
!
      INTEGER, POINTER        :: NPMAX
!brief
!
      INTEGER, POINTER        :: MXPTVS
!brief
!
      INTEGER, POINTER        :: MXELVS
!brief
!
      INTEGER, POINTER        :: LV
!
!-----------------------------------------------------------------------
!
!      10) ART_FILES AND ASSOCIATED
!
!-----------------------------------------------------------------------
!
!brief
!
      INTEGER, PARAMETER :: MAXLU_ART = 44
!brief NAME OF THE GEOMETRY FILE
! nom du fichier de geometrie
      INTEGER :: ARTGEO
!brief NAME OF THE STEERING FILE
! nom du fichier des parametres
      INTEGER :: ARTCAS
!brief NAME OF THE BOUNDARY CONDITIONS FILE
! nom du fichier des conditions aux limites
      INTEGER :: ARTCLI
!brief NAME OF THE BOTTOM TOPOGRAPHY FILE
! nom du fichier des fonds
      INTEGER :: ARTFON
!brief NAME OF THE RESULTS FILE
! nom du fichier des resultats
      INTEGER :: ARTRES
!brief NAME OF THE BINARY RESULTS FILE
! nom du fichier des resultats binaire
      INTEGER :: ARTRBI
!brief NAME OF THE FORMATTED RESULTS FILE
! nom du fichier des resultats formate
      INTEGER :: ARTRFO
!brief NAME OF THE REFERENCE FILE
! nom du fichier de reference
      INTEGER :: ARTREF
!brief NAME OF THE BINARY DATA FILE 1
! nom du fichier de donnees binaire 1
      INTEGER :: ARTBI1
!brief NAME OF THE BINARY DATA FILE 2
! nom du fichier de donnees binaire 2
      INTEGER :: ARTBI2
!brief NAME OF THE FORMATTED DATA FILE 1
! nom du fichier de donnees formate 1
      INTEGER :: ARTFO1
!brief NAME OF THE FORMATTED DATA FILE 2
! nom du fichier de donnees formate 2
      INTEGER :: ARTFO2
!brief
!
      TYPE(BIEF_FILE) :: ART_FILES(MAXLU_ART)
!
      SAVE
!
      END MODULE DECLARATIONS_ARTEMIS


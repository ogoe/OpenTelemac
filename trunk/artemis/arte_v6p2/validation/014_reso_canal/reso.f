C ============ DECLARATIONS_ARTEMIS DOIT VENIR AVANT TOUT LE RESTE
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

!brief REAL PART OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie reelle du potentiel incident au bord 
      TYPE(BIEF_OBJ), TARGET :: PRB
!brief IMMAGINARY PART OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie imaginaire du potentiel incident au bord 
      TYPE(BIEF_OBJ), TARGET :: PIB
!brief REAL PART OF GRADIENT X COMPONENT OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie reele du gradient en X du potentiel incident au bord 
      TYPE(BIEF_OBJ), TARGET :: DDXPRB
!brief REAL PART OF GRADIENT Y COMPONENT OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie reele du gradient en Y du potentiel incident au bord 
      TYPE(BIEF_OBJ), TARGET :: DDYPRB
!brief IMMAGINARY PART OF GRADIENT X COMPONENT OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie imaginaire du gradient en X du potentiel incident au bord 
      TYPE(BIEF_OBJ), TARGET :: DDXPIB
!brief IMMAGINARY PART OF GRADIENT Y COMPONENT OF INCIDENT POTENTIAL AT THE BOUNDARY
! partie imaginaire du gradient en Y du potentiel incident au bord 
      TYPE(BIEF_OBJ), TARGET :: DDYPIB
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
      TYPE(BIEF_OBJ), TARGET :: CGRX1B
      TYPE(BIEF_OBJ), TARGET :: CGRY1B
      TYPE(BIEF_OBJ), TARGET :: DGRX1B
      TYPE(BIEF_OBJ), TARGET :: DGRY1B
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

! 'COLOUR' OF BOUNDARY NODES (TAKEN IN BOUNDARY CONDITIONS FILE)
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!brief
! 'BIDON INTEGER'
      TYPE(BIEF_OBJ), TARGET :: ITB1
!brief
! 'BIDON REEL
      TYPE(BIEF_OBJ), TARGET :: TB1
!brief


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
!brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO INCIDENT POTENTIAL (KPOT)
! masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK5


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

!     OPEN BOUNDARY WITH INCIDENT POTENTIAL 
      INTEGER, PARAMETER :: KPOT  =  7

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
!brief
      INTEGER LPER
!brief
      INTEGER LDIR
      
      
      
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





C==============================================================
C==============================================================
C==============================================================
C.................. DECLARTION DES VARIABLES


!                    ************************
                     SUBROUTINE POINT_ARTEMIS
!                    ************************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   31/05/2011
!***********************************************************************
!
!brief    ALLOCATES STRUCTURES.
!
!history  J-M HERVOUET (LNH)
!+        24/04/1997
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
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
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER ISYM,MEMW1,INOSYM,ISTOP,NTR,NTRBD
      INTEGER, TARGET :: TMP=1029
      INTEGER CFG(2),CFGBOR(2),I
!-----------------------------------------------------------------------
!
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,20)
         IF(LNG.EQ.2) WRITE(LU,21)
      ENDIF
20    FORMAT(1X,///,26X,'*****************************',/,
     &26X,              '* ALLOCATION DE LA MEMOIRE  *',/,
     &26X,              '*****************************',/)
21    FORMAT(1X,///,26X,'*****************************',/,
     &26X,              '*    MEMORY ORGANIZATION    *',/,
     &26X,              '*****************************',/)
!
!-----------------------------------------------------------------------
!
!  PARAMETERS DEPENDING ON THE TYPE OF ELEMENT: (SEE ALSO NDP)
!  ISYM AND INOSYM ARE USED TO DIMENSION THE ARRAYS CONTAINING THE
!                   EXTRADIAGONAL PART OF THE MATRICES.
!
      ISYM=3
      INOSYM=6
      IELM = 11
      IELM0 = 10
      IELMB = 1
      IELMB0 = 0
!
! TYPE OF STORAGE AND PRODUCT MATRIX X VECTOR
!
      CFG(1) = OPTASS
      CFG(2) = PRODUC
!     CFG IS IMPOSED FOR BOUNDARY MATRICES
      CFGBOR(1) = 1
      CFGBOR(2) = 1
      EQUA = 'ARTEMIS'
!
!=======================================================================
!
!     ALLOCATES THE MESH STRUCTURE
!
       CALL ALMESH(MESH,'MESH  ',IELM,SPHERI,CFG,ART_FILES(ARTGEO)%LU,
     &             EQUA,FILE_FORMAT=ART_FILES(ARTGEO)%FMT)
!
!
!     ALIAS FOR CERTAIN COMPONENTS OF MESH
!
      IKLE  => MESH%IKLE
      X     => MESH%X%R
      Y     => MESH%Y%R
!
      TMP=MESH%NPTFR
      NELEM => MESH%NELEM
      NELMAX=> MESH%NELMAX
      NPTFR => MESH%NPTFR
      NPTFRX=> MESH%NPTFRX
      DIM   => MESH%DIM
      TYPELM=> MESH%TYPELM
      NPOIN => MESH%NPOIN
      NPMAX => MESH%NPMAX
      MXPTVS=> MESH%MXPTVS
      MXELVS=> MESH%MXELVS
      LV    => MESH%LV
!      WRITE(*,*) 'FRONTIERE',NPTFR,MESH%NPTFR
!
!-----------------------------------------------------------------------
!
!                     ******************
!                     *   REAL ARRAYS  *
!                     ******************
!
!-----------------------------------------------------------------------
!
!
! POTENTIAL
!
      CALL BIEF_ALLVEC(1,PHIR,'PHIR  ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,PHII,'PHII  ',IELM, 1 , 2 ,MESH)
!
! WATER DEPTH AT REST
!
      CALL BIEF_ALLVEC(1,H,'H     ',IELM, 1 , 2 ,MESH)
!
! WAVE NUMBER
      CALL BIEF_ALLVEC(1,K,'K     ',IELM, 1 , 2 ,MESH)
!
! PHASE AND GROUP VELOCITIES
      CALL BIEF_ALLVEC(1,C,'C     ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CG,'CG    ',IELM, 1 , 2 ,MESH)
!
! WAVE HEIGHT AND PHASE
      CALL BIEF_ALLVEC(1,HHO,'HHO   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,PHAS,'PHAS  ',IELM, 1 , 2 ,MESH)
!
! VELOCITIES
      CALL BIEF_ALLVEC(1,U0,'U0    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,V0,'V0    ',IELM, 1 , 2 ,MESH)
!
! AVERAGES OF THE SINES AND COSINES OF THE WAVE DIRECTION
      CALL BIEF_ALLVEC(1,MCOS,'MCOS  ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MSIN,'MSIN  ',IELM, 1 , 2 ,MESH)
! WAVE INCIDENCE
      CALL BIEF_ALLVEC(1,INCI,'INCI  ',IELM, 1 , 2 ,MESH)
!
! FREE SURFACE AND BOTTOM ELEVATION
      CALL BIEF_ALLVEC(1,S,'S     ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ZF,'ZF    ',IELM, 1 , 2 ,MESH)
!
! FRICTION COEFFICIENT (VARIABLE IN SPACE)
      CALL BIEF_ALLVEC(1,FW,'FW    ',IELM, 1 , 2 ,MESH)
!
! WAVE HEIGHT (RANDOM SEAS)
! ARRAY STORING DISCRETISED PERIODS FOR MULTIDIRECTIONAL RANDOM
! WAVES
!
      IF(ALEMON .OR. ALEMUL) THEN
        CALL BIEF_ALLVEC(1,HALE,'HALE  ',IELM , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,PALE,'PALE  ',NPALE, 1 , 0 ,MESH)
      ENDIF
!
! REFLEXION COEFFICIENTS, ANGLE OF WAVE ATTACK (FROM X AXIS)
! AND DEPHASING CAUSED BY THE WALLS
!
      CALL BIEF_ALLVEC(1,RP,'RP    ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,TETAP,'TETAP ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ALFAP,'ALFAP ',IELMB, 1 , 2 ,MESH)
!
! WAVE HEIGHT AND ANGLE OF WAVE ATTACK FOR OPEN BOUNDARIES
! (ANGLE FROM X AXIS)
      CALL BIEF_ALLVEC(1,HB,'HB    ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,TETAB,'TETAB ',IELMB, 1 , 2 ,MESH)
! INCIDENT POTENIAL ALONG THE BOUNDARY
! REAL AND IMAGINARY PART
      CALL BIEF_ALLVEC(1,PRB   ,'PRB   ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,PIB   ,'PIB   ',IELMB, 1 , 2 ,MESH)
! GRADIENT OF INCIDENT POTENIAL ALONG THE BOUNDARY
! REAL AND IMAGINARY PART, X AND Y PART
      CALL BIEF_ALLVEC(1,DDXPRB,'DDXPRB',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DDYPRB,'DDYPRB',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DDXPIB,'DDXPIB',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DDYPIB,'DDYPIB',IELMB, 1 , 2 ,MESH)
      
!
! ARRAY OF POTENTIAL, IMPOSED ALONG THE BOUNDARY (DIRICHLET)
      CALL BIEF_ALLVEC(1,PHIRB,'PHIRB ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,PHIIB,'PHIIB ',IELMB, 1 , 2 ,MESH)
! BLOCK OF THESE VALUES :
      CALL ALLBLO(PHIB,'PHIB  ')
      CALL ADDBLO(PHIB,PHIRB)
      CALL ADDBLO(PHIB,PHIIB)
!
! COEFFICIENTS FOR BOUNDARY CONDITIONS
!
      CALL BIEF_ALLVEC(1,APHI1B,'APHI1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,BPHI1B,'BPHI1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CPHI1B,'CPHI1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DPHI1B,'DPHI1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CGRX1B,'CGRX1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CGRY1B,'CGRY1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DGRX1B,'DGRX1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DGRY1B,'DGRY1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,APHI2B,'APHI2B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,BPHI2B,'BPHI2B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CPHI2B,'CPHI2B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DPHI2B,'DPHI2B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,APHI3B,'APHI3B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,BPHI3B,'BPHI3B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CPHI3B,'CPHI3B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DPHI3B,'DPHI3B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,APHI4B,'APHI4B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,BPHI4B,'BPHI4B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CPHI4B,'CPHI4B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DPHI4B,'DPHI4B',IELMB, 1 , 2 ,MESH)
!
! BIDON TABLE FOR LECLIM     
      CALL BIEF_ALLVEC(1,TB1,'TB1   ',IELMB, 1 , 2 ,MESH)

! WORKING ARRAY (SIZE NELEM)  (PROBLEM)
      MEMW1 = 10 + 3*NPOIN/NELEM
!     THIS MEMORY HAS BEEN ALLOCATED IN THE FORM OF AN ARRAY
!     P0 WITH A SECOND DIMENSION
      MEMW1 = 1 + MEMW1/BIEF_NBMPTS(IELM0,MESH)
      CALL BIEF_ALLVEC(1,W1,'W1    ',IELM0,MEMW1,1,MESH)
!
!
! WORKING ARRAY (SIZE NPOIN)
!
!  NUMBER OF ARRAYS TO ALLOCATE : NTR
!           14 : FOR CGSTAB (=2 X 7)
      NTR = 14
!     FOR GMRES: NTR DEPENDS ON THE DIMENSION OF THE KRYLOV SPACE
      IF(SLVART%SLV.EQ.7) NTR = MAX(NTR,4+4*SLVART%KRYLOV)
!     2 ADDITIONAL DIAGONALS TO STORE WITH PRECONDITIONING BLOCK-DIAGONAL
      IF(3*(SLVART%PRECON/3).EQ.SLVART%PRECON) NTR = NTR + 2
!
!  ALLOCATES: NTR WORKING ARRAYS OF DIMENSION THE MAXIMUM NUMBER
!             OF DEGREES OF FREEDOM
!
!     TB STORES ARRAYS T1,T2,...
!
      CALL ALLBLO(TB ,'TB    ')
!
!
      CALL BIEF_ALLVEC_IN_BLOCK(TB,NTR,1,'T     ',IELM,1,2,MESH)
!
!     ALIASES FOR THE FIRST 4 WORKING ARRAYS IN THE BLOCK TB
!
! 12 WORKING ARRAYS
      T1 =>TB%ADR( 1)%P
      T2 =>TB%ADR( 2)%P
      T3 =>TB%ADR( 3)%P
      T4 =>TB%ADR( 4)%P
      T5 =>TB%ADR( 5)%P
      T6 =>TB%ADR( 6)%P
      T7 =>TB%ADR( 7)%P
      T8 =>TB%ADR( 8)%P
      T9 =>TB%ADR( 9)%P
      T10 =>TB%ADR( 10)%P
      T11 =>TB%ADR( 11)%P
      T12 =>TB%ADR( 12)%P
!
! WORKING ARRAY (SIZE NPTFR)
!
      NTRBD = 4
!
!     TBBD STORES TBD1,TBD2,...
!
      CALL ALLBLO(TBBD ,'TBBD  ')
!
      CALL BIEF_ALLVEC_IN_BLOCK(TBBD,NTRBD,1,'TBD   ',IELMB,1,2,MESH)
!
!     ALIASES FOR THE FIRST 4 WORKING ARRAYS IN THE BLOCK TBBD
!
      TBD1 =>TBBD%ADR( 1)%P
      TBD2 =>TBBD%ADR( 2)%P
      TBD3 =>TBBD%ADR( 3)%P
      TBD4 =>TBBD%ADR( 4)%P
!
! MATRICES
!
      CALL BIEF_ALLMAT(AM1,'AM1   ',IELM,IELM,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(AM2,'AM2   ',IELM,IELM,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(AM3,'AM3   ',IELM,IELM,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(BM1,'BM1   ',IELM,IELM,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(BM2,'BM2   ',IELM,IELM,CFG,'Q','Q',MESH)
! BOUNDARY MATRIX
      CALL BIEF_ALLMAT(MBOR,'MBOR  ',IELMB,IELMB,CFGBOR,'Q','Q',MESH)
!
! SECOND MEMBERS
!
      CALL BIEF_ALLVEC(1,CV1,'CV1   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CV2,'CV2   ',IELM, 1 , 2 ,MESH)
!
! ARRAYS FOR THE DISSIPATION :
!
      CALL BIEF_ALLVEC(1,MU    ,'MU    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MU2   ,'MU2   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,QB    ,'QB    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,HMU   ,'HMU   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,HMUANC,'HMUANC',IELM, 1 , 2 ,MESH)
!
! ARRAYS FOR RADIATION STRESSES
!
      CALL BIEF_ALLVEC(1,SXX   ,'SXX   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,SXY   ,'SXY   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,SYY   ,'SYY   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,FX    ,'FX    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,FY    ,'FY    ',IELM, 1 , 2 ,MESH)
!
! ARRAYS FOR MEAN PERIODS
!
      CALL BIEF_ALLVEC(1,T01   ,'T01   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,T02   ,'T02   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,TM    ,'TM    ',IELM, 1 , 2 ,MESH)
!
!
! MASKS FOR BOUNDARY NODES
!
      CALL BIEF_ALLVEC(1,MASK1,'MASK1 ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MASK2,'MASK2 ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MASK3,'MASK3 ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MASK4,'MASK4 ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MASK5,'MASK5 ',IELMB, 1 , 2 ,MESH)

!
!_V5P6 : CORRECTION FOR LINUX
      CALL BIEF_ALLVEC(1,MASKEL,'MASKEL',NELMAX,1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,SBID  ,'SBID  ' ,1    ,1, 0 ,MESH)
!_V5P6 : END OF CORRECTION FOR LINUX
!
! BLOCK OF MATRICES IN BERKHO
!
      CALL ALLBLO(MAT,'MAT   ')
      CALL ADDBLO(MAT,AM1)
      CALL ADDBLO(MAT,BM1)
      CALL ADDBLO(MAT,BM2)
      CALL ADDBLO(MAT,AM2)
!
!  BLOCK OF  UNKNOWNS IN BERKHO
!
      CALL ALLBLO(UNK,'UNK   ')
      CALL ADDBLO(UNK,PHIR)
      CALL ADDBLO(UNK,PHII)
!
!  BLOCK OF SECOND MEMBERS IN BERKHO
!
      CALL ALLBLO(RHS,'RHS   ')
      CALL ADDBLO(RHS,CV1)
      CALL ADDBLO(RHS,CV2)
!
! ARRAY STORING DISCRETISED DIRECTIONS FOR MULTIDIRECTIONAL
! RANDOM WAVES
!
      IF(ALEMUL) THEN
        CALL BIEF_ALLVEC(1,DALE,'DALE  ',NDALE,1,0,MESH)
      ENDIF
!
! ARRAYS AT THE USER'S DISPOSAL
!
      CALL ALLBLO(PRIVE ,'PRIVE ')
!
      IF(NPRIV.GT.0) THEN
!       TO BE OPTIMISED (SPACE LOST FOR 4-NPRIV ARRAYS)
!       THESE ARRAYS MUST EXIST BUT CAN BE EMPTY
        CALL BIEF_ALLVEC_IN_BLOCK(PRIVE,MAX(NPRIV,4),
     &                            1,'PRIV  ',IELM,1,2,MESH)
      ELSE
        CALL BIEF_ALLVEC_IN_BLOCK(PRIVE,4,1,'PRIV  ',0,1,2,MESH)
      ENDIF
!
!
!
! --> ER : START
! --> FLOW
!      IF (COURANT) THEN
         CALL BIEF_ALLVEC(1, UC ,'UC     ',IELM, 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1, VC ,'VC     ',IELM, 1 , 2 ,MESH)
! --> RELATIVE ANGULAR FREQUENCY
         CALL BIEF_ALLVEC(1, WR ,'WR     ',IELM, 1 , 2 ,MESH)
! --> INTERMEDIATE REAL VECTOR: WAVE VECTOR AND ERROR
        CALL BIEF_ALLVEC(1, KN1 ,'KN1     ',IELM, 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1, KN2 ,'KN2     ',IELM, 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1, KNANC1 ,'KNANC1     ',IELM, 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1, KNANC2 ,'KNANC2     ',IELM, 1 , 2 ,MESH)
!      ENDIF
! --> ER : END
!  END FOR REALS
!
!
!_______________________________________________________________________
!
!                         * INTEGER ARRAYS *
!_______________________________________________________________________
!
!
      CALL BIEF_ALLVEC(2,LIUBOR,'LIUBOR',IELMB,1,1,MESH)
      CALL BIEF_ALLVEC(2,LIVBOR,'LIVBOR',IELMB,1,1,MESH)
      CALL BIEF_ALLVEC(2,LIHBOR,'LIHBOR',IELMB,1,1,MESH)
! BOUNDARY_COLOUR TABLE USED IN PARALLEL      
      CALL BIEF_ALLVEC(2,BOUNDARY_COLOUR,'BDNCOL',IELMB,1,1,MESH)
! BIDON TABLE FOR LECLIM      
      CALL BIEF_ALLVEC(2,ITB1,'ITB1  ',IELMB,1,1,MESH)
!
      
      CALL BIEF_ALLVEC(2,NUMLIQ,'NUMLIQ',IELMB,1,1,MESH)
!
      CALL BIEF_ALLVEC(2,IT1   ,'IT1   ',   10,1,2,MESH)
      CALL BIEF_ALLVEC(2,IT2   ,'IT2   ',   10,1,2,MESH)
      CALL BIEF_ALLVEC(2,IT3   ,'IT3   ',   10,1,2,MESH)
!
      CALL BIEF_ALLVEC(2,LIDIR ,'LIDIR ',IELMB,2,1,MESH)
!
! BUILDS THE BLOCK THAT CONNECTS A VARIABLE NAME
! TO ITS ARRAY
!
      CALL ALLBLO(VARSOR,'VARSOR')
! 01
      IF (ALEMON .OR. ALEMUL) THEN
         CALL ADDBLO(VARSOR,HALE)
      ELSE
         CALL ADDBLO(VARSOR,HHO)
      ENDIF
! 02
      CALL ADDBLO(VARSOR,PHAS)
! 03
      CALL ADDBLO(VARSOR,U0)
! 04
      CALL ADDBLO(VARSOR,V0)
! 05
      CALL ADDBLO(VARSOR,S)
! 06
      CALL ADDBLO(VARSOR,ZF)
! 07
      CALL ADDBLO(VARSOR,H)
! 08
      CALL ADDBLO(VARSOR,C)
! 09
      CALL ADDBLO(VARSOR,CG)
! 10
      CALL ADDBLO(VARSOR,K)
! 11
      CALL ADDBLO(VARSOR,PHIR)
! 12
      CALL ADDBLO(VARSOR,PHII)
! 13
      CALL ADDBLO(VARSOR,PRIVE%ADR(1)%P)
! 14
      CALL ADDBLO(VARSOR,PRIVE%ADR(2)%P)
! 15
      CALL ADDBLO(VARSOR,PRIVE%ADR(3)%P)
! 16
      CALL ADDBLO(VARSOR,PRIVE%ADR(4)%P)
! 17
      CALL ADDBLO(VARSOR,T01)
! 18
      CALL ADDBLO(VARSOR,T02)
! 19
      CALL ADDBLO(VARSOR,TM)
! 20
      CALL ADDBLO(VARSOR,FX)
! 21
      CALL ADDBLO(VARSOR,FY)
! 22
      CALL ADDBLO(VARSOR,INCI)
! 23
      CALL ADDBLO(VARSOR,QB)
! 24
      CALL ADDBLO(VARSOR,SXX)
! 25
      CALL ADDBLO(VARSOR,SXY)
! 26
      CALL ADDBLO(VARSOR,SYY)
!
!***********************************************************************
!
! CHECKS :
!
      ISTOP=0
      IF(ISTOP.EQ.1) STOP
!
! WRITES OUT :
!
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,22)
         IF(LNG.EQ.2) WRITE(LU,23)
      ENDIF
22    FORMAT(1X,///,21X,'****************************************',/,
     &21X,              '* FIN DE L''ALLOCATION DE LA MEMOIRE  : *',/,
     &21X,              '****************************************',/)
23    FORMAT(1X,///,21X,'*************************************',/,
     &21X,              '*    END OF MEMORY ORGANIZATION:    *',/,
     &21X,              '*************************************',/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END





!                    ****************
                     SUBROUTINE PHBOR
!                    ****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    TRANSLATES THE BOUNDARY CONDITIONS SPECIFIED
!+                BY THE USER,
!+                I.E. COMPUTES THE COEFFICIENTS
!+                APHIR, APHII, ... FOR EACH BOUNDARY SEGMENT.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
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
!history  C.PEYRARD (LNHE)
!+        01/06/2012
!+        V6P0
!+   KSORT   : end of application to neighbours
!+   KINC    : end of applicatin to the I+1 node
!+   SEGMENT : If a segment links a solid node to a liquid node, 
!+             this segment is regarded as solid. 
!+   BUG correction         : Full KINC boundaries taken into account
!+   NEW boundary condition : incident potential can be given by the user 
!+   Parallel correction    : - End of HBT,CGT,CTT,KT,XT,YT tables
!+                              only   HB%R,CG%R, etc... are used.
!+                            - No use of NCSIZE variable. 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      LOGICAL TRVDEB
!
      INTEGER I,IPREC,IG,IG0,IGP1,IFR
!
! 
      DOUBLE PRECISION GRE, GIM
      DOUBLE PRECISION DDXGRE,DDYGRE,DDXGIM,DDYGIM
!       

      DOUBLE PRECISION PI,DEGRAD
      DOUBLE PRECISION AUXI1,AUXI2,PHASOI,AUXIC,AUXIS,RADDEG,BID

      INTRINSIC COS,SIN
!
!-----------------------------------------------------------------------
!
      PARAMETER( PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0 )
      PARAMETER( RADDEG = 180.D0 / PI )
!
!-----------------------------------------------------------------------
!
! INITIALISES LIDIR TO KSORT (A DIFFERENT VALUE FROM KENT)
! IN ORDER NOT TO TAKE NODES IMPOSED IN PRIDIH INTO ACCOUNT,
! WHEN IT HAS NOT BEEN REQUESTED.
!
      DO 501 I=1,NPTFR
         LIDIR%I(I) = KSORT
!        BEWARE: IT IS ASSUMED HERE THAT NPTFRX=NPTFR
         LIDIR%I(I+NPTFR) = KSORT
         IF (LIHBOR%I(I).EQ.KENT) THEN
           LIHBOR%I(I) = KINC
         ENDIF
         APHI1B%R(I) = 0.D0
         BPHI1B%R(I) = 0.D0
         CPHI1B%R(I) = 0.D0
         DPHI1B%R(I) = 0.D0
         APHI2B%R(I) = 0.D0
         BPHI2B%R(I) = 0.D0
         CPHI2B%R(I) = 0.D0
         DPHI2B%R(I) = 0.D0
         APHI3B%R(I) = 0.D0
         BPHI3B%R(I) = 0.D0
         CPHI3B%R(I) = 0.D0
         DPHI3B%R(I) = 0.D0
         APHI4B%R(I) = 0.D0
         BPHI4B%R(I) = 0.D0
         CPHI4B%R(I) = 0.D0
         DPHI4B%R(I) = 0.D0	
         CGRX1B%R(I) = 0.D0
         CGRY1B%R(I) = 0.D0
         DGRX1B%R(I) = 0.D0
         DGRY1B%R(I) = 0.D0	

 501  CONTINUE
!
!-----------------------------------------------------------------------
!
      DO 1001 I=1,NPTFR
!
!        ********************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE I
!        ********************************
!
         IG   = MESH%NBOR%I(I)
!
!        ******************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
!        ******************************************
!
         IG0  = MESH%NBOR%I(MESH%KP1BOR%I(I+NPTFR))
!
!        ****************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
!        ****************************************
!
         IGP1 = MESH%NBOR%I(MESH%KP1BOR%I(I))

!        -------------------------------------------------
!              COEFFICIENTS FOR A INCIDENT WAVE
!        -------------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KINC) THEN
          AUXIC      = COS(TETAB%R(I)*DEGRAD)
          AUXIS      = SIN(TETAB%R(I)*DEGRAD)
          AUXI1      = GRAV/OMEGA * HB%R(I)/2.D0 

!---------- INCIDENT WAVE --> INCIDENT POTENTIAL  (REAL, IMAGINAR)
          GRE= AUXI1*SIN( ALFAP%R(I)*DEGRAD )
          GIM=-AUXI1*COS( ALFAP%R(I)*DEGRAD )
! --------- INCIDENT WAVE --> GRADIENTS           (REAL, IMAGINAR)
          DDXGRE= AUXI1*COS(ALFAP%R(I)*DEGRAD )*AUXIC*K%R(IG)
          DDYGRE= AUXI1*COS(ALFAP%R(I)*DEGRAD )*AUXIS*K%R(IG)
          DDXGIM= AUXI1*SIN(ALFAP%R(I)*DEGRAD )*AUXIC*K%R(IG)
          DDYGIM= AUXI1*SIN(ALFAP%R(I)*DEGRAD )*AUXIS*K%R(IG)

! --------- COEFFICIENTS
! -- MATRIX AM AND BM COEFFICIENTS
          APHI1B%R(I) = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                 * COS(TETAP%R(I)*DEGRAD)
          BPHI1B%R(I) = 0.D0

! -- SECOND MEMBER CV1 AND CV2 COEFFICIENTS
! ----  i * K * Gamma (multiplied by "- cos THETAP" in BERKHO)
          CPHI1B%R(I)  = ( -GIM*K%R(IG) ) *C%R(IG)*CG%R(IG)
          DPHI1B%R(I)  = (  GRE*K%R(IG) ) *C%R(IG)*CG%R(IG)

! ---- GRAD(Gamma) will be used in BERKHO...
          CGRX1B%R(I)=   ( DDXGRE ) *C%R(IG)*CG%R(IG)
          CGRY1B%R(I)=   ( DDYGRE ) *C%R(IG)*CG%R(IG)
          DGRX1B%R(I)=   ( DDXGIM ) *C%R(IG)*CG%R(IG) 
          DGRY1B%R(I)=   ( DDYGIM ) *C%R(IG)*CG%R(IG)
         ENDIF

!
!        -------------------------------------------------
!             COEFFICIENTS FOR AN INCIDENT POTENTIAL
!        -------------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KPOT) THEN

!------------ POTENTIAL (REAL, IMAGINAR)            
          GRE=PRB%R(I)
          GIM=PIB%R(I)
	 
! ----------- GRADIENTS (REAL, IMAGINAR)
          DDXGRE= DDXPRB%R(I)
          DDYGRE= DDYPRB%R(I)
          DDXGIM= DDXPIB%R(I)
          DDYGIM= DDYPIB%R(I)

! ----------- COEFFICIENTS
! -- MATRIX AM AND BM COEFFICIENTS
          APHI1B%R(I) = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                 * COS(TETAP%R(I)*DEGRAD)
          BPHI1B%R(I) = 0.D0

! -- SECOND MEMBER CV1 AND CV2 COEFFICIENTS
! ----  i * K * Gamma (multiplied by "- cos THETAP" in BERKHO)
          CPHI1B%R(I)  = ( -GIM*K%R(IG) ) *C%R(IG)*CG%R(IG)
          DPHI1B%R(I)  = (  GRE*K%R(IG) ) *C%R(IG)*CG%R(IG)

! ---- GRAD(Gamma) will be used in BERKHO...
          CGRX1B%R(I)=   ( DDXGRE ) *C%R(IG)*CG%R(IG)
          CGRY1B%R(I)=   ( DDYGRE ) *C%R(IG)*CG%R(IG)
          DGRX1B%R(I)=   ( DDXGIM ) *C%R(IG)*CG%R(IG) 
          DGRY1B%R(I)=   ( DDYGIM ) *C%R(IG)*CG%R(IG)

         ENDIF

!        -------------------------------------------------
!        COEFFICIENTS FOR A FREE EXIT BOUNDARY SEGMENT
!        -------------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KSORT) THEN
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(I)*DEGRAD)
!
            BPHI2B%R(I)  = 0.D0
!
            CPHI2B%R(I)  = 0.D0
!
            DPHI2B%R(I)  = 0.D0
!
         ELSE
            APHI2B%R(I)  = 0.D0
!
            BPHI2B%R(I)  = 0.D0
!
            CPHI2B%R(I)  = 0.D0
!
            DPHI2B%R(I)  = 0.D0
!
         ENDIF
!
!        -------------------------------------------
!        COEFFICIENTS FOR A SOLID BOUNDARY SEGMENT
!        -------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &      COS(TETAP%R(I)*DEGRAD) /
     &      ( 1.D0 + RP%R(I)*RP%R(I) +
     &        2.D0*RP%R(I)*COS(ALFAP%R(I)*DEGRAD) )
!
          APHI3B%R(I) = - (1.D0 - RP%R(I) * RP%R(I) ) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(I)*SIN(ALFAP%R(I)*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &      COS(TETAP%R(MESH%KP1BOR%I(I))*DEGRAD) /
     &      (1.D0 + RP%R(MESH%KP1BOR%I(I))*RP%R(MESH%KP1BOR%I(I))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD))
!
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I))*
     &      RP%R(MESH%KP1BOR%I(I))) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I))
     &                * SIN(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I+NPTFR)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &     COS(TETAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) /
     &     (1.D0 + RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      *RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD))
!
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &      RP%R(MESH%KP1BOR%I(I+NPTFR))) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      * SIN(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ELSE
          APHI3B%R(I)  = 0.D0
!
          BPHI3B%R(I)  = 0.D0
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ENDIF
!
!        -------------------------------------------------
!        COEFFICIENTS FOR AN IMPOSED WAVE BOUNDARY SEGMENT
!        -------------------------------------------------
!DA      -----------------------------------
!DA      KEPT FOR MEMORY !
!DA      -----------------------------------
!DA
!DA         IF (LIHBOR(I).EQ.KENT) THEN
!DA         AUXIC      = COS(TETAB(I)*DEGRAD)
!DA         AUXIS      = SIN(TETAB(I)*DEGRAD)
!DA         AUXI1      = GRAV/OMEGA * HB(I)/2.D0 * C(IG) * CG(IG) *
!DA     *                K(IG) * ( AUXIC *XSGBOR(I) +
!DA     *                          AUXIS *YSGBOR(I) )
!DA         AUXI2      = K(IG) * ( X(IG)*AUXIC +
!DA     *                          Y(IG)*AUXIS )
!DA
!DA         APHI4B(I)  = 0.D0
!DA
!DA         BPHI4B(I)  = 0.D0
!DA
!DA         CPHI4B(I)  = AUXI1 * COS( AUXI2 )
!DA
!DA         DPHI4B(I)  = AUXI1 * SIN( AUXI2 )
!DA
!DA       VALUES IMPOSED AT THE NODES OF A KENT SEGMENT
!DA         LIDIR(I)         = KENT
!DA
!DA         AUXI1 = GRAV/OMEGA * HB(I)/2.D0
!DA         AUXI2 = K(IG) * (X(IG)*AUXIC +
!DA     *                    Y(IG)*AUXIS )
!DA
!DA            PHIRB(I) =   AUXI1 * SIN( AUXI2 )
!DA            PHIIB(I) = - AUXI1 * COS( AUXI2 )
!DA         ENDIF
!
!
 1001 CONTINUE
!-----------------------------------------------------------------------
!
!
      RETURN
      END

C-----------------------------------------------------------------
C-----------------------------------------------------------------
C-----------------------------------------------------------------

C------------------------------------------------------------------------------------
C---------------------------------- BERKHO : les CL issues de PHBOR sont modifiees
C------------------------------------------------------------------------------------

!                    *****************
                     SUBROUTINE BERKHO
!                    *****************
!
     &(LT)
!
!***********************************************************************
! ARTEMIS   V6P1                                   31/05/2011
!***********************************************************************
!
!brief    SOLVES THE BERKHOFF EQUATION MODIFIED BY
!+                THE INTRODUCTION OF DISSIPATION TERMS.
!code
!+      DIV (C*CG*GRAD(PHI)) + C*CG*( K**2 + I*K*MU ) * PHI = 0
!+                                           ------
!+
!+ PHI IS A COMPLEX FUNCTION (REAL COMPONENT: PHIR AND IMAGINARY
!+ COMPONENT: PHII)
!+
!+ MU IS A DISSIPATION COEFFICIENT (A PRIORI UNKNOWN)
!+
!+ THE BOUNDARY CONDITIONS COUPLE THE EQUATIONS IN PHIR AND PHII
!+ THEY ARE:
!+
!+ D (PHI) /DN - I*K*PHI = D (F) /DN - I*K*F (N: EXTERNAL NORMAL)
!+ FOR A LIQUID BOUNDARY WITH INCIDENT WAVE CONDITION DEFINED
!+ BY THE POTENTIAL F (F=0 FOR A FREE EXIT)
!+
!+ D (PHI) /DN - I* (1-R*EXP (I*ALFA))/(1 + R*EXP (I*ALFA))*K*COS (TETA) *PHI = 0
!+ FOR A SOLID BOUNDARY, WITH WALL REFLEXION COEFFICIENT: R,
!+ ANGLE OF INCIDENCE OF THE WAVES ON THE WALL: TETA, AND DEPHASING
!+ CAUSED BY THE WALL: ALFA.
!+
!+ THUS GENERALLY :
!+ D(PHIR)/DN = APHIRB*PHII + BPHIRB*PHIR + CPHIRB
!+ D(PHII)/DN =-APHIRB*PHIR + BPHIRB*PHII + DPHIRB
!+
!+
!+ AFTER VARIATIONAL FORMULATION :
!+
!+         (  AM1          BM1     )  ( PHIR )   ( CV1 )
!+         (                       )  (      ) = (     )
!+         (                       )  (      )   (     )
!+         (  -BM1         AM1     )  ( PHII )   ( CV2 )
!+
!+           /
!+ AM1 =    / C*CG * GRAD(PSII)*GRAD(PSIJ) DS
!+         /S
!+
!+           /
!+       -  / OMEGA**2 * CG/C * PSII*PSIJ  DS
!+         /S
!+
!+           /
!+       -  /  BPHIRB * PSII*PSIJ  DB
!+         /B
!+
!+           /                         /
!+ BM1 =  - /  APHIR * PSII*PSIJ DB + /  C*CG* K * MU * PSII * PSIJ DS
!+         /B                        /S
!+
!+          /
!+ CV1 =   /   CPHIR * PSII DB
!+        /B
!+
!+          /
!+ CV2 =   /   CPHII * PSII DB
!+        /B
!+
!+
!+ WHERE S IS THE COMPUTATIONAL DOMAIN AND B ITS BOUNDARY
!+       PSII AND PSIJ ARE THE BASIC FUNCTIONS AT NODES I AND J
!+
!+ GIVEN THAT APHII=-APHIR, BM1 IS ALSO IN THE EQUATION IN PHII.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        04/06/1999
!+        V5P1
!+
!
!history
!+        02/04/2007
!+
!+   INVERSION OF THE SECOND EQUATION BEFORE CALL TO SOLVE IF DIRECT
!+   SOLVEUR IS USED
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
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
!| LT             |-->| INDICE OF THE CURRENT CALCULATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_ARTEMIS, EX_BERKHO => BERKHO
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I,LT,ITERMU

      DOUBLE PRECISION HM,HMUE,HEFF,ECRHMU,MODHMU
      DOUBLE PRECISION Q1,Q2,Q3
!
      DOUBLE PRECISION CBID,FFW
      DOUBLE PRECISION PI,DEGRAD,RADDEG
!
!-----------------------------------------------------------------------
!
      PARAMETER( PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0 )
      PARAMETER( RADDEG = 180.D0 / PI )
!
!-----------------------------------------------------------------------
!
      INTRINSIC ABS,MIN,MAX,LOG
      DOUBLE PRECISION P_DMAX
      EXTERNAL P_DMAX
      DOUBLE PRECISION TDEB1,TFIN1
!
!----------------------------------------------------------------------
!
! INITIALISES MU AND FW: SET TO 0
!         FOR THE FIRST ITERATION
!
      ITERMU=0
      IF (LT.EQ.0) THEN
         CALL OS( 'X=C     ' , MU , SBID , SBID , 0.D0 )
         CALL OS( 'X=C     ' , FW , SBID , SBID , 0.D0 )
      ENDIF
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
! ITERATIVE LOOP ON THE VARIABLE MU (DISSIPATION)
!
!
!     =========================================
!
!     COMPUTES MATRICES AND SECOND MEMBERS
!
!     =========================================
!
!     ---------------------------
!     DIFFUSION MATRIX FOR AM1
!     ---------------------------
! CER
98    CONTINUE
! CER
      CALL OS( 'X=YZ    ' , T1 , C , CG , CBID )
      CALL MATRIX(AM1,'M=N     ','MATDIF          ',IELM,IELM,
     &            1.D0,S,S,S,T1,T1,S,MESH,MSK,MASKEL)
!
!-----------------------------------------------------------------------
!
! PANCHANG, TO BE REVISITED: 7 IS GMRES
!
! THE DIFFUSION MATRIX USED FOR PRECONDITIONING IS STORED
! IF THE METHOD IS THAT OF PANCHANG ET AL. (ISOLVE(1) =7)
!
!     IF (ISOLVE(1).EQ.7) THEN
!
!        CALL OM('M=CN    ',AM3,AM1,Z,1.D0/(RELAX*(2.D0-RELAX)),MESH)
!
!     ENDIF
!
!-----------------------------------------------------------------------
!
!     -----------------------
!     MASS MATRIX FOR AM1
!     -----------------------
!
!
      CALL OS( 'X=Y/Z   ' , T1 , CG , C , CBID )
!
! SECOND ORDER BOTTOM EFFECTS ? (IPENTCO > 0 --> T1 = T1*(1+F) )
! 0 : NO EFFECT /  1 : GRADIENT / 2 : CURVATURE /  3 : GRADIENT+CURVATURE
      IF ( (IPENTCO.GT.(0.5)).AND.(IPENTCO.LT.(3.5)) ) THEN
!       on modifie T2 T4 T5 T6 T7 T9 T8 T11 T12
        CALL PENTCO(IPENTCO)
!       T3 = 1+F  
        CALL OS( 'X=YZ    ' , T1 , T1 , T3 , CBID )
      ENDIF
!
      CALL MATRIX(AM2,'M=N     ','FMATMA          ', IELM , IELM ,
     &            OMEGA**2 , T1,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     --------------------------------------------------
!     COMPUTES DIFFUSION MATRIX - MASS MATRIX
!     --------------------------------------------------
!
      CALL OM( 'M=M+CN  ' , AM1 , AM2 , C , -1.D0 , MESH )
!
!     --------------------------------
!     ADDS THE BOUNDARY TERM TO AM1
!     --------------------------------
!
!     (HERE T1 IS A DUMMY STRUCTURE)
!
!        ------------------------------
!        BOUNDARY TERM: INCIDENT WAVE
!     ------------------------------
!
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI1B,S,S,S,S,S,MESH,.TRUE.,MASK1)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
!     ------------------------------
!     BOUNDARY TERM: FREE EXIT
!     ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
!     ------------------------------
!     BOUNDARY TERM : SOLID BOUNDARY
!     ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
!     ------------------------------
!     BOUNDARY TERM: IMPOSED WAVE
!     ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
!
!     ------------------------------
!      BOUNDARY TERM: INCIDENT POTENTIAL
!     ------------------------------
!
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI1B,S,S,S,S,S,MESH,.TRUE.,MASK5)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF

!
!     ---------------------
!     SECOND MEMBERS : CV1
!     ---------------------
!
         CALL OS( 'X=C     ' , CV1, SBID , SBID , 0.D0 )
!     ------------------------------
!     BOUNDARY TERM: INCIDENT WAVE
!     ------------------------------
C       --- CALCUL DE i COS(TETAP) GAMMA         
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,CPHI1B,T2,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
	 
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK1)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )

!        --- CALCUL DE GRAD(Gamma).n : REEL
          CALL OS( 'X=Y     ' , T2,CGRX1B,SBID,0.D0)
          CALL OS( 'X=Y     ' , T3,CGRY1B,SBID,0.D0)
          CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
          CALL OS( 'X=C     ' , T4, SBID , SBID , 1.D0 )
          IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,T4,S,S,T2,T3,S,MESH,.TRUE.,MASK1)
          END IF
          CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
!     ---------------------------------
!     BOUNDARY TERM: INCIDENT POTENTIAL
!     ---------------------------------
!       --- CALCUL DE i COS(TETAP) GAMMA         
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,CPHI1B,T2,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK5)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )

!       --- CALCUL DE GRAD(Gamma).n : REEL
          CALL OS( 'X=Y     ' , T2,CGRX1B,SBID,0.D0)
          CALL OS( 'X=Y     ' , T3,CGRY1B,SBID,0.D0)
          CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
          CALL OS( 'X=C     ' , T4, SBID , SBID , 1.D0 )
          IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,T4,S,S,T2,T3,S,MESH,.TRUE.,MASK5)
          END IF
          CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )


!     ------------------------------
!     BOUNDARY TERM: FREE EXIT
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,CPHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
!     END IF
            CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         END IF
!     ------------------------------
!     BOUNDARY TERM: SOLID BOUNDARY
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,CPHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
            CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         END IF
!     ------------------------------
!     BOUNDARY TERM: IMPOSED WAVE
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,CPHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
            CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         END IF
!

CCP
!         IF (NCSIZE.GT.1) THEN
!           CALL PARCOM(CV1,2,MESH)
!	  ENDIF
CCP


!     ---------------------
!     SECOND MEMBERS : CV2
!     ---------------------
!
         CALL OS( 'X=C     ' , CV2, SBID , SBID , 0.D0 )
!     ------------------------------
!     BOUNDARY TERM: INCIDENT WAVE
!     ------------------------------
C     --- CALCUL DE i COS(TETAP) GAMMA : IMAGINAIRE
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,DPHI1B,T2,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK1)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
	 
!        --- CALCUL DE GRAD(Gamma).n : IMAGINAIRE
          CALL OS( 'X=Y     ' , T2,DGRX1B,SBID,0.D0)
          CALL OS( 'X=Y     ' , T3,DGRY1B,SBID,0.D0)
          CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
          CALL OS( 'X=C     ' , T4, SBID , SBID , 1.D0 )

          IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,T4,S,S,T2,T3,S,MESH,.TRUE.,MASK1)
          END IF
          CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
!     ---------------------------------
!     BOUNDARY TERM: INCIDENT POTENTIAL
!     ---------------------------------
!     --- CALCUL DE i COS(TETAP) GAMMA : IMAGINAIRE
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,DPHI1B,T2,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK5)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
	 
!     --- CALCUL DE GRAD(Gamma).n : IMAGINAIRE
          CALL OS( 'X=Y     ' , T2,DGRX1B,SBID,0.D0)
          CALL OS( 'X=Y     ' , T3,DGRY1B,SBID,0.D0)
          CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
          CALL OS( 'X=C     ' , T4, SBID , SBID , 1.D0 )

          IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,T4,S,S,T2,T3,S,MESH,.TRUE.,MASK5)
          END IF
          CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
!
!    ------------------------------
!    BOUNDARY TERM: FREE EXIT
!    ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,DPHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
!     ------------------------------
!     BOUNDARY TERM: SOLID BOUNDARY
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,DPHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
            CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
         END IF
!     ------------------------------
!     BOUNDARY TERM: IMPOSED WAVE
!     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,DPHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
            CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
         END IF
CCP
!          IF (NCSIZE.GT.1) THEN
!           CALL PARCOM(CV2,2,MESH)
!	  ENDIF
CCP

!     ----------------------------------------------------------
!     COMPUTES THE MATRIX BM1 FOR THE MU VALUES SPECIFIED
!     FOR THE ITERATION 'ITERMU'
!     ----------------------------------------------------------
!
      CALL OS( 'X=YZ    ' , T1 , C  , CG , CBID )
      CALL OS( 'X=YZ    ' , T2 , K  , MU , CBID )
      CALL OS( 'X=YZ    ' , T1 , T1 , T2 , CBID )
      CALL MATRIX(BM1,'M=N     ','FMATMA          ', IELM , IELM ,
     &            1.D0 , T1,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     -------------------------------------------
!     ADDS THE BOUNDARY TERM TO BM1
!     -------------------------------------------
!
      IF (NPTFR .GT. 0) THEN
!        ------------------------------
!        BOUNDARY TERM: INCIDENT WAVE
!        ------------------------------
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,APHI1B,S,S,S,S,S,MESH,.TRUE.,MASK1)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
!     ------------------------------
!        BOUNDARY TERM: FREE EXIT
!        ------------------------------
      IF (NPTFR .GT. 0) THEN
            CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &           -1.D0,APHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
!        ------------------------------
!        BOUNDARY TERM: SOLID BOUNDARY
!        ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,APHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
!        ------------------------------
!        BOUNDARY TERM: IMPOSED WAVE
!        ------------------------------
      IF (NPTFR .GT. 0) THEN
            CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &           -1.D0,APHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
            CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
!        ------------------------------
!        BOUNDARY TERM: INCIDENT POTENTIAL
!        ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,APHI1B,S,S,S,S,S,MESH,.TRUE.,MASK5)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF

!     ---------
!     AM2 = AM1
!     ---------
!
      CALL OM( 'M=N     ' , AM2 , AM1 , SBID , CBID , MESH )
!
!     --------------------------
!     BM1 BECOMES NONSYMMETRICAL
!     --------------------------
!
      CALL OM( 'M=X(M)  ' , BM1 , BM1 , SBID , CBID , MESH )
!
!     ----------------------------
!     TRIES MASS-LUMPING OF BM1
!     ----------------------------
!
!     MASLU = 1.D0
!     CALL LUMP(T1,BM1,MESH,XMESH,MASLU,MSK,MASKEL)
!     CALL OM( 'M=CN    ' , BM1 , BM1 , T1 , 1.D0-MASLU , MESH )
!     CALL OM( 'M=M+D   ' , BM1 , BM1 , T1 , C          , MESH )
!
!     ----------
!     BM2 = -BM1
!     ----------
!
      CALL OM( 'M=CN    ' , BM2 , BM1 , C , -1.D0 , MESH )
!
!     =======================================
!
!     TAKES INTO ACCOUNT DIRICHLET POINTS
!
!     =======================================
!
      IF (DEFERL .OR. FROTTE) THEN
         IF (LNG.EQ.1) WRITE(LU,220) ITERMU+1
         IF (LNG.EQ.2) WRITE(LU,221) ITERMU+1
 220     FORMAT(/,1X,'SOUS-ITERATION NUMERO :',1X,I3,/)
 221     FORMAT(/,1X,'SUB-ITERATION NUMBER :',1X,I3,/)
      ENDIF
      CALL DIRICH(UNK,MAT,RHS,PHIB,LIDIR%I,TB,MESH,KENT,MSK,MASKEL)
!
!     ===============================================================
!
!     INHIBITS POSSIBLE DIAGONAL PRECONDITIONING
!     IF AN ELEMENT OF DAM1 IS NEGATIVE OR NULL
!
!     ===============================================================
!
      CALL CNTPRE(AM1%D%R,NPOIN,SLVART%PRECON,SLVART%PRECON)
!      IF (LNG.EQ.1) WRITE(LU,230) SLVART%PRECON
!      IF (LNG.EQ.2) WRITE(LU,231) SLVART%PRECON
! 230  FORMAT(/,1X,'PRECONDITIONNEMENT APRES CONTROLE :',1X,I3)
! 231  FORMAT(/,1X,'PRECONDITIONNING AFTER CONTROL :',1X,I3)
!
!     ==========================================================
!
!     PRECONDITIONING BLOCK-DIAGONAL:
!                 THE MATRICES BECOME NONSYMMETRICAL.
!
!     ==========================================================
!
      IF (3*(SLVART%PRECON/3).EQ.SLVART%PRECON) THEN
       CALL OM( 'M=X(M)  ' , AM1 , AM1 , SBID , CBID , MESH )
        CALL OM( 'M=X(M)  ' , AM2 , AM2 , SBID , CBID , MESH )
      ENDIF
!
!     ==============================
!
!     SOLVES THE LINEAR SYSTEM
!
!     ==============================
!
!     ----------------------------
!     INITIALISES THE UNKNOWN
!     ----------------------------
!
      IF(ITERMU.EQ.0.AND.LT.EQ.0) THEN
        CALL LUMP(T1,AM1,MESH,1.D0)
        CALL OS( 'X=Y/Z   ' , PHIR , CV1 , T1 , CBID )
        CALL LUMP(T1,AM2,MESH,1.D0)
        CALL OS( 'X=Y/Z   ' , PHII , CV2 , T1 , CBID )
      ENDIF
!
      IF (LNG.EQ.1) WRITE(LU,240)
      IF (LNG.EQ.2) WRITE(LU,241)
 240  FORMAT(/,1X,'RESOLUTION DU SYSTEME LINEAIRE (SOLVE)',/)
 241  FORMAT(/,1X,'LINEAR SYSTEM SOLVING (SOLVE)',/)
!
      IF(SLVART%SLV.EQ.8 .OR. SLVART%SLV.EQ.9 ) THEN
!
!      CHANGES THE SIGN OF THE SECOND EQUATION
!
       CALL OS('X=-Y    ',X=MAT%ADR(3)%P%D,Y=MAT%ADR(3)%P%D)
       CALL OS('X=-Y    ',X=MAT%ADR(4)%P%D,Y=MAT%ADR(4)%P%D)
       CALL OS('X=-Y    ',X=MAT%ADR(3)%P%X,Y=MAT%ADR(3)%P%X)
       CALL OS('X=-Y    ',X=MAT%ADR(4)%P%X,Y=MAT%ADR(4)%P%X)
       CALL OS('X=-Y    ',X=RHS%ADR(2)%P,Y=RHS%ADR(2)%P)
      ENDIF
!
!
      CALL SOLVE(UNK,MAT,RHS,TB,SLVART,INFOGR,MESH,AM3)
!
!
!     ============================================================
!
!     COMPUTES THE TOTAL DISSIPATION COEFFICIENT MU_DEFERL + MU_FROTTE
!                                                  (MU2)       (T1)
!     IF BREAKING OR BOTTOM FRICTION TAKEN INTO ACCOUNT
!     ============================================================
!
!
      IF (DEFERL .OR. FROTTE) THEN
         ECRHMU = 0.D0
!
!     --------------------------------------------
!     INITIALISES MU2 AND T3: SET TO 0
!     MU2: NEW DISSIPATION COEFFICIENT
!     T3: QB FOR THE CURRENT PERIOD
!     --------------------------------------------
!
         CALL OS( 'X=C     ' , MU2 , SBID , SBID , 0.D0 )
         CALL OS( 'X=C     ' , T3  , SBID , SBID , 0.D0 )
!
!        ----------------------------------------------------
!        COMPUTES THE WAVE HEIGHT HMU CORRESPONDING TO
!        THE SOLUTION OF THE SYSTEM
!
         CALL OS( 'X=N(Y,Z)', T1  , PHIR , PHII , CBID )
         CALL OS( 'X=CY    ', HMU , T1   , SBID , 2.D0*OMEGA/GRAV )
!
!        --------------
!        IF BREAKING
!        --------------
!
         IF (DEFERL) THEN
!
!        ------------------------------------------------------
!        TESTS IF HMU > HM (THERE IS BREAKING) OR NOT,
!        AND CALCULATES MU2 ACCORDING TO DALLY OR BATTJES & JANSSEN
!        (IF REGULAR WAVES)
!        ------------------------------------------------------
!
            IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
               DO 20 I = 1,NPOIN
                  HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
!
!     HMUE = HMU/SQRT(2)
!
                  HMUE = HMU%R(I)/1.4142D0
                  HEFF=MIN(HMUE,HM)
                  HEFF=MAX(HEFF,1.D-5)
                  Q1 = 1.D-10
                  Q2 = (HEFF/HM)**2
!     ADDED BY JMH BECAUSE OF THE LOG FUNCTION, LATER ON
                  Q2 = MAX(Q2,1.D-9)
!
!     ------------
!     COMPUTES QB
!     ------------
!
                  CALL CALCQB(Q1,Q2,Q3)
!
!     ALGORITHM SPECIFIC TO REGULAR WAVES
!     FOR THE COMPUTATION OF THE RATE OF BREAKING
!
                  IF (ITERMU.EQ.0) THEN
                     IF (Q3.LT.0.19D0) THEN
                        T3%R(I) = 0.D0
                     ELSE
                        T3%R(I) = 1.D0
                     ENDIF
!
!                 T3 COMPUTED AT ITERMU = 0
!                 IS TEMPORARILY STORED IN QB
!
                     QB%R(I) = T3%R(I)
                  ELSE
                     IF (QB%R(I).EQ.1.D0) THEN
                        IF (Q3.LT.0.1D0) THEN
                           T3%R(I) = 0.D0
                        ELSE
                           T3%R(I) = 1.D0
                        ENDIF
                     ENDIF
                  ENDIF
 20            CONTINUE
!
!
!           --------------------------------
!           DALLY AND AL 1985
!           --------------------------------
!
               IF (IBREAK.EQ.2) THEN
                  DO 30 I = 1,NPOIN
                    HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
                     HEFF=MIN(HMU%R(I),HM)
                     HEFF=MAX(HEFF,1.D-5)
                     MU2%R(I)=T3%R(I)*KDALLY*
     &                    (1.D0-(GDALLY*H%R(I)/HEFF)**2)/H%R(I)
 30               CONTINUE
               ENDIF
!
!     -------------------------------------
!     BATTJES & JANSSEN 1978
!     -------------------------------------
!
               IF (IBREAK.EQ.1) THEN
                  DO 40 I = 1,NPOIN
                   HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
                     HEFF=MIN(HMU%R(I),HM)
                     HEFF=MAX(HEFF,1.D-5)
                     MU2%R(I) = T3%R(I)*2.D0*HEFF/(H%R(I)*CG%R(I)*PER)
 40               CONTINUE
               ENDIF
!
!     -------------------------------------------------------------
!     COMPUTES FIRST QB=T3, PROPORTION OF BREAKING OR BROKEN WAVES,
!     THEN MU2 ACCORDING TO B&J 78 (RANDOM SEAS)
!     -------------------------------------------------------------
!
            ELSE
               DO 50 I = 1,NPOIN
                  HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
!
!     HMUE = HMU/SQRT (2)
!
               HMUE = HMU%R(I)/1.4142D0
               HEFF=MIN(HMUE,HM)
               HEFF=MAX(HEFF,1.D-5)
               Q1 = 1.D-10
               Q2 = (HEFF/HM)**2
!     ADDED BY JMH BECAUSE OF THE LOG FUNCTION, LATER ON
               Q2 = MAX(Q2,1.D-9)
!
!              ------------
!              COMPUTES QB
!              ------------
!
               CALL CALCQB(Q1,Q2,Q3)
               T3%R(I) = Q3
!
!              -------------------------
!              COMPUTES MU2
!              -------------------------
!
               HEFF = MIN((HMU%R(I)/1.4142D0),HM)
               HEFF=MAX(HEFF,1.D-5)
               MU2%R(I)=ALFABJ*OMEGA*T3%R(I)*((HM/HEFF)**2)/
     &                (3.141592653589D0*CG%R(I))
 50         CONTINUE
!
         END IF
!
!        ------------------
!        END 'IF BREAKING'
!        ------------------
!
         ENDIF
!
!        --------------------------------
!        RE-INITIALISES T1 = 0 BECAUSE
!        T1 REPRESENTS MU_FROTTEMENT IN THE FOLLOWING
!        --------------------------------
!
         CALL OS( 'X=C     ' , T1 , C , CG , 0.D0 )
!
!        ---------------------
!        IF BOTTOM FRICTION
!        ---------------------
!
         IF (FROTTE) THEN
!
!           ------------------------------------------------
!           IF ENTFW=TRUE, THE FRICTION COEFFICIENT FW
!           IS THE SAME EVERYWHERE IN THE DOMAIN
!           ------------------------------------------------
!
            IF (ENTFW) THEN
               CALL FWSPEC(FW%R,FWCOEF,MESH%X%R,MESH%Y%R,
     &                     NPOIN,PRIVE,ZF%R)
            ELSE
               DO 70 I = 1,NPOIN
                  CALL CALCFW
     &                   (I,H%R,C%R,CG%R,K%R,HMU%R,
     &                    NPOIN,OMEGA,GRAV,
     &                    VISCO,DIAM90,DIAM50,MVSED,MVEAU,
     &                    FORMFR,REGIDO,RICOEF,
     &                    ENTREG,ENTRUG,FFW)
                  FW%R(I) = FFW
 70            CONTINUE
            ENDIF
!
!           -----------------------------------------
!           COMPUTES THE DISSIPATION COEFFICIENT FOR
!           BOTTOM FRICTION
!           -----------------------------------------
!
            IF (FORMFR .EQ. 1) THEN
!
!           ---------------------------------------------------
!           COMPUTES AN EFFECTIVE SPEED
!           UE = 1.2D0*(0.5*((DPHIR/DX)**2 + (DPHIR/DY)**2
!                         +(DPHII/DX)**2 + (DPHII/DY)**2))**0.5
!           UE IS STORED IN T4 HERE
!           ---------------------------------------------------
!
               CALL CALCUE
!
!              ----------------------------------------
!              THE DISSIPATION COEFFICIENT MU FOR
!              FRICTION IS STORED IN T1
!              ----------------------------------------
!
               CALL OS( 'X=C     ' , T1 , SBID , SBID , 0.D0 )
!
               DO 80 I = 1,NPOIN
                  T1%R(I) = (0.5D0*FW%R(I)*T4%R(I))/
     &                    (H%R(I)*((COSH(K%R(I)*H%R(I)))**2))
                  T1%R(I) = T1%R(I)/CG%R(I)
 80            CONTINUE
            ENDIF
!
            IF (FORMFR .EQ. 2) THEN
               CALL OS( 'X=C     ' , T1 , SBID , SBID , 0.D0 )
               DO 90 I = 1,NPOIN
                  T1%R(I) = (2*FW%R(I)*HMU%R(I)*
     &                    ((OMEGA/SINH(K%R(I)*H%R(I)))**3))
                  T1%R(I) = T1%R(I)/(3.D0*3.14159D0*GRAV)
                  T1%R(I) = T1%R(I)/CG%R(I)
 90            CONTINUE
            ENDIF
!
!        -------------------------
!        END 'IF BOTTOM FRICTION'
!        -------------------------
!
         END IF
!
!        -------------------------------------------------------
!        RELAXATION ON MU2 TO TRY AND AVOID OSCILLATIONS IN THE
!        CONVERGENCE OF THE SOLVEUR
!        -------------------------------------------------------
!
!
         MODHMU = 1.D-9
         DO I = 1,NPOIN
!
!           --------------------------
!           MU = MU_DEFERL + MU_FROTTE
!           --------------------------
!
            MU2%R(I) = MU2%R(I) + T1%R(I)
!
!           ----------
!           RELAXATION
!           ----------
!
            MU2%R(I) = MU%R(I) + RELDIS * (MU2%R(I) - MU%R(I))
            IF(ITERMU.EQ.0) THEN
               HMUANC%R(I) = HMU%R(I)
               ECRHMU = 1.D0
               MU%R(I) = MU2%R(I)
            ELSE
               ECRHMU = MAX(ECRHMU,ABS(HMU%R(I)-HMUANC%R(I)))
               MODHMU = MAX(MODHMU,ABS(HMU%R(I)))
               MU%R(I) = MU2%R(I)
               HMUANC%R(I) = HMU%R(I)
            ENDIF
         ENDDO
!
!
!        RELAXES THE RELAXATION AT EACH SUB-ITERATION
!        TO FACILITATE CONVERGENCE OF THE ALGORITHM USED TO
!        COMPUTE DISSIPATION (REGULAR WAVES)
!
         IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
            RELDIS = RELDIS * 0.85D0
         ENDIF
!
         IF (NCSIZE .NE. 0) THEN
            ECRHMU = P_DMAX(ECRHMU)
            MODHMU = P_DMAX(MODHMU)
         END IF
         IF (LNG.EQ.1) WRITE(LU,*) 'ECART ENTRE DEUX
     &        SOUS-ITERATIONS (%)',
     &        100*ECRHMU/MODHMU
         IF (LNG.EQ.2) WRITE(LU,*) 'DIFF. BETWEEN TWO
     &        SUB-ITERATIONS (%) ',
     &        100*ECRHMU/MODHMU
         ITERMU = ITERMU + 1
!
!
!        -----------------------------------------------------------
!        IF NUMBER OF SUB-ITERATIONS FOR MU >= MAX NUMBER OF SUB-ITERATIONS
!        EXITS THE LOOP OVER MU AND SETS THE RELATIVE DIFFERENCE
!        ECRHMU/MODHMU TO 10 % OF EPSDIS
!        -----------------------------------------------------------
!
         IF(ITERMU.GE.NITDIS) THEN
            IF (LNG.EQ.1) WRITE(LU,100) ITERMU
            IF (LNG.EQ.2) WRITE(LU,101) ITERMU
 100        FORMAT(/,1X,'BERKHO (ARTEMIS): NOMBRE DE SOUS-ITERATIONS',
     & 1X,'MAXIMUM ATTEINT :',1X,I3)
 101        FORMAT(/,1X,'BERKHO (ARTEMIS): YOU REACHED THE MAXIMUM',
     & 1X,'NUMBER OF SUB-ITERATIONS :)',1X,I3)
            ECRHMU = EPSDIS*MODHMU/10.D0
         ENDIF
!
!        ------------------------------------------------
!        CHECKS CONVERGENCE ON THE DISSIPATION LOOP
!        ------------------------------------------------
!
         WRITE(LU,*) ' '
         WRITE(LU,*) '----------------------------------------------- '
         IF (ECRHMU.GT.EPSDIS*MODHMU) GOTO 98
!
         IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
            CALL OS( 'X=Y     ', QB,T3,SBID,CBID)
         ELSE
            CALL OS( 'X=X+Y   ', QB,T3,SBID,CBID)
         ENDIF
!
         IF (LNG.EQ.1) WRITE(LU,200) ITERMU
         IF (LNG.EQ.2) WRITE(LU,201) ITERMU
 200     FORMAT(/,1X,'NOMBRE DE SOUS-ITERATIONS POUR LA DISSIPATION:',
     &   1X,I3)
 201     FORMAT(/,1X,'NUMBER OF SUB-ITERATIONS FOR DISSIPATION:',
     &   1X,I3)
!
!     ========================================
!
!     END 'IF BREAKING OR BOTTOM FRICTION'
!
!     ========================================
!
      ENDIF
!
! END OF THE ITERATIVE LOOP ON THE DISSIPATION TERM MU
!
!-----------------------------------------------------------------------
!
      RETURN
      END




!                    *************************
                     SUBROUTINE MASQUE_ARTEMIS
!                    *************************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FILLS THE ARRAYS MASK1, MASK2, MASK3, MASK4, MASK5
!+
!+      MASK1: CORRESPONDS TO INCIDENT WAVES (KINC)
!+      MASK2: CORRESPONDS TO FREE EXIT (KSORT)
!+      MASK3: CORRESPONDS TO SOLID BOUNDARY (KLOG)
!+      MASK4: CORRESPONDS TO IMPOSED WAVES (KENT)
!+      MASK5: CORRESPONDS TO INCIDENT POTENTIAL (KPOT)
!
!history  D. AELBRECHT (LNH)
!+        06/07/1999
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
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
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!
      INTEGER IK
!
!-----------------------------------------------------------------------
!
!
!     INITIALISES (SETS TO 0) ALL MASKING VECTORS
!
      CALL OS( 'X=C     ' , MASK1 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK2 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK3 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK4 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK5 , SBID , SBID , 0.D0 )
!
      DO 6 IK=1,NPTFR
!
         IF (LIHBOR%I(IK).EQ.KLOG) THEN
            MASK3%R(IK) = 1.D0
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(IK)).NE.KLOG) THEN
            IF (LIHBOR%I(IK).EQ.KINC) THEN
               MASK1%R(IK) = 1.D0
            ENDIF
            IF (LIHBOR%I(IK).EQ.KSORT) THEN
               MASK2%R(IK) = 1.D0
            ENDIF
            IF (LIHBOR%I(IK).EQ.KENT) THEN
               MASK4%R(IK) = 1.D0
            ENDIF
            IF (LIHBOR%I(IK).EQ.KPOT) THEN
               MASK5%R(IK) = 1.D0
            ENDIF
         ELSE
            MASK3%R(IK) = 1.D0
         ENDIF
 6    CONTINUE
        RETURN
      END



!                    ******************
                     SUBROUTINE ARTEMIS
!                    ******************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE MODIFIED BERKHOFF EQUATION.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/04/1999
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        21/06/2010
!+        V6P0
!+   PARALLEL VERSION
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
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE GRACESTOP
!
!-----------------------------------------------------------------------
! DECLARES TYPES AND DIMENSIONS
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
! INTEGERS
!
      INTEGER LT,NPERBA,I,J
      INTEGER NELBRD,NPFMAX,NELBRX
!      INTEGER LPER,LDIR
      INTEGER ALIRE(MAXVAR)
!
! VARIABLE FOR SUBROUTINE DISMOY
!
      INTEGER LISHHO
!
! REAL SCALARS
!
      DOUBLE PRECISION RADDEG,HIST(1)
!
! VARIABLES FOR CALLS TO TELEMAC-2D SUBROUTINES
!
      INTEGER NVARCL,ISTO
      DOUBLE PRECISION LAMBD0
      LOGICAL RESU,FROVAR,PROLIN,TRAC
!
! USED FOR DUMMY ARGUMENTS
!
      INTEGER IBID
      DOUBLE PRECISION BID
!
      INTEGER  P_IMAX,P_IMIN
      DOUBLE PRECISION P_DMIN
      EXTERNAL P_IMAX,P_IMIN,P_DMIN
!
      DATA HIST /9999.D0/
!
!-----------------------------------------------------------------------
!
!  VARIABLES TO READ IF COMPUTATION IS CONTINUED :
!  0 : DISCARD    1 : READ  (SEE SUBROUTINE NOMVAR)
!
      DATA ALIRE /1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
!
!-----------------------------------------------------------------------
!
      RADDEG = 180.D0/3.141592654D0
!
!=======================================================================
!
! : 1          READS, PREPARES AND CONTROLS THE DATA
!
!=======================================================================
!
!  TYPES OF DISCRETISATION:
!
!  TRIANGLES : P1
      IELM  = 11
!  SEGMENTS  : P1 FOR THE BOUNDARY
      IELMB = 1
!
!
!  MAXIMUM SIZE (CASE OF AN ADAPTIVE GRID)
!  THESE PARAMETERS ARE USED IN BIEF CALLS
!
!     NODES
      NPMAX = NPOIN
!     ELEMENTS
      NELMAX = NELEM
!     BOUNDARY ELEMENTS
      NELBRD = NPTFR
!     BOUNDARY ELEMENTS (MAXIMUM NUMBER)
      NPFMAX = NPTFR
!     BOUNDARY NODES
      NELBRX = NPTFR
!
      IF(BALAYE) THEN
        NPERBA = INT((PERFIN-PERDEB)/PERPAS) + 1
      ENDIF
!
!=======================================================================
!
      RESU   = .TRUE.
      FROVAR = .FALSE.
      PROLIN = .FALSE.
      SPHERI = .FALSE.
      TRAC   = .FALSE.
      NVARCL = 0
!
! IN TELEMAC-2D, LIHBOR = KINC IS AUTOMATICALLY CHANGED TO KSORT
! HAS TO MODIFY THE VALUE OF KINC FOR PREDA2, TO AVOID THIS AUTOMATIC CHANGE
! IN ADDITION, IN TELEMAC-2D, LIHBOR = KADH (NOT KNOWN HERE) GENERATES
! A MESSAGE. TO AVOID IT, ISTO IS ALSO USED IN PLACE OF KADH.
!
!
      ISTO = 100
!
!-----------------------------------------------------------------------
!
! READS THE BOUNDARY CONDITIONS AND INDICES FOR THE BOUNDARY NODES.
!
! CCP : WARNING : 
!       V6P2 LECLIM_ARTEMIS IS NOT USED ANYMORE. 
!       IN LECLIM we use 0 0 0 0 0 0 values for KENT,KENTU, etc...
!       This way LECLIM ONLY READ the boundary conditions file and
!       DO NOT CHANGE the LIHBOR values
!
      WRITE(6,*) 'ON ENTRE DANS LECLIM'
      CALL LECLIM (LIHBOR%I   , LIUBOR%I , ITB1%I , ITB1%I,
     &             TB1%R      , TB1%R    , TB1%R  , TB1%R ,
     &             TB1%R      , TB1%R    , TB1%R  ,
     &             MESH%NPTFR , 3        ,.FALSE. ,
     &             ART_FILES(ARTCLI)%LU,
     &             0       , 0    , 0 ,  0 , 0 , 0,
     &             NUMLIQ%I   ,MESH,BOUNDARY_COLOUR%I)
      WRITE(6,*) 'ON SORT DE LECLIM'

!-----------------------------------------------------------------------
!
! COMPLEMENTS THE DATA STRUCTURE FOR BIEF
!
      CALL INBIEF(LIHBOR%I,KLOG,IT1,IT2,IT3,LVMAC,IELM,
     &         LAMBD0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA)
!-----------------------------------------------------------------------
!  LOOKS FOR BOTTOM AND BOTTOM FRICTION IN THE GEOMETRY FILE :
!-----------------------------------------------------------------------
!
      CALL FONSTR(T1,ZF,T2,FW,ART_FILES(ARTGEO)%LU,ART_FILES(ARTFON)%LU,
     &            ART_FILES(ARTFON)%NAME,MESH,FFON,LISTIN)
!-----------------------------------------------------------------------
!
! PREPARES THE RESULTS FILE (OPTIONAL)
!
!     STANDARD SELAFIN FORMAT
!
        ! CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES.
        ! THE DATA ARE CREATED IN THE FILE: NRES, AND ARE
        ! CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
        ! CONTAINED IN THE FILE.
        CALL CREATE_DATASET(ART_FILES(ARTRES)%FMT, ! RESULTS FILE FORMAT
     &                      ART_FILES(ARTRES)%LU,  ! LU FOR RESULTS FILE
     &                      TITCAS,     ! TITLE
     &                      MAXVAR,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                      TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                      SORLEO)     ! PRINT TO FILE OR NOT
        ! WRITES THE MESH IN THE OUTPUT FILE :
        ! IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
        ! THE REST OF THE INFORMATION IS IN MESH.
        ! ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
        ! ORIGIN.
        CALL WRITE_MESH(ART_FILES(ARTRES)%FMT, ! RESULTS FILE FORMAT
     &                  ART_FILES(ARTRES)%LU,  ! LU FOR RESULTS FILE
     &                  MESH,          ! CHARACTERISES MESH
     &                  1,             ! NUMBER OF PLANES /NA/
     &                  MARDAT,        ! START DATE
     &                  MARTIM,        ! START TIME
     &                  I_ORIG,J_ORIG) ! COORDINATES OF THE ORIGIN.
!
!-----------------------------------------------------------------------
!
!     INITIALISES PRIVE
!
      IF(NPRIV.GT.0) CALL OS('X=C     ',PRIVE,PRIVE,PRIVE,0.D0)
!
!=======================================================================
!
      IF(NCSIZE.GT.1) THEN
         NFRLIQ=0
         DO I=1,NPTFR
            NFRLIQ=MAX(NFRLIQ,NUMLIQ%I(I))
         ENDDO
         NFRLIQ=P_IMAX(NFRLIQ)
         WRITE(LU,*) ' '
         IF(LNG.EQ.1) WRITE(LU,*) 'NOMBRE DE FRONTIERES LIQUIDES :',
     &        NFRLIQ
         IF(LNG.EQ.2) WRITE(LU,*) 'NUMBER OF LIQUID BOUNDARIES:',NFRLIQ
      ELSE
         CALL FRONT2(NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,
     &        LIHBOR%I,LIUBOR%I,
     &        MESH%X%R,MESH%Y%R,MESH%NBOR%I,MESH%KP1BOR%I,
     &        IT1%I,NPOIN,NPTFR,KLOG,LISTIN,NUMLIQ%I,MAXFRO)
      ENDIF
! LOCATES THE BOUNDARIES
!
!=======================================================================
!
! CORRECTS THE VALUES OF THE BOTTOM (OPTIONAL)
!
! STANDARD SUBROUTINE DOES NOT DO ANYTHING
!
      CALL CORFON
!
!
!=======================================================================
!
! INITIALISES THE WAVE HEIGHT FOR RANDOM SEAS AT 0.
!
      IF (ALEMON .OR. ALEMUL) THEN
       CALL OS('X=C     ', HALE , SBID , SBID , 0.D0 )
      ENDIF
!
!
! DETERMINES THE DIFFERENT PERIODS FOR A RANDOM SEA COMPUTATION
!
      IF (ALEMON.OR.ALEMUL) THEN
         CALL PERALE(PALE%R,GAMMA,PERPIC,NPALE,T1%R,NPOIN,PRIVE,
     &               NPRIV,PMIN,PMAX)
         PER = PALE%R(1)
      ENDIF
!
!
! DETERMINES THE DIFFERENT DIRECTIONS FOR A MULTIDIRECTIONAL RANDOM
! SEA COMPUTATION
!
      IF (ALEMUL) THEN
         CALL DIRALE(DALE%R,EXPOS,TETAH,TETMIN,TETMAX,NDALE,
     &               T1%R,NPOIN,PRIVE,NPRIV)
      ENDIF
!
!
!
!=======================================================================
!
! START OF COMPUTATION
!
! LT REFERS TO THE CURRENT TIME STEP (STARTS FROM 0 SO THAT
! THE FIRST COMPUTATION ALWAYS BE RECORDED)
!
      LT = 0
!
! INITIALISES QB, T01, T02 AND TM : SET TO 0 AT THE START OF COMPUTATION
!
      CALL OS('X=C     ', QB , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', T01 , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', T02 , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', TM , SBID , SBID , 0.D0 )
!
!
! INITIALISES RADIATION STRESSES AND
! FORCINGS
!
      CALL OS('X=C     ', FX , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', FY , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', SXX , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', SXY , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', SYY , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', MCOS , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', MSIN , SBID , SBID , 0.D0 )
!
! FOR A RANDOM SEA COMPUTATION, LPER AND LDIR REFER TO THE COMPUTED
! PERIOD AND DIRECTION
!
      LPER = 1
      LDIR = 1
!
100   CONTINUE
!
      IF (BALAYE) THEN
         CALL ENTART(1,PER,LT,LPER,NPERBA,ALEMON,ALEMUL,BALAYE)
      ELSE
         CALL ENTART(1,PER,LT,LPER,NPALE,ALEMON,ALEMUL,BALAYE)
      ENDIF
!
!=======================================================================
!
! : 2                  INITIALISES
!
!=======================================================================
!
! INITIALISES PHYSICAL PARAMETERS
!
!
      CALL CONDIH
!
!=======================================================================
!
! : 3                  BOUNDARY CONDITIONS
!
!=======================================================================
!
! CALLS THE USER SUBROUTINE
!
      CALL BORH
!
! MASKING FOR THE BOUNDARY CONDITIONS
!
! IN MULTIDIRECTIONAL RANDOM SEA, THE DIRECTIONS OF PROPAGATION
! (AT THE BOUNDARY) HAVE BEEN CALCULATED IN DALE.
!
200   IF (ALEMUL) THEN
         CALL OS('X=C     ', TETAB ,SBID,SBID, DALE%R(LDIR) )
         CALL ENTART(2,DALE%R(LDIR),LT,LDIR,NDALE,ALEMON,ALEMUL,BALAYE)
      ENDIF
!
! CALCULATES THE BOUNDARY CONDITIONS ON THE POTENTIAL FROM USER INPUT.
!
!
!      IF (LT .EQ. 0) THEN
      CALL MASQUE_ARTEMIS
!
      CALL PHBOR
!      END IF
!
!=======================================================================
!
! : 4                  SOLVES THE BERKHOFF EQUATION
!
!=======================================================================
!
      WRITE(6,*) 'ENTREE DANS BERKHO'
      CALL BERKHO (LT)
      WRITE(6,*) 'SORTIE DANS BERKHO'

!
!=======================================================================
!
! : 5.1        COMPUTES SPEED, FREE SURFACE ELEVATION,
!              WAVE HEIGHT AND PHASE
!
!=======================================================================
!
      CALL CALRES
!
      IF (ALEMON .OR. ALEMUL) THEN
!
!        CUMULATIVELY COMPUTES THE M1, M2, AND MT1 MOMENTUMS
!        STORED UNTIL THE LAST COMPUTATION IN T01, T02, AND TM
!
!
         CALL CALCMN
!
      ENDIF
!
!
!=======================================================================
!
! : 5.2        COMPUTES RADIATION STRESSES AND
!              DRIVING FORCES FOR REGULAR WAVES.
!
!=======================================================================
!
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL) THEN
!
       IF (LISHOU) THEN
         CALL DISMOY
     &   (NPOIN,NELEM,MESH%X%R,MESH%Y%R,MESH%IKLE%I,K%R,LISHHO)
       ELSE
         LISHHO = 0
       ENDIF
!
         CALL RADIA1 (LISHHO)
!
      ELSE
         LISHHO = 0
      ENDIF
!=======================================================================
!
! : 6   CALLS A USER SUBROUTINE FOR PRINT OUTS, ANALYTICAL SOLUTIONS...
!       (STANDARD SUBROUTINE DOES NOT DO ANYTHING)
!
!=======================================================================
!
      CALL UTIMP
     &(PHIR%R,PHII%R,C%R,CG%R,K%R,MESH%X%R,MESH%Y%R,ZF%R,H%R,
     & HHO%R,U0%R,V0%R,PHAS%R,S%R,T1%R,T2%R,T3%R,T4%R,INCI%R,
     & GRAV,PER,OMEGA,MESH%IKLE%I,MESH%NBOR%I,MESH%KP1BOR%I,
     & NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN,PRIVE)
!
!=======================================================================
!
! : 7                  PRINTS OUT THE RESULTS
!
!=======================================================================
!
!
! FOR RANDOM SEAS,
! OUTPUTS ONLY AT THE PEAK PERIOD
!
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL) THEN
!
!=======================================================================
!
!     CONVERTS INCI INTO DEGREES
!
!=======================================================================
!
         CALL OS('X=CX    ', INCI , SBID , SBID , RADDEG )
!
! RUBENS FILE
!
         CALL BIEF_DESIMP(ART_FILES(ARTRES)%FMT,VARSOR,
     &            HIST,0,NPOIN,ART_FILES(ARTRES)%LU,'STD',PER,0,
     &            LISPRD,LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
!
!=======================================================================
!
!              COMPARISON AGAINST A REFERENCE FILE
!
!=======================================================================
!
!     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
!     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
!     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
!
         IF(VALID) THEN
           CALL BIEF_VALIDA(TB,TEXTE,
     &                      ART_FILES(ARTREF)%LU,ART_FILES(ARTREF)%FMT,
     &                      VARSOR,TEXTE,
     &                      ART_FILES(ARTRES)%LU,ART_FILES(ARTRES)%FMT,
     &                      MAXVAR,NPOIN,LT,LT,ALIRE)
         ENDIF
!
      ENDIF
!
!=======================================================================
!
! : 8                  GOES TO NEXT PERIOD
!
!=======================================================================
!
! IF SWEEPS A RANGE OF PERIODS
!
      IF (BALAYE) THEN
         LT   = LT  + 1
         LPER = LPER + 1
         PER  = PER + PERPAS
         IF (PER.LE.PERFIN) GOTO 100
      ENDIF
!
!
!=======================================================================
!
! IF RANDOM SEAS
!
!=======================================================================
!
      IF (ALEMON .OR. ALEMUL) THEN
!
         LT  = LT  + 1
!
         IF (LT.LT.NPALE*NDALE) THEN
!
!           REACTUALISES THE ENERGY OF THE RANDOM SEA
            CALL OS('X=X+CYZ ',HALE,HHO,HHO,1.D0/DBLE(NPALE*NDALE))
!
!           GOES TO NEXT DIRECTION
            LDIR = LDIR + 1
            IF (LDIR.LE.NDALE) GOTO 200
!
!           GOES TO NEXT PERIOD
            LDIR = 1
            LPER = LPER + 1
            PER = PALE%R(LPER)
            GOTO 100
!
         ELSE
!
!           LAST COMPUTATION: DETERMINES THE MEAN PERIODS
!           (T01 AND T02), AND THE MEAN DIRECTION (INCI)
!
!
            CALL CALCTM
!
!           DETERMINES MEAN K, C AND CG
!
            CALL CALRE2
!
!           TAKES INTO ACCOUNT THE LAST WAVE HEIGHT
!           FOR RANDOM SEAS
!
            CALL OS('X=X+CYZ ',HALE,HHO,HHO,1.D0/DBLE(NPALE*NDALE))
            CALL OS('X=SQR(Y)', HALE , HALE , SBID , BID )
            CALL OS('X=CX    ',QB,SBID,SBID,1.D0/DBLE(NPALE*NDALE))
!
!=======================================================================
!
!           COMPUTES RADIATION STRESSES
!           AND DRIVING FORCES FOR RANDOM SEAS
!
!=======================================================================
!
            CALL RADIA2 (LISHHO)
!
!=======================================================================
!
!          CONVERTS INCI INTO DEGREES
!
!=======================================================================
!
            CALL OS('X=CX    ', INCI , SBID , SBID , RADDEG )
!
!=======================================================================
!
!           RUBENS FILE
!
!=======================================================================
!
            CALL BIEF_DESIMP(ART_FILES(ARTRES)%FMT,VARSOR,
     &            HIST,0,NPOIN,ART_FILES(ARTRES)%LU,'STD',PERPIC,0,
     &            LISPRD,LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
!
!=======================================================================
!
!              COMPARISON AGAINST A REFERENCE FILE
!
!=======================================================================
!
!
!     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
!     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
!     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
!
            IF(VALID) THEN
              CALL BIEF_VALIDA(TB,TEXTE,
     &                       ART_FILES(ARTREF)%LU,ART_FILES(ARTREF)%FMT,
     &                       VARSOR,TEXTE,
     &                       ART_FILES(ARTRES)%LU,ART_FILES(ARTRES)%FMT,
     &                       MAXVAR,NPOIN,LT,LT,ALIRE)
            ENDIF
!
         ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
 


C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C------------------------------------------------------------------------

C DEBUT ROUTINE "USER"


C                       ***************
                        SUBROUTINE BORH
C                       ***************
C
C***********************************************************************
C
C  ARTEMIS    VERSION 6.2    07/12   D. AELBRECHT (LNH) 01 30 87 74 12 
C
C  LINKED TO BIEF VERS. 5.0          J-M HERVOUET (LNH) 01 30 87 80 18
C
C***********************************************************************
C
C      FONCTION:    PREND EN COMPTE LES CONDITIONS AUX LIMITES
C                   DE L'UTILISATEUR
C                   ELLES SONT DONNEES PAR SEGMENT.
C
C      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   RP           |<-- |  COEFFICIENTS DE REFLEXION DES PAROIS        |
C |   TETAP        |<-- |  ANGLE D'ATTAQUE DE LA HOULE SUR LES LIMITES |
C |                |    |  PAS SEULEMENT LES PAROIS, MAIS AUSSI LES    |
C |                |    |  LES FRONTIERES LIQUIDES                     |
C |                |    |  (COMPTE PAR RAPPORT A LA NORMALE EXTERIEURE |
C |                |    |   DANS LE SENS DIRECT)                       |
C |   ALFAP        |<-- |  DEPHASAGE INDUIT PAR LA PAROI ENTRE L'ONDE  |
C |                |    |  REFLECHIE ET L'ONDE INCIDENTE (SI ALFAP EST |
C |                |    |  POSITIF, L'ONDE REFLECHIE EST EN RETARD)    |
C |   HB           |<-- |  HAUTEUR DE LA HOULE AUX FRONTIERES OUVERTES |
C |   TETAB        |<-- |  ANGLE D'ATTAQUE DE LA HOULE (FRONT. OUV.)   |
C |                |    |  (COMPTE PAR RAPPORT A L'AXE DES X DANS LE   |
C |                |    |   SENS DIRECT)                               |
C |    H           | -->|  HAUTEUR D'EAU                               |
C |    K           | -->|  NOMBRE D'ONDE                               |
C |    C,CG        | -->|  VITESSES DE PHASE ET DE GROUPE              |
C |    C           | -->|  CELERITE AU TEMPS N                         |
C |    ZF          | -->|  FOND                                        |
C |    X,Y         | -->|  COORDONNEES DES POINTS DU MAILLAGE          |
C |  TRA01,...,3   |<-->|  TABLEAUX DE TRAVAIL                         |
C | XSGBOR,YSGBOR  | -->|  NORMALES EXTERIEURES AUX SEGMENTS DE BORD   |
C |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
C |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
C |   KP1BOR       | -->|  NUMERO DU POINT FRONTIERE SUIVANT           |
C |   OMEGA        | -->|  PULSATION DE LA HOULE                       |
C |   PER          | -->|  PERIODE DE LA HOULE                         |
C |   TETAH        | -->|  ANGLE DE PROPAGATION DE LA HOULE            |
C |   GRAV         | -->|  GRAVITE                                     |
C |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
C |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
C |   KENT,KLOG    | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
C |   KSORT,KINC   |    |  LIMITES                                     |
C |                |    |  KENT  : ENTREE (VALEUR IMPOSEE)             |
C |                |    |  KLOG  : PAROI                               |
C |                |    |  KSORT : SORTIE                              |
C |                |    |  KINC  : ONDE INCIDENTE                      |
C |   PRIVE        | -->|  TABLEAU DE TRAVAIL (DIMENSION DANS PRINCI)  |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : ARTEMI
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I
C
      DOUBLE PRECISION PI,BID         
C
C     ---------------------------------------- 
C     VOS NOUVELLES DECLARATIONS DE VARIABLES :
C     ---------------------------------------- 
C           
C                                                                   
C JCB :    
      INTEGER IG              
                                                              
C
CCP
      INTEGER IG0 ,JB             
      DOUBLE PRECISION PHASOI,AUXIC,AUXIS,DEGRAD,X0,Y0,KK
CCP
C
      PARAMETER( PI = 3.1415926535897932384626433D0)
C
      INTRINSIC COS,SIN
C
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C CONDITIONS AUX LIMITES
C UN SEGMENT EST SOLIDE SI IL EST DE TYPE KLOG.
C UN SEGMENT EST ONDE INCIDENTE SI IL EST DE TYPE KINC.
C UN SEGMENT EST UNE ENTREE SI IL EST DE TYPE KENT.
C UN SEGMENT EST UNE SORTIE SI IL EST DE TYPE KSORT.
C
C TOUS LES ANGLES SONT EN DEGRES
C                         ------
C ---------------------------------------
C INITIALISATION DES VARIABLES PAR DEFAUT
C ---------------------------------------
      TETAB%R(:) = TETAH
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 1.D0 
C
C PAROIS SOLIDES
C
      DO I=1,NPTFR
       JB=BOUNDARY_COLOUR%I(I)
      
      IF(JB.GE.101.AND.JB.LE.304)THEN
	 LIHBOR%I(I)=KLOG
	 RP%R(I)=1.D0
      ENDIF
C PAROIS LIQUIDES -FRONTIERE LIBRE
C
      IF(JB.GE.42.AND.JB.LE.100)THEN
	 LIHBOR%I(I)=KSORT
      ENDIF
      IF(JB.GE.305.AND.JB.LE.363)THEN
	 LIHBOR%I(I)=KSORT
      ENDIF
C
C PAROIS LIQUIDES - FRONTIERE ONDE INCIDENTE
C
      DEGRAD=PI/180.D0
      PHASOI=0.D0
      X0=2000.
      Y0=4500.
      AUXIC =COS(TETAH*DEGRAD)
      AUXIS =SIN(TETAH*DEGRAD)
C
C     
      IF(JB.GE.364.AND.JB.LE.484)THEN
	 LIHBOR%I(I)=KINC
         TETAB%R(I)=0.
	 HB%R(I)=1.D0
C   ---- PHASE	 
	 IG   = MESH%NBOR%I(I)
	 KK=K%R(IG)
	 PHASOI=KK*AUXIC*(X(IG)-X0)+KK*AUXIS*(Y(IG)-Y0)
         ALFAP%R(I) = PHASOI/DEGRAD
      ENDIF
      IF(JB.GE.1.AND.JB.LE.41)THEN
	 LIHBOR%I(I)=KINC
         TETAB%R(I)=0.
	 HB%R(I)=1.D0
C   ---- PHASE	 
	 IG   = MESH%NBOR%I(I)
	 KK=K%R(IG)
	 PHASOI=KK*AUXIC*(X(IG)-X0)+KK*AUXIS*(Y(IG)-Y0)
         ALFAP%R(I) = PHASOI/DEGRAD
      ENDIF
      ENDDO
C
C ------------ 
C                                                                       
C JCB :                                                                       
C
C-----------------------------------------------------------------------
C                                                                       
      RETURN                                                            
      END                                                               
C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C PROGICIEL : TELEMAC 5.0          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORFON
C
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C
C-----------------------------------------------------------------------
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      ZF        |<-->| FOND A MODIFIER.
C |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C |      A         |<-- | MATRICE
C |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
C |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
C |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
C |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
C |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I
C
      DOUBLE PRECISION PI,BID
C
      PARAMETER( PI = 3.1415926535897932384626433D0)
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  LISSAGES EVENTUELS DU FOND
C
      IF(LISFON.GT.0) THEN
C
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
      ENDIF
C
			 

C-----------------------------------------------------------------------
C
C EXEMPLE :
C
C
C      DO 10 I = 1,NPOIN
C        ZF%R(I) = -1.D0 -0.02D0*Y(I)
C        IF (Y(I).GE.700.D0) THEN
C           ZF%R(I) = -15.D0
C        ENDIF
C10    CONTINUE
C-----------------------------------------------------------------------
C VOTRE MODIFICATION DES FONDS :
C-----------------------------------------------------------------------
C
C JCB :
C
C
      RETURN
      END 
 

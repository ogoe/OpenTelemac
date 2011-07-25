!                    *****************************
                     MODULE DECLARATIONS_TELEMAC2D
!                    *****************************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DECLARATION OF PRINICIPAL TELEMAC2D VARIABLES
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
      USE FRICTION_DEF
!
!       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
!
!      01) VECTORS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!      02) MATRICES (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!      03) BLOCKS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!      04) INTEGERS
!      05) LOGICAL VALUES
!      06) REALS
!      07) STRINGS
!      08) SLVCFG STRUCTURES
!      09) MESH STRUCTURE
!      10) ALIASES
!
!
!       ALL BIEF_OBJ AND BIEF_MESH STRUCTURES ARE ALLOCATED
!       IN SUBROUTINE POINT_TELEMAC2D
!
!-----------------------------------------------------------------------
!
!       1) VECTORS
!
!-----------------------------------------------------------------------
!
!     COMPONENTS OF VELOCITY
! 
      TYPE(BIEF_OBJ), TARGET :: U,V
!
!     DEPTH AT NEW TIME-STEP
! 
      TYPE(BIEF_OBJ), TARGET :: H
!
!     TRACERS AT NEW TIME-STEP
! 
      TYPE(BIEF_OBJ), TARGET :: T
!
!     K AT NEW TIME-STEP
!  
      TYPE(BIEF_OBJ), TARGET :: AK
!
!     EPSILON AT NEW TIME-STEP
! 
      TYPE(BIEF_OBJ), TARGET :: EP
!
!     U AND V AFTER ADVECTION BY CHARACTERISTICS
! 
      TYPE(BIEF_OBJ), TARGET :: UTILD,VTILD
!
!     H AFTER ADVECTION BY CHARACTERISTICS
! 
      TYPE(BIEF_OBJ), TARGET :: HTILD
!
!     T AFTER ADVECTION BY CHARACTERISTICS
! 
      TYPE(BIEF_OBJ), TARGET :: TTILD
!
!     AK AFTER ADVECTION BY CHARACTERISTICS
! 
      TYPE(BIEF_OBJ), TARGET :: AKTILD
!
!     EP AFTER ADVECTION BY CHARACTERISTICS
! 
      TYPE(BIEF_OBJ), TARGET :: EPTILD
!
!     U AND V AT OLD TIME-STEP
!
      TYPE(BIEF_OBJ), TARGET :: UN,VN
!
!     H AT OLD TIME-STEP
! 
      TYPE(BIEF_OBJ), TARGET :: HN
!
!     T AT OLD TIME-STEP
! 
      TYPE(BIEF_OBJ), TARGET :: TN
!
!     AK AT OLD TIME-STEP
! 
      TYPE(BIEF_OBJ), TARGET :: AKN
!
!     EP AT OLD TIME-STEP
! 
      TYPE(BIEF_OBJ), TARGET :: EPN
!
!     INCREMENT OF H IN ONE TIME STEP
! 
      TYPE(BIEF_OBJ), TARGET :: DH
!brief INCREMENT OF U IN ONE TIME STEP
! accriossements en u
      TYPE(BIEF_OBJ), TARGET :: DU
!brief INCREMENT OF V IN ONE TIME STEP
! accriossements en v
      TYPE(BIEF_OBJ), TARGET :: DV
!brief INCREMENT OF HN IN ONE TIME STEP
! accriossements en hn
      TYPE(BIEF_OBJ), TARGET :: DHN
!brief COMPONENTS OF ADVECTING FIELD
! composantes des vitesses du convecteur
      TYPE(BIEF_OBJ), TARGET :: UCONV
!brief COMPONENTS OF ADVECTING FIELD
! composantes des vitesses du convecteur
      TYPE(BIEF_OBJ), TARGET :: VCONV
!brief DEPTH IN THE PROPAGATION TERM I.E. IN H DIV(U)
! hauteur de propagation
      TYPE(BIEF_OBJ), TARGET :: HPROP
!brief VALUES ON BOUNDARIES: U
! valeurs imposees de u au bord
      TYPE(BIEF_OBJ), TARGET :: UBOR
!brief VALUES ON BOUNDARIES: V
! valeurs imposees de v au bord
      TYPE(BIEF_OBJ), TARGET :: VBOR
!brief VALUES ON BOUNDARIES: WALL FRICTION COEFFICIENTS
      TYPE(BIEF_OBJ), TARGET :: AUBOR,UETUTA
!brief VALUES ON BOUNDARIES: H
! valeurs imposees de h au bord
      TYPE(BIEF_OBJ), TARGET :: HBOR
!brief VALUES ON BOUNDARIES: T
! valeurs imposees du traceur au bord
      TYPE(BIEF_OBJ), TARGET :: TBOR
!brief VALUES ON BOUNDARIES: K
! valeurs imposees de la dissipation turbulente au bord
      TYPE(BIEF_OBJ), TARGET :: KBOR
!brief VALUES ON BOUNDARIES: EP
! valeurs imposees de l'energie turbulente au bord
      TYPE(BIEF_OBJ), TARGET :: EBOR
!brief FLUX ON THE BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: FLBOR
!brief FLUX ON THE BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: FLBORTRA
!brief ON BOUNDARIES: COEFFICIENTS FOR HEAT FLUXES
! coefficients d'echange thermique au bord
      TYPE(BIEF_OBJ), TARGET :: ATBOR
!brief ON BOUNDARIES: COEFFICIENTS FOR HEAT FLUXES
! coefficients d'echange thermique au bord
      TYPE(BIEF_OBJ), TARGET :: BTBOR
!brief ADIMENSIONAL FRICTION COEFFICIENTS (BOTTOM OR WALL)
! coefficient de frottement variable en espace
      TYPE(BIEF_OBJ), TARGET :: CF
!brief ADIMENSIONAL FRICTION COEFFICIENTS
!
      TYPE(BIEF_OBJ), TARGET :: CFBOR
!brief VOID STRUCTURE
!
      TYPE(BIEF_OBJ), TARGET :: S
!brief BOTTOM TOPOGRAPHY, PER POINT
! cote du fond
      TYPE(BIEF_OBJ), TARGET :: ZF
!brief BOTTOM TOPOGRAPHY, PER ELEMENTS
      TYPE(BIEF_OBJ), TARGET :: ZFE
!brief VELOCITY DIFFUSIVITY
      TYPE(BIEF_OBJ), TARGET :: VISC
!brief TRACER DIFFUSIVITY
      TYPE(BIEF_OBJ), TARGET :: VISCT
!brief VELOCITY DIFFUSIVITY SAVED
      TYPE(BIEF_OBJ), TARGET :: VISC_S
!brief SOURCE TERMS IN THE MOMENTUM EQUATION
      TYPE(BIEF_OBJ), TARGET :: FU,FV
!brief WAVE STRESSES (FROM ARTEMIS OR TOMAWAC)
      TYPE(BIEF_OBJ), TARGET :: FXWAVE,FYWAVE
!brief INITIAL CELERITY OF WAVES
      TYPE(BIEF_OBJ), TARGET :: C0
!brief INITIAL DEPTH
      TYPE(BIEF_OBJ), TARGET :: H0
!brief MASK FOR TRACER
      TYPE(BIEF_OBJ), TARGET :: MASKTR
!brief FREE SURFACE ELEVATION OF INCIDENT WAVE
! onde resultat
      TYPE(BIEF_OBJ), TARGET :: COTOND
!
!     ATMOSPHERIC PRESSURE
! 
      TYPE(BIEF_OBJ), TARGET :: PATMOS
!
!     COMPONENTS OF WIND VELOCITY
!  
      TYPE(BIEF_OBJ), TARGET :: WINDX,WINDY
!
!     DENSITY
! 
      TYPE(BIEF_OBJ), TARGET :: RO
!
!     INTEGRAL OF BASES, AND THE SAME AFTER ASSEMBLING IN PARALLEL
!
      TYPE(BIEF_OBJ), TARGET :: VOLU2D,V2DPAR
!
!     INVERSE OF INTEGRAL OF BASES
!
      TYPE(BIEF_OBJ), TARGET :: UNSV2D
!
!     RIGHT HAND SIDES OF LINEAR SYSTEMS
! 
      TYPE(BIEF_OBJ), TARGET :: CV1,CV2,CV3,CV1S
!
!     RIGHT HAND SIDE OF CONTINUITY EQUATION
! 
      TYPE(BIEF_OBJ), TARGET :: SMH
!
!     P0 WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: TE1,TE2,TE3
!
!     WORKING ARRAYS FOR OPTION 3 OF TREATMENT OF TIDAL FLATS
!
      TYPE(BIEF_OBJ), TARGET :: TE4,TE5
! 
!     WORKING ARRAY (SIZE MULTIPLE OF NELMAX)
!
      TYPE(BIEF_OBJ), TARGET :: W1
!
!     FOR FLUXES ACCROSS SECTIONS
!
      TYPE(BIEF_OBJ), TARGET :: MSKSEC
!brief
!
      TYPE(BIEF_OBJ), TARGET :: ZFLATS
!brief FRICTION COEFFICIENT, IN TERMS OF STRICKLER, CHEZY, ETC
! tableau des coefficients de frottement sur le fond
      TYPE(BIEF_OBJ), TARGET :: CHESTR
!brief TYPES OF BOUNDARY CONDITIONS
! types de conditions aux limites sur u
      TYPE(BIEF_OBJ), TARGET :: LIUBOR
!brief TYPES OF BOUNDARY CONDITIONS
! types de conditions aux limites sur v
      TYPE(BIEF_OBJ), TARGET :: LIVBOR
!brief TYPES OF BOUNDARY CONDITIONS
! types de conditions aux limites sur h
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!brief TYPES OF BOUNDARY CONDITIONS
! types de conditions aux limites sur le traceur
      TYPE(BIEF_OBJ), TARGET :: LITBOR
!brief
! types de conditions aux limites sur le traceur
      TYPE(BIEF_OBJ), TARGET :: LIMTRA
!brief
! types de conditions aux limites sur k et epsilon
      TYPE(BIEF_OBJ), TARGET :: LIMKEP
!brief
! types de conditions aux limites pour la propagation
      TYPE(BIEF_OBJ), TARGET :: LIMPRO
!brief
! types de conditions aux limites sur h (recopie de lihbor)
      TYPE(BIEF_OBJ), TARGET :: CLH
!brief
! types de conditions aux limites sur u (recopie de liubor)
      TYPE(BIEF_OBJ), TARGET :: CLU
!brief
! types de conditions aux limites sur v (recopie de livbor)
      TYPE(BIEF_OBJ), TARGET :: CLV
!brief
!
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!brief POSITIONS OF FLOATING BODIES
! positions des flotteurs
      TYPE(BIEF_OBJ), TARGET :: XFLOT
!brief POSITIONS OF FLOATING BODIES
! positions des flotteurs
      TYPE(BIEF_OBJ), TARGET :: YFLOT
!brief
!
      TYPE(BIEF_OBJ), TARGET :: SHPFLO,XLAG,YLAG,SHPLAG
!brief
! cote de la digue au i-eme point de la n-ieme singularite
      TYPE(BIEF_OBJ), TARGET :: ZDIG
!brief
! coefficient de debit au i-eme point de la n-ieme singularite
      TYPE(BIEF_OBJ), TARGET :: PHIDIG
!brief TIME STEP OF INITIAL RELEASE
! numeros des pas de temps de largage pour chaque flotteur
      TYPE(BIEF_OBJ), TARGET :: DEBFLO
!brief TIME STEP OF END OF FOLLOW UP
! numeros des pas de temps de fin de calcul de derive pour chaque flotteur
      TYPE(BIEF_OBJ), TARGET :: FINFLO
!brief
!
      TYPE(BIEF_OBJ), TARGET :: ELTFLO
!brief
! table de connectivite utilisee pour la sortie des trajectoires sous forme de maillage
      TYPE(BIEF_OBJ), TARGET :: IKLFLO
!brief TIME STEP AT THE BEGINNING
!
      TYPE(BIEF_OBJ), TARGET :: DEBLAG
!brief TIME STEP AT THE END
!
      TYPE(BIEF_OBJ), TARGET :: FINLAG
!brief
!
      TYPE(BIEF_OBJ), TARGET :: ELTLAG
!brief
! numdig(k,n,i): numero des points des digues dans la numerotation des points de bord des conditions aux limites) du i-eme point sur le cote k de l'ouvrage n
      TYPE(BIEF_OBJ), TARGET :: NUMDIG
!brief WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT1
!brief WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT2
!brief WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT3
!brief WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT4
!brief
! composantes du debit au temps n
      TYPE(BIEF_OBJ), TARGET :: QU
!brief
! composantes du debit au temps n
      TYPE(BIEF_OBJ), TARGET :: QV
!brief
! hauteurs d'eau  stockees
      TYPE(BIEF_OBJ), TARGET :: HSTOK
!brief
! termes sources du traceur
      TYPE(BIEF_OBJ), TARGET :: SMTR
!brief
! reference des noeuds frontiere
      TYPE(BIEF_OBJ), TARGET :: LOGFR
!brief
! h reconstruit ordre 2   corrige  stocke
      TYPE(BIEF_OBJ), TARGET :: HCSTOK
!brief
! flux  traceur incremente
      TYPE(BIEF_OBJ), TARGET :: FLUXT
!brief
!
      TYPE(BIEF_OBJ), TARGET :: HT
!brief WATER DEPTH : MAX(H,HMIN)
! h reconstruit ordre 2   corrige
      TYPE(BIEF_OBJ), TARGET :: HC
!brief
! variations de z pour ordre 2
      TYPE(BIEF_OBJ), TARGET :: DSZ
!brief
! flux de masse pour traceur
      TYPE(BIEF_OBJ), TARGET :: FLUXTEMP
!brief
! flux  traceur frontiere incremente
      TYPE(BIEF_OBJ), TARGET :: FLUHBOR
!brief
! flux bord pour traceur
      TYPE(BIEF_OBJ), TARGET :: FLUHBTEMP
!brief
!
      TYPE(BIEF_OBJ), TARGET :: SECMOU , IFAMAS
!brief
! tableau de masquage des points (1: normal; 0:masque)
      TYPE(BIEF_OBJ), TARGET :: MASKPT
!brief
! tableau de masquage des elements (1: normal; 0:masque)
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!brief LIQUID BOUNDARY NUMBERS
!
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!brief MAXIMUM ELEVATIONS
!
      TYPE(BIEF_OBJ), TARGET :: MAXZ
!brief CORRESPONDING TIMES FOR MAXIMUM ELEVATIONS
!
      TYPE(BIEF_OBJ), TARGET :: TMAXZ
!brief MAXIMUM VELOCITIES
!
      TYPE(BIEF_OBJ), TARGET :: MAXV
!brief CORRESPONDING TIMES FOR MAXIMUM VELOCITIES
!
      TYPE(BIEF_OBJ), TARGET :: TMAXV
!brief FOR STORING ZONE NUMBERS
!
      TYPE(BIEF_OBJ), TARGET :: ZONE
!brief FOR STORING RESULTS OF FOURIER ANALYSIS
!
      TYPE(BIEF_OBJ), TARGET :: AMPL
!brief FOR STORING RESULTS OF FOURIER ANALYSIS
!
      TYPE(BIEF_OBJ), TARGET :: PHAS
!brief CHESTR FOR BOUNDARY CONDITIONS
!
      TYPE(BIEF_OBJ),TARGET :: CHBORD
!brief FOR DELWAQ
!
      TYPE(BIEF_OBJ),TARGET :: UDEL
!brief FOR DELWAQ
!
      TYPE(BIEF_OBJ),TARGET :: VDEL
!brief FOR DELWAQ
!
      TYPE(BIEF_OBJ),TARGET :: DM1
!brief FOR DELWAQ
!
      TYPE(BIEF_OBJ),TARGET :: ZCONV
!brief FOR DELWAQ
!
      TYPE(BIEF_OBJ),TARGET :: FLODEL
!brief FOR DELWAQ
!
      TYPE(BIEF_OBJ),TARGET :: FLULIM
!
!     EXPLICIT SOURCE TERMS FOR TRACERS
! 
      TYPE(BIEF_OBJ),TARGET :: TEXP
!
!     TERMS DUE TO SOURCES FOR TRACERS
! 
      TYPE(BIEF_OBJ),TARGET :: TSCEXP
!
!     IMPLICIT SOURCE TERMS FOR TRACERS
!
      TYPE(BIEF_OBJ),TARGET :: TIMP
!
!     FLUXES FOR FINITE VOLUMES
!
      TYPE(BIEF_OBJ),TARGET :: FLUX_OLD
!
!     FOR TIDAL BOUNDARY CONDITIONS
!
      TYPE(BIEF_OBJ),TARGET :: HBTIDE,UBTIDE,VBTIDE,NUMTIDE
!
!-----------------------------------------------------------------------
!
!       2) MATRICES
!
!-----------------------------------------------------------------------
!
!       MATRICES
!
!       SYSTEM SOLVED IN PROPAG WILL BE :
!
!     ( AM1  BM1  BM2 )   (DH)      ( CV1 )
!     (               )   (  )      (     )
!     ( CM1  AM2  A23 )   (U )  =   ( CV2 )
!     (               )   (  )      (     )
!     ( CM2  A32  AM3 )   (V )      ( CV3 )
!
!
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: AM1
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: AM2
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: AM3
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: BM1
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: BM2
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: BM1S
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: BM2S
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: CM1
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: CM2
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: A23
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: A32
!brief
! matrice
      TYPE(BIEF_OBJ), TARGET :: MBOR
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!brief
! matrice de diffusion
      TYPE(BIEF_OBJ), TARGET :: TM1
!brief BLOCK FOR DIRICHLET VALUES HBOR, UBOR AND VBOR
!
      TYPE(BIEF_OBJ), TARGET :: DIRBOR
!brief BLOCK OF MASKS
!
      TYPE(BIEF_OBJ), TARGET :: MASK
!brief BLOCK OF WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: TB
!brief BLOCK OF PRIVATE VECTORS
! tableau de travail prive pour l'utilisateur (defini dans princi)
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!brief BLOCKS OF VARIABLES AT TIME N+1
!
      TYPE(BIEF_OBJ), TARGET :: F
!brief BLOCKS OF VARIABLES AT TIME N
!
      TYPE(BIEF_OBJ), TARGET :: FN
!brief BLOCKS OF VARIABLES AFTER ADVECTION BY CHARACTERISTICS
!
      TYPE(BIEF_OBJ), TARGET :: FTILD
!brief
!
      TYPE(BIEF_OBJ), TARGET :: FNCAR
!brief BLOCKS OF MATRICES FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: MAT
!brief BLOCKS OF RIGHT HAND SIDES FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: RHS
!brief BLOCKS OF UNKNOWNS FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: UNK
!brief BLOCK OF CLANDESTINE VARIABLES
!
      TYPE(BIEF_OBJ), TARGET :: VARCL
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
!       KEY-WORDS AND PARAMETERS
!
!brief MAXIMUM NUMBER OF SOURCE POINTS
! nombre maximum de points sources
      INTEGER, PARAMETER :: MAXSCE = 100
!brief MAXIMUM NUMBER OF LIQUID / SOLID BOUNDARIES
!
      INTEGER, PARAMETER :: MAXFRO = 300
!brief MAXIMUM NUMBER OF OUTPUT VARIABLES
!
      INTEGER, PARAMETER :: MAXVAR = 100
!brief MAXIMUM NUMBER OF TRACERS
!
      INTEGER, PARAMETER :: MAXTRA = 20
!brief NUMBER OF NODES FOR THE CONTROL SECTIONS (TWO TIMES THE NUMBER OF CONTROL SECTIONS)
!
      INTEGER NCP
!brief ARRAY CONTAINING THE GLOBAL NUMBER OF POINTS IN THE CONTROL SECTIONS
! donnees sur les sections de controle
      INTEGER, ALLOCATABLE :: CTRLSC(:)
!
!     MAXIMUM RANK OF LOGICAL UNITS AS DECLARED IN SUBMIT STRINGS IN THE DICTIONARY
!
      INTEGER, PARAMETER :: MAXLU_T2D = 47
!
!     MAXIMUM NUMBER OF POINTS ON ONE SIDE OF A SINGULARITY (READ IN THE DATA)
! 
      INTEGER NPSMAX
!
!     MAXIMUM NUMBER OF SINGULARITIES
! 
      INTEGER, PARAMETER :: NWRMAX = 10
!brief
! nombre de points de chaque cote de chaque singularite
      INTEGER NPSING(NWRMAX)
!brief GEOMETRY FILE NUMBER
! fichier de geometrie
      INTEGER T2DGEO
!
!     BOUNDARY CONDITIONS FILE NUMBER
! 
      INTEGER T2DCLI
!
!     PREVIOUS COMPUTATION FILE NUMBER
!
      INTEGER T2DPRE
!
!     RESULTS FILE NUMBER
! 
      INTEGER T2DRES
!brief BOTTOM TOPOGRAPHY FILE NUMBER
! fichier des fonds
      INTEGER T2DFON
!brief BINARY DATA FILE 1
! fichier de donnees binaire 1
      INTEGER T2DBI1
!brief BINARY DATA FILE 2
! fichier de donnees binaire 2
      INTEGER T2DBI2
!brief FORMATTED DATA FILE 1
! fichier de donnees formate 1
      INTEGER T2DFO1
!brief FORMATTED DATA FILE 2
! fichier de donnees formate 2
      INTEGER T2DFO2
!brief BINARY RESULTS FILE NUMBER
! fichier de resultats binaire
      INTEGER T2DRBI
!brief FORMATTED RESULTS FILE NUMBER
! fichier de resultats formate
      INTEGER T2DRFO
!brief REFERENCE FILE NUMBER
! fichier de reference
      INTEGER T2DREF
!brief
!
      INTEGER T2DIMP
!brief FRICTION DATA FILE NUMBER
! fichier de donnees pour le frottement
      INTEGER T2DCOF
!
!     DELWAQ FILES NUMBERS
!
      INTEGER T2DDL1,T2DDL2,T2DDL3,T2DDL4,T2DDL5,T2DDL6,T2DDL7,T2DDL8
      INTEGER T2DDL9,T2DL10,T2DL11
!
!     STAGE-DISCHARGE CURVES FILE NUMBER
! 
      INTEGER T2DMAB
!
!     SOURCES FILE NUMBER
! 
      INTEGER T2DVEF
!
!     SECTIONS INPUT FILE NUMBER
! 
      INTEGER T2DSEC
!
!     SECTIONS OUTPUT FILE NUMBER
! 
      INTEGER T2DSEO
!
!     MIGRHYCAR STEERING FILE NUMBER
! 
      INTEGER T2DMIG
!
!     HARMONIC CONSTANTS FILE NUMBER
! 
      INTEGER T2DHAR
!
!     TIDAL MODEL FILE NUMBER
! 
      INTEGER T2DTID
!
!     TIDAL MODEL DATA BASE FILE NUMBER
! 
      INTEGER T2DBDD
!
!     GRAPHIC PRINTOUT PERIOD
! 
      INTEGER LEOPRD
!
!     LISTING PRINTOUT PERIOD
! 
      INTEGER LISPRD
!
!     NUMBER OF TIME STEPS
! 
      INTEGER NIT
!
!     TYPE OF ADVECTION (1:u and v, 2:h, 3:tracers, 4:k and epsilon)
!
      INTEGER ICONVF(4)
!
!     TURBULENCE MODEL
! 
      INTEGER ITURB
!
!     LAW OF BOTTOM FRICTION
! 
      INTEGER KFROT
!
!     NUMBER OF SUB-ITERATIONS FOR NON-LINEARITIES
!
      INTEGER NSOUI
!brief NOT USED (PROPAGATION OPTION)
! non utilise (option de propagation)
      INTEGER OPTPRO
!brief INITIAL GUESS FOR H
! ordre du tir initial pour h
      INTEGER IORDRH
!brief INITIAL GUESS FOR U
! ordre du tir initial pour u
      INTEGER IORDRU
!brief NUMBER OF SUB-ITERATIONS FOR NON-LINEARITIES
! nombre de sous-iterations pour les non-linearites
      INTEGER NSOUSI
!brief NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
! numero du premier pas de temps pour les sorties graphiques
      INTEGER PTINIG
!brief NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
! numero du premier pas de temps pour les sorties listing
      INTEGER PTINIL
!brief TURBULENCE MODEL FOR SOLID BOUNDARIES
! regime de turbulence pour les parois
      INTEGER LISRUG
!brief
! nombre de frontieres a debit impose
      INTEGER NDEBIT
!brief
! nombre de frontieres a cote imposee
      INTEGER NCOTE
!brief
! nombre de frontieres a vitesse imposee
      INTEGER NVITES
!brief
! nombre de frontieres a traceur impose
      INTEGER NTRACE
!brief
!
      INTEGER NVARCL
!brief VECTOR LENGTH
! longueur du vecteur
      INTEGER LVMAC
!brief OPTION FOR THE TREATMENT OF TIDAL FLATS
! option de traitement des bancs decouvrants
      INTEGER OPTBAN
!brief NUMBER OF DROGUES
! nombre de flotteurs
      INTEGER NFLOT
!brief PRINTOUT PERIOD FOR DROGUES
! periode pour les sorties flotteurs
      INTEGER FLOPRD
!brief NUMBER OF LAGRANGIAN DRIFTS
! nombre de derives lagrangiennes
      INTEGER NLAG
!brief MAXIMUM NUMBER OF RECORDS OF SUCCESIVE POSITIONS OF FLOATING BODIES
! nombre maximal d'enregistrements des positions successives des flotteurs
      INTEGER NITFLO
!brief BOTTOM SMOOTHINGS
! nombre de lissages du fond
      INTEGER LISFON
!brief DISCRETIZATIONS IN SPACE
! discretisations en espace
      INTEGER DISCRE(4)
!brief
! nombre de sources/puits
      INTEGER NREJET
!brief
! nombre de vitesses des sources donnees (si nrejeu=0 on considere que la vitesse des sources est egale a celle du courant)
      INTEGER NREJEU
!brief
! nombre de rejets de traceur
      INTEGER NREJTR
!brief SUPG OPTION
! option de supg
      INTEGER OPTSUP(4)
!brief HYBRID SCHEME OPTION
! option du schema hybride
      INTEGER OPTHYB(4)
!brief ORIGINAL DATE OF TIME
! tableau contenant la date de l'origine des temps
      INTEGER MARDAT(3)
!brief ORIGINAL HOUR OF TIME
! tableau contenant l'heure de l'origine des temps
      INTEGER MARTIM(3)
!brief
!
      INTEGER IELM0,IELM1,IELMH,IELMU,IELMT,IELMK,IELME
!brief MATRIX STORAGE
! stockage des matrices
      INTEGER OPTASS
!brief MATRIX-VECTOR PRODUCT
! produit matrice-vecteur
      INTEGER PRODUC
!brief NUMBER OF WEIRS
! nombre de seuils
      INTEGER NWEIRS
!brief NUMBER OF CULVERTS
! nombre de siphons
      INTEGER NSIPH
!brief
!
      INTEGER NTYPFR
!brief OPTION FOR LIQUID BOUNDARIES
! type de traitement pour les frontieres liquides
      INTEGER FRTYPE(MAXFRO)
!brief TREATMENT OF THE LINEAR SYSTEM
! traitement du systeme lineaire
      INTEGER SOLSYS
!brief VELOCITY PROFILES
! option pour les profils de vitesse
      INTEGER PROVEL(MAXFRO)
!brief NUMBER OF LIQUID BOUNDARIES
! nombre de frontieres liquides
      INTEGER NFRLIQ
!brief
!
      INTEGER NFRSOL
!brief
! numero du premier point de la frontiere liquide
      INTEGER DEBLIQ(MAXFRO)
!brief
! numero du dernier point de la frontiere liquide
      INTEGER FINLIQ(MAXFRO)
!brief
!
      INTEGER DEBSOL(MAXFRO),FINSOL(MAXFRO)
!brief
! points les plus proches des rejets
      INTEGER ISCE(MAXSCE)
!brief
! numero de l'entree d'une buse dans la numerotation des sources
      INTEGER ENTSIP(MAXSCE/2)
!brief
! numero de la sortie d'une buse dans la numerotation des sources
      INTEGER SORSIP(MAXSCE/2)
!brief OPTION FOR THE DIFFUSION OF TRACERS
! option pour la diffusion des traceurs
      INTEGER OPDTRA
!brief OPTION FOR THE DIFFUSION OF VELOCITIES
! option pour la diffusion des vitesses
      INTEGER OPDVIT
!brief TYPE OF SOURCES
! type des sources
      INTEGER OPTSOU
!brief NUMBER OF PRIVATE ARRAYS
! nombre de tableaux prives
      INTEGER NPRIV
!brief DELWAQ PRINTOUT PERIOD
! periode de sortie pour delwaq
      INTEGER WAQPRD
!brief TREATMENT OF NEGATIVE DEPTHS
! traitement des hauteurs negatives
      INTEGER OPT_HNEG
!brief CURRENT ITERATION NUMBER
! numero de l'iteration courante
      INTEGER LT
!brief RECORD NUMBER IN THE WAVE DRIVEN CURRENTS FILE
! numero de l'enregistrement dans le fichier de houle
      INTEGER NPTH
!brief NUMBER OF USER-DEFINED ZONES (E.G. FOR SETTING FRICTION) IN SUBROUTINE DEF_ZONES
!
      INTEGER NZONE
!brief NUMBER OF PERIODS FOR FOURIER ANALYSIS
! number of waves
      INTEGER NPERIAF
!brief OPTION FOR PARAMETER ESTIMATION, IDENTIFICATION METHOD
! methode d'identification
      INTEGER OPTID
!brief NUMBER OF REMARKABLE POINTS
!
      INTEGER NPTS
!brief LIST OF REMARKABLE POINTS
! liste de points
      INTEGER LIST_PTS(100)
!brief CHOICE OF COST-FUNCTION
! fonction cout
      INTEGER OPTCOST
!brief MAX. NUMB. OF ITER. FOR IDENTIFICATION
! maximum d'iterations pour l'identification
      INTEGER MAXEST
!brief COUPLING PERIOD
      INTEGER PERCOU
!brief COUPLING PERIOD FOR TOMAWAC
      INTEGER PERCOU_WAC
!brief FINITE VOLUME SCHEME
! option schema en volumes finis
      INTEGER OPTVF
!brief ORIGIN OF THE COORDINATE SYSTEM
! coordonnees de l'origine
      INTEGER I_ORIG
!brief ORIGIN OF THE COORDINATE SYSTEM
! coordonnees de l'origine
      INTEGER J_ORIG
!
!     NUMBER OF TRACERS
! 
      INTEGER NTRAC
!
!     INDEX FOR TEMPERATURE, FOR SALINITY
!
      INTEGER IND_T,IND_S
!
!     NUMBER OF POINTS GIVEN FOR EACH DISCHARGE-ELEVATIONS CURVES
!
      INTEGER PTS_CURVES(MAXFRO)
!
!     OPTION FOR TREATING STAGE-DISCHARGE CURVES
! 
      INTEGER STA_DIS_CURVES(MAXFRO)
!
!     DEBUGGER
!  
      INTEGER DEBUG
!
!     DEPTH IN FRICTION TERMS
!  
      INTEGER HFROT
!
!     LAW OF FRICTION ON LATERAL BOUNDARIES
!
      INTEGER KFROTL
!
!     TREATMENT OF FLUXES At THE BOUNDARIES
!
      INTEGER DIRFLU
!
!     OPTION FOR TIDAL BOUNDARY CONDITIONS
!
      INTEGER TIDALTYPE
!
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
!brief IF YES, COMPUTATION CONTINUED
! si oui, suite de calcul
      LOGICAL DEBU
!brief IF YES, LISTING PRINTOUT IS REQUIRED
! si oui, messages imprimes sur listing
      LOGICAL LISTIN
!brief IF YES, DIFFUSION OF VELOCITY
! si oui, il faut faire la diffusion de u,v
      LOGICAL DIFVIT
!brief IF YES, AIR PRESSURE IS TAKEN INTO ACCOUNT
! si oui, patmos (pression atmospherique) est rempli
      LOGICAL ATMOS
!brief IF YES, ADVECTION
! si oui, convection
      LOGICAL CONV
!brief IF YES, ADVECTION OF U AND V (1), H (2), TRACERS (3), K AND EPSILON (4)
! si oui, convection de u et v (1), h (2), traceurs (3), k et epsilon (4)
      LOGICAL CONVV(4)
!brief IF YES, DIFFUSION OF TRACERS
! si oui, il faut faire la diffusion du traceur
      LOGICAL DIFT
!brief IF YES, CORIOLIS
! si oui, prise en compte des efforts de coriolis
      LOGICAL CORIOL
!brief IF YES, INFORMATION PRINTED ON LISTING
! si oui, informations sur le gradient a restituer
      LOGICAL INFOGR
!brief IF YES, MASS-BALANCE
! si oui, on fait le bilan de masse
      LOGICAL BILMAS
!brief IF YES, H CLIPPING
! si oui, clipping de h
      LOGICAL CLIPH
!brief IF YES, WIND IS TAKEN INTO ACCOUNT
! si oui, prise en compte des efforts dus au vent
      LOGICAL VENT
!brief IF YES, PROPAGATION
! si oui, propagation
      LOGICAL PROPA
!brief IF YES, SPHERICAL COORDINATES
! si oui, coordonnees spheriques
      LOGICAL SPHERI
!brief IF YES, LINEARISED PROPAGATION
! si oui, la propagaton est linearisee
      LOGICAL PROLIN
!brief IF YES, VALIDATION
! si oui, validation
      LOGICAL VALID
!brief
!
      LOGICAL VERLIM
!brief IF YES, CONTINUITY CORRECTION
! si oui, correction de continuite sur les points a hauteur imposee (on corrige les vitesses)
      LOGICAL CORCON
!brief IF YES, INITIAL TIME SET TO ZERO
! si oui, remise a zero du temps
      LOGICAL RAZTIM
!brief IF YES, INFORMATION ABOUT K-EPSILON MODEL PRINTED ON LISTING
! si oui, informations sur le modele k-epsilon a restituer
      LOGICAL INFOKE
!brief
! si oui, presence d'elements masques
      LOGICAL MSK
!brief IF YES, STOP WHEN A STEADY STATE IS REACHED
! si oui, arret quand un etat permanent est atteint
      LOGICAL STOPER
!brief IF YES, ELEMENTS MASKED BY USER
! si oui, elements masques par l'utilisateur
      LOGICAL MSKUSE
!brief
!
      LOGICAL SORLEO(MAXVAR),SORIMP(MAXVAR)
!brief IF YES, DENSITY EFFECTS INCLUDED
! si oui, la masse volumique est variable
      LOGICAL ROVAR
!brief IF YES,TIDE GENERATING FORCE
! si oui, force generatrice de la maree
      LOGICAL MAREE
!brief
! si oui, traitement par caracteristiques des frontieres liquides
      LOGICAL THOMFR
!brief IF YES, C-U PRECONDITIONING
! si oui, preconditionnement c-u
      LOGICAL PRECCU
!brief IF YES, VARIABLE TIME-STEP
! si oui, pas de temps variable
      LOGICAL DTVARI
!brief IF YES, TIDAL FLATS
! si oui, bancs decouvrants
      LOGICAL BANDEC
!brief
! si oui, messages imprimes (messages d'erreurs toujours imprimes)
      LOGICAL ENTET
!brief IF YES, OIL SPILL MODEL
! si oui, modele de nappes d'hydrocarbures
      LOGICAL SPILL_MODEL
!brief IF YES, WAVE DRIVEN CURRENTS ARE TAKEN INTO ACCOUNT
! si oui, prise en compte des courants de houle
      LOGICAL COUROU
!brief IF YES, VERTICAL STRUCTURES ARE TAKEN INTO ACCOUNT
! si oui, prise en compte des structures verticales
      LOGICAL VERTIC
!brief IF YES, OUTPUT INITIAL CONDITIONS
! si oui, sortie des conditions initiales
      LOGICAL OUTINI
!brief IF YES, THE USER WANTS TO DEFINE ZONES
! si oui, definition de zones
      LOGICAL DEFZON
!brief IF YES, COMPATIBLE COMPUTATION OF FLUXES (RELATED TO FLUXES THROUGH SECTIONS)
! si oui, calcul compatible des flux
      LOGICAL COMFLU
!brief IF YES, PRINTS CUMULATED FLOWRATES
! si oui, impression du cumul des flux
      LOGICAL CUMFLO
!brief IF YES, OUTPUT OF SALINITY FOR DELWAQ
! si oui, salinite pour delwaq
      LOGICAL SALI_DEL
!brief IF YES, OUTPUT OF TEMPERATURE FOR DELWAQ
! si oui, temperature pour delwaq
      LOGICAL TEMP_DEL
!brief IF YES, OUTPUT OF VELOCITY FOR DELWAQ
! si oui, vitesse pour delwaq
      LOGICAL VELO_DEL
!brief IF YES, OUTPUT OF DIFFUSIVITY FOR DELWAQ
! si oui, diffusion pour delwaq
      LOGICAL DIFF_DEL
!
!-----------------------------------------------------------------------
!
!       6) REALS
!
!-----------------------------------------------------------------------
!
!brief TIME STEP
! pas de temps hydro
      DOUBLE PRECISION DT
!brief ACCELERATION OF GRAVITY
! acceleration de la pesanteur
      DOUBLE PRECISION GRAV
!brief FRICTION COEFFICIENT
! coefficient de frottement
      DOUBLE PRECISION FFON
!brief COEFFICIENT OF CORIOLIS
! parametre de coriolis
      DOUBLE PRECISION FCOR
!brief COEFFICIENT OF WIND INFLUENCE
! coefficient de frottement de l'air
      DOUBLE PRECISION FAIR
!brief
!
      DOUBLE PRECISION FUAIR,FVAIR
!brief INITIAL VALUES OF TRACERS
! valeurs initiales des traceurs
      DOUBLE PRECISION TRAC0(MAXTRA)
!brief COEFFICIENT FOR DIFFUSION OF TRACERS
! coefficient de diffusion des traceurs
      DOUBLE PRECISION DIFNU
!brief (SEMI-)IMPLICITATION FOR TRACERS
! (semi-)implicitation pour les traceurs
      DOUBLE PRECISION TETAT
!brief VELOCITY DIFFUSIVITY
! coefficient de diffusion des vitesses (viscosite laminaire)
      DOUBLE PRECISION PROPNU
!brief
! valeurs limites des tableaux h,u,v,t dans l'ordre suivant : hmin,hmax,umin,umax,...
      DOUBLE PRECISION BORNES(8)
!brief
!
      DOUBLE PRECISION EPSOUI
!brief IMPLICITATION FOR DEPTH
! implicitation pour la hauteur
      DOUBLE PRECISION TETAC
!brief IMPLICITATION FOR VELOCITY
! implicitation pour la vitesse (u et v)
      DOUBLE PRECISION TETAU
!brief IMPLICITATION FOR DIFFUSION OF VELOCITY
! implicitation pour la diffusion des vitesses
      DOUBLE PRECISION TETAD
!brief MASS-LUMPING COEFFICIENT ON H
! coefficient de mass-lumping sur h
      DOUBLE PRECISION AGGLOC
!brief MASS-LUMPING COEFFICIENT ON VELOCITY
! coefficient de mass-lumping sur la vitesse
      DOUBLE PRECISION AGGLOU
!brief MINIMUM VALUE OF DEPTH
! valeur minimum de h
      DOUBLE PRECISION HMIN
!brief TIME STEP REDUCTION FOR K-EPSILON MODEL
! reduction du pas de temps pour le modele k-epsilon
      DOUBLE PRECISION REDUC
!brief MEAN DEPTH FOR LINEARIZATION
! profondeur moyenne pour la linearisation
      DOUBLE PRECISION HAULIN
!brief WATER DENSITY
! masse volumique de l'eau a temperature moyenne, quand la salinite est nulle
      DOUBLE PRECISION ROEAU
!brief LATITUDE OF ORIGIN POINT
! latitude du point origine
      DOUBLE PRECISION LAMBD0
!
!     ROUGHNESS COEFFICIENT OF BOUNDARIES
!
      DOUBLE PRECISION SB
!brief INITIAL ELEVATION
! cote initiale
      DOUBLE PRECISION COTINI
!brief INITIAL DEPTH
! hauteur initiale
      DOUBLE PRECISION HAUTIN
!brief PRESCRIBED FLOWRATES
! tableau de debits imposes
      DOUBLE PRECISION DEBIT(MAXFRO)
!brief PRESCRIBED ELEVATIONS
! tableau de cotes de la surface libre imposees
      DOUBLE PRECISION COTE(MAXFRO)
!brief PRESCRIBED VELOCITIES
! tableau de composantes normales de la vitesse imposees
      DOUBLE PRECISION VITES(MAXFRO)
!brief PRESCRIBED TRACERS VALUES
! tableau de valeurs du traceur imposees
      DOUBLE PRECISION TRACER(MAXFRO*MAXTRA)
!brief
!
      DOUBLE PRECISION FLUX_BOUNDARIES(MAXFRO)
!brief ABSCISSAE OF SOURCES
! abscisses des rejets
      DOUBLE PRECISION XSCE(MAXSCE)
!brief ORDINATES OF SOURCES
! ordonnees des rejets
      DOUBLE PRECISION YSCE(MAXSCE)
!brief SOURCE WATER DISCHARGES TAKEN FROM THE STEERING FILE
! debits des rejets (entrant positif)
      DOUBLE PRECISION DSCE(MAXSCE)
!brief SOURCE DISCHARGES WITH VARIATION IN TIME
!
      DOUBLE PRECISION DSCE2(MAXSCE)
!brief SOURCE TRACER DISCHARGES TAKEN FROM THE STERING FILE
! valeurs des traceurs aux rejets
      DOUBLE PRECISION TSCE(MAXSCE,MAXTRA)
!brief SOURCES OF TRACERS WITH VARIATIONS IN TIME
!
      DOUBLE PRECISION TSCE2(MAXSCE,MAXTRA)
!brief VELOCITIES OF THE SOURCES ALONG X
! composante u du courant aux rejets
      DOUBLE PRECISION USCE(MAXSCE)
!brief VELOCITIES OF THE SOURCES ALONG Y
! composante v du courant aux rejets
      DOUBLE PRECISION VSCE(MAXSCE)
!brief
! section des siphons (numerotation des sources)
      DOUBLE PRECISION SECSCE(MAXSCE)
!brief
! coefficients de perte de charge lors d'un fonctionnement en sortie
      DOUBLE PRECISION CSSCE(MAXSCE)
!brief
! cote des entrees/sorties de buses
      DOUBLE PRECISION ALTSCE(MAXSCE)
!brief
! coefficients de perte de charge lors d'un fonctionnement en entree
      DOUBLE PRECISION CESCE(MAXSCE)
!brief
! angle des buses avec la verticale
      DOUBLE PRECISION DELSCE(MAXSCE)
!brief
! angle des buses avec l'axe ox
      DOUBLE PRECISION ANGSCE(MAXSCE)
!brief
! perte de charge lineaire de la conduite
      DOUBLE PRECISION LSCE(MAXSCE)
!brief UPWIND COEFFICIENTS
! coefficients de decentrement pour s.u.p.g (1:u et v, 2:h, 3:traceur, 4:k et epsilon)
      DOUBLE PRECISION COSUPG(4)
!brief STOP CRITERIA
! criteres d'arret dans l'ordre suivant : h, uv, t
      DOUBLE PRECISION CRIPER(3)
!brief
!
      DOUBLE PRECISION TMOY
!brief DIRECTION OF NORTH, COUNTER-CLOCK-WISE, STARTING FROM VERTICAL AXIS
! direction du nord en degres par rapport a l'axe des y (sens trigonometrique)
      DOUBLE PRECISION NORD
!brief NON-DIMENSIONAL DISPERSION COEFFICIENTS
! coefficients adimensionnels de dispersion
      DOUBLE PRECISION ELDER(2)
!brief LONGITUDE OF ORIGIN POINT
! longitude du point origine
      DOUBLE PRECISION PHI0
!brief MASS-LUMPING ON TRACERS
! mass-lumping sur les traceurs
      DOUBLE PRECISION AGGLOT
!brief DURATION
! duree du calcul
      DOUBLE PRECISION DUREE
!brief DESIRED COURANT NUMBER
! nombre de courant souhaite
      DOUBLE PRECISION CFLWTD
!brief MINIMUM DEPTH TO TAKE WIND INTO ACCOUNT
! profondeur limite pour le vent
      DOUBLE PRECISION HWIND
!brief THRESHOLD FOR NEGATIVE DEPTHS
! seuil pour les profondeurs negatives
      DOUBLE PRECISION HNEG
!brief FREE SURFACE GRADIENT COMPATIBILITY
! compatibilite du gradient de surface libre
      DOUBLE PRECISION TETAZCOMP
!brief TIME RANGE FOR FOURIER ANALYSIS
! bornes en temps pour l'analyse de fourier
      DOUBLE PRECISION TAFBGN
!brief TIME RANGE FOR FOURIER ANALYSIS
! bornes en temps pour l'analyse de fourier
      DOUBLE PRECISION TAFEND
!brief CURRENT TIME
! temps
      DOUBLE PRECISION AT
!brief FOURIER ANALYSIS PERIODS
! periodes d'analyse de fourier
      DOUBLE PRECISION PERIAF(50)
!brief ARRAY OF REALS TO READ INTO SELAFIN FILES
!
      REAL, ALLOCATABLE :: W(:)
!
!     TOLERANCES FOR IDENTIFICATION
!  
      DOUBLE PRECISION TOLEST(4)
!
!     NEWMARK TIME INTEGRATION COEFFICIENT
!  
      DOUBLE PRECISION GAMMA
!
!     ARRAY WITH STAGE-DISCHARGE CURVES
!
      DOUBLE PRECISION, ALLOCATABLE :: QZ(:,:,:)
!
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!
!brief TITLE
! titre du fichier cas
      CHARACTER*72 TITCAS
!brief
!
      CHARACTER*72 VARDES
!brief INITIAL CONDITIONS
! conditions initiales
      CHARACTER*72 CDTINI
!brief VARIABLES TO BE PRINTED
! variables a imprimer
      CHARACTER*72 VARIMP
!brief EQUATIONS
! equations
      CHARACTER*20 EQUA
!brief NAMES OF CLANDESTINE VARIABLES
! noms des variables clandestines
      CHARACTER*32 VARCLA(10)
!brief
!
      CHARACTER*32 TEXTE(MAXVAR),TEXTPR(MAXVAR)
!brief NAMES OF REMARKABLE POINTS
! noms des points
      CHARACTER*32 NAME_PTS(100)
!brief NAMES OF TRACERS
! noms des traceurs
      CHARACTER(LEN=32) NAMETRAC(MAXVAR)
!
!-----------------------------------------------------------------------
!
!       8) SLVCFG STRUCTURES
!
!-----------------------------------------------------------------------
!
!brief
!
      TYPE(SLVCFG) :: SLVPRO
!brief STRUCTURE WITH SOLVER OPTIONS FOR K
!
      TYPE(SLVCFG) :: SLVK
!brief STRUCTURE WITH SOLVER OPTIONS FOR E
!
      TYPE(SLVCFG) :: SLVEP
!brief SOLVER FOR DIFFUSION OF TRACERS
! solveur pour la diffusion des traceurs
      TYPE(SLVCFG) :: SLVTRA
!
!-----------------------------------------------------------------------
!
!       9) MESH STRUCTURE
!
!-----------------------------------------------------------------------
!
!brief
! structure du maillage
      TYPE(BIEF_MESH) :: MESH
!
!-----------------------------------------------------------------------
!
!      10) ALIASES
!
!-----------------------------------------------------------------------
!
!     DECLARATION OF POINTERS FOR ALIASES.
!     TARGETS ARE DEFINED IN POINT_TELEMAC2D.
!
!     ALIASES FOR WORKING VECTORS IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11
      TYPE(BIEF_OBJ),POINTER :: T12,T13,T14,T15,T16,T17,T18,T19,T20
      TYPE(BIEF_OBJ),POINTER :: T21,T22
!
!     ALIASES FOR VECTOR IN BLOCK PRIVE
!
      DOUBLE PRECISION,POINTER :: PRIVE1(:),PRIVE2(:)
      DOUBLE PRECISION,POINTER :: PRIVE3(:),PRIVE4(:)
!
!     CONNECTIVITY TABLE
! 
      TYPE(BIEF_OBJ), POINTER :: IKLE
!
!     COORDINATES OF POINTS IN THE MESH
! 
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X,Y
!
!     NUMBER OF ELEMENTS IN THE MESH
! 
      INTEGER, POINTER:: NELEM
!brief MAXIMUM NUMBER OF ELEMENTS IN THE MESH
! nombre maximal d'elements dans le maillage 2d
      INTEGER, POINTER:: NELMAX
!brief NUMBER OF BOUNDARY POINTS IN THE MESH
! nombre de points frontiere du maillage
      INTEGER, POINTER:: NPTFR
!brief MAXIMUM NUMBER OF BOUNDARY POINTS IN THE MESH
!
      INTEGER, POINTER:: NPTFRX
!brief DIMENSION OF SPACE
!
      INTEGER, POINTER:: DIM
!brief TYPE OF ELEMENT
!
      INTEGER, POINTER:: TYPELM
!brief NUMBER OF POINTS IN THE MESH
! nombre de points du maillage
      INTEGER, POINTER:: NPOIN
!brief MAXIMUM NUMBER OF POINTS IN THE MESH
!
      INTEGER, POINTER:: NPMAX
!brief MAXIMUM NUMBER OF POINTS NEIGHBOURS OF A POINT
!
      INTEGER, POINTER:: MXPTVS
!brief MAXIMUM NUMBER OF ELEMENTS NEIGHBOURS OF A POINT
!
      INTEGER, POINTER:: MXELVS
!brief VECTOR LENGTH OF THE MACHINE
!
      INTEGER, POINTER:: LV
!
!=======================================================================
!
!       ADDED DECLARATIONS FOR THE IDENTIFICATION OF PARAMETERS
!
!
!-----------------------------------------------------------------------
!
!       1) VECTORS
!
!-----------------------------------------------------------------------
!
!brief ADJOINT VARIABLE
!
      TYPE(BIEF_OBJ), TARGET :: PP
!brief ADJOINT VARIABLE
!
      TYPE(BIEF_OBJ), TARGET :: QQ
!brief ADJOINT VARIABLE
!
      TYPE(BIEF_OBJ), TARGET :: RR
!brief MEASURE
!
      TYPE(BIEF_OBJ), TARGET :: UD
!brief MEASURE
!
      TYPE(BIEF_OBJ), TARGET :: VD
!brief MEASURE
!
      TYPE(BIEF_OBJ), TARGET :: HD
!brief DIRECT VARIABLE AT ITERATION IT
!
      TYPE(BIEF_OBJ), TARGET :: UU
!brief DIRECT VARIABLE AT ITERATION IT
!
      TYPE(BIEF_OBJ), TARGET :: VV
!brief DIRECT VARIABLE AT ITERATION IT
!
      TYPE(BIEF_OBJ), TARGET :: HH
!brief DIRECT VARIABLE AT ITERATION IT+1
!
      TYPE(BIEF_OBJ), TARGET :: UIT1
!brief DIRECT VARIABLE AT ITERATION IT+1
!
      TYPE(BIEF_OBJ), TARGET :: VIT1
!brief DIRECT VARIABLE AT ITERATION IT+1
!
      TYPE(BIEF_OBJ), TARGET :: HIT1
!brief BOUNDARY VALUES FOR ADJOINT VARIABLE (ONLY DIRICHLET)
!
      TYPE(BIEF_OBJ), TARGET :: PBOR
!brief BOUNDARY VALUES FOR ADJOINT VARIABLE (ONLY DIRICHLET)
!
      TYPE(BIEF_OBJ), TARGET :: QBOR
!brief BOUNDARY VALUES FOR ADJOINT VARIABLE (ONLY DIRICHLET)
!
      TYPE(BIEF_OBJ), TARGET :: RBOR
!brief VECTOR USED TO CHANGE THE SET OF STRICKLERS'
!
      TYPE (BIEF_OBJ) :: DESC
!brief GRADIENT OF COST FUNCTION (ITERATION K)
!
      TYPE (BIEF_OBJ) :: GRADJ
!brief GRADIENT OF COST FUNCTION (ITERATION K-1)
!
      TYPE (BIEF_OBJ) :: GRADJN
!brief SET OF STRICKLERS' (ZONES): NEW
!
      TYPE (BIEF_OBJ) :: SETSTR
!brief SET OF STRICKLERS' (ZONES): OLD
!
      TYPE (BIEF_OBJ) :: SETSTR2
!brief GRADIENT
!
      TYPE (BIEF_OBJ) :: ALPHA1
!brief GRADIENT
!
      TYPE (BIEF_OBJ) :: ALPHA2
!brief GRADIENT
!
      TYPE (BIEF_OBJ) :: ALPHA3
!
!
!-----------------------------------------------------------------------
!
!       2) MATRICES
!
!-----------------------------------------------------------------------
!
!       MATRICES
!
!       SYSTEM SOLVED IN PROPAG WILL BE :
!
!     ( TAM1  TCM1  TCM2 )   (PP )      ( CV1 )
!     (                  )   (   )      (     )
!     ( TBM1  TAM2  A23  )   (QQ )  =   ( CV2 )
!     (                  )   (   )      (     )
!     ( TBM2  A32   TAM3 )   (RR )      ( CV3 )
!
!
! PAY ATTENTION TO CV1, ...
!             I USE THE SAME SYMBOLS OF THE RIGHT HAND SIDE OF DIRECT
!             SYSTEM.
!
!brief
!
      TYPE(BIEF_OBJ), TARGET :: TAM1,TAM2,TAM3,TBM1,TBM2
!brief
!
      TYPE(BIEF_OBJ), TARGET :: TCM1,TCM2
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!       BLOCKS OF MATRICES, RIGHT HAND SIDES AND UNKNOWNS
!       FOR LINEAR SYSTEMS
!
!brief BLOCKS OF MATRICES, RIGHT HAND SIDES FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: MATADJ
!brief BLOCKS OF UNKNOWNS FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: UNKADJ
!brief BLOCK OF ADJOINT VARIABLES FOR OUTPUT
!
      TYPE(BIEF_OBJ), TARGET :: VARSORA
!brief BLOCK OF DIRICHLET CONDITION FOR ADJOINT VARIABLES
!
      TYPE(BIEF_OBJ), TARGET :: ADJDIR
!
!-----------------------------------------------------------------------
!
!       4) INTEGERS
!
!brief NUMBER OF VARIABLES FOR LITENR (READ BY SKIPGEO)
!
      INTEGER NVARRES
!brief NUMBER OF ITERATIONS FOR PARAMETERS CALIBRATION
!
      INTEGER NITERA
!
!-----------------------------------------------------------------------
!
!        5) LOGICALS
!
!brief IF YES: ACTIVATES THE ADJOINT MODE IN TELEMAC2D/PROPAG
!
      LOGICAL ADJO
!brief WHETHER THE ADJOINT SYSTEM RESULTS ARE PRINTED; NB 100=MAXVAR
!
      LOGICAL SORLEOA(100)
!brief WHETHER ...; NB 100=MAXVAR
!
      LOGICAL SORIMPA(100)
!
!-----------------------------------------------------------------------
!
!       6) STRINGS
!
!-----------------------------------------------------------------------
!
!brief NAMES OF VARIABLES IN RES FILE
!
      CHARACTER(LEN=32) TEXRES(100)
!brief NAMES OF VARIABLES IN RBI FILE
!
      CHARACTER(LEN=32) TEXRBI(100)
!brief NAMES OF VARIABLES IN REF FILE
!
      CHARACTER(LEN=32) TEXREF(100)
!brief KEYWORD "PARAMETER ESTIMATION"
! estimation de parametre
      CHARACTER(LEN=72) ESTIME
!
!=======================================================================
!
!brief
!
      CHARACTER(LEN=144) SUBMIT(4,300)
!
!-----------------------------------------------------------------------
!
!       7) FRICTION DEFINITION BY ZONE
!
!-----------------------------------------------------------------------
!
!brief
!
      TYPE(FRICTION_OBJ), TARGET :: FRTAB
!brief
!
      TYPE(BIEF_OBJ), TARGET :: KFROPT, NKFROT
!brief
!
      TYPE(BIEF_OBJ), TARGET :: NDEFMA, LINDDP, LINDSP
!brief
!
      TYPE(BIEF_OBJ), TARGET :: NDEF_B, KFRO_B
!brief
!
      INTEGER:: NZONES
!brief MAXIMUM NUMBER OF FRICTION DOMAINS
! nombre maximum de domaines de frottement
      INTEGER:: NZONMX
!brief IF YES, NON-SUBMERGED VEGETATION FRICTION
! si oui, frottement pour la vegetation non submergee
      LOGICAL:: LINDNER
!brief IF YES, FRICTION DATA
! si oui, donnees pour le frottement
      LOGICAL:: FRICTB
!brief DEFAULT MANNING VALUE (FOR COLEBROOK-WHITE LAW)
! valeur par defaut du manning pour la loi de colebrook-white
      DOUBLE PRECISION :: NDEF
!brief DIAMETER OF ROUGHNESS ELEMENTS
! diametre des elements de frottement
      DOUBLE PRECISION :: DP
!brief SPACING OF ROUGHNESS ELEMENTS
! espacement des elements de frottement
      DOUBLE PRECISION :: SP
!
!-----------------------------------------------------------------------
!
!      8) TELEMAC-2D FILES
!
!-----------------------------------------------------------------------
!
!brief TELEMAC-2D FILES
!
      TYPE(BIEF_FILE) :: T2D_FILES(MAXLU_T2D)
!
!-----------------------------------------------------------------------
!
!      9) SECTIONS
!
!-----------------------------------------------------------------------
!
!brief
!
      TYPE (CHAIN_TYPE), ALLOCATABLE :: CHAIN(:)
!
      SAVE
!
      END MODULE DECLARATIONS_TELEMAC2D

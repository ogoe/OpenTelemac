!                    ***************************
                     MODULE DECLARATIONS_SISYPHE
!                    ***************************
!
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    DECLARATION OF PRINICIPAL SISYPHE VARIABLES
!
!history  CV
!+        15/03/2009
!+
!+   ADDED VARIABLES MU, KS, KSP, KSR
!
!history  CV
!+        18/07/2009
!+
!+   VITCE AND VITCD ARE CONSTANT
!
!history  JMH
!+        13/08/2009
!+
!+   IT5 DELETED
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
!brief EVOLUTION
!
      TYPE(BIEF_OBJ), TARGET :: E
!brief EVOLUTION SAVED FOR CONSTANT FLOW DISCHARGE
!
      TYPE(BIEF_OBJ), TARGET :: ECPL
!brief FREE SURFACE ELEVATION
! cote de la surface libre
      TYPE(BIEF_OBJ), TARGET :: Z
!brief INCREMENT OF FREE SURFACE ELEVATION WHEN READING AN HYDRO FILE
!
      TYPE(BIEF_OBJ), TARGET :: DEL_Z
!brief EVOLUTION DUE TO BEDLOAD
!
      TYPE(BIEF_OBJ), TARGET :: ZF_C
!brief EVOLUTION DUE TO SUSPENSION
!
      TYPE(BIEF_OBJ), TARGET :: ZF_S
!brief CUMULATED BED EVOLUTION
! evolution totale
      TYPE(BIEF_OBJ), TARGET :: ESOMT
!brief MAXIMUM EVOLUTION
!
      TYPE(BIEF_OBJ), TARGET :: EMAX
!brief COMPONENTS OF DEPTH-AVERAGED FLOW RATE
! composantes du debit vectoriel
      TYPE(BIEF_OBJ), TARGET :: QU
!brief COMPONENTS OF DEPTH-AVERAGED FLOW RATE
! composantes du debit vectoriel
      TYPE(BIEF_OBJ), TARGET :: QV
!brief INCREMENTS OF FLOW RATE COMPONENTS WHEN READING AN HYDRO FILE
!
      TYPE(BIEF_OBJ), TARGET :: DEL_QU
!brief INCREMENTS OF FLOW RATE COMPONENTS WHEN READING AN HYDRO FILE
!
      TYPE(BIEF_OBJ), TARGET :: DEL_QV
!brief FLOW RATE
! debit liquide
      TYPE(BIEF_OBJ), TARGET :: Q
!brief SOLID DISCHARGE
!
      TYPE(BIEF_OBJ), TARGET :: QS
!brief SOLID DISCHARGE
!
      TYPE(BIEF_OBJ), TARGET :: QSX
!brief SOLID DISCHARGE
!
      TYPE(BIEF_OBJ), TARGET :: QSY
!brief SOLID DISCHARGE (BEDLOAD)
!
      TYPE(BIEF_OBJ), TARGET :: QS_C
!brief SOLID DISCHARGE (BEDLOAD)
!
      TYPE(BIEF_OBJ), TARGET :: QSXC
!brief SOLID DISCHARGE (BEDLOAD)
!
      TYPE(BIEF_OBJ), TARGET :: QSYC
!brief SOLID DISCHARGE (SUSPENSION)
!
      TYPE(BIEF_OBJ), TARGET :: QS_S
!brief SOLID DISCHARGE (SUSPENSION)
!
      TYPE(BIEF_OBJ), TARGET :: QSXS
!brief SOLID DISCHARGE (SUSPENSION)
!
      TYPE(BIEF_OBJ), TARGET :: QSYS
!brief WATER DEPTH
! hauteur d'eau au temps n
      TYPE(BIEF_OBJ), TARGET :: HN
!brief
!
      TYPE(BIEF_OBJ), TARGET :: HCLIP
!brief
!
      TYPE(BIEF_OBJ), TARGET :: U2D,V2D
!brief
! intensite du courant
      TYPE(BIEF_OBJ), TARGET :: UNORM
!brief WATER DEPTH SAVED FOR CONSTANT FLOW DISCHARGE
!
      TYPE(BIEF_OBJ), TARGET :: HCPL
!brief IMPOSED BED EVOLUTION AT THE BOUNDARY
! evolution aux points de bord
      TYPE(BIEF_OBJ), TARGET :: EBOR
!brief IMPOSED SOLID TRANSPORT AT THE BOUNDARY
!
      TYPE(BIEF_OBJ), TARGET :: QBOR
!brief ZF VALUES ON BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: FLBOR
!brief BOTTOM ELEVATION
! cote du fond
      TYPE(BIEF_OBJ), TARGET :: ZF
!brief NON ERODABLE (RIGID) BOTTOM ELEVATION
!
      TYPE(BIEF_OBJ), TARGET :: ZR
!brief REFERENCE ELEVATION
!
      TYPE(BIEF_OBJ), TARGET :: ZREF
!brief INTEGRAL OF BASES
!
      TYPE(BIEF_OBJ), TARGET :: VOLU2D
!brief INTEGRAL OF BASES IN PARALLEL
!
      TYPE(BIEF_OBJ), TARGET :: V2DPAR
!brief INVERSE OF INTEGRAL OF BASES
!
      TYPE(BIEF_OBJ), TARGET :: UNSV2D
!brief BOTTOM FRICTION COEFFICIENT (CHEZY, NIKURADSE OR STICKLER)
! coefficients de frottement sur le  fond (mot cle)
      TYPE(BIEF_OBJ), TARGET :: CHESTR
!brief ANGLE BETWEEN QS AND Q
!
      TYPE(BIEF_OBJ), TARGET :: CALFA
!brief ANGLE BETWEEN QS AND Q
!
      TYPE(BIEF_OBJ), TARGET :: SALFA
!brief VOID STRUCTURE
!
      TYPE(BIEF_OBJ), TARGET :: S
!brief MASK
!
      TYPE(BIEF_OBJ), TARGET :: MASKPT
!brief MASK
!
      TYPE(BIEF_OBJ), TARGET :: MASKTR
!brief MASK
!
      TYPE(BIEF_OBJ), TARGET :: MASKB
!brief MASK
!
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!brief MASK
!
      TYPE(BIEF_OBJ), TARGET :: MSKTMP
!brief WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: W1
!
! WAVE DATA
! --------
!
!brief WAVE DIRECTION (DEG WRT OX AXIS)    !!!!!SOME SAY OY AXIS!!!!!
! angle d'attaque de la houle (par rapport a l'axe oy)
      TYPE(BIEF_OBJ), TARGET :: THETAW
!brief FRICTION COEFFICIENT (WAVES)
! coefficient de frottement quadratique de la houle
      TYPE(BIEF_OBJ), TARGET :: FW
!brief ORBITAL VELOCITY
! courant orbital
      TYPE(BIEF_OBJ), TARGET :: UW
!brief SIGNIFICANT WAVE HEIGHT
! hauteur de houle
      TYPE(BIEF_OBJ), TARGET :: HW
!brief MEAN WAVE PERIOD
! periode de houle
      TYPE(BIEF_OBJ), TARGET :: TW
!brief
!
      TYPE(BIEF_OBJ), TARGET :: INDIC,IFAMAS
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT1
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT2
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT3
!brief INTEGER WORKING ARRAY
!
      TYPE(BIEF_OBJ), TARGET :: IT4
!brief TYPE OF BOUNDARY CONDITIONS ON BED EVOLUTION
! types de conditions aux limites sur l'evolution
      TYPE(BIEF_OBJ), TARGET :: LIEBOR
!brief TYPE OF BOUNDARY CONDITIONS ON SAND TRANSPORT RATE
!
      TYPE(BIEF_OBJ), TARGET :: LIQBOR
!brief TYPE OF BOUNDARY CONDITIONS
!
      TYPE(BIEF_OBJ), TARGET :: LIMTEC
!brief IMPACT OF THE SLOPE EFFECT ON AMPLITUDE
!
      TYPE(BIEF_OBJ), TARGET :: COEFPN
!brief LIQUID BOUNDARY NUMBERING
!
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!brief SHEAR STRESS
! contrainte de frottement en courant seul
      TYPE(BIEF_OBJ), TARGET :: TOB
!brief FRICTION COEFFICIENT
! coefficient de frottement quadratique du courant
      TYPE(BIEF_OBJ), TARGET :: CF
!brief WAVE INDUCED SHEAR STRESS
! contrainte de frottement en houle seule
      TYPE(BIEF_OBJ), TARGET :: TOBW
!brief
! rapport entre la contrainte de frottement de peau et la contrainte totale
      TYPE(BIEF_OBJ), TARGET :: MU
!brief
! rugosite totale
      TYPE(BIEF_OBJ), TARGET :: KS
!brief
! rugosite de peau
      TYPE(BIEF_OBJ), TARGET :: KSP
!brief
! rugosite de ride
      TYPE(BIEF_OBJ), TARGET :: KSR
!brief BED LEVEL CHANGE FOR GRAIN-FEEDING
!
      TYPE(BIEF_OBJ), TARGET :: DZF_GF
!
! NON-EQUILIBRIUM BEDLOAD AND NON-UNIFORM BED MATERIA (BMD AND MGDL)
! --------
!
!brief MEAN DIAMETER OF ACTIVE-LAYER
! diametre moyen du sediment
      TYPE(BIEF_OBJ), TARGET :: ACLADM
!brief MEAN DIAMETER OF UNDER-LAYER
!
      TYPE(BIEF_OBJ), TARGET :: UNLADM
!brief NUMBER OF LAYERS FOR EACH POINT
!
      TYPE(BIEF_OBJ), TARGET :: NLAYER
!brief HIDING FACTOR FOR PARTICULAR SIZE CLASS
!
      TYPE(BIEF_OBJ), TARGET :: HIDING
!brief ACTIVE LAYER THICKNESS FOR EACH POINT
!
      TYPE(BIEF_OBJ), TARGET :: ELAY
!brief ACTIVE STRATUM THICKNESS FOR EACH POINT
!
      TYPE(BIEF_OBJ), TARGET :: ESTRAT
!
! SUSPENSION  (F. MENARD)
! --------
!
!brief DEPOSITION FLUX
!
      TYPE(BIEF_OBJ), TARGET :: FLUDP
!brief DEPOSITION FLUX
!
      TYPE(BIEF_OBJ), TARGET :: FLUDPT
!brief EROSION FLUX
!
      TYPE(BIEF_OBJ), TARGET :: FLUER
!brief EROSION FLUX
!
      TYPE(BIEF_OBJ), TARGET :: FLUERT
!brief CONCENTRATION AT TIME N
!
      TYPE(BIEF_OBJ), TARGET :: CS
!brief
!
      TYPE(BIEF_OBJ), TARGET :: CST, CTILD, CSTAEQ
!brief IMPOSED SUSPENDED SAND CONCENTRATION AT THE BOUNDARY (DIM.NPTFR)
!
      TYPE(BIEF_OBJ), TARGET :: CBOR
!brief CONCENTRATION IN G/L
!
      TYPE(BIEF_OBJ), TARGET :: CSGL
!brief COORDINATES OF VELOCITY VECTORS
!
      TYPE(BIEF_OBJ), TARGET ::  UCONV
!brief COORDINATES OF VELOCITY VECTORS
!
      TYPE(BIEF_OBJ), TARGET ::  VCONV
!brief PROPAGATION HEIGHT
!
      TYPE(BIEF_OBJ), TARGET :: HPROP
!brief
!
      TYPE(BIEF_OBJ), TARGET :: DISP,DISP_C
!brief
!
      TYPE(BIEF_OBJ), TARGET :: AFBOR  , BFBOR
!brief FLUX AT THE BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: FLBOR_SIS
!brief FLUX AT THE BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: FLBORTRA
!
!     BOUNDARY CONDITIONS FOR SEDIMENT                     : LICBOR
!     TYPES OF BOUNDARY CONDITIONS FOR H                   : LIHBOR
!     TYPES OF BOUNDARY CONDITIONS FOR PROPAGATION         : LIMPRO
!                    POINTS   :    .1:H  .2:U  .3:V
!                    SEGMENTS :    .4:H  .5:U  .6:V
!
!brief TYPE OF BOUNDARY CONDITIONS ON SUSPENDED SAND CONCENTRATION
!
      TYPE(BIEF_OBJ), TARGET :: LICBOR
!brief TYPE OF BOUNDARY CONDITIONS FOR H
!
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!brief TYPE OF BOUNDARY CONDITIONS FOR PROPAGATION
!
      TYPE(BIEF_OBJ), TARGET :: LIMPRO
!brief
!
      TYPE(BIEF_OBJ), TARGET :: LIMDIF
!brief LAST LINE OF THE BOUNDARY CONDITION FILE
!
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!brief BOUNDARY CONDITIONS FOR TRACER (MODIFIED LITBOR)
!
      TYPE(BIEF_OBJ), TARGET :: CLT
!brief BOUNDARY CONDITIONS FOR U
!
      TYPE(BIEF_OBJ), TARGET :: CLU
!brief BOUNDARY CONDITIONS FOR V
!
      TYPE(BIEF_OBJ), TARGET :: CLV
!brief WORKING ARRAY FOR ELEMENTS
!
      TYPE(BIEF_OBJ), TARGET :: TE1
!brief WORKING ARRAY FOR ELEMENTS
!
      TYPE(BIEF_OBJ), TARGET :: TE2
!brief WORKING ARRAY FOR ELEMENTS
!
      TYPE(BIEF_OBJ), TARGET :: TE3
!brief COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!
      TYPE(BIEF_OBJ), TARGET :: KX
!brief COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!
      TYPE(BIEF_OBJ), TARGET :: KY
!brief COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!
      TYPE(BIEF_OBJ), TARGET :: KZ
!brief ARRAY THAT INDICATES WHETHER THE NON-ERODABLE BOTTOM HAS BEEN REACHED (VF)
!
      TYPE(BIEF_OBJ), TARGET :: BREACH
!brief FOR MIXED SEDIMENTS
!
      TYPE(BIEF_OBJ), TARGET :: FLUER_VASE
!brief FOR MIXED SEDIMENTS
!
      TYPE(BIEF_OBJ), TARGET :: TOCE_MIXTE
!brief FOR MIXED SEDIMENTS
!
      TYPE(BIEF_OBJ), TARGET :: MS_SABLE
!brief FOR MIXED SEDIMENTS
!
      TYPE(BIEF_OBJ), TARGET :: MS_VASE
!
!-----------------------------------------------------------------------
!
!       2) MATRICES
!
!-----------------------------------------------------------------------
!
!brief
!
      TYPE(BIEF_OBJ), TARGET :: MBOR
!brief
!
      TYPE(BIEF_OBJ), TARGET :: AM1_S,AM2_S
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!brief BLOCK OF MASKS
!
      TYPE(BIEF_OBJ), TARGET :: MASK
!brief BLOCK OF WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: TB
!brief BLOCK OF PRIVATE VECTORS
!
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!brief BLOCK OF CLANDESTINE VARIABLES
!
      TYPE(BIEF_OBJ), TARGET :: VARCL
!brief BLOCK OF VARIABLES FOR INPUT
!
      TYPE(BIEF_OBJ), TARGET :: VARHYD
!brief BLOCK OF VARIABLES FOR OUTPUT
!
      TYPE(BIEF_OBJ), TARGET :: VARSOR
!brief SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
!
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE::AVAIL
!brief SEDIMENT COMPOSITION
!
      TYPE(BIEF_OBJ), TARGET :: AVAI
!brief LAYER THICKNESSES
!
      TYPE(BIEF_OBJ), TARGET :: LAYTHI
!brief
!
      TYPE(BIEF_OBJ), TARGET :: QSCL
!brief
!
      TYPE(BIEF_OBJ), TARGET :: QSCLX , QSCLY
!brief
!
      TYPE(BIEF_OBJ), TARGET :: QSCL_C
!brief
!
      TYPE(BIEF_OBJ), TARGET :: QSCLXC, QSCLYC
!brief
!
      TYPE(BIEF_OBJ), TARGET :: QSCL_S
!brief
!
      TYPE(BIEF_OBJ), TARGET :: QSCLXS, QSCLYS
!brief
!
      TYPE(BIEF_OBJ), TARGET :: ZFCL
!brief
!
      TYPE(BIEF_OBJ), TARGET :: ZFCL_C
!brief
!
      TYPE(BIEF_OBJ), TARGET :: ZFCL_S
!
!-----------------------------------------------------------------------
!
!       4) INTEGERS
!
!-----------------------------------------------------------------------
!
!       KEYWORDS AND PARAMETERS
!
!brief MAXIMUM NUMBER OF OUTPUT VARIABLES
!
      INTEGER, PARAMETER :: MAXVAR = 500
!brief MAXIMUM NUMBER OF (LIQUID BOUNDARIES, SOLID BOUNDARIES)
!
      INTEGER, PARAMETER :: MAXFRO = 300
!brief
!
      INTEGER NFRLIQ,NFRSOL
!brief
!
      INTEGER DEBLIQ(MAXFRO),FINLIQ(MAXFRO)
!brief
!
      INTEGER DEBSOL(MAXFRO),FINSOL(MAXFRO)
!brief OPTION FOR THE DIFFUSION OF TRACER
! option pour la diffusion du traceur
      INTEGER OPDTRA
!brief OPTION FOR THE DISPERSION
! option pour la dispersion
      INTEGER OPTDIF
!brief 'SUPG OPTION'
!
      INTEGER OPTSUP
!brief NUMBER OF ITERATIONS WITH CONSTANT FLOW DISCHARGE
! nombre d'iterations pour telemac
      INTEGER :: NCONDIS
!brief LAW OF BOTTOM FRICTION
! loi de frottement sur le fond
      INTEGER KFROT
!brief BED-LOAD TRANSPORT FORMULA
! formule de transport solide
      INTEGER ICF
!brief
!
      INTEGER NPAS
!brief NUMBER OF TIDES OR FLOODS
! nombre de marees ou crues
      INTEGER NMAREE
!brief
!
      INTEGER LEOPR
!brief
!
      INTEGER LISPR
!brief
!
      INTEGER NVARCL
!brief
!
      INTEGER IELMT,IELMH_SIS,IELMU_SIS,IELMX
!brief
! standard du fichier de geometrie
      INTEGER STDGEO
!brief
!
      INTEGER LOGDES ,LOGPRE ,OPTBAN ,LVMAC
!brief HYDRODYNAMIC CODE
! code de calcul utilise pour l'hydrodynamique
      INTEGER HYDRO
!brief MATRIX STORAGE
! stockage des matrices
      INTEGER OPTASS
!brief NUMBER OF SUB-ITERATIONS
! nombre de sous-iterations
      INTEGER NSOUS
!brief
!
      INTEGER MARDAT(3),MARTIM(3),PRODUC
!brief OPTION FOR THE TREATMENT OF NON ERODABLE BEDS
! option de traitement des fonds non erodables
      INTEGER CHOIX
!brief
!
      INTEGER PTINIL, PTINIG
!brief NUMBER OF PRIVATE ARRAYS
! nombre de tableaux prives
      INTEGER NPRIV
!brief COUPLING PERIOD
! periode de couplage
      INTEGER PERCOU
!brief NUMERO DU PAS DE TEMPS
!
      INTEGER LT
!brief
!
      INTEGER RESOL
!brief
!
      INTEGER DEPER
!brief FORMULA FOR DEVIATION
! formule pour la deviation
      INTEGER DEVIA
!brief FORMULA FOR SLOPE EFFECT
! formule pour effet de pente
      INTEGER SLOPEFF
!
! NON-EQUILIBRIUM BEDLOAD AND NON-UNIFORM BED MATERIA (BMD AND MGDL)
! --------
!
!brief MAXIMUM NUMBER OF SIZE-CLASSES
!
      INTEGER, PARAMETER :: NSICLM = 10
!brief NUMBER OF SIZE-CLASSES OF BED MATERIAL (LESS THAN 10)
! nombre de classes granulometriques
      INTEGER :: NSICLA
!brief MAXIMUM NUMBER OF LAYERS ON THE MESH
!
      INTEGER,PARAMETER :: NLAYMAX = 10
!brief NUMBER OF BED LOAD MODEL LAYERS
! nombre de couches pour granulo etendue
      INTEGER NOMBLAY
!brief FORMULATION FOR THE HIDING FACTOR
!
      INTEGER HIDFAC
!brief
!
      INTEGER, PARAMETER :: TMCOD_SISTEL = 0
!brief FOR NON-EQUILIBRIUM BEDLOAD
!
      INTEGER :: LOADMETH
!brief DEBUGGER
! debugger
      INTEGER :: DEBUG
!brief REFERENCE CONCENTRATION FORMULA
! formule pour la concentration de reference
      INTEGER :: ICQ
!brief NUMBER OF CONTROL SECTIONS POINTS
!
      INTEGER NCP
!brief ARRAY CONTAINING THE GLOBAL NUMBER OF THE POINTS IN THE CONTROL SECTIONS
! donnees sur les sections de controle
      INTEGER, ALLOCATABLE :: CTRLSC(:)
!brief COORDINATE OF THE ORIGIN
      INTEGER I_ORIG
!brief COORDINATE OF THE ORIGIN
      INTEGER J_ORIG
!brief NUMBER OF LAYERS FOR CONSOLIDATION
      INTEGER NCOUCH_TASS
!brief SKIN FRICTION CORRECTION
      INTEGER ICR
!brief BED ROUGHNESS PREDICTOR OPTION
      INTEGER IKS
!brief CONSOLIDATION MODEL
      INTEGER ITASS
!
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
!
!brief GRAPHICAL OUTPUT
!
      LOGICAL :: SORLEO(MAXVAR)
!brief LISTING OUTPUT
!
      LOGICAL :: SORIMP(MAXVAR)
!brief MASKING
!
      LOGICAL :: MSK
!brief WRITES OUT (OR NOT)
!
      LOGICAL :: ENTET
!brief RESOLUTION FOR SUSPENSION IS IMPLICIT (OR NOT)
!
      LOGICAL :: YASMI
!brief SPHERICAL EQUATIONS (HARD-CODED)
!
      LOGICAL :: SPHERI
!brief STEADY HYDRODYNAMICS
!
      LOGICAL :: PERMA
!brief TIDAL FLATS
!
      LOGICAL :: BANDEC
!brief WAVE EFFECT
! si oui, prise en compte de la houle
      LOGICAL :: HOULE
!brief FALL VELOCITY (PARTIALLY HARD-CODED)
!
      LOGICAL :: CALWC
!brief SHIELDS PARAMETER
!
      LOGICAL :: CALAC
!brief BEDLOAD
!
      LOGICAL :: CHARR
!brief LOADING LAW USED OR NOT
!
      LOGICAL :: NOEQUBED
!brief FINITE VOLUMES
! si oui, volumes finis
      LOGICAL :: VF
!brief MASS-LUMPING
!
      LOGICAL :: LUMPI
!brief CONSTANT FLOW DISCHARGE
!
      LOGICAL :: LCONDIS
!brief GRAIN-FEEDING
! si oui, grain-feeding
      LOGICAL :: LGRAFED
!brief CONSTANT ACTIVE LAYER THICKNESS
! si oui, epaisseur de couche active constante
      LOGICAL :: CONST_ALAYER
!brief SUSPENSION
! si oui, prise en compte de la suspension
      LOGICAL :: SUSP
!brief MASS BALANCE
!
      LOGICAL :: BILMA
!brief VALIDATION
! si oui, validation
      LOGICAL :: VALID
!brief IMPOSED CONCENTRATION IN INFLOW
! si oui, concentration d'equilibre en entree
      LOGICAL :: IMP_INFLOW_C
!brief SECONDARY CURRENTS
! si oui, courants secondaires
      LOGICAL :: SECCURRENT
!brief MASS CONCENTRATIONS IN G/L
! si oui, concentration massique
      LOGICAL :: UNIT
!brief CORRECTION ON CONVECTION VELOCITY
! si oui, correction du champ convecteur
      LOGICAL :: CORR_CONV
!brief COMPUTATION CONTINUED
! si oui, suite de calcul
      LOGICAL :: DEBU
!brief DIFFUSION OF SUSPENDED SEDIMENT CONCENTRATION
! si oui, diffusion
      LOGICAL :: DIFT
!brief SEDIMENT SLIDE
! si oui, glissement du sediment
      LOGICAL :: SLIDE
!brief COHESIVE SEDIMENTS (FOR EACH CLASS)
! si oui, sediments cohesifs
      LOGICAL :: SEDCO(NSICLM)
!brief CONSOLIDATION TAKEN INTO ACCOUNT
! si oui, tassement du lit cohesif
      LOGICAL :: TASS
!brief MIXED SEDIMENTS
! si oui, sediment mixte
      LOGICAL :: MIXTE
!brief COUPLING WITH DREDGESIM
! si oui, couplage avec dredgesim
      LOGICAL :: DREDGESIM
!brief BED FRICTION PREDICTION
!
      LOGICAL :: KSPRED
!
!-----------------------------------------------------------------------
!
!       6) REALS
!
!-----------------------------------------------------------------------
!
!brief
!
      DOUBLE PRECISION RC
!brief WATER DENSITY
! masse volumique de l'eau
      DOUBLE PRECISION XMVE
!brief SAND DENSITY
! masse volumique du sediment
      DOUBLE PRECISION XMVS
!brief COEFFICIENT FUNCTION OF THE POROSITY
! coefficient fonction de la porosite
      DOUBLE PRECISION XKV
!brief GRAVITY ACCELERATION
! acceleration de la pesanteur
      DOUBLE PRECISION GRAV
!brief
!
      DOUBLE PRECISION SFON
!brief FLOW VISCOSITY
! viscosite de l'eau
      DOUBLE PRECISION VCE
!brief
!
      DOUBLE PRECISION TETA
!brief MINIMAL VALUE OF THE WATER HEIGHT
! hauteur d'eau minimale
      DOUBLE PRECISION HMIN
!brief
!
      DOUBLE PRECISION BETA ,DELT
!brief TIDAL PERIOD
!
      DOUBLE PRECISION PMAREE
!brief STARTING TIME OF THE HYDROGRAM
! temps d'origine de l'hydrogramme
      DOUBLE PRECISION TPREC
!brief
!
      DOUBLE PRECISION PHI0
!brief
! pas de temps
      DOUBLE PRECISION DT
!brief CRITERION TO UPDATE THE FLOW (WITH CONSTANT FLOW DISCHARGE)
! critere pour mettre a jour l'hydrodynamique
      DOUBLE PRECISION :: CRIT_CFD
!brief
!
      DOUBLE PRECISION :: FRACSED_GF(NSICLM)
!brief INITIAL SUSPENSION CONCENTRATIONS
! concentrations initiales en suspension
      DOUBLE PRECISION :: CS0(NSICLM)
!brief MASS EXCHANGED BY SOURCE TERM
!
      DOUBLE PRECISION MASSOU
!brief VOLUME CONCENTRATION OF THE COHESIVE BED
! concentration volumique du lit cohesif
      DOUBLE PRECISION CSF_VASE
!brief
!
      DOUBLE PRECISION CSF_SABLE
!brief SETTLING VELOCITIES
! vitesses de chute
      DOUBLE PRECISION XWC(NSICLM)
!brief CRITICAL SHIELDS PARAMETER
!
      DOUBLE PRECISION AC(NSICLM)
!brief TETA SUSPENSION
!
      DOUBLE PRECISION TETA_SUSP
!brief
!
      DOUBLE PRECISION  XKX, XKY
!brief FRICTION ANGLE OF THE SEDIMENT
! angle de frottement du sediment
      DOUBLE PRECISION PHISED
!brief PARAMETER FOR DEVIATION
! parametre pour la deviation
      DOUBLE PRECISION BETA2
!
! NON-EQUILIBRIUM BEDLOAD AND NON-UNIFORM BED MATERIA (BMD AND MGDL)
! --------
!
!brief HIDING FACTOR FOR PARTICULAR SIZE CLASS WHEN THE USER SUBROUTINE INIT_HIDING IS NOT USED
! hiding factor par classe granulo
      DOUBLE PRECISION HIDI(NSICLM)
!brief
!
      DOUBLE PRECISION VSET(NSICLM)
!brief D90
! d90
      DOUBLE PRECISION FD90(NSICLM)
!brief SEDIMENT DIAMETERS
! diametres des grains
      DOUBLE PRECISION FDM(NSICLM)
!brief INITIAL SEDIMENT COMPOSITION FOR PARTICULAR SIZE CLASS WHEN INIT_COMPO IS NOT USED
! fraction initiale par classe sedimentologique
      DOUBLE PRECISION AVA0(NSICLM)
!brief WANTED ACTIVE LAYER THICKNESS; ELAYO=FIXED VALUE, ELAY=REAL VALUE FOR EACH POINT; WHEN ENOUGH SEDIMENT ELAY = ELAY0
! epaisseur de couche active
      DOUBLE PRECISION ELAY0
!brief TOTAL VOLUME OF SEDIMENT IN EACH CLASS
!
      DOUBLE PRECISION VOLTOT(10)
!brief CRITICAL SHEAR VELOCITY FOR MUD DEPOSITION
! vitesse critique de depot de la vase
      DOUBLE PRECISION :: VITCD
!brief CRITICAL EROSION SHEAR VELOCITY OF THE MUD
! vitesse critique d'erosion de la vase
      DOUBLE PRECISION :: VITCE
!brief SUSPENDED MASS BALANCE
!
      DOUBLE PRECISION :: MASED0(NSICLM)
!brief SUSPENDED MASS BALANCE
!
      DOUBLE PRECISION :: MASINI(NSICLM)
!brief
!
      DOUBLE PRECISION :: MASTEN(NSICLM), MASTOU(NSICLM)
!brief
!
      DOUBLE PRECISION :: MASTCP(NSICLM), MASFIN(NSICLM)
!brief
!
      DOUBLE PRECISION :: MASDEP(NSICLM), MASDEPT(NSICLM)
!brief LAYER THICKNESSES AS DOUBLE PRECISION
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE :: ES
!brief FOR NON-EQUILIBIRUM BEDLOAD
!
      DOUBLE PRECISION :: LS0
!brief RATIO BETWEEN SKIN FRICTION AND MEAN DIAMETER
! ratio entre la rugosite de peau et le diametre moyen
      DOUBLE PRECISION :: KSPRATIO
!brief KARIM, HOLLY & YANG CONSTANT
!
      DOUBLE PRECISION :: KARIM_HOLLY_YANG
!brief KARMAN CONSTANT
! constante de karman
      DOUBLE PRECISION :: KARMAN
!brief PARTHENIADES CONSTANT
! constante de partheniades
      DOUBLE PRECISION :: PARTHENIADES
!brief MAXIMUM CONCENTRATION
!
      DOUBLE PRECISION :: CMAX
!brief PI
!
      DOUBLE PRECISION :: PI
!brief ZERO OF THE CODE
!
      DOUBLE PRECISION :: ZERO
!brief B VALUE FOR THE BIJKER FORMULA
! coefficient b de la formule de bijker
      DOUBLE PRECISION :: BIJK
!brief MUD CONCENTRATION AT BOUNDARIES FOR EACH CLASS
! concentrations par classe aux frontieres
      DOUBLE PRECISION :: CBOR_CLASSE(10*MAXFRO)
!brief MUD CONCENTRATION FOR EACH LAYER
! concentrations de vase par couche
      DOUBLE PRECISION :: CONC_VASE(10)
!brief MASS TRANSFER BETWEEN LAYERS
! transfert de masse par couche
      DOUBLE PRECISION :: TRANS_MASS(10)
!brief CRITICAL EROSION SHEAR STRESS OF THE MUD PER LAYER
! contrainte critique d'erosion de la vase par couche
      DOUBLE PRECISION :: TOCE_VASE(10)
! Modele de Thiebot
      DOUBLE PRECISION :: CONC_GEL, VSTOKES
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!
!brief
!
      CHARACTER(LEN=72) TITCA,SORTIS,VARIM
!brief
!
      CHARACTER(LEN=3) BINGEOSIS,BINPRESIS,BINHYDSIS
!brief
!
      CHARACTER(LEN=3) BINRESSIS,BINREFSIS
!brief
!
      CHARACTER(LEN=32) VARCLA(10),TEXTE(MAXVAR),TEXTPR(MAXVAR)
!brief
!
      CHARACTER(LEN=20) EQUA
!brief
!
      CHARACTER(LEN=8) MNEMO(MAXVAR)
!brief
!
      CHARACTER(LEN=144) COUPLINGSIS
!
!-----------------------------------------------------------------------
!
!       8) SLVCFG STRUCTURES
!
!-----------------------------------------------------------------------
!
!brief
!
      TYPE(SLVCFG) :: SLVSED
!brief
!
      TYPE(SLVCFG) :: SLVTRA
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
!       DECLARATION OF POINTERS FOR ALIASES
!       TARGETS ARE DEFINED IN POINT_TELEMAC2D
!
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T1
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T2
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T3
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T4
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T5
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T6
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T7
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T8
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T9
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T10
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T11
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T12
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T13
!brief ALIAS FOR WORKING VECTOR IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T14
!
! USEFUL COMPONENTS IN STRUCTURE MESH
! --------
!
!brief CONNECTIVITY TABLE
! tableaux de connectivite local-global
      TYPE(BIEF_OBJ),   POINTER :: IKLE
!brief 2D COORDINATES OF THE MESH
! coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X
!brief 2D COORDINATES OF THE MESH
! coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Y
!brief NUMBER OF ELEMENTS IN THE MESH
! nombre d'elements du maillage
      INTEGER, POINTER:: NELEM
!brief MAXIMUM NUMBER OF ELEMENTS IN THE MESH
! nombre maximum d'elements du maillage
      INTEGER, POINTER:: NELMAX
!brief NUMBER OF BOUNDARY POINTS
! nombre de points frontieres
      INTEGER, POINTER:: NPTFR
!brief
!
      INTEGER, POINTER:: NPTFRX
!brief
!
      INTEGER, POINTER:: DIM
!brief
!
      INTEGER, POINTER:: TYPELM
!brief NUMBER OF 2D POINTS IN THE MESH
! nombre de points 2d du maillage
      INTEGER, POINTER:: NPOIN
!brief
!
      INTEGER, POINTER:: NPMAX
!brief
!
      INTEGER, POINTER:: MXPTVS
!brief
!
      INTEGER, POINTER:: MXELVS
!brief
!
      INTEGER, POINTER:: LV
!
!-----------------------------------------------------------------------
!
!      11) SISYPHE FILES + INTEGER DECLARATION FOR MED APPROACH
!
!-----------------------------------------------------------------------
!
!brief MAXIMUM RANK OF LOGICAL UNITS AS DECLARED IN SUBMIT STRINGS IN THE DICTIONARY
!
      INTEGER, PARAMETER :: MAXLU_SIS = 46
!brief
!
      TYPE(BIEF_FILE) :: SIS_FILES(MAXLU_SIS)
!brief
!
      INTEGER SISRES
!brief
!
      INTEGER SISREF
!brief
!
      INTEGER SISPRE
!brief
!
      INTEGER SISHYD
!brief
!
      INTEGER SISCOU
!brief
!
      INTEGER SISGEO
!brief
!
      INTEGER SISCLI
!brief
!
      INTEGER SISCAS
!brief
!
      INTEGER SISFON
!brief
!
      INTEGER SISMAF
!brief
!
      INTEGER SISSEC
!brief
!
      INTEGER SISSEO
!
!-----------------------------------------------------------------------
!
!      12) SECTIONS
!
!-----------------------------------------------------------------------
!
!brief
!
      TYPE (CHAIN_TYPE), ALLOCATABLE :: CHAIN(:)
!
      SAVE   ! VERY IMPORTANT
!
      END MODULE DECLARATIONS_SISYPHE
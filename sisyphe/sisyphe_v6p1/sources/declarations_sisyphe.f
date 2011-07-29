!                    ***************************
                     MODULE DECLARATIONS_SISYPHE
!                    ***************************
!
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    DECLARATION OF PRINICIPAL SISYPHE VARIABLES
!
!history  CV
!+        15/03/2009
!+
!+   ADDED VARIABLES MU, KS, KSP, KSR
!+
!+   VITCE AND VITCD ARE CONSTANT
!
!history  JMH
!+        13/08/2009
!+
!+   IT5 DELETED
!
!history  CV+RK+UW
!+        15/03/2011
!+
!+   ADDED VARIABLES MPM_ARRAY, MPM, MOFAC,ALPHA
!+
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
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF_DEF
C
C       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
C
C       1) VECTORS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C       2) MATRICES (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C       3) BLOCKS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C       4) INTEGERS
C       5) LOGICAL VALUES
C       6) REALS
C       7) STRINGS
C       8) SLVCFG STRUCTURES
C       9) MESH STRUCTURE
C      10) ALIASES
C
C-----------------------------------------------------------------------
C
C       1) VECTORS
C
C-----------------------------------------------------------------------
C
!> @brief EVOLUTION
C
      TYPE(BIEF_OBJ), TARGET :: E
!> @brief EVOLUTION SAVED FOR CONSTANT FLOW DISCHARGE
C
      TYPE(BIEF_OBJ), TARGET :: ECPL
!> @brief FREE SURFACE ELEVATION
C cote de la surface libre
      TYPE(BIEF_OBJ), TARGET :: Z
!> @brief INCREMENT OF FREE SURFACE ELEVATION WHEN READING AN HYDRO FILE
C
      TYPE(BIEF_OBJ), TARGET :: DEL_Z
!> @brief EVOLUTION DUE TO BEDLOAD
C
      TYPE(BIEF_OBJ), TARGET :: ZF_C
!> @brief EVOLUTION DUE TO SUSPENSION
C
      TYPE(BIEF_OBJ), TARGET :: ZF_S
!> @brief CUMULATED BED EVOLUTION
C evolution totale
      TYPE(BIEF_OBJ), TARGET :: ESOMT
!> @brief MAXIMUM EVOLUTION
C
      TYPE(BIEF_OBJ), TARGET :: EMAX
!> @brief COMPONENTS OF DEPTH-AVERAGED FLOW RATE
C composantes du debit vectoriel
      TYPE(BIEF_OBJ), TARGET :: QU
!> @brief COMPONENTS OF DEPTH-AVERAGED FLOW RATE
C composantes du debit vectoriel
      TYPE(BIEF_OBJ), TARGET :: QV
!> @brief INCREMENTS OF FLOW RATE COMPONENTS WHEN READING AN HYDRO FILE
C
      TYPE(BIEF_OBJ), TARGET :: DEL_QU
!> @brief INCREMENTS OF FLOW RATE COMPONENTS WHEN READING AN HYDRO FILE
C
      TYPE(BIEF_OBJ), TARGET :: DEL_QV
!> @brief FLOW RATE
C debit liquide
      TYPE(BIEF_OBJ), TARGET :: Q
!> @brief SOLID DISCHARGE
C
      TYPE(BIEF_OBJ), TARGET :: QS
!> @brief SOLID DISCHARGE
C
      TYPE(BIEF_OBJ), TARGET :: QSX
!> @brief SOLID DISCHARGE
C
      TYPE(BIEF_OBJ), TARGET :: QSY
!> @brief SOLID DISCHARGE (BEDLOAD)
C
      TYPE(BIEF_OBJ), TARGET :: QS_C
!> @brief SOLID DISCHARGE (BEDLOAD)
C
      TYPE(BIEF_OBJ), TARGET :: QSXC
!> @brief SOLID DISCHARGE (BEDLOAD)
C
      TYPE(BIEF_OBJ), TARGET :: QSYC
!> @brief SOLID DISCHARGE (SUSPENSION)
C
      TYPE(BIEF_OBJ), TARGET :: QS_S
!> @brief SOLID DISCHARGE (SUSPENSION)
C
      TYPE(BIEF_OBJ), TARGET :: QSXS
!> @brief SOLID DISCHARGE (SUSPENSION)
C
      TYPE(BIEF_OBJ), TARGET :: QSYS
!> @brief WATER DEPTH
C hauteur d'eau au temps n
      TYPE(BIEF_OBJ), TARGET :: HN
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: HCLIP
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: U2D,V2D
!> @brief
C intensite du courant
      TYPE(BIEF_OBJ), TARGET :: UNORM
!> @brief WATER DEPTH SAVED FOR CONSTANT FLOW DISCHARGE
C
      TYPE(BIEF_OBJ), TARGET :: HCPL
!> @brief IMPOSED BED EVOLUTION AT THE BOUNDARY
C evolution aux points de bord
      TYPE(BIEF_OBJ), TARGET :: EBOR
!> @brief IMPOSED SOLID TRANSPORT AT THE BOUNDARY
C
      TYPE(BIEF_OBJ), TARGET :: QBOR
!> @brief ZF VALUES ON BOUNDARIES
C
      TYPE(BIEF_OBJ), TARGET :: FLBOR
!> @brief BOTTOM ELEVATION
C cote du fond
      TYPE(BIEF_OBJ), TARGET :: ZF
!> @brief NON ERODABLE (RIGID) BOTTOM ELEVATION
C
      TYPE(BIEF_OBJ), TARGET :: ZR
!> @brief REFERENCE ELEVATION
C
      TYPE(BIEF_OBJ), TARGET :: ZREF
!> @brief INTEGRAL OF BASES
C
      TYPE(BIEF_OBJ), TARGET :: VOLU2D
!> @brief INTEGRAL OF BASES IN PARALLEL
C
      TYPE(BIEF_OBJ), TARGET :: V2DPAR
!> @brief INVERSE OF INTEGRAL OF BASES
C
      TYPE(BIEF_OBJ), TARGET :: UNSV2D
!> @brief BOTTOM FRICTION COEFFICIENT (CHEZY, NIKURADSE OR STICKLER)
C coefficients de frottement sur le  fond (mot cle)
      TYPE(BIEF_OBJ), TARGET :: CHESTR
!> @brief ANGLE BETWEEN QS AND Q
C
      TYPE(BIEF_OBJ), TARGET :: CALFA
!> @brief ANGLE BETWEEN QS AND Q
C
      TYPE(BIEF_OBJ), TARGET :: SALFA
!> @brief VOID STRUCTURE
C
      TYPE(BIEF_OBJ), TARGET :: S
!> @brief MASK
C
      TYPE(BIEF_OBJ), TARGET :: MASKPT
!> @brief MASK
C
      TYPE(BIEF_OBJ), TARGET :: MASKTR
!> @brief MASK
C
      TYPE(BIEF_OBJ), TARGET :: MASKB
!> @brief MASK
C
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!> @brief MASK
C
      TYPE(BIEF_OBJ), TARGET :: MSKTMP
!> @brief WORKING ARRAYS
C
      TYPE(BIEF_OBJ), TARGET :: W1
C
C WAVE DATA
C --------

!> @brief WAVE DIRECTION (DEG WRT OX AXIS)    !!!!!SOME SAY OY AXIS!!!!!
C angle d'attaque de la houle (par rapport a l'axe oy)
      TYPE(BIEF_OBJ), TARGET :: THETAW
!> @brief FRICTION COEFFICIENT (WAVES)
C coefficient de frottement quadratique de la houle
      TYPE(BIEF_OBJ), TARGET :: FW
!> @brief ORBITAL VELOCITY
C courant orbital
      TYPE(BIEF_OBJ), TARGET :: UW
!> @brief SIGNIFICANT WAVE HEIGHT
C hauteur de houle
      TYPE(BIEF_OBJ), TARGET :: HW
!> @brief MEAN WAVE PERIOD
C periode de houle
      TYPE(BIEF_OBJ), TARGET :: TW
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: INDIC,IFAMAS
!> @brief INTEGER WORKING ARRAY
C
      TYPE(BIEF_OBJ), TARGET :: IT1
!> @brief INTEGER WORKING ARRAY
C
      TYPE(BIEF_OBJ), TARGET :: IT2
!> @brief INTEGER WORKING ARRAY
C
      TYPE(BIEF_OBJ), TARGET :: IT3
!> @brief INTEGER WORKING ARRAY
C
      TYPE(BIEF_OBJ), TARGET :: IT4
!> @brief TYPE OF BOUNDARY CONDITIONS ON BED EVOLUTION
C types de conditions aux limites sur l'evolution
      TYPE(BIEF_OBJ), TARGET :: LIEBOR
!> @brief TYPE OF BOUNDARY CONDITIONS ON SAND TRANSPORT RATE
C
      TYPE(BIEF_OBJ), TARGET :: LIQBOR
!> @brief TYPE OF BOUNDARY CONDITIONS
C
      TYPE(BIEF_OBJ), TARGET :: LIMTEC
!> @brief IMPACT OF THE SLOPE EFFECT ON AMPLITUDE
C
      TYPE(BIEF_OBJ), TARGET :: COEFPN
!> @brief LIQUID BOUNDARY NUMBERING
C
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!> @brief SHEAR STRESS
C contrainte de frottement en courant seul
      TYPE(BIEF_OBJ), TARGET :: TOB
!> @brief FRICTION COEFFICIENT
C coefficient de frottement quadratique du courant
      TYPE(BIEF_OBJ), TARGET :: CF
!> @brief WAVE INDUCED SHEAR STRESS
C contrainte de frottement en houle seule
      TYPE(BIEF_OBJ), TARGET :: TOBW
!> @brief
C rapport entre la contrainte de frottement de peau et la contrainte totale
      TYPE(BIEF_OBJ), TARGET :: MU
!> @brief
C rugosite totale
      TYPE(BIEF_OBJ), TARGET :: KS
!> @brief
C rugosite de peau
      TYPE(BIEF_OBJ), TARGET :: KSP
!> @brief
C rugosite de ride
      TYPE(BIEF_OBJ), TARGET :: KSR
!> @brief BED LEVEL CHANGE FOR GRAIN-FEEDING
C
      TYPE(BIEF_OBJ), TARGET :: DZF_GF
C
C NON-EQUILIBRIUM BEDLOAD AND NON-UNIFORM BED MATERIA (BMD AND MGDL)
C --------

!> @brief MEAN DIAMETER OF ACTIVE-LAYER
C diametre moyen du sediment
      TYPE(BIEF_OBJ), TARGET :: ACLADM
!> @brief MEAN DIAMETER OF UNDER-LAYER
C
      TYPE(BIEF_OBJ), TARGET :: UNLADM
!> @brief NUMBER OF LAYERS FOR EACH POINT
C
      TYPE(BIEF_OBJ), TARGET :: NLAYER
!> @brief HIDING FACTOR FOR PARTICULAR SIZE CLASS
C
      TYPE(BIEF_OBJ), TARGET :: HIDING
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: ELAY
!> @brief ACTIVE STRATUM THICKNESS
C
      TYPE(BIEF_OBJ), TARGET :: ESTRAT
C
C SUSPENSION  (F. MENARD)
C --------

!> @brief DEPOSITION FLUX
C
      TYPE(BIEF_OBJ), TARGET :: FLUDP
!> @brief DEPOSITION FLUX
C
      TYPE(BIEF_OBJ), TARGET :: FLUDPT
!> @brief EROSION FLUX
C
      TYPE(BIEF_OBJ), TARGET :: FLUER
!> @brief EROSION FLUX
C
      TYPE(BIEF_OBJ), TARGET :: FLUERT
!> @brief CONCENTRATION AT TIME N
C
      TYPE(BIEF_OBJ), TARGET :: CS
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: CST, CTILD, CSTAEQ
!> @brief IMPOSED SUSPENDED SAND CONCENTRATION AT THE BOUNDARY (DIM.NPTFR)
C
      TYPE(BIEF_OBJ), TARGET :: CBOR
!> @brief CONCENTRATION IN G/L
C
      TYPE(BIEF_OBJ), TARGET :: CSGL
!> @brief COORDINATES OF VELOCITY VECTORS
C
      TYPE(BIEF_OBJ), TARGET ::  UCONV
!> @brief COORDINATES OF VELOCITY VECTORS
C
      TYPE(BIEF_OBJ), TARGET ::  VCONV
!> @brief PROPAGATION HEIGHT
C
      TYPE(BIEF_OBJ), TARGET :: HPROP
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: DISP,DISP_C
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: AFBOR  , BFBOR
!> @brief FLUX AT THE BOUNDARIES
C
      TYPE(BIEF_OBJ), TARGET :: FLBOR_SIS
!> @brief FLUX AT THE BOUNDARIES
C
      TYPE(BIEF_OBJ), TARGET :: FLBORTRA
C
C     BOUNDARY CONDITIONS FOR SEDIMENT                     : LICBOR
C     TYPES OF BOUNDARY CONDITIONS FOR H                   : LIHBOR
C     TYPES OF BOUNDARY CONDITIONS FOR PROPAGATION         : LIMPRO
C                    POINTS   :    .1:H  .2:U  .3:V
C                    SEGMENTS :    .4:H  .5:U  .6:V
C
!> @brief TYPE OF BOUNDARY CONDITIONS ON SUSPENDED SAND CONCENTRATION
C
      TYPE(BIEF_OBJ), TARGET :: LICBOR
!> @brief TYPE OF BOUNDARY CONDITIONS FOR H
C
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!> @brief TYPE OF BOUNDARY CONDITIONS FOR PROPAGATION
C
      TYPE(BIEF_OBJ), TARGET :: LIMPRO
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: LIMDIF
!> @brief LAST LINE OF THE BOUNDARY CONDITION FILE
C
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!> @brief BOUNDARY CONDITIONS FOR TRACER (MODIFIED LITBOR)
C
      TYPE(BIEF_OBJ), TARGET :: CLT
!> @brief BOUNDARY CONDITIONS FOR U
C
      TYPE(BIEF_OBJ), TARGET :: CLU
!> @brief BOUNDARY CONDITIONS FOR V
C
      TYPE(BIEF_OBJ), TARGET :: CLV
!> @brief WORKING ARRAY FOR ELEMENTS
C
      TYPE(BIEF_OBJ), TARGET :: TE1
!> @brief WORKING ARRAY FOR ELEMENTS
C
      TYPE(BIEF_OBJ), TARGET :: TE2
!> @brief WORKING ARRAY FOR ELEMENTS
C
      TYPE(BIEF_OBJ), TARGET :: TE3
!> @brief COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
C
      TYPE(BIEF_OBJ), TARGET :: KX
!> @brief COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
C
      TYPE(BIEF_OBJ), TARGET :: KY
!> @brief COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
C
      TYPE(BIEF_OBJ), TARGET :: KZ
!> @brief ARRAY THAT INDICATES WHETHER THE NON-ERODABLE BOTTOM HAS BEEN REACHED (VF)
C
      TYPE(BIEF_OBJ), TARGET :: BREACH
!> @brief FOR MIXED SEDIMENTS
C
      TYPE(BIEF_OBJ), TARGET :: FLUER_VASE
!> @brief FOR MIXED SEDIMENTS
C
      TYPE(BIEF_OBJ), TARGET :: TOCE_MIXTE
!> @brief FOR MIXED SEDIMENTS
C
      TYPE(BIEF_OBJ), TARGET :: MS_SABLE
!> @brief FOR MIXED SEDIMENTS
C
      TYPE(BIEF_OBJ), TARGET :: MS_VASE
C
C-----------------------------------------------------------------------
C
C       2) MATRICES
C
C-----------------------------------------------------------------------
C
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: MBOR
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: AM1_S,AM2_S
C
C-----------------------------------------------------------------------
C
C       3) BLOCKS
C
C-----------------------------------------------------------------------
C
!> @brief BLOCK OF MASKS
C
      TYPE(BIEF_OBJ), TARGET :: MASK
!> @brief BLOCK OF WORKING ARRAYS
C
      TYPE(BIEF_OBJ), TARGET :: TB
!> @brief BLOCK OF PRIVATE VECTORS
C
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!> @brief BLOCK OF CLANDESTINE VARIABLES
C
      TYPE(BIEF_OBJ), TARGET :: VARCL
!> @brief BLOCK OF VARIABLES FOR INPUT
C
      TYPE(BIEF_OBJ), TARGET :: VARHYD
!> @brief BLOCK OF VARIABLES FOR OUTPUT
C
      TYPE(BIEF_OBJ), TARGET :: VARSOR
!> @brief SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
C
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE::AVAIL
!> @brief SEDIMENT COMPOSITION
C
      TYPE(BIEF_OBJ), TARGET :: AVAI
!> @brief LAYER THICKNESSES
C
      TYPE(BIEF_OBJ), TARGET :: LAYTHI
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: QSCL
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: QSCLX , QSCLY
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: QSCL_C
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: QSCLXC, QSCLYC
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: QSCL_S
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: QSCLXS, QSCLYS
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: ZFCL
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: ZFCL_C
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: ZFCL_S
!> @brief MEYER PETER MUELLER factor
C	  
      TYPE(BIEF_OBJ), TARGET :: MPM_ARAY
C
C     FLUX LIMITATION PER SEGMENT
C
      TYPE(BIEF_OBJ), TARGET :: FLULIM	  
C
C-----------------------------------------------------------------------
C
C       4) INTEGERS
C
C-----------------------------------------------------------------------
C
C       KEYWORDS AND PARAMETERS
C
!> @brief MAXIMUM NUMBER OF OUTPUT VARIABLES
C
      INTEGER, PARAMETER :: MAXVAR = 500
!> @brief MAXIMUM NUMBER OF (LIQUID BOUNDARIES, SOLID BOUNDARIES)
C
      INTEGER, PARAMETER :: MAXFRO = 300
!> @brief
C
      INTEGER NFRLIQ,NFRSOL
!> @brief
C
      INTEGER DEBLIQ(MAXFRO),FINLIQ(MAXFRO)
!> @brief
C
      INTEGER DEBSOL(MAXFRO),FINSOL(MAXFRO)
!> @brief OPTION FOR THE DIFFUSION OF TRACER
C option pour la diffusion du traceur
      INTEGER OPDTRA
!> @brief OPTION FOR THE DISPERSION
C option pour la dispersion
      INTEGER OPTDIF
!> @brief 'SUPG OPTION'
C
      INTEGER OPTSUP
!> @brief NUMBER OF ITERATIONS WITH CONSTANT FLOW DISCHARGE
C nombre d'iterations pour telemac
      INTEGER :: NCONDIS
!> @brief LAW OF BOTTOM FRICTION
C loi de frottement sur le fond
      INTEGER KFROT
!> @brief BED-LOAD TRANSPORT FORMULA
C formule de transport solide
      INTEGER ICF
!> @brief
C
      INTEGER NPAS
!> @brief NUMBER OF TIDES OR FLOODS
C nombre de marees ou crues
      INTEGER NMAREE
!> @brief
C
      INTEGER LEOPR
!> @brief
C
      INTEGER LISPR
!> @brief
C
      INTEGER NVARCL
!> @brief
C
      INTEGER IELMT,IELMH_SIS,IELMU_SIS,IELMX
!> @brief
C standard du fichier de geometrie
      INTEGER STDGEO
!> @brief
C
      INTEGER LOGDES ,LOGPRE ,OPTBAN ,LVMAC
!> @brief HYDRODYNAMIC CODE
C code de calcul utilise pour l'hydrodynamique
      INTEGER HYDRO
!> @brief MATRIX STORAGE
C stockage des matrices
      INTEGER OPTASS
!> @brief NUMBER OF SUB-ITERATIONS
C nombre de sous-iterations
      INTEGER NSOUS
!> @brief
C
      INTEGER MARDAT(3),MARTIM(3),PRODUC
!> @brief OPTION FOR THE TREATMENT OF NON ERODABLE BEDS
C option de traitement des fonds non erodables
      INTEGER CHOIX
!> @brief
C
      INTEGER PTINIL, PTINIG
!> @brief NUMBER OF PRIVATE ARRAYS
C nombre de tableaux prives
      INTEGER NPRIV
!> @brief COUPLING PERIOD
C periode de couplage
      INTEGER PERCOU
!> @brief NUMERO DU PAS DE TEMPS
C
      INTEGER LT
!> @brief
C
      INTEGER RESOL
!> @brief
C
      INTEGER DEPER
!> @brief FORMULA FOR DEVIATION
C formule pour la deviation
      INTEGER DEVIA
!> @brief FORMULA FOR SLOPE EFFECT
C formule pour effet de pente
      INTEGER SLOPEFF
C
C NON-EQUILIBRIUM BEDLOAD AND NON-UNIFORM BED MATERIA (BMD AND MGDL)
C --------
C
!> @brief MAXIMUM NUMBER OF SIZE-CLASSES
C
      INTEGER, PARAMETER :: NSICLM = 10
!> @brief NUMBER OF SIZE-CLASSES OF BED MATERIAL (LESS THAN 10)
C nombre de classes granulometriques
      INTEGER :: NSICLA
!> @brief MAXIMUM NUMBER OF LAYERS ON THE MESH
C
      INTEGER,PARAMETER :: NLAYMAX = 10
!> @brief NUMBER OF BED LOAD MODEL LAYERS
C nombre de couches pour granulo etendue
      INTEGER NOMBLAY
!> @brief FORMULATION FOR THE HIDING FACTOR
C
      INTEGER HIDFAC
!> @brief
C
      INTEGER, PARAMETER :: TMCOD_SISTEL = 0
!> @brief FOR NON-EQUILIBRIUM BEDLOAD
C
      INTEGER :: LOADMETH
!> @brief DEBUGGER
C debugger
      INTEGER :: DEBUG
!> @brief REFERENCE CONCENTRATION FORMULA
C formule pour la concentration de reference
      INTEGER :: ICQ
!> @brief NUMBER OF CONTROL SECTIONS POINTS
C
      INTEGER NCP
!
!     ARRAY CONTAINING THE GLOBAL NUMBER OF THE POINTS IN THE CONTROL SECTIONS
!
      INTEGER, ALLOCATABLE :: CTRLSC(:)
!
!     COORDINATES OF THE ORIGIN
!
      INTEGER I_ORIG,J_ORIG
!
!     NUMBER OF LAYERS FOR CONSOLIDATION
!
      INTEGER NCOUCH_TASS
!
!     SKIN FRICTION CORRECTION
!
      INTEGER ICR
!
!     BED ROUGHNESS PREDICTOR OPTION
!
      INTEGER IKS
!
!     CONSOLIDATION MODEL
!
      INTEGER ITASS
!
!     TREATMENT OF FLUXES AT THE BOUNDARIES
!
      INTEGER DIRFLU
C
C-----------------------------------------------------------------------
C
C       5) LOGICAL VALUES
C
C-----------------------------------------------------------------------
C
C
!> @brief GRAPHICAL OUTPUT
C
      LOGICAL :: SORLEO(MAXVAR)
!> @brief LISTING OUTPUT
C
      LOGICAL :: SORIMP(MAXVAR)
!> @brief MASKING
C
      LOGICAL :: MSK
!> @brief WRITES OUT (OR NOT)
C
      LOGICAL :: ENTET
!> @brief RESOLUTION FOR SUSPENSION IS IMPLICIT (OR NOT)
C
      LOGICAL :: YASMI
!> @brief SPHERICAL EQUATIONS (HARD-CODED)
C
      LOGICAL :: SPHERI
!> @brief STEADY HYDRODYNAMICS
C
      LOGICAL :: PERMA
!> @brief TIDAL FLATS
C
      LOGICAL :: BANDEC
!> @brief WAVE EFFECT
C si oui, prise en compte de la houle
      LOGICAL :: HOULE
!> @brief FALL VELOCITY (PARTIALLY HARD-CODED)
C
      LOGICAL :: CALWC
!> @brief SHIELDS PARAMETER
C
      LOGICAL :: CALAC
!> @brief BEDLOAD
C
      LOGICAL :: CHARR
!> @brief LOADING LAW USED OR NOT
C
      LOGICAL :: NOEQUBED
!> @brief FINITE VOLUMES
C si oui, volumes finis
      LOGICAL :: VF
!> @brief MASS-LUMPING
C
      LOGICAL :: LUMPI
!> @brief CONSTANT FLOW DISCHARGE
C
      LOGICAL :: LCONDIS
!> @brief GRAIN-FEEDING
C si oui, grain-feeding
      LOGICAL :: LGRAFED
!> @brief CONSTANT ACTIVE LAYER THICKNESS
C si oui, epaisseur de couche active constante
      LOGICAL :: CONST_ALAYER
!> @brief SUSPENSION
C si oui, prise en compte de la suspension
      LOGICAL :: SUSP
!> @brief MASS BALANCE
C
      LOGICAL :: BILMA
!> @brief VALIDATION
C si oui, validation
      LOGICAL :: VALID
!> @brief IMPOSED CONCENTRATION IN INFLOW
C si oui, concentration d'equilibre en entree
      LOGICAL :: IMP_INFLOW_C
!> @brief SECONDARY CURRENTS
C si oui, courants secondaires
      LOGICAL :: SECCURRENT
!> @brief MASS CONCENTRATIONS IN G/L
C si oui, concentration massique
      LOGICAL :: UNIT
!> @brief CORRECTION ON CONVECTION VELOCITY
C si oui, correction du champ convecteur
      LOGICAL :: CORR_CONV
!> @brief COMPUTATION CONTINUED
C si oui, suite de calcul
      LOGICAL :: DEBU
!> @brief DIFFUSION OF SUSPENDED SEDIMENT CONCENTRATION
C si oui, diffusion
      LOGICAL :: DIFT
!> @brief SEDIMENT SLIDE
C si oui, glissement du sediment
      LOGICAL :: SLIDE
!> @brief COHESIVE SEDIMENTS (FOR EACH CLASS)
C si oui, sediments cohesifs
      LOGICAL :: SEDCO(NSICLM)
!> @brief CONSOLIDATION TAKEN INTO ACCOUNT
C si oui, tassement du lit cohesif
      LOGICAL :: TASS
!> @brief MIXED SEDIMENTS
C si oui, sediment mixte
      LOGICAL :: MIXTE
!> @brief COUPLING WITH DREDGESIM
C si oui, couplage avec dredgesim
      LOGICAL :: DREDGESIM
!> @brief BED FRICTION PREDICTION
C 
      LOGICAL :: KSPRED
C
C-----------------------------------------------------------------------
C
C       6) REALS
C
C-----------------------------------------------------------------------
C
!> @brief
C
      DOUBLE PRECISION RC
!> @brief WATER DENSITY
C masse volumique de l'eau
      DOUBLE PRECISION XMVE
!> @brief SAND DENSITY
C masse volumique du sediment
      DOUBLE PRECISION XMVS
!> @brief COEFFICIENT FUNCTION OF THE POROSITY
C coefficient fonction de la porosite
      DOUBLE PRECISION XKV
!> @brief GRAVITY ACCELERATION
C acceleration de la pesanteur
      DOUBLE PRECISION GRAV
!> @brief
C
      DOUBLE PRECISION SFON
!> @brief FLOW VISCOSITY
C viscosite de l'eau
      DOUBLE PRECISION VCE
!> @brief
C
      DOUBLE PRECISION TETA
!> @brief MINIMAL VALUE OF THE WATER HEIGHT
C hauteur d'eau minimale
      DOUBLE PRECISION HMIN
!> @brief
C
      DOUBLE PRECISION BETA ,DELT
!> @brief TIDAL PERIOD
C
      DOUBLE PRECISION PMAREE
!> @brief STARTING TIME OF THE HYDROGRAM
C temps d'origine de l'hydrogramme
      DOUBLE PRECISION TPREC
!> @brief
C
      DOUBLE PRECISION PHI0
!> @brief
C pas de temps
      DOUBLE PRECISION DT
!> @brief CRITERION TO UPDATE THE FLOW (WITH CONSTANT FLOW DISCHARGE)
C critere pour mettre a jour l'hydrodynamique
      DOUBLE PRECISION :: CRIT_CFD
!> @brief
C
      DOUBLE PRECISION :: FRACSED_GF(NSICLM)
!> @brief INITIAL SUSPENSION CONCENTRATIONS
C concentrations initiales en suspension
      DOUBLE PRECISION :: CS0(NSICLM)
!> @brief MASS EXCHANGED BY SOURCE TERM
C
      DOUBLE PRECISION MASSOU
!> @brief VOLUME CONCENTRATION OF THE COHESIVE BED
C concentration volumique du lit cohesif
      DOUBLE PRECISION CSF_VASE
!> @brief
C
      DOUBLE PRECISION CSF_SABLE
!> @brief SETTLING VELOCITIES
C vitesses de chute
      DOUBLE PRECISION XWC(NSICLM)
!> @brief CRITICAL SHIELDS PARAMETER
C
      DOUBLE PRECISION AC(NSICLM)
!> @brief TETA SUSPENSION
C
      DOUBLE PRECISION TETA_SUSP
!> @brief
C
      DOUBLE PRECISION  XKX, XKY
!> @brief FRICTION ANGLE OF THE SEDIMENT
C angle de frottement du sediment
      DOUBLE PRECISION PHISED
!> @brief PARAMETER FOR DEVIATION
C parametre pour la deviation
      DOUBLE PRECISION BETA2
C
C NON-EQUILIBRIUM BEDLOAD AND NON-UNIFORM BED MATERIA (BMD AND MGDL)
C --------
C
!> @brief HIDING FACTOR FOR PARTICULAR SIZE CLASS WHEN THE USER SUBROUTINE INIT_HIDING IS NOT USED
C hiding factor par classe granulo
      DOUBLE PRECISION HIDI(NSICLM)
!> @brief
C
      DOUBLE PRECISION VSET(NSICLM)
!> @brief D90
C d90
      DOUBLE PRECISION FD90(NSICLM)
!> @brief SEDIMENT DIAMETERS
C diametres des grains
      DOUBLE PRECISION FDM(NSICLM)
!> @brief INITIAL SEDIMENT COMPOSITION FOR PARTICULAR SIZE CLASS WHEN INIT_COMPO IS NOT USED
C fraction initiale par classe sedimentologique
      DOUBLE PRECISION AVA0(NSICLM)
!> @brief WANTED ACTIVE LAYER THICKNESS; ELAYO=FIXED VALUE, ELAY=REAL VALUE FOR EACH POINT; WHEN ENOUGH SEDIMENT ELAY = ELAY0
C epaisseur de couche active
      DOUBLE PRECISION ELAY0
!> @brief TOTAL VOLUME OF SEDIMENT IN EACH CLASS
C
      DOUBLE PRECISION VOLTOT(10)
!> @brief CRITICAL SHEAR VELOCITY FOR MUD DEPOSITION
C vitesse critique de depot de la vase
      DOUBLE PRECISION :: VITCD
!> @brief CRITICAL EROSION SHEAR VELOCITY OF THE MUD
C vitesse critique d'erosion de la vase
      DOUBLE PRECISION :: VITCE
!> @brief SUSPENDED MASS BALANCE
C
      DOUBLE PRECISION :: MASED0(NSICLM)
!> @brief SUSPENDED MASS BALANCE
C
      DOUBLE PRECISION :: MASINI(NSICLM)
!> @brief
C
      DOUBLE PRECISION :: MASTEN(NSICLM), MASTOU(NSICLM)
!> @brief
C
      DOUBLE PRECISION :: MASTCP(NSICLM), MASFIN(NSICLM)
!> @brief
C
      DOUBLE PRECISION :: MASDEP(NSICLM), MASDEPT(NSICLM)
!> @brief LAYER THICKNESSES AS DOUBLE PRECISION
C
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE :: ES
!> @brief FOR NON-EQUILIBIRUM BEDLOAD
C
      DOUBLE PRECISION :: LS0
!> @brief RATIO BETWEEN SKIN FRICTION AND MEAN DIAMETER
C ratio entre la rugosite de peau et le diametre moyen
      DOUBLE PRECISION :: KSPRATIO
!> @brief KARIM, HOLLY & YANG CONSTANT
C
      DOUBLE PRECISION :: KARIM_HOLLY_YANG
!> @brief KARMAN CONSTANT
C constante de karman
      DOUBLE PRECISION :: KARMAN
!> @brief PARTHENIADES CONSTANT
C constante de partheniades
      DOUBLE PRECISION :: PARTHENIADES
!> @brief MAXIMUM CONCENTRATION
C
      DOUBLE PRECISION :: CMAX
!> @brief PI
C
      DOUBLE PRECISION :: PI
!> @brief Meyer Peter Mueller-Coefficient 
      DOUBLE PRECISION :: MPM
!> @brief Secondary Current Alpha Coefficient
      DOUBLE PRECISION :: ALPHA
!> @brief Morphological Factor
      DOUBLE PRECISION :: MOFAC
!> @brief ZERO OF THE CODE
C
      DOUBLE PRECISION :: ZERO
!> @brief B VALUE FOR THE BIJKER FORMULA
C coefficient b de la formule de bijker
      DOUBLE PRECISION :: BIJK
!> @brief MUD CONCENTRATION AT BOUNDARIES FOR EACH CLASS
C concentrations par classe aux frontieres
      DOUBLE PRECISION :: CBOR_CLASSE(10*MAXFRO)
!> @brief MUD CONCENTRATION FOR EACH LAYER
C concentrations de vase par couche
      DOUBLE PRECISION :: CONC_VASE(10)
!> @brief MASS TRANSFER BETWEEN LAYERS
C transfert de masse par couche
      DOUBLE PRECISION :: TRANS_MASS(10)
!> @brief CRITICAL EROSION SHEAR STRESS OF THE MUD PER LAYER
C contrainte critique d'erosion de la vase par couche
      DOUBLE PRECISION :: TOCE_VASE(10),TOCE_SABLE
C Modele de Thiebot 
      DOUBLE PRECISION :: CONC_GEL, COEF_N
C-----------------------------------------------------------------------
C
C       7) STRINGS
C
C-----------------------------------------------------------------------
C
!> @brief
C
      CHARACTER(LEN=72) TITCA,SORTIS,VARIM
!> @brief
C
      CHARACTER(LEN=3) BINGEOSIS,BINPRESIS,BINHYDSIS
!> @brief
C
      CHARACTER(LEN=3) BINRESSIS,BINREFSIS
!> @brief
C
      CHARACTER(LEN=32) VARCLA(10),TEXTE(MAXVAR),TEXTPR(MAXVAR)
!> @brief
C
      CHARACTER(LEN=20) EQUA
!> @brief
C
      CHARACTER(LEN=8) MNEMO(MAXVAR)
!> @brief
C
      CHARACTER(LEN=144) COUPLINGSIS
C
C-----------------------------------------------------------------------
C
C       8) SLVCFG STRUCTURES
C
C-----------------------------------------------------------------------
C
!> @brief
C
      TYPE(SLVCFG) :: SLVSED
!> @brief
C
      TYPE(SLVCFG) :: SLVTRA
C
C-----------------------------------------------------------------------
C
C       9) MESH STRUCTURE
C
C-----------------------------------------------------------------------
C
!> @brief MESH STRUCTURE
C
      TYPE(BIEF_MESH) :: MESH
C
C-----------------------------------------------------------------------
C
C      10) ALIASES
C
C-----------------------------------------------------------------------
C
C       DECLARATION OF POINTERS FOR ALIASES
C       TARGETS ARE DEFINED IN POINT_TELEMAC2D
C
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T1
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T2
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T3
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T4
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T5
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T6
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T7
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T8
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T9
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T10
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T11
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T12
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T13
!> @brief ALIAS FOR WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T14
C
C USEFUL COMPONENTS IN STRUCTURE MESH
C --------
C
!> @brief CONNECTIVITY TABLE
C tableaux de connectivite local-global
      TYPE(BIEF_OBJ),   POINTER :: IKLE
!> @brief 2D COORDINATES OF THE MESH
C coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X
!> @brief 2D COORDINATES OF THE MESH
C coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Y
!> @brief NUMBER OF ELEMENTS IN THE MESH
C nombre d'elements du maillage
      INTEGER, POINTER:: NELEM
!> @brief MAXIMUM NUMBER OF ELEMENTS IN THE MESH
C nombre maximum d'elements du maillage
      INTEGER, POINTER:: NELMAX
!> @brief NUMBER OF BOUNDARY POINTS
C nombre de points frontieres
      INTEGER, POINTER:: NPTFR
!> @brief
C
      INTEGER, POINTER:: NPTFRX
!> @brief
C
      INTEGER, POINTER:: DIM
!> @brief
C
      INTEGER, POINTER:: TYPELM
!> @brief NUMBER OF 2D POINTS IN THE MESH
C nombre de points 2d du maillage
      INTEGER, POINTER:: NPOIN
!> @brief
C
      INTEGER, POINTER:: NPMAX
!> @brief
C
      INTEGER, POINTER:: MXPTVS
!> @brief
C
      INTEGER, POINTER:: MXELVS
!> @brief
C
      INTEGER, POINTER:: LV
C
C-----------------------------------------------------------------------
C
C      11) SISYPHE FILES + INTEGER DECLARATION FOR MED APPROACH
C
C-----------------------------------------------------------------------
C
!> @brief MAXIMUM RANK OF LOGICAL UNITS AS DECLARED IN SUBMIT STRINGS IN THE DICTIONARY
C
      INTEGER, PARAMETER :: MAXLU_SIS = 46
!> @brief
C
      TYPE(BIEF_FILE) :: SIS_FILES(MAXLU_SIS)
!> @brief
C
      INTEGER SISRES
!> @brief
C
      INTEGER SISREF
!> @brief
C
      INTEGER SISPRE
!> @brief
C
      INTEGER SISHYD
!> @brief
C
      INTEGER SISCOU
!> @brief
C
      INTEGER SISGEO
!> @brief
C
      INTEGER SISCLI
!> @brief
C
      INTEGER SISCAS
!> @brief
C
      INTEGER SISFON
!> @brief
C
      INTEGER SISMAF
!> @brief
C
      INTEGER SISSEC
!> @brief
C
      INTEGER SISSEO
C
C-----------------------------------------------------------------------
C
C      12) SECTIONS
C
C-----------------------------------------------------------------------
!> @brief
C
      TYPE (CHAIN_TYPE), ALLOCATABLE :: CHAIN(:)
C
      SAVE   ! VERY IMPORTANT
C
      END MODULE DECLARATIONS_SISYPHE
C
C#######################################################################
C

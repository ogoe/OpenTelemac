!                    ***************************
                     MODULE DECLARATIONS_SISYPHE
!                    ***************************
!
!
!***********************************************************************
! SISYPHE   V6P2                                   30/07/2012
!***********************************************************************
!
!brief    DECLARATION OF PRINCIPAL SISYPHE VARIABLES
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
!history  MAK (HRW)
!+        01/01/2012
!+        V6P2
!+   CHANGE create parameter for ratio depth averaged and reference concentration.
!+   CSRATIO
!
!history  JWI (HRW)
!+        31/05/2012
!+        V6P2
!+ added lines to include wave orbital velocities
!+ TYPE(BIEF_OBJ), TARGET :: DEL_UW
!
!history  CV (EDF)
!+        30/07/2012
!+        V6P2
!+ added new variable ZFCL_MS 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
!
!       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
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
!     EVOLUTION
!
      TYPE(BIEF_OBJ), TARGET :: E
!
!     EVOLUTION SAVED FOR CONSTANT FLOW DISCHARGE
!
      TYPE(BIEF_OBJ), TARGET :: ECPL
!
!     FREE SURFACE ELEVATION
!
      TYPE(BIEF_OBJ), TARGET :: Z
!
!     INCREMENT OF FREE SURFACE ELEVATION WHEN READING AN HYDRO FILE
!
      TYPE(BIEF_OBJ), TARGET :: DEL_Z
!
!     EVOLUTION DUE TO BEDLOAD
!
      TYPE(BIEF_OBJ), TARGET :: ZF_C
!
!     EVOLUTION DUE TO SUSPENSION
!
      TYPE(BIEF_OBJ), TARGET :: ZF_S
!
!     CUMULATED BED EVOLUTION
!
      TYPE(BIEF_OBJ), TARGET :: ESOMT
!
!     MAXIMUM EVOLUTION
!
      TYPE(BIEF_OBJ), TARGET :: EMAX
!
!     COMPONENTS OF DEPTH-AVERAGED FLOW RATE
!
      TYPE(BIEF_OBJ), TARGET :: QU
!
!     COMPONENTS OF DEPTH-AVERAGED FLOW RATE
!
      TYPE(BIEF_OBJ), TARGET :: QV
!
!     INCREMENTS OF FLOW RATE COMPONENTS WHEN READING AN HYDRO FILE
!
      TYPE(BIEF_OBJ), TARGET :: DEL_QU
!
!     INCREMENTS OF FLOW RATE COMPONENTS WHEN READING AN HYDRO FILE
!
      TYPE(BIEF_OBJ), TARGET :: DEL_QV
! JWI 31/05/2012 - added lines to include wave orbital velocities
!> @brief INCREMENTS OF WAVE ORBITAL VELOCITY WHEN READING AN HYDRO FILE
C
      TYPE(BIEF_OBJ), TARGET :: DEL_UW
! JWI END
!
!     FLOW RATE
!
      TYPE(BIEF_OBJ), TARGET :: Q
!
!     SOLID DISCHARGE
!
      TYPE(BIEF_OBJ), TARGET :: QS
!
!     SOLID DISCHARGE ,ALONG X AND Y
!
      TYPE(BIEF_OBJ), TARGET :: QSX,QSY
!
!     SOLID DISCHARGE (BEDLOAD)
!
      TYPE(BIEF_OBJ), TARGET :: QS_C
!
!     SOLID DISCHARGE (BEDLOAD), ALONG X AND Y
!
      TYPE(BIEF_OBJ), TARGET :: QSXC,QSYC
!
!     SOLID DISCHARGE (SUSPENSION)
!
      TYPE(BIEF_OBJ), TARGET :: QS_S
!
!     SOLID DISCHARGE (SUSPENSION), ALONG X AND Y
!
      TYPE(BIEF_OBJ), TARGET :: QSXS,QSYS
!
!     WATER DEPTH
!
      TYPE(BIEF_OBJ), TARGET :: HN
!
!     DEPTH AFTER CLIPPING
!
      TYPE(BIEF_OBJ), TARGET :: HCLIP
!
!     COMPONENTS OF DEPTH-AVERAGED VELOCITY
!
      TYPE(BIEF_OBJ), TARGET :: U2D,V2D
!
!     FLOW INTENSITY
!
      TYPE(BIEF_OBJ), TARGET :: UNORM
!
!     WATER DEPTH SAVED FOR CONSTANT FLOW DISCHARGE
!
      TYPE(BIEF_OBJ), TARGET :: HCPL
!
!     IMPOSED BED EVOLUTION AT THE BOUNDARY
!
      TYPE(BIEF_OBJ), TARGET :: EBOR
!
!     IMPOSED SOLID TRANSPORT AT THE BOUNDARY
!     QBOR : IN M3/S, FOR EVERY CLASS
!     Q2BOR: IN M2/S, TOTAL, READ IN THE BOUNDARY CONDITIONS FILE
!
      TYPE(BIEF_OBJ), TARGET :: QBOR,Q2BOR
!
!     ZF VALUES ON BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: FLBOR
!
!     BOTTOM ELEVATION
!
      TYPE(BIEF_OBJ), TARGET :: ZF
!
!     NON ERODABLE (RIGID) BOTTOM ELEVATION
!
      TYPE(BIEF_OBJ), TARGET :: ZR
!
!     REFERENCE ELEVATION
!
      TYPE(BIEF_OBJ), TARGET :: ZREF
!
!     INTEGRAL OF BASES
!
      TYPE(BIEF_OBJ), TARGET :: VOLU2D
!
!     INTEGRAL OF BASES IN PARALLEL
!
      TYPE(BIEF_OBJ), TARGET :: V2DPAR
!
!     INVERSE OF INTEGRAL OF BASES
!
      TYPE(BIEF_OBJ), TARGET :: UNSV2D
!
!     BOTTOM FRICTION COEFFICIENT (CHEZY, NIKURADSE OR STRICKLER)
!
      TYPE(BIEF_OBJ), TARGET :: CHESTR
!
!     ANGLE BETWEEN QS AND Q
!
      TYPE(BIEF_OBJ), TARGET :: CALFA
!
!     ANGLE BETWEEN QS AND Q
!
      TYPE(BIEF_OBJ), TARGET :: SALFA
!
!     VOID STRUCTURE
!
      TYPE(BIEF_OBJ), TARGET :: S
!
!     MASK ON POINTS
!
      TYPE(BIEF_OBJ), TARGET :: MASKPT
!
!     MASK
!
      TYPE(BIEF_OBJ), TARGET :: MASKTR
!
!     MASK
!
      TYPE(BIEF_OBJ), TARGET :: MASKB
!
!     MASK
!
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!
!     MASK
!
      TYPE(BIEF_OBJ), TARGET :: MSKTMP
!
!     WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: W1
!
! WAVE DATA
! --------
!
!     WAVE DIRECTION (DEG WRT OX AXIS)    !!!!!SOME SAY OY AXIS!!!!!
!
      TYPE(BIEF_OBJ), TARGET :: THETAW
!
!     FRICTION COEFFICIENT (WAVES)
!
      TYPE(BIEF_OBJ), TARGET :: FW
!
!     ORBITAL VELOCITY
!
      TYPE(BIEF_OBJ), TARGET :: UW
!
!     SIGNIFICANT WAVE HEIGHT
!
      TYPE(BIEF_OBJ), TARGET :: HW
!
!     MEAN WAVE PERIOD
!
      TYPE(BIEF_OBJ), TARGET :: TW
!
!
!
      TYPE(BIEF_OBJ), TARGET :: INDIC,IFAMAS
!
!     INTEGER WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: IT1,IT2,IT3,IT4
!
!     TYPE OF BOUNDARY CONDITIONS ON BED EVOLUTION
!
      TYPE(BIEF_OBJ), TARGET :: LIEBOR
!
!     TYPE OF BOUNDARY CONDITIONS ON SAND TRANSPORT RATE
!
      TYPE(BIEF_OBJ), TARGET :: LIQBOR
!
!     TYPE OF BOUNDARY CONDITIONS
!
      TYPE(BIEF_OBJ), TARGET :: LIMTEC
!
!     IMPACT OF THE SLOPE EFFECT ON AMPLITUDE
!
      TYPE(BIEF_OBJ), TARGET :: COEFPN
!
!     LIQUID BOUNDARY NUMBERING
!
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!
!     SHEAR STRESS
!
      TYPE(BIEF_OBJ), TARGET :: TOB
!
!     FRICTION COEFFICIENT
!
      TYPE(BIEF_OBJ), TARGET :: CF
!
!     WAVE INDUCED SHEAR STRESS
!
      TYPE(BIEF_OBJ), TARGET :: TOBW
!
!     rapport entre la contrainte de frottement de peau et la contrainte totale
!
      TYPE(BIEF_OBJ), TARGET :: MU
!
!     rugosite totale
!
      TYPE(BIEF_OBJ), TARGET :: KS
!
!     rugosite de peau
!
      TYPE(BIEF_OBJ), TARGET :: KSP
!
!     rugosite de ride
!
      TYPE(BIEF_OBJ), TARGET :: KSR
!
!     BED LEVEL CHANGE FOR GRAIN-FEEDING
!
      TYPE(BIEF_OBJ), TARGET :: DZF_GF
!
!     MEAN DIAMETER OF ACTIVE-LAYER
!
      TYPE(BIEF_OBJ), TARGET :: ACLADM
!
!     MEAN DIAMETER OF UNDER-LAYER
!
      TYPE(BIEF_OBJ), TARGET :: UNLADM
!
!     NUMBER OF LAYERS FOR EACH POINT
!
      TYPE(BIEF_OBJ), TARGET :: NLAYER
!
!     HIDING FACTOR FOR PARTICULAR SIZE CLASS
!
      TYPE(BIEF_OBJ), TARGET :: HIDING
!
! 
!
      TYPE(BIEF_OBJ), TARGET :: ELAY
!
!     ACTIVE STRATUM THICKNESS
!
      TYPE(BIEF_OBJ), TARGET :: ESTRAT
!
!     DEPOSITION FLUX
!
      TYPE(BIEF_OBJ), TARGET :: FLUDP
!
!     DEPOSITION FLUX
!
      TYPE(BIEF_OBJ), TARGET :: FLUDPT
!
!     EROSION FLUX
!
      TYPE(BIEF_OBJ), TARGET :: FLUER
!
!     EROSION FLUX
!
      TYPE(BIEF_OBJ), TARGET :: FLUERT
!
!     CONCENTRATION AT TIME N
!
      TYPE(BIEF_OBJ), TARGET :: CS

!     MAK CHANGE create parameter for ratio depth averaged and reference concentration.
!     TYPE(BIEF_OBJ), TARGET :: CST, CTILD, CSTAEQ
      TYPE(BIEF_OBJ), TARGET :: CST, CTILD, CSTAEQ, CSRATIO
!
!     IMPOSED SUSPENDED SAND CONCENTRATION AT THE BOUNDARY (DIM.NPTFR)
!
      TYPE(BIEF_OBJ), TARGET :: CBOR
!
!     CONCENTRATION IN G/L
!
      TYPE(BIEF_OBJ), TARGET :: CSGL
!
!     COMPONENTS OF VELOCITY VECTORS
!
      TYPE(BIEF_OBJ), TARGET ::  UCONV,VCONV
!
!     PROPAGATION HEIGHT
!
      TYPE(BIEF_OBJ), TARGET :: HPROP
!
! 
!
      TYPE(BIEF_OBJ), TARGET :: DISP,DISP_C
!
!     FLUX CONDITION NU DF/DN=AFBOR * F + BFBOR
!
      TYPE(BIEF_OBJ), TARGET :: AFBOR,BFBOR
!
!     FLUX AT THE BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: FLBOR_SIS
!
!     FLUX AT THE BOUNDARIES FOR TRACER
!
      TYPE(BIEF_OBJ), TARGET :: FLBORTRA
!
!     BOUNDARY CONDITIONS FOR SEDIMENT                     : LICBOR
!     TYPES OF BOUNDARY CONDITIONS FOR H                   : LIHBOR
!     TYPES OF BOUNDARY CONDITIONS FOR PROPAGATION         : LIMPRO
!                    POINTS   :    .1:H  .2:U  .3:V
!                    SEGMENTS :    .4:H  .5:U  .6:V
!
!     TYPE OF BOUNDARY CONDITIONS ON SUSPENDED SAND CONCENTRATION
!
      TYPE(BIEF_OBJ), TARGET :: LICBOR
!
!     TYPE OF BOUNDARY CONDITIONS FOR H
!
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!
!     TYPE OF BOUNDARY CONDITIONS FOR PROPAGATION
!
      TYPE(BIEF_OBJ), TARGET :: LIMPRO
!
!     TYPE OF BOUNDARY CONDITIONS FOR DIFFUSION
!
      TYPE(BIEF_OBJ), TARGET :: LIMDIF
!
!     LAST COLUMN OF THE BOUNDARY CONDITION FILE
!
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!
!     BOUNDARY CONDITIONS FOR TRACER, U AND V (MODIFIED LITBOR, LIUBOR,LIVBOR)
!
      TYPE(BIEF_OBJ), TARGET :: CLT,CLU,CLV
!
!     WORK ARRAYS FOR ELEMENTS
!
      TYPE(BIEF_OBJ), TARGET :: TE1,TE2,TE3
!
!     COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!
      TYPE(BIEF_OBJ), TARGET :: KX,KY,KZ
!
!     ARRAY SAYING WHETHER THE NON-ERODABLE BOTTOM HAS BEEN REACHED (VF)
!
      TYPE(BIEF_OBJ), TARGET :: BREACH
!
!     FOR MIXED SEDIMENTS
!
      TYPE(BIEF_OBJ), TARGET :: FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE
!
!-----------------------------------------------------------------------
!
!       2) MATRICES
!
!-----------------------------------------------------------------------
!
!     BOUNDARY MATRIX
!
      TYPE(BIEF_OBJ), TARGET :: MBOR
!
!     MATRICES
!
      TYPE(BIEF_OBJ), TARGET :: AM1_S,AM2_S
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!     BLOCK OF MASKS
!
      TYPE(BIEF_OBJ), TARGET :: MASK
!
!     BLOCK OF WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: TB
!
!     BLOCK OF PRIVATE VECTORS
!
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!
!     BLOCK OF CLANDESTINE VARIABLES
!
      TYPE(BIEF_OBJ), TARGET :: VARCL
!
!     BLOCK OF VARIABLES FOR INPUT
!
      TYPE(BIEF_OBJ), TARGET :: VARHYD
!
!     BLOCK OF VARIABLES FOR OUTPUT
!
      TYPE(BIEF_OBJ), TARGET :: VARSOR

! UHM / PAT

!     VERTICAL SORTING PROFILE: FRACTION FOR EACH LAYER, CLASS, POINT

      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE::PRO_F

!     VERTICAL SORTING PROFILE: DEPTH FOR EACH LAYER, CLASS, POINT

      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE::PRO_D

! UHM / PAT

!
!     SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
!
      DOUBLE PRECISION,DIMENSION(:,:,:),TARGET,ALLOCATABLE::AVAIL
!
!     LAYER THICKNESSES AS DOUBLE PRECISION
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE :: ES
!
!     LAYER THICKNESSES OF THE MUD AS DOUBLE PRECISION
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE :: ES_VASE
!
!     LAYER THICKNESSES OF THE SAND AS DOUBLE PRECISION
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE :: ES_SABLE
!
!     SEDIMENT COMPOSITION
!
      TYPE(BIEF_OBJ), TARGET :: AVAI
!
!     LAYER THICKNESSES
!
      TYPE(BIEF_OBJ), TARGET :: LAYTHI
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: LAYCONC
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: QSCL
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
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: ZFCL_MS

!> @brief MEYER PETER MUELLER factor
C	  
      TYPE(BIEF_OBJ), TARGET :: MPM_ARAY
C
C     FLUX LIMITATION PER SEGMENT
C
      TYPE(BIEF_OBJ), TARGET :: FLULIM
!
!     FLUXES AT BOUNDARY FOR EVERY CLASS
!
      TYPE(BIEF_OBJ), TARGET :: FLBCLA		  
! 
!     CV modifs V6P2 new variables for consolidation model
! 
!     VOID INDEX OF BED LAYERS
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::IVIDE
!
!     CONCENTRATION OF BED LAYER
!
      DOUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::CONC
!
!-----------------------------------------------------------------------
!
!       4) INTEGERS
!
!-----------------------------------------------------------------------
!
!      KEYWORDS AND PARAMETERS

!     Maximum Layer Number in a VERTICAL SORTING PROFILE FOR EACH POINT (UHM)
      INTEGER, ALLOCATABLE :: PRO_MAX(:)
!
!      MAXIMUM NUMBER OF OUTPUT VARIABLES
!
      INTEGER, PARAMETER :: MAXVAR = 500
!
!     MAXIMUM NUMBER OF (LIQUID BOUNDARIES, SOLID BOUNDARIES)
!
      INTEGER, PARAMETER :: MAXFRO = 300
!
!     NUMBER OF LIQUID, SOLID BOUNDARIES
!
      INTEGER NFRLIQ,NFRSOL
!
!     BEGINNING AND END OF LIQUID BOUNDARIES
!
      INTEGER DEBLIQ(MAXFRO),FINLIQ(MAXFRO)
!
!     BEGINNING AND END OF SOLID BOUNDARIES
!
      INTEGER DEBSOL(MAXFRO),FINSOL(MAXFRO)
!
!     OPTION FOR THE DIFFUSION OF TRACER
!
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
!
!     FORMULA FOR DEVIATION
!
      INTEGER DEVIA
!
!     FORMULA FOR SLOPE EFFECT
!
      INTEGER SLOPEFF
!
! NON-EQUILIBRIUM BEDLOAD AND NON-UNIFORM BED MATERIA (BMD AND MGDL)
! --------
!
!     MAXIMUM NUMBER OF SIZE-CLASSES
!
      INTEGER, PARAMETER :: NSICLM = 10
!
!     NUMBER OF SIZE-CLASSES OF BED MATERIAL (LESS THAN 10)
!
      INTEGER :: NSICLA
!
!     MAXIMUM NUMBER OF LAYERS ON THE MESH
!
      INTEGER, PARAMETER :: NLAYMAX = 20
!
!     NUMBER OF BED LOAD MODEL LAYERS
!
      INTEGER NOMBLAY
!     
!     FORMULATION FOR THE HIDING FACTOR
!
      INTEGER HIDFAC
!
      INTEGER :: LOADMETH
!
!     DEBUGGER
!
      INTEGER :: DEBUG
!
!     REFERENCE CONCENTRATION FORMULA
!
      INTEGER :: ICQ
!
!     NUMBER OF CONTROL SECTIONS POINTS
!
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
!
!     NUMBER OF GIVEN SSOLID DISCHARGES GIVEN BY USER
!
      INTEGER NSOLDIS

!// UHM // For the Continous Vertical Sorting MODEL
!
!     Type of the Vertical Grain Sorting: Hirano Layers or Continous-VSM
!
      INTEGER VSMTYPE
!
!     Maximum Number of Profile SECTIONS
!
      INTEGER PRO_MAX_MAX
!
!     Printout Period for Full Vertical Sorting Model: PRO_D & PRO_F
!
      INTEGER CVSMPPERIOD
!
!     CHOOSE POINTS or FULL MODEL AS PRINTOUT
!
      INTEGER CVSMOUTPUT(100)    !Limited to 100 for no specific reason
!
!     CHOOSE A MODEL FOR ESTIMATION OF A DYNAMIC ACTIVE LAYER THICKNESS
!
      INTEGER ALT_MODEL

!// UHM //
!
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
!
!  C-VSM WRITES OUT (OR NOT) IN THIS TIMESTEP
C
      LOGICAL :: CVSM_OUT !UHM

!  C-VSM_FULL WRITES OUT (OR NOT) EVER
C
      LOGICAL :: CVSM_OUT_FULL !UHM

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
      DOUBLE PRECISION VOLTOT(NSICLM)
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
!> @brief
C CV: masse du lit sedimentaire pour bilan mixte et cohesif
!     FOR MASS BALANCE OF COHESIVE SEDIMENT AND MIXTE
      DOUBLE PRECISION :: MASVT,MASV0,MASST,MASS0
!..
!!> @brief FOR NON-EQUILIBIRUM BEDLOAD
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
!
!     Secondary Current Alpha Coefficient
!
      DOUBLE PRECISION :: ALPHA
!
!     Morphological Factor
!
      DOUBLE PRECISION :: MOFAC
!> @brief ZERO OF THE CODE
C
      DOUBLE PRECISION :: ZERO
!
!     B VALUE FOR THE BIJKER FORMULA
!
      DOUBLE PRECISION :: BIJK
!
!     MUD CONCENTRATION AT BOUNDARIES FOR EACH CLASS
!
      DOUBLE PRECISION :: CBOR_CLASSE(NSICLM*MAXFRO)
!
!     MUD CONCENTRATION FOR EACH LAYER (Constante
!
      DOUBLE PRECISION :: CONC_VASE(NLAYMAX)
!
!     MASS TRANSFER BETWEEN LAYERS
!
      DOUBLE PRECISION :: TRANS_MASS(NLAYMAX)
!
!     CRITICAL EROSION SHEAR STRESS OF THE MUD PER LAYER
!
      DOUBLE PRECISION :: TOCE_VASE(NLAYMAX)
!
!     CRITICAL EROSION SHEAR STRESS OF THE SAND
!
      DOUBLE PRECISION :: TOCE_SABLE
!
!     THIEBOT MODEL
! 
      DOUBLE PRECISION :: CONC_GEL, COEF_N,CONC_MAX
!
!     PRESCRIBED SOLID DISCHARGES
! 
      DOUBLE PRECISION :: SOLDIS(MAXFRO)
!
!     FOR MASS BALANCE OF COHESIVE SEDIMENT
!
      DOUBLE PRECISION :: MASBED0,MASBED
!
!
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!
!
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
!
!-----------------------------------------------------------------------
!
!       9) MESH STRUCTURE
!
!-----------------------------------------------------------------------
!
!     MESH STRUCTURE
!
      TYPE(BIEF_MESH) :: MESH
!
!-----------------------------------------------------------------------
!
!      10) ALIASES
!
!-----------------------------------------------------------------------
!
!     DECLARATION OF POINTERS FOR ALIASES
!     TARGETS ARE DEFINED IN POINT_TELEMAC2D
!
!     ALIASES FOR WORK VECTORS IN TB
!
      TYPE(BIEF_OBJ),POINTER :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12
      TYPE(BIEF_OBJ),POINTER :: T13,T14
!
!     USEFUL COMPONENTS IN STRUCTURE MESH
! 
!
!     CONNECTIVITY TABLE
!
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
!
!-----------------------------------------------------------------------
!
!      11) SISYPHE FILES + INTEGER DECLARATION FOR MED APPROACH
!
!-----------------------------------------------------------------------
!
!     MAXIMUM RANK OF LOGICAL UNITS AS DECLARED IN SUBMIT STRINGS IN THE DICTIONARY
!
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
!
!     RANK OF 'FILE FOR LIQUID BOUNDARIES' IN SIS_FILES
!
      INTEGER SISLIQ
!
!-----------------------------------------------------------------------
!
!      12) SECTIONS
!
!-----------------------------------------------------------------------
!
      TYPE (CHAIN_TYPE), ALLOCATABLE :: CHAIN(:)
!
      SAVE   ! VERY IMPORTANT
!
      END MODULE DECLARATIONS_SISYPHE


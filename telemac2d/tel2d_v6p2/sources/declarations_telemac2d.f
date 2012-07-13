!                    *****************************
                     MODULE DECLARATIONS_TELEMAC2D
!                    *****************************
!
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
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
!history  C.COULET (ARTELIA)
!+        30/03/2012
!+        V6P2
!+   Modification for adding "bridge" file and separation of weirs and
!+   culvert file
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
!
!     INCREMENTS OF U AND V IN ONE TIME STEP
! 
      TYPE(BIEF_OBJ), TARGET :: DU,DV
!
!     INCREMENT OF HN IN ONE TIME STEP
! 
      TYPE(BIEF_OBJ), TARGET :: DHN
!
!     COMPONENTS OF ADVECTING FIELD
! 
      TYPE(BIEF_OBJ), TARGET :: UCONV,VCONV
!
!     DEPTH IN THE PROPAGATION TERM I.E. IN H DIV(U)
! 
      TYPE(BIEF_OBJ), TARGET :: HPROP
!
!     PRESCRIBED VALUES ON BOUNDARIES FOR U AND V
! 
      TYPE(BIEF_OBJ), TARGET :: UBOR,VBOR
!
!     VALUES ON BOUNDARIES: WALL FRICTION COEFFICIENTS
!
      TYPE(BIEF_OBJ), TARGET :: AUBOR,UETUTA
!
!     PRESCRIBED VALUES ON BOUNDARIES FOR DEPTH
! 
      TYPE(BIEF_OBJ), TARGET :: HBOR
!
!     PRESCRIBED VALUES ON BOUNDARIES FOR TRACERS
! 
      TYPE(BIEF_OBJ), TARGET :: TBOR
!
!     PRESCRIBED VALUES ON BOUNDARIES: K AND EPSILON
! 
      TYPE(BIEF_OBJ), TARGET :: KBOR,EBOR
!
!     FLUX ON THE BOUNDARIES: WATER AND TRACERS
!
      TYPE(BIEF_OBJ), TARGET :: FLBOR,FLBORTRA
!
!     ON BOUNDARIES: COEFFICIENTS FOR HEAT FLUXES
! 
      TYPE(BIEF_OBJ), TARGET :: ATBOR,BTBOR
!
!     ADIMENSIONAL FRICTION COEFFICIENTS (BOTTOM AND BOUNDARIES)
! 
      TYPE(BIEF_OBJ), TARGET :: CF,CFBOR
!
!     VOID STRUCTURE
!
      TYPE(BIEF_OBJ), TARGET :: S
!
!     BOTTOM TOPOGRAPHY: PER POINT, PER ELEMENT
! 
      TYPE(BIEF_OBJ), TARGET :: ZF,ZFE
!
!     VELOCITY DIFFUSIVITY: VELOCITY, TRACERS, SAVED
!
      TYPE(BIEF_OBJ), TARGET :: VISC,VISCT,VISC_S
!
!     SOURCE TERMS IN THE MOMENTUM EQUATION
!
      TYPE(BIEF_OBJ), TARGET :: FU,FV
!
!     WAVE STRESSES (FROM ARTEMIS OR TOMAWAC)
!
      TYPE(BIEF_OBJ), TARGET :: FXWAVE,FYWAVE
!
!     INITIAL CELERITY OF WAVES
!
      TYPE(BIEF_OBJ), TARGET :: C0
!
!     INITIAL DEPTH
!
      TYPE(BIEF_OBJ), TARGET :: H0
!
!     MASK FOR TRACERS
!
      TYPE(BIEF_OBJ), TARGET :: MASKTR
!
!     FREE SURFACE ELEVATION OF INCIDENT WAVE
! 
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
!
      TYPE(BIEF_OBJ), TARGET :: ZFLATS
!
!     FRICTION COEFFICIENT, IN TERMS OF STRICKLER, CHEZY, ETC
! 
      TYPE(BIEF_OBJ), TARGET :: CHESTR
!
!     TYPES OF BOUNDARY CONDITIONS ON U, V, H AND TRACERS
! 
      TYPE(BIEF_OBJ), TARGET :: LIUBOR,LIVBOR,LIHBOR,LITBOR
!
!     TYPES OF BOUNDARY CONDITIONS ON TRACERS
!
      TYPE(BIEF_OBJ), TARGET :: LIMTRA
!
!     TYPES OF BOUNDARY CONDITIONS ON K AND EPSILON
!
      TYPE(BIEF_OBJ), TARGET :: LIMKEP
!
!     TYPES OF BOUNDARY CONDITIONS FOR PROPAGATION
!
      TYPE(BIEF_OBJ), TARGET :: LIMPRO
!
!     BOUNDARY CONDITIONS ON H, U, V (COPIES OF LIHBOR,LIUBOR,LIVBOR)
!
      TYPE(BIEF_OBJ), TARGET :: CLH,CLU,CLV
!
!     ORIGINAL BOUNDARY NODE NUMBER (OR COLOUR GIVEN BY USER)
!
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!
!     POSITIONS OF FLOATING BODIES
! 
      TYPE(BIEF_OBJ), TARGET :: XFLOT,YFLOT
!
!     FOR LAGRANGIAN DRIFTS
!
      TYPE(BIEF_OBJ), TARGET :: SHPFLO,XLAG,YLAG,SHPLAG
!
!     ELEVATION OF POINTS IN A WEIR
!
      TYPE(BIEF_OBJ), TARGET :: ZDIG
!
!     DISCHARGE COEFFICIENTS OF POINTS IN WEIRS
!
      TYPE(BIEF_OBJ), TARGET :: PHIDIG
!
!     TIME STEP OF INITIAL RELEASE FOR FLOATING BODIES, IDEM FOR FINAL
! 
      TYPE(BIEF_OBJ), TARGET :: DEBFLO,FINFLO
!
!     ELEMENT WHERE THE FLOATING BODIES ARE
!
      TYPE(BIEF_OBJ), TARGET :: ELTFLO
!
!     CONNECTIVITY TABLE FOR OUTPUT OF FLOATING BODIES TRAJECTORY
!
      TYPE(BIEF_OBJ), TARGET :: IKLFLO
!
!     TIME STEP AT THE BEGINNING (END) OF LAGRANGIAN DRIFTS
!
      TYPE(BIEF_OBJ), TARGET :: DEBLAG,FINLAG
!
!     ELEMENT WHERE IS THE LAGRANGIAN DRIFT
!
      TYPE(BIEF_OBJ), TARGET :: ELTLAG
!
!     BOUNDARY NUMBERS OF POINTS IN A WEIR
!
      TYPE(BIEF_OBJ), TARGET :: NUMDIG
!
!     INTEGERS WORKING ARRAYS
!
      TYPE(BIEF_OBJ), TARGET :: IT1,IT2,IT3,IT4
!
!     COMPONENTS OF DISCHARGE AT TIME N
!
      TYPE(BIEF_OBJ), TARGET :: QU,QV
!
!     STORED WATER DEPTH, AND CORRECTED STORED WATER DEPTH
!
      TYPE(BIEF_OBJ), TARGET :: HSTOK,HCSTOK
!
!     SOURCE TERMS OF TRACERS
!
      TYPE(BIEF_OBJ), TARGET :: SMTR
!
!     REFERENCE OF BOUNDARY NODES
!
      TYPE(BIEF_OBJ), TARGET :: LOGFR
!
!     TRACER FLUX
!
      TYPE(BIEF_OBJ), TARGET :: FLUXT
!
!     RECONSTRUCTED DEPTH, AND CORRECTED VERSION
!
      TYPE(BIEF_OBJ), TARGET :: HT,HC
!
!     VARIATIONS OF Z OF ORDER 2
!
      TYPE(BIEF_OBJ), TARGET :: DSZ
!
!     FLUX OF MASS FOR TRACER
!
      TYPE(BIEF_OBJ), TARGET :: FLUXTEMP
!
!     TRACER FLUX AT BOUNDARY, TEMPORARY VALUE
!
      TYPE(BIEF_OBJ), TARGET :: FLUHBOR,FLUHBTEMP
!
!     ??????
!
      TYPE(BIEF_OBJ), TARGET :: SECMOU , IFAMAS
!
!     MASK FOR POINTS (1: normal; 0:masked)
!
      TYPE(BIEF_OBJ), TARGET :: MASKPT
!
!     MASK FOR ELEMENTS (1: normal; 0:masked)
!
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!
!     LIQUID BOUNDARY NUMBERS
!
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!
!     MAXIMUM ELEVATIONS
!
      TYPE(BIEF_OBJ), TARGET :: MAXZ
!
!     CORRESPONDING TIMES FOR MAXIMUM ELEVATIONS
!
      TYPE(BIEF_OBJ), TARGET :: TMAXZ
!
!     MAXIMUM VELOCITIES
!
      TYPE(BIEF_OBJ), TARGET :: MAXV
!
!     CORRESPONDING TIMES FOR MAXIMUM VELOCITIES
!
      TYPE(BIEF_OBJ), TARGET :: TMAXV
!
!     FOR STORING ZONE NUMBERS
!
      TYPE(BIEF_OBJ), TARGET :: ZONE
!
!     FOR STORING RESULTS OF FOURIER ANALYSIS (AMPLITUDE AND PHASE)
!
      TYPE(BIEF_OBJ), TARGET :: AMPL,PHAS
!
!     FRICTION COEFFICIENT FOR BOUNDARY CONDITIONS
!
      TYPE(BIEF_OBJ),TARGET :: CHBORD
!
!     COMPATIBLE COMPONENTS OF VELOCITY FOR DELWAQ
!
      TYPE(BIEF_OBJ),TARGET :: UDEL,VDEL
!
!     VELOCITY WILL BE UCONV + DM1*GRAD(ZCONV)
!
      TYPE(BIEF_OBJ),TARGET :: DM1,ZCONV
!
!     COMPATIBLE FLUXES FOR DELWAQ
!
      TYPE(BIEF_OBJ),TARGET :: FLODEL
!
!     COEFFICIENTS OF LIMITATION FOR DELWAQ
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
!     FOR RAIN-EVAPORATION
!
      TYPE(BIEF_OBJ),TARGET :: PLUIE
!
!     FOR CULVERT MANAGEMENT
      TYPE(BIEF_OBJ),TARGET :: ENTSIP, SORSIP, SECSIP, ALTSIP
      TYPE(BIEF_OBJ),TARGET :: CSSIP , CESIP , DELSIP, ANGSIP, LSIP
      TYPE(BIEF_OBJ),TARGET :: USIP, VSIP, DSIP, TSIP
!
!     FOR TUBES/BRIDGES MANAGEMENT
      TYPE(BIEF_OBJ),TARGET :: ENTBUS, SORBUS, LRGBUS, HAUBUS, ALTBUS
      TYPE(BIEF_OBJ),TARGET :: CSBUS , CEBUS , DELBUS, ANGBUS, LBUS
      TYPE(BIEF_OBJ),TARGET :: UBUS, VBUS, DBUS, TBUS, CLPBUS
!
!      NEIGHBORS OF SEGMENT (FOR WAF SCHEME)
!
      TYPE(BIEF_OBJ),TARGET :: NEISEG
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
!     ( AM1  BM1  BM2 )   (DH)      ( CV1 )
!     (               )   (  )      (     )
!     ( CM1  AM2  A23 )   (U )  =   ( CV2 )
!     (               )   (  )      (     )
!     ( CM2  A32  AM3 )   (V )      ( CV3 )
!
!
!     MATRICES OF THE FINAL LINEAR SYSTEM OF THE PRIMITIVE EQUATIONS OPTION
!
      TYPE(BIEF_OBJ), TARGET :: AM1,AM2,AM3,BM1,BM2,CM1,CM2,A23,A32
!  
!     BOUNDARY MATRIX
!
      TYPE(BIEF_OBJ), TARGET :: MBOR
!
!     BM1 AND BM2 SAVED
!
      TYPE(BIEF_OBJ), TARGET :: BM1S,BM2S
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!     DIFFUSION MATRIX
!
      TYPE(BIEF_OBJ), TARGET :: TM1
!
!     BLOCK FOR DIRICHLET VALUES HBOR, UBOR AND VBOR
!
      TYPE(BIEF_OBJ), TARGET :: DIRBOR
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
!     BLOCKS OF VARIABLES AT TIME N+1, AT TIME N
!
      TYPE(BIEF_OBJ), TARGET :: F,FN
!
!     BLOCKS OF VARIABLES AFTER ADVECTION BY CHARACTERISTICS
!
      TYPE(BIEF_OBJ), TARGET :: FTILD
!
!     BLOCK OF VARIABLES TO BE ADVECTED BY CHARACTERISTICS
!
      TYPE(BIEF_OBJ), TARGET :: FNCAR
!
!     BLOCKS OF MATRICES FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: MAT
!
!     BLOCKS OF RIGHT HAND SIDES FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: RHS
!
!     BLOCKS OF UNKNOWNS FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: UNK
!
!     BLOCK OF CLANDESTINE VARIABLES
!
      TYPE(BIEF_OBJ), TARGET :: VARCL
!
!     BLOCK OF VARIABLES FOR OUTPUT
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
!     MAXIMUM NUMBER OF SOURCE POINTS
! 
      INTEGER, PARAMETER :: MAXSCE = 100
!
!     MAXIMUM NUMBER OF LIQUID / SOLID BOUNDARIES
!
      INTEGER, PARAMETER :: MAXFRO = 300
!
!     MAXIMUM NUMBER OF OUTPUT VARIABLES
!
      INTEGER, PARAMETER :: MAXVAR = 100
!
!     MAXIMUM NUMBER OF TRACERS
!
      INTEGER, PARAMETER :: MAXTRA = 20
!
!     NUMBER OF NODES FOR THE CONTROL SECTIONS (TWO TIMES THE NUMBER OF CONTROL SECTIONS)
!
      INTEGER NCP
!
!     ARRAY CONTAINING THE GLOBAL NUMBER OF POINTS IN THE CONTROL SECTIONS
! 
      INTEGER, ALLOCATABLE :: CTRLSC(:)
!
!     MAXIMUM RANK OF LOGICAL UNITS AS DECLARED IN SUBMIT STRINGS IN THE DICTIONARY
!
      INTEGER, PARAMETER :: MAXLU_T2D = 50
!
!     MAXIMUM NUMBER OF POINTS ON ONE SIDE OF A SINGULARITY (READ IN THE DATA)
! 
      INTEGER NPSMAX
!
!     MAXIMUM NUMBER OF SINGULARITIES
! 
      INTEGER, PARAMETER :: NWRMAX = 10
!
!     NUMBER OF POINTS OF EITHER SIDES OF SINGULARITIES
!
      INTEGER NPSING(NWRMAX)
!
!     GEOMETRY FILE NUMBER
! 
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
!
!     BOTTOM TOPOGRAPHY FILE NUMBER
! 
      INTEGER T2DFON
!
!     BINARY DATA FILE 1,2
! 
      INTEGER T2DBI1,T2DBI2
!
!     FORMATTED DATA FILE 1,2
! 
      INTEGER T2DFO1,T2DFO2
!
!     BINARY RESULTS FILE NUMBER
! 
      INTEGER T2DRBI
!
!     FORMATTED RESULTS FILE NUMBER
! 
      INTEGER T2DRFO
!
!     REFERENCE FILE NUMBER
! 
      INTEGER T2DREF
!
!     LIQUID BOUNDARIES FILE NUMBER
!
      INTEGER T2DIMP
!
!     FRICTION DATA FILE NUMBER
! 
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
!     ASCII TIDAL MODEL DATABASE FILE NUMBER
! 
      INTEGER T2DBDD
!
!     BINARY TIDAL MODEL DATABASES 1 AND 2 FILE NUMBER
!
      INTEGER T2DBB1,T2DBB2
!
!     WEIR DATA FILE NUMBER
!
      INTEGER T2DSEU
!
!     CULVERT DATA FILE NUMBER
!
      INTEGER T2DSIP
!
!     TUBES/BRIDGES DATA FILE NUMBER
!
      INTEGER T2DBUS
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
!
!     NOT USED (PROPAGATION OPTION)
! 
      INTEGER OPTPRO
!
!     INITIAL GUESS FOR H
! 
      INTEGER IORDRH
!
!     INITIAL GUESS FOR U
! 
      INTEGER IORDRU
!
!     NUMBER OF SUB-ITERATIONS FOR NON-LINEARITIES
! 
      INTEGER NSOUSI
!
!     NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
! 
      INTEGER PTINIG
!
!     NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
! 
      INTEGER PTINIL
!
!     TURBULENCE MODEL FOR SOLID BOUNDARIES
! 
      INTEGER LISRUG
!
!     NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
!
      INTEGER NDEBIT
!
!     NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
!
      INTEGER NCOTE
!
!     NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
!
      INTEGER NVITES
!
!     NUMBER OF BOUNDARIES WITH PRESCRIBED TRACER
!
      INTEGER NTRACE
!
!     NUMBER OF CLANDESTINE VARIABLES
!
      INTEGER NVARCL
!
!     VECTOR LENGTH
! 
      INTEGER LVMAC
!
!     OPTION FOR THE TREATMENT OF TIDAL FLATS
! 
      INTEGER OPTBAN
!
!     NUMBER OF DROGUES
! 
      INTEGER NFLOT
!
!     PRINTOUT PERIOD FOR DROGUES
! 
      INTEGER FLOPRD
!
!     NUMBER OF LAGRANGIAN DRIFTS
! 
      INTEGER NLAG
!
!     MAXIMUM NUMBER OF RECORDS OF SUCCESIVE POSITIONS OF FLOATING BODIES
!
      INTEGER NITFLO
!
!     BOTTOM SMOOTHINGS
! 
      INTEGER LISFON
!
!     DISCRETIZATIONS IN SPACE
! 
      INTEGER DISCRE(4)
!
!     NUMBER OF SINK/SOURCES
!
      INTEGER NREJET
!
!     NUMBER OF SINK/SOURCES WITH GIVEN VELOCITY
!
      INTEGER NREJEU
!
!     NUMBER OF SINK/SOURCES WITH GIVEN TRACER
!
      INTEGER NREJTR
!
!     SUPG OPTION
! 
      INTEGER OPTSUP(4)
!
!     ORIGINAL DATE OF TIME
! 
      INTEGER MARDAT(3)
!
!     ORIGINAL HOUR OF TIME
! 
      INTEGER MARTIM(3)
!
!     VARIOUS DISCRETISATION TIMES
!
      INTEGER IELM0,IELM1,IELMH,IELMU,IELMT,IELMK,IELME
!
!     MATRIX STORAGE
! 
      INTEGER OPTASS
!
!     MATRIX-VECTOR PRODUCT
! 
      INTEGER PRODUC
!
!     NUMBER OF WEIRS
! 
      INTEGER NWEIRS
!
!     NUMBER OF CULVERTS
! 
      INTEGER NSIPH
!
!     NUMBER OF TUBES/BRIDGES
! 
      INTEGER NBUSE
!
!     NUMBER OF BOUNDARIES WITH GIVEN OPTION FOR LIQUID BOUNDARIES
!
      INTEGER NTYPFR
!
!     OPTION FOR LIQUID BOUNDARIES
! 
      INTEGER FRTYPE(MAXFRO)
!
!     TREATMENT OF THE LINEAR SYSTEM
! 
      INTEGER SOLSYS
!
!     VELOCITY PROFILES
! 
      INTEGER PROVEL(MAXFRO)
!
!     NUMBER OF LIQUID BOUNDARIES
! 
      INTEGER NFRLIQ
!
!     NUMBER OF SOLID BOUNDARIES
!
      INTEGER NFRSOL
!
!     RANK OF FIRST,LAST POINT OF LIQUID BOUNDARY
!
      INTEGER DEBLIQ(MAXFRO),FINLIQ(MAXFRO)
!
!     RANK OF FIRST,LAST POINT OF SOLID BOUNDARY
!
      INTEGER DEBSOL(MAXFRO),FINSOL(MAXFRO)
!
!     NEAREST POINTS OF SOURCES
!
      INTEGER ISCE(MAXSCE)
!
!     OPTION FOR THE DIFFUSION OF TRACERS
! 
      INTEGER OPDTRA
!
!     OPTION FOR THE DIFFUSION OF VELOCITIES
! 
      INTEGER OPDVIT
!
!     TYPE OF SOURCES
! 
      INTEGER OPTSOU
!
!     NUMBER OF PRIVATE ARRAYS
! 
      INTEGER NPRIV
!
!     DELWAQ PRINTOUT PERIOD
! 
      INTEGER WAQPRD
!
!     TREATMENT OF NEGATIVE DEPTHS
! 
      INTEGER OPT_HNEG
!
!     CURRENT ITERATION NUMBER
! 
      INTEGER LT
!
!     RECORD NUMBER IN THE WAVE DRIVEN CURRENTS FILE
! 
      INTEGER NPTH
!
!     NUMBER OF USER-DEFINED ZONES (E.G. FOR SETTING FRICTION) IN SUBROUTINE DEF_ZONES
!
      INTEGER NZONE
!
!     NUMBER OF PERIODS FOR FOURIER ANALYSIS
! 
      INTEGER NPERIAF
!
!     OPTION FOR PARAMETER ESTIMATION, IDENTIFICATION METHOD
! 
      INTEGER OPTID
!
!     NUMBER OF REMARKABLE POINTS
!
      INTEGER NPTS
!
!     LIST OF REMARKABLE POINTS
! 
      INTEGER LIST_PTS(100)
!
!     CHOICE OF COST-FUNCTION
! 
      INTEGER OPTCOST
!
!     MAXIMUM NUMBER OF ITERATIONS FOR IDENTIFICATION
! 
      INTEGER MAXEST
!
!     COUPLING PERIOD (WITH SISYPHE)
!
      INTEGER PERCOU
!
!     COUPLING PERIOD FOR TOMAWAC
!
      INTEGER PERCOU_WAC
!
!     FINITE VOLUME SCHEME
! 
      INTEGER OPTVF
!
!     ORIGINS OF THE COORDINATE SYSTEM
! 
      INTEGER I_ORIG,J_ORIG
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
!     OPTION FOR TSUNAMI GENERATION
!
      INTEGER OPTTSUNAMI
!
!     TIDAL DATA BASE
!
      INTEGER TIDALDB
!
!     GEOGRAPHIC SYSTEM
!
      INTEGER GEOSYST
!
!     ZONE NUMBER IN GEOGRAPHIC SYSTEM
!
      INTEGER NUMZONE
!
!     LAW OF TRACERS DEGRADATION
!
      INTEGER LOITRAC(MAXTRA)
!
!     RECORD NUMBER FOR RESTART
!
      INTEGER START_RECORD
!
!-----------------------------------------------------------------------
!
!       5) LOGICAL VALUES
!
!-----------------------------------------------------------------------
!
!     IF YES, COMPUTATION CONTINUED
! 
      LOGICAL DEBU
!
!     IF YES, LISTING PRINTOUT IS REQUIRED
! 
      LOGICAL LISTIN
!
!     IF YES, DIFFUSION OF VELOCITY
! 
      LOGICAL DIFVIT
!
!     IF YES, AIR PRESSURE IS TAKEN INTO ACCOUNT
! 
      LOGICAL ATMOS
!
!     IF YES, ADVECTION
! 
      LOGICAL CONV
!
!     IF YES, ADVECTION OF U AND V (1), H (2), TRACERS (3), K AND EPSILON (4)
! 
      LOGICAL CONVV(4)
!
!     IF YES, DIFFUSION OF TRACERS
! 
      LOGICAL DIFT
!
!     IF YES, CORIOLIS
! 
      LOGICAL CORIOL
!
!     IF YES, INFORMATION PRINTED ON LISTING
! 
      LOGICAL INFOGR
!
!     IF YES, MASS-BALANCE
! 
      LOGICAL BILMAS
!
!     IF YES, H CLIPPING
! 
      LOGICAL CLIPH
!
!     IF YES, WIND IS TAKEN INTO ACCOUNT
! 
      LOGICAL VENT
!
!     IF YES, PROPAGATION
! 
      LOGICAL PROPA
!
!     IF YES, SPHERICAL COORDINATES
! 
      LOGICAL SPHERI
!
!     IF YES, LINEARISED PROPAGATION
! 
      LOGICAL PROLIN
!
!     IF YES, VALIDATION
! 
      LOGICAL VALID
!
!     IF YES, VERIFICATION OF LIMITS
!
      LOGICAL VERLIM
!
!     IF YES, CONTINUITY CORRECTION
!  
      LOGICAL CORCON
!
!     IF YES, INITIAL TIME SET TO ZERO
! 
      LOGICAL RAZTIM
!
!     IF YES, INFORMATION ABOUT K-EPSILON MODEL PRINTED ON LISTING
! 
      LOGICAL INFOKE
! 
!     IF YES, THERE ARE MASKED ELEMENTS
!
      LOGICAL MSK
!
!     IF YES, STOP WHEN A STEADY STATE IS REACHED
! 
      LOGICAL STOPER
!
!     IF YES, ELEMENTS MASKED BY USER
! 
      LOGICAL MSKUSE
!
!     TO KNOW IF A VARIABLE WILL BE EXITED ON FILE, ON LISTING
!
      LOGICAL SORLEO(MAXVAR),SORIMP(MAXVAR)
!
!     VARIABLE DENSITY. IF YES, DENSITY EFFECTS INCLUDED
! 
      LOGICAL ROVAR
!
!     TIDE GENERATING FORCE
! 
      LOGICAL MAREE
!
!     IF YES, THERE IS A TREATMENT OF BOUNDARY CONDITIONS WITH 
!     THOMPSON METHOD
! 
      LOGICAL THOMFR
!
!     IF YES, C-U PRECONDITIONING
! 
      LOGICAL PRECCU
!
!     IF YES, VARIABLE TIME-STEP
! 
      LOGICAL DTVARI
!
!     IF YES, TIDAL FLATS
!  
      LOGICAL BANDEC
!
!     IF YES, PRINT INFORMATIONS ON LISTING
!
      LOGICAL ENTET
!
!     IF YES, OIL SPILL MODEL
! 
      LOGICAL SPILL_MODEL
!
!     IF YES, WAVE DRIVEN CURRENTS ARE TAKEN INTO ACCOUNT
! 
      LOGICAL COUROU
!
!     IF YES, VERTICAL STRUCTURES ARE TAKEN INTO ACCOUNT
! 
      LOGICAL VERTIC
!
!     IF YES, OUTPUT INITIAL CONDITIONS
! 
      LOGICAL OUTINI
!
!     IF YES, THE USER WANTS TO DEFINE ZONES
!  
      LOGICAL DEFZON
!
!     IF YES, COMPATIBLE COMPUTATION OF FLUXES (RELATED TO FLUXES THROUGH SECTIONS)
! 
      LOGICAL COMFLU
!
!     IF YES, PRINTS CUMULATED FLOWRATES
! 
      LOGICAL CUMFLO
!
!     IF YES, OUTPUT OF SALINITY FOR DELWAQ
! 
      LOGICAL SALI_DEL
!
!     IF YES, OUTPUT OF TEMPERATURE FOR DELWAQ
! 
      LOGICAL TEMP_DEL
!
!     IF YES, OUTPUT OF VELOCITY FOR DELWAQ
! 
      LOGICAL VELO_DEL
!
!     IF YES, OUTPUT OF DIFFUSIVITY FOR DELWAQ
!  
      LOGICAL DIFF_DEL
!
!     IF YES, RAIN OR EVAPORATION TAKEN INTO ACCOUNT
!  
      LOGICAL RAIN
!
!     IF YES, INFERENCE OF MINOR CONSTITUENTS
!  
      LOGICAL INTMICON
!
!-----------------------------------------------------------------------
!
!       6) REALS
!
!-----------------------------------------------------------------------
!
!     TIME STEP
! 
      DOUBLE PRECISION DT
!
!     GRAVITY ACCELERATION
! 
      DOUBLE PRECISION GRAV
!
!     FRICTION COEFFICIENT
! 
      DOUBLE PRECISION FFON
!
!     COEFFICIENT OF CORIOLIS
! 
      DOUBLE PRECISION FCOR
!
!     COEFFICIENT OF WIND INFLUENCE
! 
      DOUBLE PRECISION FAIR
!
!     COMPONENTS OF WIND VELOCITY
!
      DOUBLE PRECISION FUAIR,FVAIR
!
!     INITIAL VALUES OF TRACERS
! 
      DOUBLE PRECISION TRAC0(MAXTRA)
!
!     COEFFICIENT FOR DIFFUSION OF TRACERS
! 
      DOUBLE PRECISION DIFNU
!
!     IMPLICITATION FOR TRACERS
! 
      DOUBLE PRECISION TETAT
!
!     VELOCITY DIFFUSIVITY
! 
      DOUBLE PRECISION PROPNU
!
!     THRESHOLD VALUES FOR STOPPING THE COMPUTATION
! 
      DOUBLE PRECISION BORNES(8)
!
!     IMPLICITATION FOR DEPTH
! 
      DOUBLE PRECISION TETAC
!
!     IMPLICITATION FOR VELOCITY
! 
      DOUBLE PRECISION TETAU
!
!     IMPLICITATION FOR DIFFUSION OF VELOCITY
! 
      DOUBLE PRECISION TETAD
!
!     MASS-LUMPING COEFFICIENT ON H
! 
      DOUBLE PRECISION AGGLOC
!
!     MASS-LUMPING COEFFICIENT ON VELOCITY
! 
      DOUBLE PRECISION AGGLOU
!
!     MINIMUM VALUE OF DEPTH
! 
      DOUBLE PRECISION HMIN
!
!     TIME STEP REDUCTION FOR K-EPSILON MODEL
! 
      DOUBLE PRECISION REDUC
!
!     MEAN DEPTH FOR LINEARIZATION
! 
      DOUBLE PRECISION HAULIN
!
!     WATER DENSITY
! 
      DOUBLE PRECISION ROEAU
!
!     LATITUDE OF ORIGIN POINT
! 
      DOUBLE PRECISION LAMBD0
!
!     ROUGHNESS COEFFICIENT OF BOUNDARIES
!
      DOUBLE PRECISION SB
!
!     INITIAL ELEVATION
! 
      DOUBLE PRECISION COTINI
!
!     INITIAL DEPTH
! 
      DOUBLE PRECISION HAUTIN
!
!     PRESCRIBED FLOWRATES
! 
      DOUBLE PRECISION DEBIT(MAXFRO)
!
!     PRESCRIBED ELEVATIONS
! 
      DOUBLE PRECISION COTE(MAXFRO)
!
!     PRESCRIBED VELOCITIES
! 
      DOUBLE PRECISION VITES(MAXFRO)
!
!     PRESCRIBED TRACERS VALUES
! 
      DOUBLE PRECISION TRACER(MAXFRO*MAXTRA)
!
!     FLUXES AT BOUNDARIES
!
      DOUBLE PRECISION FLUX_BOUNDARIES(MAXFRO)
!
!     ABSCISSAE, ORDINATES OF SOURCES
! 
      DOUBLE PRECISION XSCE(MAXSCE),YSCE(MAXSCE)
!
!     SOURCES WATER DISCHARGES TAKEN FROM THE STEERING FILE
! 
      DOUBLE PRECISION DSCE(MAXSCE)
!
!     SOURCES DISCHARGES WITH VARIATION IN TIME
!
      DOUBLE PRECISION DSCE2(MAXSCE)
!
!     SOURCES TRACER DISCHARGES TAKEN FROM THE STERING FILE
! 
      DOUBLE PRECISION TSCE(MAXSCE,MAXTRA)
!
!     SOURCES OF TRACERS WITH VARIATIONS IN TIME
!
      DOUBLE PRECISION TSCE2(MAXSCE,MAXTRA)
!
!     VELOCITIES OF THE SOURCES ALONG X,Y
! 
      DOUBLE PRECISION USCE(MAXSCE),VSCE(MAXSCE)
!
!     UPWIND COEFFICIENTS FOR SUPG (1:u and v, 2:h, 3:tracers, 4:k and epsilon)
!
      DOUBLE PRECISION COSUPG(4)
!
!     STOP CRITERIA (ORDER: H,U AND V,T)
! 
      DOUBLE PRECISION CRIPER(3)
!
!     REFERENCE TEMPERATURE FOR COMPUTING DENSITY
!
      DOUBLE PRECISION TMOY
!
!     DIRECTION OF NORTH, COUNTER-CLOCK-WISE, STARTING FROM VERTICAL AXIS
! 
      DOUBLE PRECISION NORD
!
!     NON-DIMENSIONAL DISPERSION COEFFICIENTS
! 
      DOUBLE PRECISION ELDER(2)
!
!     LONGITUDE OF ORIGIN POINT
! 
      DOUBLE PRECISION PHI0
!
!     MASS-LUMPING ON TRACERS
! 
      DOUBLE PRECISION AGGLOT
!
!     DURATION OF COMPUTATION
! 
      DOUBLE PRECISION DUREE
!
!     DESIRED COURANT NUMBER
! 
      DOUBLE PRECISION CFLWTD
!
!     MINIMUM DEPTH TO TAKE WIND INTO ACCOUNT
! 
      DOUBLE PRECISION HWIND
!
!     THRESHOLD FOR NEGATIVE DEPTHS
! 
      DOUBLE PRECISION HNEG
!
!     FREE SURFACE GRADIENT COMPATIBILITY
! 
      DOUBLE PRECISION TETAZCOMP
!
!     TIME RANGE FOR FOURIER ANALYSIS
! 
      DOUBLE PRECISION TAFBGN,TAFEND
!
!     CURRENT TIME
! 
      DOUBLE PRECISION AT
!
!     FOURIER ANALYSIS PERIODS
! 
      DOUBLE PRECISION PERIAF(50)
!
!     ARRAY OF REALS TO READ INTO SELAFIN FILES
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
!     RAIN OR EVAPORATION IN MM PER DAY
!
      DOUBLE PRECISION RAIN_MMPD
!
!     PHYSICAL CHARACTERISTICS OF THE TSUNAMI
!
      DOUBLE PRECISION COETSUNAMI(10)
!
!     VALUES OF TRACERS IN THE RAIN'
!
      DOUBLE PRECISION TRAIN(MAXTRA)
!
!     COEFFICIENT TO CALIBRATE TIDAL RANGE
!
      DOUBLE PRECISION CTIDE
!
!     COEFFICIENT TO CALIBRATE TIDAL VELOCITIES
!
      DOUBLE PRECISION CTIDEV
!
!     COEFFICIENT TO CALIBRATE SEA LEVEL
!
      DOUBLE PRECISION MSL
!
!     COEFFICIENT 1 FOR LAW OF TRACERS DEGRADATION
!     (1 IN CASE OF FUTURE LAW WITH MORE COEF.)
!
      INTEGER COEF1TRAC(MAXTRA)
!
!-----------------------------------------------------------------------
!
!       7) STRINGS
!
!-----------------------------------------------------------------------
!
!     TITLE OF STUDY
! 
      CHARACTER*72 TITCAS
!
!     VARIABLES FOR GRAPHIC PRINTOUT
!
      CHARACTER*72 VARDES
!
!     INITIAL CONDITIONS
! 
      CHARACTER*72 CDTINI
!
!     VARIABLES TO BE PRINTED
! 
      CHARACTER*72 VARIMP
!
!     EQUATIONS
! 
      CHARACTER*20 EQUA
!
!     NAMES OF CLANDESTINE VARIABLES
! 
      CHARACTER*32 VARCLA(10)
!
!     NAMES AND UNITS OF VARIABLES
!
      CHARACTER*32 TEXTE(MAXVAR),TEXTPR(MAXVAR)
!
!     NAMES OF REMARKABLE POINTS
! 
      CHARACTER*32 NAME_PTS(100)
!
!     NAMES OF TRACERS
! 
      CHARACTER(LEN=32) NAMETRAC(MAXVAR)
!
!-----------------------------------------------------------------------
!
!       8) SLVCFG STRUCTURES
!
!-----------------------------------------------------------------------
!
!     STRUCTURE WITH SOLVER FOR PROPAGATION
!
      TYPE(SLVCFG) :: SLVPRO
!
!     STRUCTURE WITH SOLVER OPTIONS FOR K
!
      TYPE(SLVCFG) :: SLVK
!
!     STRUCTURE WITH SOLVER OPTIONS FOR E
!
      TYPE(SLVCFG) :: SLVEP
!
!     SOLVER FOR DIFFUSION OF TRACERS
! 
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
!
!     MAXIMUM NUMBER OF ELEMENTS IN THE MESH
! 
      INTEGER, POINTER:: NELMAX
!
!     NUMBER OF BOUNDARY POINTS IN THE MESH
! 
      INTEGER, POINTER:: NPTFR
!
!     MAXIMUM NUMBER OF BOUNDARY POINTS IN THE MESH
!
      INTEGER, POINTER:: NPTFRX
!
!     DIMENSION OF SPACE
!
      INTEGER, POINTER:: DIM
!
!     TYPE OF ELEMENT
!
      INTEGER, POINTER:: TYPELM
!
!     NUMBER OF POINTS IN THE MESH
! 
      INTEGER, POINTER:: NPOIN
!
!     MAXIMUM NUMBER OF POINTS IN THE MESH
!
      INTEGER, POINTER:: NPMAX
!
!     MAXIMUM NUMBER OF POINTS NEIGHBOURS OF A POINT
!
      INTEGER, POINTER:: MXPTVS
!
!     MAXIMUM NUMBER OF ELEMENTS NEIGHBOURS OF A POINT
!
      INTEGER, POINTER:: MXELVS
!
!     VECTOR LENGTH OF THE MACHINE
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
!     ADJOINT VARIABLES
!
      TYPE(BIEF_OBJ), TARGET :: PP,QQ,RR
!
!     MEASUREMENTS
!
      TYPE(BIEF_OBJ), TARGET :: UD,VD,HD
!
!     DIRECT VARIABLES AT ITERATION IT
!
      TYPE(BIEF_OBJ), TARGET :: UU,VV,HH
!
!     DIRECT VARIABLES AT ITERATION IT+1
!
      TYPE(BIEF_OBJ), TARGET :: UIT1,VIT1,HIT1
!
!     BOUNDARY VALUES FOR ADJOINT VARIABLE (ONLY DIRICHLET)
!
      TYPE(BIEF_OBJ), TARGET :: PBOR,QBOR,RBOR
!
!     VECTOR USED TO CHANGE THE SET OF STRICKLERS
!
      TYPE (BIEF_OBJ) :: DESC
!
!     GRADIENT OF COST FUNCTION (ITERATION K)
!
      TYPE (BIEF_OBJ) :: GRADJ
!
!     GRADIENT OF COST FUNCTION (ITERATION K-1)
!
      TYPE (BIEF_OBJ) :: GRADJN
!
!     SET OF STRICKLERS (ZONES): NEW
!
      TYPE (BIEF_OBJ) :: SETSTR
!
!     SET OF STRICKLERS' (ZONES): OLD
!
      TYPE (BIEF_OBJ) :: SETSTR2
!
!     GRADIENTS
!
      TYPE (BIEF_OBJ) :: ALPHA1,ALPHA2,ALPHA3
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
!       SYSTEM SOLVED WILL BE :
!
!     ( TAM1  TCM1  TCM2 )   (PP )      ( CV1 )
!     (                  )   (   )      (     )
!     ( TBM1  TAM2  A23  )   (QQ )  =   ( CV2 )
!     (                  )   (   )      (     )
!     ( TBM2  A32   TAM3 )   (RR )      ( CV3 )
!
!
!     MATRICES FOR ADJOINT SYSTEM
!
      TYPE(BIEF_OBJ), TARGET :: TAM1,TAM2,TAM3,TBM1,TBM2,TCM1,TCM2
!
!-----------------------------------------------------------------------
!
!       3) BLOCKS
!
!-----------------------------------------------------------------------
!
!     BLOCKS OF MATRICES, RIGHT HAND SIDES AND UNKNOWNS
!     FOR LINEAR SYSTEMS
!
!     BLOCKS OF MATRICES, RIGHT HAND SIDES FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: MATADJ
!
!     BLOCKS OF UNKNOWNS FOR LINEAR SYSTEMS
!
      TYPE(BIEF_OBJ), TARGET :: UNKADJ
!
!     BLOCK OF ADJOINT VARIABLES FOR OUTPUT
!
      TYPE(BIEF_OBJ), TARGET :: VARSORA
!
!     BLOCK OF DIRICHLET CONDITION FOR ADJOINT VARIABLES
!
      TYPE(BIEF_OBJ), TARGET :: ADJDIR
!
!-----------------------------------------------------------------------
!
!     4) INTEGERS
!
!     NUMBER OF VARIABLES FOR LITENR (READ BY SKIPGEO)
!
      INTEGER NVARRES
!
!     NUMBER OF ITERATIONS FOR PARAMETERS CALIBRATION
!
      INTEGER NITERA
!
!-----------------------------------------------------------------------
!
!     5) LOGICALS
!
!     IF YES: ACTIVATES THE ADJOINT MODE IN TELEMAC2D/PROPAG
!
      LOGICAL ADJO
!
!     WHETHER THE ADJOINT SYSTEM RESULTS ARE PRINTED; NB 100=MAXVAR
!
      LOGICAL SORLEOA(100)
!
!     WHETHER ...; NB 100=MAXVAR
!
      LOGICAL SORIMPA(100)
!
!-----------------------------------------------------------------------
!
!       6) STRINGS
!
!-----------------------------------------------------------------------
!
!     NAMES OF VARIABLES IN RES FILE
!
      CHARACTER(LEN=32) TEXRES(100)
!
!     NAMES OF VARIABLES IN RBI FILE
!
      CHARACTER(LEN=32) TEXRBI(100)
!
!     NAMES OF VARIABLES IN REF FILE
!
      CHARACTER(LEN=32) TEXREF(100)
!
!     KEYWORD "PARAMETER ESTIMATION"
! 
      CHARACTER(LEN=72) ESTIME
!
!=======================================================================
!
!     COPY OF SUBMIT STRINGS IN THE DICTIONARY
!
      CHARACTER(LEN=144) SUBMIT(4,300)
!
!-----------------------------------------------------------------------
!
!       7) FRICTION DEFINITION BY ZONE
!
!-----------------------------------------------------------------------
!
!     ???????
!
      TYPE(FRICTION_OBJ), TARGET :: FRTAB
!
!     ???????
!
      TYPE(BIEF_OBJ), TARGET :: KFROPT, NKFROT
!
!     ???????
!
      TYPE(BIEF_OBJ), TARGET :: NDEFMA, LINDDP, LINDSP
!
!     ???????
!
      TYPE(BIEF_OBJ), TARGET :: NDEF_B, KFRO_B
!
!     NUMBER OF ZONES
!
      INTEGER:: NZONES
!
!     MAXIMUM NUMBER OF FRICTION DOMAINS
! 
      INTEGER:: NZONMX
!
!     IF YES, NON-SUBMERGED VEGETATION FRICTION
! 
      LOGICAL:: LINDNER
!
!     IF YES, FRICTION DATA
! 
      LOGICAL:: FRICTB
!
!     DEFAULT MANNING VALUE (FOR COLEBROOK-WHITE LAW)
! 
      DOUBLE PRECISION :: NDEF
!
!     DIAMETER OF ROUGHNESS ELEMENTS
! 
      DOUBLE PRECISION :: DP
!
!     SPACING OF ROUGHNESS ELEMENTS
! 
      DOUBLE PRECISION :: SP
!
!-----------------------------------------------------------------------
!
!      8) TELEMAC-2D FILES
!
!-----------------------------------------------------------------------
!
!     TELEMAC-2D FILES
!
      TYPE(BIEF_FILE) :: T2D_FILES(MAXLU_T2D)
!
!-----------------------------------------------------------------------
!
!      9) SECTIONS
!
!-----------------------------------------------------------------------
!
!     ???????
!
      TYPE (CHAIN_TYPE), ALLOCATABLE :: CHAIN(:)
!
      SAVE
!
      END MODULE DECLARATIONS_TELEMAC2D

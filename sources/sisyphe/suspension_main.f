!                       **************************
                        SUBROUTINE SUSPENSION_MAIN
!                       **************************
!
     &(SLVTRA,HN,HN_TEL,MU,TOB,FDM,FD90,KSP,KSR,KS,VOLU2D,V2DPAR,UNSV2D,
     & AFBOR,BFBOR,ZF,LICBOR,IFAMAS,MASKEL,MASKPT,U2D,V2D,NSICLA,NPOIN,
     & NPTFR,IELMT,OPTDIF,RESOL,LT,NIT,OPTBAN,OPTADV,OPDTRA,
     & KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,DEBUG,
     & DTS,CSF_SABLE,ZERO,GRAV,XKX,XKY,KARMAN,
     & XMVE,XMVS,VCE,HMIN,XWC,VITCD,VITCE,PARTHENIADES,BILMA,MSK,
     & CHARR,IMP_INFLOW_C,MESH,ZF_S,CS,CST,CTILD,CBOR,DISP,
     & IT1,IT2,IT3,IT4,TB,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T14,W1,
     & TE1,CLT,TE2,TE3,S,AM1_S,AM2_S,MBOR,ELAY,LIMDIF,
     & MASKTR, TETA_SUSP, AC, MASED0, MASINI, MASTEN,
     & MASTOU, ES,ES_SABLE, ES_VASE,AVAIL,  ENTETS, PASS, ZFCL_S,
     & HPROP, FLUDPT, FLUDP, FLUER, DISP_C, KX, KY,
     & KZ, UCONV, VCONV,QSXS, QSYS, QSCLXS, QSCLYS, QSCL_S,
     & QS_S,QS_C,CSTAEQ,CSRATIO,ICQ,MASTCP,MASFIN,MASDEPT,MASDEP,MASSOU,
     & CORR_CONV,ZREF,SEDCO,VISC_TEL,CODE,
     & DIFT,DM1,UCONV_TEL,VCONV_TEL,ZCONV,SOLSYS,FLBOR_TEL,FLBOR_SIS,
     & FLBORTRA,NUMLIQ,NFRLIQ,MIXTE,NOMBLAY,CONC,
     & TOCE_VASE,TOCE_SABLE,FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE,TASS,
     & DIRFLU,MAXADV)
!
!***********************************************************************
! SISYPHE   V6P2                                   18/06/2012
!***********************************************************************
!
!brief    MAIN SUBROUTINE FOR THE SUSPENDED-LOAD TRANSPORT.
!
!history  F. HUVELIN
!+        22/12/2004
!+
!+
!history  JMH
!+        25/06/2008
!+        V5P9
!+   CALL TO DIFFIN MOVED IN SUSPENSION_COMPUTATION
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
!+
!history  C. VILLARET
!+        20/03/2011
!+        V6P1
!+        BUG CORRECTION : SEND FDM(I) instead of ACLADM
!+        suppression of NSOUS, ISOUS
!+        adding VCE, TOCE_SABLE
!+
!history  J.-M. HERVOUET
!+        19/04/2011
!+        V6P1
!+        COMPUTATION OF INITIAL MASS CHANGED
!+
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables
!
!history  MAK (HRW)
!+        31/05/2012
!+        V6P2
!+  Include CSRATIO
!
!history  PAT (LNHE)
!+        18/06/2012
!+        V6P2
!+   updated version with HRW's development for Soulsby-van Rijn's concentration
!+       V6P2
!
!history  C. VILLARET
!+        21/08/2012
!+        V6P2
!+    Added call variable to suspension_computation
!
!history  C. VILLARET
!+        28/08/2012
!+        V6P2
!+    Added ES_SABLE and ES_VASE
!+    Replaced CONC_VASE by CONC
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/04/2014
!+        V7P0
!+   OPTSUP replaced by OPTADV in the call to suspension_computation.
!+   (see keyword SCHME OPTION FOR ADVECTION)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/03/2017
!+        V7P3
!+   HPROP is not equal to HN in coupling, but to HN_TEL, in Sisyphe
!+   what is called HN is in fact H in Telemac-2D or 3D.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AFBOR          |-->| BOUNDARY CONDITION ON F: NU*DF/DN=AFBOR*F+BFBOR
!| AM1_S          |<->| MATRIX OBJECT
!| AM2_S          |<->| MATRIX OBJECT
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS AND PER LAYER
!| BFBOR          |-->| BOUNDARY CONDITION ON F: NU*DF/DN=AFBOR*F+BFBOR
!| BILMA          |-->| MASS BALANCE
!| CBOR           |<->| IMPOSED SUSPENDED SAND CONCENTRATION AT THE BOUNDARY
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| CHARR          |-->| LOGICAL, BEDLOAD OR NOT
!| CLT            |<->| BOUNDARY CONDITIONS FOR TRACER (MODIFIED LITBOR)
!| CODE           |-->| HYDRODYNAMIC CODE IN CASE OF COUPLING
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER (KG/M3)
!| CORR_CONV      |-->| LOGICAL, CORRECTION ON CONVECTION VELOCITY OR NOT
!| CS             |<->| CONCENTRATION AT TIME N
!| CSF_SABLE      |-->| VOLUME CONCENTRATION OF SAND (1-POROSITY)
!| CST            |<->| CONCENTRATION AT TIME T(N+1)
!| CSTAEQ         |<->| EQUILIBRIUM CONCENTRATION
!| CTILD          |<->| CONCENTRATION AFTER ADVECTION
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| DIFT           |-->| DIFFUSION OF SUSPENDED SEDIMENT CONCENTRATION
!| DISP           |-->| VISCOSITY COEFFICIENTS ALONG X,Y AND Z .
!|                |   | IF P0 : PER ELEMENT
!|                |   | IF P1 : PERR POINT
!| DISP_S         |<->| WORK ARRAY FOR SAVING DISPC
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV)
!| DTS            |-->| TIME STEP FOR SUSPENSION
!| ELAY           |<->| THICKNESS OF TOP ACTIVE LAYER (SANG GRADING ALGORITHM)
!|                |<->| THICKNESS OF THE WHOLE COHESIVE SEDIMENT BED (CONSOLIDATION)
!| ENTET          |<->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION
!| ENTETS         |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION FOR SUSPENSION
!| ES             |<->| THICKNESS OF EACH LAYER (M)
!| ES_VASE        |<->| THICKNESS OF THE MUD LAYER (M)
!| ES_SABLE       |<->| THICKNESS OF THE SAND LAYER (M)
!| FDM            |-->| GRAIN SIZE PER SEDIMENT CLASS
!| FLBORTRA       |<->| FLUXES AT BOUNDARIES TRACER
!| FLBOR_SIS      |<->| FLUXES AT BOUNDARIES SISYPHE
!| FLBOR_TEL      |-->| FLUXES AT BOUNDARIES TELEMAC
!| FLUDP          |<->| DEPOSITION FLUX (M/S)
!| FLUDPT         |<->| DEPOSITION FLUX (IMPLICIT)
!| FLUER          |<->| EROSION FLUX (M/S)
!| FLUER_VASE     |<->| EROSION FLUX (M/S)FOR MIXED SEDIMENTS
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH (M)
!| HN             |-->| WATER DEPTH (M)
!| HN_TEL         |-->| WATER DEPTH SENT BY TELEMAC OR CALLING CODE
!| HPROP          |<->| PROPAGATION DEPTH (DONE IN CVDFTR)
!| ICQ            |-->| FLAG FOR REFERENCE CONCENTRATION FORMULA
!| IELMT          |-->| NUMBER OF ELEMENTS
!| IFAMAS         |-->| A MODIFIED IFABOR WHEN ELEMENTS ARE MASKED
!| IMP_INFLOW_C   |-->| LOGICAL, IMPOSED EQUILIBRIUM CONCENTRATION AT THE INFLOW OR NOT
!| IT1            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT2            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT3            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| IT4            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KS             |-->| TOTAL BED ROUGHNESS
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| KSP            |-->| SKIN BED ROUGHNESS
!| KSR            |-->| RIPPLE BED ROUGHNESS
!| KX             |<->| COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!| KY             |<->| COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!| KZ             |<->| COEFFICIENTS OF THE DISPERSION TENSOR (DIM. NPOIN)
!| LICBOR         |-->| BOUNDARY CONDITIONS FOR SEDIMENT
!| LIMDIF         |<->| BOUNDARY CONDITIONS FOR DIFFUSION
!| LT             |-->| ITERATION
!| MASDEP         |<--| TOTAL DEPOSITED MASS
!| MASDEPT        |<--| DEPOSITED MASS DURING THE TIME STEP
!| MASED0         |<->| SUSPENDED MASS BALANCE
!| MASFIN         |<--| MASS AT THE END
!| MASINI         |<->| INITIAL MASS
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASKPT         |-->| MASKING PER POINT
!| MASKTR         |<->| MASKING FOR TRACERS, PER POINT
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!|                |   | SEE DIFSOU
!| MASTCP         |<--| ??? NE SERT A RIEN, A SUPPRIMER
!| MASTEN         |<->| MASS ENTERED THROUGH LIQUID BOUNDARY
!| MASTOU         |<->| MASS CREATED BY SOURCE TERM
!| MAXADV         |-->| MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES
!| MBOR           |<->| MATRIX OBJECT
!| MESH           |<->| MESH STRUCTURE
!| MIXTE          |-->| LOGICAL, MIXTE SEDIMENT OR NOT
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYERv (KG/M2)
!| MU             |-->| CORRECTION FACTOR FOR BED ROUGHNESS
!| NOMBLAY        |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NIT            |-->| TOTAL NUMBER OF ITERATIONS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| OPDTRA         |-->| OPTION FOR THE DIFFUSION OF TRACERS
!| OPTBAN         |-->| OPTION FOR THE TREATMENT OF TIDAL FLATS
!| OPTDIF         |-->| OPTION FOR THE DISPERSION
!| OPTADV         |-->| SCHEME OPTION FOR ADVECTION
!| PARTHENIADES   |-->| CONSTANT OF THE KRONE AND PARTHENIADES EROSION LAW (M/S)
!| PASS           |<->| IN FACT PASS_SUSP IN SISYPHE.F, ARRIVES AS .TRUE.
!|                |   | AT FIRST CALL AND IS CHANGED INTO .FALSE. BELOW
!| QSCLXS         |<->| SUSPENDED LOAD TRANSPORT RATE FOR EACH CLASS X-DIRECTION
!| QSCLYS         |<->| SUSPENDED LOAD TRANSPORT RATE FOR EACH CLASS Y-DIRECTION
!| QSCL_S         |<->| SUSPENDED LOAD TRANSPORT RATE
!| QSXS           |<->| SOLID DISCHARGE X (SUSPENSION)
!| QSYS           |<->| SOLID DISCHARGE Y (SUSPENSION)
!| QS_C           |-->| BEDLOAD TRANSPORT RATE
!| QS_S           |<->| SUSPENDED LOAD TRANSPORT RATE
!| RESOL          |-->| CHOICE OF ADVECTION SCHEME
!| S              |<->| VOID STRUCTURE
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| SLVTRA         |<->| SLVCFG STRUCTURE
!| SOLSYS         |-->| SLVCFG STRUCTURE
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| T11            |<->| WORK BIEF_OBJ STRUCTURE
!| T12            |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| T9             |<->| WORK BIEF_OBJ STRUCTURE
!| TASS           |-->| LOGICAL, CONSOLIDATION TAKEN INTO ACCOUNT OR NOT
!| TB             |-->| BLOCK OF WORKING ARRAYS
!| TE1            |<->| WORKING ARRAY FOR ELEMENTS
!| TE2            |<->| WORKING ARRAY FOR ELEMENTS
!| TE3            |<->| WORKING ARRAY FOR ELEMENTS
!| TETA_SUSP      |<->| IMPLICITATION FACTOR FOR THE DEPOSITION FLUX AND DIFFUSION
!| TOB            |-->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOCE_SABLE     |<->| CRITICAL SHEAR STRESS FOR SAND (N/M2)
!| TOCE_MIXTE     |<->| CRITICAL SHEAR STRESS FOR MIXED SEDIMENTS (N/M2)
!| TOCE_VASE      |<->| CRITICAL EROSION SHEAR STRESS OF THE MUD PER LAYER (N/M2)
!| U2D            |-->| MEAN FLOW VELOCITY X-DIRECTION
!| UCONV          |<->| X-COMPONENT ADVECTION FIELD (SISYPHE)
!| UCONV_TEL      |-->| X-COMPONENT ADVECTION FIELD (TELEMAC)
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| V2D            |-->| MEAN FLOW VELOCITY Y-DIRECTION
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| VCE            |-->| FLOW VISCOSITY
!| VCONV          |<->| Y-COMPONENT ADVECTION FIELD (SISYPHE)
!| VCONV_TEL      |-->| Y-COMPONENT ADVECTION FIELD (TELEMAC)
!| VISC_TEL       |-->| VELOCITY DIFFUSIVITY (TELEMAC)
!| VITCD          |-->| CRITICAL SHEAR VELOCITY FOR MUD DEPOSITION
!| VITCE          |-->| CRITICAL EROSION SHEAR VELOCITY OF THE MUD (A SUPPRIMER)
!| VOLU2D         |-->| INTEGRAL OF BASES
!| W1             |<->| WORKING ARRAY
!| XKX            |-->| COEFFICIENT USED FOR COMPUTING THE DISPERSION
!|                |   | DEPENDS OF OPTIONS
!| XKY            |-->| COEFFICIENT USED FOR COMPUTING THE DISPERSION
!|                |   | DEPENDS OF OPTIONS
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!| XWC            |-->| SETTLING VELOCITIES PER CLASS OF SEDIMENT
!| ZCONV          |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV)
!| ZERO           |-->| ZERO
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFCL_S         |<->| BED EVOLUTION PER CLASS, DUE TO SUSPENDED SEDIMENT
!| ZF_S           |<->| ACCUMULATED BED EVOLUTION DUE TO SUSPENDED SEDIMENT
!| ZREF           |-->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SUSPENSION_MAIN => SUSPENSION_MAIN
      USE BIEF
      USE DECLARATIONS_SPECIAL
!##> JR @ RWTH: ALLOW COMPILERS TO CHECK PARALLEL INTERFACE
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
!##< JR @ RWTH
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (SLVCFG),    INTENT(INOUT) :: SLVTRA
      TYPE (BIEF_OBJ),  INTENT(IN)    :: HN,HN_TEL,MU,TOB
      TYPE (BIEF_OBJ),  INTENT(IN)    :: KSP,KSR,KS
      TYPE (BIEF_OBJ),  INTENT(IN)    :: VOLU2D,AFBOR,BFBOR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)    :: V2DPAR,UNSV2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: LICBOR, IFAMAS, MASKEL, MASKPT
      TYPE (BIEF_OBJ),  INTENT(IN)    :: U2D, V2D,DM1,ZCONV,FLBOR_TEL
      INTEGER,          INTENT(IN)    :: NSICLA, NPOIN, NPTFR, IELMT
      INTEGER,          INTENT(IN)    :: OPTDIF, RESOL,LT, NIT
      INTEGER,          INTENT(IN)    :: OPTBAN,OPTADV,OPDTRA,NFRLIQ
      INTEGER,          INTENT(IN)    :: KENT, KSORT, KLOG, KINC, KNEU
      INTEGER,          INTENT(IN)    :: KDIR,KDDL
      INTEGER,          INTENT(IN)    :: DEBUG,SOLSYS,NOMBLAY,MAXADV
      INTEGER,          INTENT(IN)    :: NUMLIQ(NFRLIQ)
      DOUBLE PRECISION, INTENT(IN)    :: DTS,CSF_SABLE
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,GRAV
      DOUBLE PRECISION, INTENT(IN)    :: FDM(NSICLA),FD90(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: XKX,XKY,KARMAN,VCE
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, HMIN, XWC(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: VITCD, VITCE
      DOUBLE PRECISION, INTENT(IN)    :: PARTHENIADES
      LOGICAL,          INTENT(IN)    :: BILMA, MSK, CHARR
      LOGICAL,          INTENT(IN)    :: IMP_INFLOW_C
      LOGICAL,          INTENT(IN)    :: SEDCO(NSICLA),MIXTE,TASS
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZF_S,CS,CST,CTILD,CBOR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP,IT1,IT2,IT3,IT4,TB
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T9,T10,T11,T12,T14,W1,TE1,CLT
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: TE2,TE3,S,AM1_S,AM2_S,MBOR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ELAY, LIMDIF,FLBORTRA
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MASKTR
      DOUBLE PRECISION, INTENT(INOUT) :: TETA_SUSP, AC(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASED0(NSICLA), MASINI(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN(NSICLA), MASTOU(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: TOCE_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: TOCE_SABLE
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      LOGICAL,          INTENT(INOUT) :: ENTETS, PASS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZFCL_S,HPROP,ZREF
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT,FLUDP,FLUER
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP_C,KX,KY,KZ,UCONV
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: VCONV,FLBOR_SIS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: QSXS,QSYS,QSCLXS,QSCLYS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: QSCL_S,QS_S,CSTAEQ,CSRATIO
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_VASE,TOCE_MIXTE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MS_SABLE,MS_VASE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: QS_C,VISC_TEL
      TYPE (BIEF_OBJ),  INTENT(IN)    :: UCONV_TEL,VCONV_TEL
      DOUBLE PRECISION, INTENT(OUT)   :: MASTCP(NSICLA),MASFIN(NSICLA)
      DOUBLE PRECISION, INTENT(OUT)   :: MASDEPT(NSICLA),MASDEP(NSICLA)
      DOUBLE PRECISION, INTENT(OUT)   :: MASSOU
      INTEGER, INTENT(IN)             :: ICQ,DIRFLU
      LOGICAL, INTENT (IN)            :: CORR_CONV,DIFT
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I,J
!
!##> JR @ RWTH: INTERFACE CHECKED SO NO NEED FOR EXTERNALS
!      DOUBLE PRECISION, EXTERNAL :: P_DSUM
!##< JR @ RWTH
!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      IF(PASS) THEN
!
        ! *************************  !
        ! III - INITIAL MASS-BALANCE !
        ! *************************  !
!
        IF(BILMA) THEN
          DO I = 1,NSICLA
!           JMH 18/04/2011: MUST BE DONE LIKE IN SUSPENSION_BILAN
!                           I.E. WITH MASS-LUMPING AGGLOT=1. WHICH IS
!                           SET LATER IN SUSPENSION_COMPUTATION...
            CALL OS('X=YZ    ',X=T1,Y=VOLU2D,Z=CS%ADR(I)%P)
!           CALL VECTOR(T1, '=', 'MASVEC          ', IELMT, 1.D0,
!    &                  CS%ADR(I)%P, T1, T1, T1, T1, T1, MESH, MSK,
!    &                  MASKEL)
!
!           JMH 19/04/2011
            IF(CODE(1:7).EQ.'TELEMAC') THEN
!             WITH COUPLING, HN-TEL IS THE OLD DEPTH
!                            HN     IS THE NEW DEPTH
              MASED0(I) = DOTS(T1,HN_TEL)
            ELSE
!             SISYPHE WITHOUT COUPLING, MASS CONSERVATION
!             DIFFICULT TO CHECK...
              MASED0(I) = DOTS(T1,HN)
            ENDIF
            IF(NCSIZE.GT.1) MASED0(I)=P_DSUM(MASED0(I))
            MASINI(I) = MASED0(I)
            MASTEN(I) = 0.D0
            MASTOU(I) = 0.D0
            MASTCP(I) = 0.D0
            IF(LNG.EQ.1) WRITE(LU,1) I, MASED0(I)
            IF(LNG.EQ.2) WRITE(LU,2) I, MASED0(I)
          ENDDO
        ENDIF
!
        !----------------------------------------------------------------!
001     FORMAT(1X,'QUANTITE INITIALE EN SUSPENSION POUR LA CLASSE ',
     &         I2,' : ', G16.7, ' M3')
        !----------------------------------------------------------------!
002     FORMAT(1X,'INITIAL QUANTITY IN SUSPENSION FOR CLASS ',
     &         I2,' : ', G16.7, ' M3')
        !----------------------------------------------------------------!
!       END OF IF(PASS)
      ENDIF
!
      PASS = .FALSE.
!
      ! ********************************* !
      ! V - COMPUTES THE DISPERSION       !
      ! ********************************* !
      IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_DISPERSION'
      CALL SUSPENSION_DISPERSION
     &     (TOB,XMVE,HN,OPTDIF,NPOIN,XKX,XKY,T1,T2,T3,KX,KY,KZ,DISP,
     &      U2D,V2D,VISC_TEL,CODE)
      IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_DISPERSION'

      ! ************************************************ !
      ! VI  - COMPUTES THE CONCENTRATION AND EVOLUTION   !
      ! ************************************************ !

      IF(CODE(1:7).EQ.'TELEMAC') THEN
        CALL OS('X=Y     ', X=HPROP, Y=HN_TEL)
      ELSE
        CALL OS('X=Y     ', X=HPROP, Y=HN)
      ENDIF
      DO I = 1, NSICLA
        CALL OS('X=0     ', X=ZFCL_S%ADR(I)%P)
        IF(DEBUG > 0) WRITE(LU,*)
     &               'SUSPENSION_COMPUTATION : ',I,'/',NSICLA
        CALL SUSPENSION_COMPUTATION(SLVTRA,HN,HN_TEL,UCONV,
     &VCONV,MU,TOB,FDM(I),FD90(I),KSP,KSR,KS,ELAY,AVAIL(1:NPOIN,1,I),
     &AFBOR,BFBOR,LIMDIF,CLT,MASKEL,MASKTR,MASKPT,IFAMAS,NPOIN,IELMT,
     &NPTFR,I,LT,NIT,RESOL,OPTBAN,KENT,KDDL,KDIR,KSORT,KLOG,KINC,KNEU,
     &OPTADV,OPDTRA,DEBUG, CSF_SABLE, TETA_SUSP,DTS,
     &MASED0(I),ZERO,XWC(I),KARMAN,XMVE,XMVS,VCE,GRAV,HMIN,VITCD,
     &VITCE,PARTHENIADES,ENTETS,BILMA,
     &MSK,CHARR,IMP_INFLOW_C,MESH,ZF,CS%ADR(I)%P,
     &CST%ADR(I)%P,CTILD%ADR(I)%P,CBOR%ADR(I)%P,DISP,IT1,IT2,
     &IT3,IT4,TB,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T14,
     &W1,TE1,TE2,TE3,S,AM1_S,AM2_S,MBOR,MASTEN(I),MASTOU(I),
     &MASINI(I),AC(I),ZFCL_S%ADR(I)%P,FLUDPT%ADR(I)%P,FLUDP%ADR(I)%P,
     &FLUER%ADR(I)%P, HPROP,DISP_C,CSTAEQ, CSRATIO,
     &MASFIN(I),MASDEPT(I),MASDEP(I),MASSOU,QS_C,ICQ,ZREF,
     &CORR_CONV,U2D,V2D,SEDCO(I),DIFT,DM1,ZCONV,UCONV_TEL,
     &VCONV_TEL,SOLSYS,FLBOR_TEL,FLBOR_SIS,FLBORTRA,CODE,VOLU2D,
     &V2DPAR,UNSV2D,NUMLIQ,NFRLIQ,LICBOR,MIXTE,AVAIL,NSICLA,ES,
     &ES_SABLE,ES_VASE,NOMBLAY,CONC,TOCE_VASE,TOCE_SABLE,
     &FLUER_VASE,TOCE_MIXTE,MS_SABLE%R,MS_VASE%R,TASS,DIRFLU,
     &QSCLXS%ADR(I)%P,QSCLYS%ADR(I)%P,MAXADV)
        IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_COMPUTATION'
!
      ENDDO
!
! FOR MIXTE OR COHESIVE SEDIMENTS ELAY UPDATED
!
! REACTUALISATION DU ELAY ET DES AVAI fait dans suspension_MAIN
      IF(SEDCO(1).OR.MIXTE) THEN
        DO I = 1, NPOIN
          ELAY%R(I)= 0.D0
          DO J= 1, NOMBLAY
            ES(I,J) = ES_VASE(I,J)
            IF(MIXTE) THEN
              ES(I,J)= ES_VASE (I,J) + ES_SABLE(I,J)
              IF(ES(I,J).GT.1.D-04) THEN
                AVAIL(I,J,1)= ES_SABLE(I,J)/ES(I,J)
                AVAIL (I,J,2)= ES_VASE(I,J)/ES(I,J)
!CVL          ELSE
!CVL            AVAIL(I,J,1)=0.5 D0
!CVL            AVAIL(I,J,2)=0.5 D0
              ENDIF
            ENDIF
            ELAY%R(I)=ELAY%R(I)+ES(I,J)
          ENDDO
        ENDDO
      ENDIF
!
      ! *********************************************************** !
      ! VII  - UPDATES EVOLUTION, CONCENTRATION AND TRANSPORT RATE  !
      ! *********************************************************** !
!
      IF (DEBUG > 0) WRITE(LU,*) 'UPDATING_DATA'
!
!     COULD BE OPTIMISED: FIRST CLASS ON ZF_S, THEN ADDING OTHERS...
!
      CALL OS('X=0     ', X=QSXS)
      CALL OS('X=0     ', X=QSYS)
      CALL OS('X=0     ', X=ZF_S)
!
      DO I = 1, NSICLA
        CALL OS('X=X+Y   ', X=ZF_S, Y=ZFCL_S%ADR(I)%P)
        CALL OS('X=X+Y   ', X=QSXS, Y=QSCLXS%ADR(I)%P)
        CALL OS('X=X+Y   ', X=QSYS, Y=QSCLYS%ADR(I)%P)
      ENDDO
      CALL OS('X=N(Y,Z)', X=QSCL_S, Y=QSCLXS, Z=QSCLYS)
      CALL OS('X=N(Y,Z)', X=QS_S, Y=QSXS, Z=QSYS)
      IF (DEBUG > 0) WRITE(LU,*) 'END_UPDATING_DATA'
!
!======================================================================!
!======================================================================!
!
      RETURN
      END

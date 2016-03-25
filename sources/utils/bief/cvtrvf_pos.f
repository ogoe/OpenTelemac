!                    *********************
                     SUBROUTINE CVTRVF_POS
!                    *********************
!
     &(F,FN,FSCEXP,DIFT,CONV,H,HN,HPROP,UDEL,VDEL,DM1,ZCONV,SOLSYS,
     & VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,FBOR,MASKTR,MESH,
     & T1,T2,T3,T4,T5,T6,T7,T8,HNT,HT,AGGLOH,TE1,DT,ENTET,BILAN,
     & OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,LIMTRA,KDIR,KDDL,NPTFR,FLBOR,
     & YAFLBOR,V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT,GLOSEG1,GLOSEG2,NBOR,
     & OPTION,FLULIM,YAFLULIM,RAIN,PLUIE,TRAIN,GIVEN_FLUX,FLUX_GIVEN,
     & NITMAX,NCO_DIST)
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    FINITE VOLUMES, UPWIND, EXPLICIT AND MONOTONIC
!+                ADVECTOR EVEN WITH TIDAL FLATS.
!
!warning  AFBOR AND BFBOR MUST BE 0 FOR THE BOUNDARY ELEMENTS
!+            WITH NO FRICTION
!warning  DISCRETISATION OF VISC
!
!warning  SEE BELOW FOR DEFINITION OF IOPT1 AND IOPT2, RETRIEVED FROM IOPT
!+        IOPT2=1 NOT TREATED HERE, MASS-CONSERVATION WILL BE DOWNGRADED
!+        IN THIS CASE (A CORRECT TREATMENT MAY RESULT IN INFINITE F)
!+        THE PROGRAM WILL NOT STOP IF IOPT2=1
!
!history  J-M HERVOUET   (LNHE)
!+        19/11/2010
!+        V6P0
!+   OPTIMIZATION (2 ABS SUPPRESSED)
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
!history  J-M HERVOUET   (LNHE)
!+        20/04/2011
!+        V6P1
!+   Option IOPT2=1 taken into account when there is an implicit source
!+   term. In other cases mass-conservation is not ensured (risk of
!+   division by 0). The implicit source must be negative, like settling
!+   velocity in Sisyphe. It is impossible to have mass conservation and
!+   monotonicity when the advection field does not obey the continuity
!+   equation. The only known application so far is Sisyphe.
!+
!+   Limitation of fluxes now programmed (see YAFLULIM and FLULIM)
!
!history  J-M HERVOUET (LNHE)
!+        24/02/2012
!+        V6P2
!+   Rain and evaporation added (after initiative by O. Boutron, from
!+   Tour du Valat, and O. Bertrand, Artelia group)
!
!history  J-M HERVOUET   (LNHE)
!+        17/07/2012
!+        V6P2
!+   T2 set to 0 to start with required in parallel
!+   Arguments flux_given and given_flux added.
!
!history  J-M HERVOUET   (LNHE)
!+        16/04/2013
!+        V6P2
!+   Intent of FLBOR changed and conditional building of FLBOR added
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        30/05/2013
!+        V6P2
!+   Argument NITMAX added.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/06/2014
!+        V7P0
!+   LIMTRA and FBOR corrected depending on the fluxes at boundaries,
!+   their intent modified accordingly.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/06/2015
!+        V7P1
!+   Adaptation to the fact that MESH%FAC is now replaced by MESH%IFAC.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/03/2016
!+        V7P2
!+   OPTION now active 1: ERIA algorithm
!+                     2: edge-based, classic NERD
!+   Argument NCO_DIST added.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AGGLOH         |-->| MASS-LUMPING UTILISE DANS L'EQUATION DE CONTINUITE
!| BILAN          |-->| LOGICAL TRIGGERING A MASS BALANCE INFORMATION
!| CONV           |-->| LOGICAL, IF YES THERE IS ADVECTION OF F
!| DIFT           |-->| LOGICAL, IF YES THERE IS DIFFUSION OF F
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| DT             |-->| TIME STEP
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS
!|                |   | CONSERVATION.
!| F              |<--| F AT TIME T(N+1)
!| FBOR           |-->| DIRICHLET CONDITIONS ON F.
!| FLBOR          |-->| FLUXES AT BOUNDARIES
!| FLBORTRA       |<->| TRACER FLUXES AT BOUNDARIES
!| FLUX_GIVEN     |-->| IF GIVEN_FLUX=YES, THE FLUX IS GIVEN IN
!|                |   | GIVEN_FLUX
!| FN             |-->| F AT TIME T(N)
!| FSCEXP         |-->| EXPLICIT PART OF THE SOURCE TERM
!|                |   | EQUAL TO ZERO EVERYWHERE BUT ON SOURCES
!|                |   | WHERE THERE IS FSCE - (1-TETAT) FN
!|                |   | SEE DIFSOU
!| GIVEN_FLUX     |-->| IF GIVEN_FLUX=YES, THE FLUX IS GIVEN IN
!|                |   | GIVEN_FLUX AND WILL NOT BE COMPUTED HERE
!| GLOSEG1        |-->| FIRST POINT OF SEGMENTS
!| GLOSEG2        |-->| SECOND POINT OF SEGMENTS
!| HNT,HT         |<--| WORK ARRAYS (MODIFIED DEPTHS TO TAKE MASS-LUMPING
!|                |   | INTO ACCOUNT)
!| HPROP          |-->| PROPAGATION DEPTH (DONE IN CVDFTR).
!| IOPT           |-->| OPTIONS FOR COMPUTATION (NUMBER BETWEEN 0 AND 13)
!|                |   | THE TENS (IOPT2, I.E. 0 OR 1):
!|                |   | 0: UCONV OBEYS THE CONTINUITY EQUATION
!|                |   | 1: UCONV DOES NOT OBEY THE CONTINUITY EQUATION
!|                |   | THE UNITS (IOPT1, I.E. 0 TO 3): VARIANT FOR FLUXES
!|                |   | 0: CONSTANT PER ELEMENT = 0
!|                |   | 1: CHI-TUAN PHAM'S CONSTANT
!|                |   | 2: N SCHEME
!|                |   | 3: PSI SCHEME
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| LIMTRA         |-->| BOUNDARY CONDITIONS ON BOOUNDARY POINTS
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!| MASSOU         |-->| MASS OF TRACER ADDED BY SOURCE TERM
!|                |   | SEE DIFSOU
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NCO_DIST       |-->| NUMBER OF CORRECTIONS IN DISTRIBUTIVE SCHEMES
!| NITMAX         |-->| MAXIMUM NUMBER OF ITERATIONS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPDTRA         |-->| OPTION FOR THE DIFFUSION OF TRACERS
!| OPTION         |-->| 1: ELEMENT BASED
!|                |   | 2: EDGE-BASED
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| PLUIE          |-->| RAIN OR EVAPORATION, IN M/S
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| S              |-->| VOID STRUCTURE
!| SM             |-->| SOURCE TERMS.
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SMI            |-->| IMPLICIT SOURCE TERM
!| SOLSYS         |-->| 1 OR 2. IF 2 ADVECTION FIELD IS UCONV + DM1*GRAD(ZCONV)
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| TE1            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TRAIN          |-->| VALUE OF TRACER IN THE RAIN
!| UDEL           |-->| X-COMPONENT OF ADVECTION VELOCITY
!| UNSV2D         |-->| INVERSE OF V2DPAR
!| V2DPAR         |-->| INTEGRAL OF 2D TEST FUNCTIONS, ASSEMBLED
!|                |   | IN PARALLEL.
!| VDEL           |-->| X-COMPONENT OF ADVECTION VELOCITY
!| VISC           |-->| VISCOSITY COEFFICIENTS ALONG X,Y AND Z .
!|                |   | IF P0 : PER ELEMENT
!|                |   | IF P1 : PERR POINT
!| VISC_S         |<->| WORK ARRAY FOR SAVING VISC
!| YAFLBOR        |-->| IF YES FLBOR IS GIVEN
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!| YASMI          |-->| IF YES, SMI MUST BE TAKEN INTO ACCOUNT
!| ZCONV          |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CVTRVF_POS => CVTRVF_POS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: OPDTRA,OPTSOU,KDIR,NPTFR,SOLSYS
      INTEGER, INTENT(IN)             :: KDDL,IOPT,OPTION,NITMAX
      INTEGER, INTENT(IN)             :: NCO_DIST
      INTEGER, INTENT(IN)             :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(INOUT)          :: LIMTRA(NPTFR)
!                                                               NSEG
      DOUBLE PRECISION, INTENT(IN)    :: DT,AGGLOH,TRAIN,FLULIM(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      LOGICAL, INTENT(IN)             :: BILAN,CONV,YASMH,YAFLBOR,RAIN
      LOGICAL, INTENT(IN)             :: DIFT,MSK,ENTET,YASMI,YAFLULIM
      LOGICAL, INTENT(IN)             :: FLUX_GIVEN
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,H,HN,DM1,ZCONV,MASKPT
      TYPE(BIEF_OBJ), INTENT(IN)      :: V2DPAR,UNSV2D,HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: F,SM,HNT,HT
      TYPE(BIEF_OBJ), INTENT(IN)      :: UDEL,VDEL,FN,SMI,SMH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TE1,FLBORTRA,FBOR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ), INTENT(IN)      :: FSCEXP,S,MASKTR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: VISC_S,VISC,PLUIE,GIVEN_FLUX
      TYPE(BIEF_MESH)                 :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION P_DSUM,P_DMIN,P_DMAX
      EXTERNAL         P_DSUM,P_DMIN,P_DMAX
!
      INTEGER I,IOPT1,IOPT2,NPOIN,IPTFR,I1,I2,I3,NITER,NEWREMAIN
      INTEGER IR,N,NELEM,IELEM,REMAIN,NSEG
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION C,CPREV,CINIT,HFL1,HFL2,TET,HSEG1,HSEG2
      DOUBLE PRECISION F1,F2,F3,COEF,FI1,FI2,FI3,VOL1,VOL2,VOL3
      DOUBLE PRECISION DT1,DT2,DT3
      DOUBLE PRECISION SURDT,FITOT,BETA1,BETA2,BETA3,A1,A2,A3
      DOUBLE PRECISION FP1,FP2,FP3,FIP1,FIP2,FIP3
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: FXMAT
      DOUBLE PRECISION, PARAMETER :: TIERS=1.D0/3.D0
      LOGICAL TESTING
      DATA TESTING/.FALSE./
      DOUBLE PRECISION EPS_FLUX
      DATA             EPS_FLUX/1.D-15/
!
!-----------------------------------------------------------------------
!
!     INDIC WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
!
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      INTEGER, ALLOCATABLE      :: INDIC(:)
      DOUBLE PRECISION, POINTER :: FLOP1(:),FLOP2(:),FLOP3(:)
      DOUBLE PRECISION, POINTER :: DTLIM1(:),DTLIM2(:),DTLIM3(:)
      DOUBLE PRECISION, POINTER :: SVOL1(:),SVOL2(:),SVOL3(:)
      SAVE
      NELEM=MESH%NELEM
      NSEG=MESH%NSEG
      NPOIN=H%DIM1
      SURDT=1.D0/DT
!
      IF(.NOT.DEJA) THEN
        IF(OPTION.EQ.1) THEN
          ALLOCATE(INDIC(NELEM))
        ELSEIF(OPTION.EQ.2) THEN
          ALLOCATE(INDIC(MESH%NSEG))
        ENDIF
        DEJA=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(OPTION.NE.1.AND.OPTION.NE.2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'OPTION INCONNUE DANS CVTRVF_POS : ',OPTION
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'UNKNOWN OPTION IN CVTRVF_POS: ',OPTION
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------   
!
!     THERE IS PLENTY OF MEMORY AVAILABLE IN A BIEF_MESH STRUCTURE...
!
      IF(OPTION.EQ.1) THEN      
        FLOP1=>MESH%MSEG%X%R(        1:  NELEM)
        FLOP2=>MESH%MSEG%X%R(  NELEM+1:2*NELEM)
        FLOP3=>MESH%MSEG%X%R(2*NELEM+1:3*NELEM)
        SVOL1=>     MESH%W%R(        1:  NELEM)
        SVOL2=>     MESH%W%R(  NELEM+1:2*NELEM)
        SVOL3=>     MESH%W%R(2*NELEM+1:3*NELEM)
        DTLIM1=>  MESH%M%X%R(        1:  NELEM)
        DTLIM2=>  MESH%M%X%R(  NELEM+1:2*NELEM)
        DTLIM3=>  MESH%M%X%R(2*NELEM+1:3*NELEM)
      ELSEIF(OPTION.EQ.2) THEN
        FXMAT=>MESH%MSEG%X%R(1:NSEG)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXTRACTING OPTIONS, AND CONTROL
!
      IOPT2=IOPT/10
      IOPT1=IOPT-10*IOPT2
      IF(IOPT1.LT.0.OR.IOPT1.GT.3) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CVTRVF_POS : OPTION IOPT1 INCONNUE : ',IOPT1
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CVTRVF_POS: OPTION IOPT1 UNKNOWN: ',IOPT1
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(IOPT2.NE.0.AND.IOPT2.NE.1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CVTRVF_POS : OPTION IOPT2 INCONNUE : ',IOPT2
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CVTRVF_POS: OPTION IOPT2 UNKNOWN: ',IOPT2
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     STARTING AGAIN FROM NON CORRECTED DEPTH
!
      IF(TESTING) THEN
        C=1.D99
        CINIT=1.D99
        DO I=1,NPOIN
          C    =MIN(C    ,H%R(I))
          CINIT=MIN(CINIT,HN%R(I))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          C=P_DMIN(C)
          CINIT=P_DMIN(CINIT)
        ENDIF
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, H MIN=',C
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, HN MIN=',CINIT
      ENDIF
!
      IF(OPTION.EQ.1) THEN
!
!       COMPUTING THE FLUXES LEAVING NODES
!
        FORMUL='HUGRADP         '
        IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
        CALL VECTOR(T1,'=',FORMUL,H%ELM,-1.D0,
     &              HPROP,DM1,ZCONV,UDEL,VDEL,VDEL,MESH,MSK,MASKEL)
!                   T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
!                   BUT TO GET THE NON ASSEMBLED FORM MESH%W
!   
!       COPYING EBE FLUXES INTO FLOPOINT
!
        DO I=1,NELEM
          FLOP1(I)=MESH%W%R(I)
          FLOP2(I)=MESH%W%R(I+NELEM)
          FLOP3(I)=MESH%W%R(I+2*NELEM)
        ENDDO
!
!       CHANGING FLUXES FROM POINTS INTO N FLUXES BETWEEN POINTS    
        DO IELEM = 1,NELEM
          A1 = ABS(FLOP1(IELEM))
          A2 = ABS(FLOP2(IELEM))
          A3 = ABS(FLOP3(IELEM))          
          IF(A1.GE.A2.AND.A1.GE.A3) THEN
!           ALL FLOW TO AND FROM NODE 1
            FLOP1(IELEM)=-FLOP2(IELEM)
            FLOP2(IELEM)=0.D0
!           FLOP3(IELEM)= UNCHANGED!
          ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
!           ALL FLOW TO AND FROM NODE 2
!           FLOP1(IELEM)= UNCHANGED!
            FLOP2(IELEM)=-FLOP3(IELEM)
            FLOP3(IELEM)=0.D0
          ELSE
!           ALL FLOW TO AND FROM NODE 3
            FLOP3(IELEM)=-FLOP1(IELEM)
            FLOP1(IELEM)=0.D0
!           FLOP2(IELEM)= UNCHANGED!
          ENDIF
        ENDDO
!
      ELSEIF(OPTION.EQ.2) THEN
!
!       CALCUL DES FLUX PAR NOEUDS
        IF(.NOT.FLUX_GIVEN) THEN
          FORMUL='HUGRADP         '
          IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
          CALL VECTOR(T1,'=',FORMUL,H%ELM,-1.D0,
     &                HPROP,DM1,ZCONV,UDEL,VDEL,VDEL,MESH,MSK,MASKEL)
!                     T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
!                     BUT TO GET THE NON ASSEMBLED FORM MESH%W
!         CALCUL DES FLUX PAR SEGMENT (TE1 SUIVI DE FALSE NON UTILISE)
!         FXMAT IS NOT ASSEMBLED IN //
          CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                    MESH%ELTSEG%I,MESH%ORISEG%I,
     &                    MESH%IKLE%I,.TRUE.,IOPT1)
!         LIMITATION OF FLUXES (IF REQUESTED, USED BY SISYPHE)
          IF(YAFLULIM) THEN
            DO I=1,MESH%NSEG
              FXMAT(I)=FXMAT(I)*FLULIM(I)
            ENDDO
          ENDIF
        ELSE
          DO I=1,MESH%NSEG
            FXMAT(I)=GIVEN_FLUX%R(I)
          ENDDO
        ENDIF
!       INTERFACE SEGMENTS: ONLY ONE OF THE TWINS WILL RECEIVE
!       THE ASSEMBLED FLUX
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM2_SEG(FXMAT,FXMAT,FXMAT,MESH%NSEG,1,2,1,MESH,
     &                     1,11)
          CALL MULT_INTERFACE_SEG(FXMAT,MESH%NH_COM_SEG%I,
     &                            MESH%NH_COM_SEG%DIM1,
     &                            MESH%NB_NEIGHB_SEG,
     &                            MESH%NB_NEIGHB_PT_SEG%I,
     &                            MESH%LIST_SEND_SEG%I,MESH%NSEG)
        ENDIF
!
      ENDIF
!
      CALL CPSTVC(H,T1)
      CALL CPSTVC(H,T2)
      CALL CPSTVC(F,T3)
      CALL CPSTVC(H,T4)
      CALL CPSTVC(F,T5)
      CALL CPSTVC(F,T6)
      CALL CPSTVC(H,T7)
!
!     T2 WILL BE THE ASSEMBLED FLBOR, INITIALISATION HERE
!     IS USELESS EXCEPT THAT PARCOM MAY ADD UNDEFINED
!     NUMBERS (THAT WILL NOT BE USED BUT THAT WILL STOP
!     A COMPILER... TOO BAD!)
      IF(NCSIZE.GT.1) CALL OS('X=0     ',X=T2)
!
!     INITIALIZING F AT THE OLD VALUE
!
      CALL OS('X=Y     ',X=F,Y=FN)
!
!     BOUCLE SUR LES SEGMENTS, POUR PRENDRE EN COMPTE LES FLUX
!     ADMISSIBLES
!
!     ADDING THE SOURCES (SMH IS NATURALLY ASSEMBLED IN //)
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            HT%R(I)=HN%R(I)+DT*SMH%R(I)
            F%R(I)=FN%R(I)+DT/MAX(HT%R(I),1.D-4)*SMH%R(I)*FSCEXP%R(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            HT%R(I)=HN%R(I)+DT*SMH%R(I)*UNSV2D%R(I)
            F%R(I)=FN%R(I)+DT/MAX(HT%R(I),1.D-4)*
     &                       UNSV2D%R(I)*SMH%R(I)*FSCEXP%R(I)
          ENDDO
        ENDIF
      ELSE
        DO I=1,NPOIN
          HT%R(I)=HN%R(I)
        ENDDO
      ENDIF
!
!     RAIN-EVAPORATION: RAIN FIRST, EVAPORATION IN THE END
!
      IF(RAIN) THEN
        DO I=1,NPOIN
          C=MAX(PLUIE%R(I),0.D0)
          HT%R(I)=HT%R(I)+DT*C
!                                                VALUE IN RAIN
          F%R(I)=F%R(I)+DT/MAX(HT%R(I),1.D-4)*C*(TRAIN-F%R(I))
        ENDDO
      ENDIF
!
      IF(.NOT.YAFLBOR) THEN
!       MASK=8 FOR LIQUID BOUNDARIES
        CALL VECTOR(FLBOR,'=','FLUBDF          ',1,1.D0,
     &              HPROP,HPROP,HPROP,
     &              UDEL , VDEL, VDEL,MESH,.TRUE.,MASKTR%ADR(8)%P)
      ENDIF
!
!     BOUNDARY FLUXES : ADDING THE ENTERING (NEGATIVE) FLUXES
!     FIRST PUTTING FLBOR (BOUNDARY) IN T2 (DOMAIN)
      CALL OSDB( 'X=Y     ' ,T2,FLBOR,FLBOR,0.D0,MESH)
!     ASSEMBLING T2 (FLBOR IS NOT ASSEMBLED)
      IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
!     POSSIBLE CORRECTION OF LIMTRA AND FBOR (LIMTRA HAS BEEN DONE BY
!     DIFFIN WITH U.N)
      DO I=1,MESH%NPTFR
        N=MESH%NBOR%I(I)
        IF(LIMTRA(I).EQ.KDIR.AND.T2%R(N).GT.0.D0) THEN
          LIMTRA(I)=KDDL
        ELSEIF(LIMTRA(I).EQ.KDDL.AND.T2%R(N).LT.0.D0) THEN
          LIMTRA(I)=KDIR
          FBOR%R(I)=FN%R(N)
        ENDIF
      ENDDO
!
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        HT%R(I)=HT%R(I)-DT*UNSV2D%R(I)*MIN(T2%R(I),0.D0)
!       ENTERING FLUXES OF TRACERS
!       THE FINAL DEPTH IS TAKEN
        IF(LIMTRA(IPTFR).EQ.KDIR) THEN
          F%R(I)=FN%R(I)-DT/MAX(HT%R(I),1.D-4)*
     &       UNSV2D%R(I)*T2%R(I)*(FBOR%R(IPTFR)-FN%R(I))
        ELSEIF(LIMTRA(IPTFR).EQ.KDDL) THEN
!         FLBORTRA IS NOT ASSEMBLED
          FLBORTRA%R(IPTFR)=FLBOR%R(IPTFR)*FN%R(I)
        ENDIF
      ENDDO
!
!     FOR OPTIMIZING THE LOOP ON SEGMENTS, ONLY SEGMENTS
!     WITH NON ZERO FLUXES WILL BE CONSIDERED, THIS LIST
!     WILL BE UPDATED. TO START WITH, ALL FLUXES ASSUMED NON ZERO
!
      IF(OPTION.EQ.1) THEN
        REMAIN=MESH%NELEM
      ELSEIF(OPTION.EQ.2) THEN
        REMAIN=NSEG
      ENDIF
!
      DO I=1,REMAIN
        INDIC(I)=I
      ENDDO
!
!     MAXIMUM INITIAL FLUX
!
      CPREV=0.D0
      IF(OPTION.EQ.1) THEN
        DO IR=1,NELEM
          CPREV=CPREV+ABS(FLOP1(IR))+ABS(FLOP2(IR))+ABS(FLOP3(IR))
        ENDDO
      ELSEIF(OPTION.EQ.2) THEN
        DO I=1,MESH%NSEG
          CPREV=CPREV+ABS(FXMAT(I))
        ENDDO
      ENDIF
      IF(NCSIZE.GT.1) CPREV=P_DSUM(CPREV)      
      IF(TESTING) WRITE(LU,*) 'INITIAL SUM OF FLUXES=',CPREV
      CINIT=CPREV
!
      NITER = 0
777   CONTINUE
      NITER = NITER + 1
!
      IF(OPTION.EQ.1) THEN
!       T4 IS THE EVOLUTION OF VOLUME OF WATER, HERE INITIALISED TO 0
!       T5 IS THE EVOLUTION OF MASSES OF TRACER, HERE INITIALISED TO 0
!       T6 IS THE INITIAL TRACER*DEPTH
        CALL OS('X=0     ',X=T4)
        CALL OS('X=0     ',X=T5)
        DO I=1,F%DIM1
          T6%R(I)=F%R(I)*HT%R(I)
        ENDDO
!       COMPUTING DEMAND (T1) AND OFFER (T1)
        CALL OS('X=0     ',X=T7)
        CALL OS('X=0     ',X=T1)
        DO IR=1,REMAIN
          I=INDIC(IR)
          I1=MESH%IKLE%I(I        )
          I2=MESH%IKLE%I(I  +NELEM)
          I3=MESH%IKLE%I(I+2*NELEM)
!         A PRIORI AVAILABLE VOLUMES
          VOL1=MESH%SURFAC%R(I)*HT%R(I1)*TIERS   
          VOL2=MESH%SURFAC%R(I)*HT%R(I2)*TIERS   
          VOL3=MESH%SURFAC%R(I)*HT%R(I3)*TIERS
!         FLUXES FROM POINTS
          F1= FLOP1(I)-FLOP3(I)
          F2=-FLOP1(I)+FLOP2(I)
          F3=-FLOP2(I)+FLOP3(I)
!         DEMAND OR OFFER
          IF(F1*DT.GT.VOL1) THEN
            T7%R(I1)=T7%R(I1)+F1*DT-VOL1
          ELSE
            T1%R(I1)=T1%R(I1)+MIN(VOL1,VOL1-F1*DT)
          ENDIF
          IF(F2*DT.GT.VOL2) THEN
            T7%R(I2)=T7%R(I2)+F2*DT-VOL2
          ELSE
            T1%R(I2)=T1%R(I2)+MIN(VOL2,VOL2-F2*DT)
          ENDIF
          IF(F3*DT.GT.VOL3) THEN
            T7%R(I3)=T7%R(I3)+F3*DT-VOL3
          ELSE
            T1%R(I3)=T1%R(I3)+MIN(VOL3,VOL3-F3*DT)
          ENDIF
        ENDDO
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(T7,2,MESH)
          CALL PARCOM(T1,2,MESH)
        ENDIF
      ELSEIF(OPTION.EQ.2) THEN
        IF(NITER.EQ.1) THEN
          DO I=1,NPOIN
            T1%R(I)=0.D0
            T4%R(I)=HT%R(I)
            T6%R(I)=F%R(I)
            T5%R(I)=HT%R(I)*F%R(I)
          ENDDO
          IF(NCSIZE.GT.1) THEN
            DO IPTFR=1,NPTIR
              I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
!             AVAILABLE DEPTH AND TRACER QUANTITY ARE SHARED BETWEEN PROCESSORS
!             FOR POINTS GIVING WATER, THEY WILL BE CANCELLED LATER
              HT%R(I)=HT%R(I)*MESH%IFAC%I(I)
              T5%R(I)=HT%R(I)*F%R(I)
            ENDDO
          ENDIF
        ELSE
!         NOT ALL THE POINTS NEED TO BE INITIALISED NOW
          DO IR=1,REMAIN
            I=INDIC(IR)
            I1=GLOSEG1(I)
            I2=GLOSEG2(I)
            T1%R(I1)=0.D0
            T1%R(I2)=0.D0
!           SAVING THE DEPTH AND TRACER
            T4%R(I1)=HT%R(I1)
            T4%R(I2)=HT%R(I2)
            T6%R(I1)=F%R(I1)
            T6%R(I2)=F%R(I2)
            T5%R(I1)=HT%R(I1)*F%R(I1)
            T5%R(I2)=HT%R(I2)*F%R(I2)
          ENDDO
!         CANCELLING INTERFACE POINTS (SOME MAY BE ISOLATED IN A SUBDOMAIN
!         AT THE TIP OF AN ACTIVE SEGMENT WHICH IS ELSEWHERE)
          IF(NCSIZE.GT.1) THEN
            DO IPTFR=1,NPTIR
              I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
              T1%R(I)=0.D0
!             SAVING THE DEPTH AND TRACER
              T4%R(I)=HT%R(I)
              T6%R(I)=F%R(I)
!             AVAILABLE DEPTH AND TRACER QUANTITY ARE SHARED BETWEEN PROCESSORS
!             FOR POINTS GIVING WATER, THEY WILL BE CANCELLED LATER
              HT%R(I)=HT%R(I)*MESH%IFAC%I(I)
              T5%R(I)=HT%R(I)*F%R(I)
            ENDDO
          ENDIF
        ENDIF
        DO IR=1,REMAIN
          I=INDIC(IR)
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          IF(FXMAT(I).GT.EPS_FLUX) THEN
            T1%R(I1)=T1%R(I1)+FXMAT(I)
            HT%R(I1)=0.D0
            T5%R(I1)=0.D0
          ELSEIF(FXMAT(I).LT.-EPS_FLUX) THEN
            T1%R(I2)=T1%R(I2)-FXMAT(I)
            HT%R(I2)=0.D0
            T5%R(I2)=0.D0
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
!
!       FOR ISOLATED POINTS CONNECTED TO AN ACTIVE SEGMENT
!       THAT IS IN ANOTHER SUBDOMAIN
        IF(NCSIZE.GT.1) THEN
          DO IPTFR=1,NPTIR
            I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            IF(T1%R(I).GT.EPS_FLUX) THEN
              HT%R(I)=0.D0
              T5%R(I)=0.D0
            ENDIF
          ENDDO
        ENDIF
!
      ENDIF
!
      IF(OPTION.EQ.1) THEN
!
!     PREDICTEUR
!
!     INITIALISING T3, FUTURE VALUE OF F BECAUSE POINTS THAT DO NOT RECEIVE
!     WATER WILL NOT BE TREATED
!
      DO I=1,F%DIM1
        T3%R(I)=F%R(I)
      ENDDO
!
      DO IR=1,REMAIN
!
        I=INDIC(IR)
!
        I1=MESH%IKLE%I(I        )
        I2=MESH%IKLE%I(I  +NELEM)
        I3=MESH%IKLE%I(I+2*NELEM)
!
!       A PRIORI AVAILABLE VOLUMES FOR THIS ELEMENT
!
        VOL1=MESH%SURFAC%R(I)*HT%R(I1)*TIERS   
        VOL2=MESH%SURFAC%R(I)*HT%R(I2)*TIERS   
        VOL3=MESH%SURFAC%R(I)*HT%R(I3)*TIERS
!
        F1= FLOP1(I)-FLOP3(I)
        F2=-FLOP1(I)+FLOP2(I)
        F3=-FLOP2(I)+FLOP3(I)
!
!       PRELIMINARY REDISTRIBUTION OF VOLUMES ACCORDING TO DEMAND AND OFFER
!
        IF(F1*DT.GT.VOL1) THEN
          IF(T7%R(I1).GT.T1%R(I1)) THEN
!                                  Commas very important to have T1/T7 <1
!                                  before multiplication
!                                  (                 )
            VOL1=VOL1+(F1*DT-VOL1)*(T1%R(I1)/T7%R(I1))
          ELSE
            VOL1=F1*DT
          ENDIF
        ELSE
          IF(T1%R(I1).GT.T7%R(I1)) THEN
            VOL1=VOL1-MIN(VOL1,VOL1-F1*DT)*(T7%R(I1)/T1%R(I1))
          ELSE
            VOL1=MAX(F1,0.D0)*DT
          ENDIF
        ENDIF
        IF(F2*DT.GT.VOL2) THEN
          IF(T7%R(I2).GT.T1%R(I2)) THEN
            VOL2=VOL2+(F2*DT-VOL2)*(T1%R(I2)/T7%R(I2))
          ELSE
            VOL2=F2*DT
          ENDIF
        ELSE
          IF(T1%R(I2).GT.T7%R(I2)) THEN
            VOL2=VOL2-MIN(VOL2,VOL2-F2*DT)*(T7%R(I2)/T1%R(I2))
          ELSE
            VOL2=MAX(F2,0.D0)*DT
          ENDIF
        ENDIF
        IF(F3*DT.GT.VOL3) THEN
          IF(T7%R(I3).GT.T1%R(I3)) THEN
            VOL3=VOL3+(F3*DT-VOL3)*(T1%R(I3)/T7%R(I3))
          ELSE
            VOL3=F3*DT
          ENDIF
        ELSE
          IF(T1%R(I3).GT.T7%R(I3)) THEN
            VOL3=VOL3-MIN(VOL3,VOL3-F3*DT)*(T7%R(I3)/T1%R(I3))
          ELSE
            VOL3=MAX(F3,0.D0)*DT
          ENDIF
        ENDIF
!
!       SAVING VOLUMES FOR CORRECTOR
!
        SVOL1(I)=VOL1
        SVOL2(I)=VOL2
        SVOL3(I)=VOL3      
!
        IF(F1*DT.GT.VOL1) THEN
          DT1=DT*(VOL1/(F1*DT))
        ELSE
          DT1=DT
        ENDIF
        IF(F2*DT.GT.VOL2) THEN
          DT2=DT*(VOL2/(F2*DT))
        ELSE
          DT2=DT
        ENDIF
        IF(F3*DT.GT.VOL3) THEN
          DT3=DT*(VOL3/(F3*DT))
        ELSE
          DT3=DT
        ENDIF
!
!       LIMITED VOLUMES TRANSITING BETWEEN POINTS (1/DT MISSING)
!
        DTLIM1(I)=MIN(DT1,DT2)
        DTLIM2(I)=MIN(DT2,DT3)
        DTLIM3(I)=MIN(DT3,DT1)
!
        FP1=FLOP1(I)*DTLIM1(I)
        FP2=FLOP2(I)*DTLIM2(I)
        FP3=FLOP3(I)*DTLIM3(I)
!
!       CORRESPONDING VARIATIONS OF VOLUMES OF POINTS (DT MISSING SO OK)
!
        T4%R(I1)=T4%R(I1)-( FP1-FP3)
        T4%R(I2)=T4%R(I2)-(-FP1+FP2)
        T4%R(I3)=T4%R(I3)-(-FP2+FP3)
!
!       VARIATIONS OF MASSES OF TRACER DURING DT
!
!       FIRST C * DELTA(H)
!
        T5%R(I1)=T5%R(I1)-( FP1-FP3)*F%R(I1)
        T5%R(I2)=T5%R(I2)-(-FP1+FP2)*F%R(I2)
        T5%R(I3)=T5%R(I3)-(-FP2+FP3)*F%R(I3)
!
!       THEN H * DELTA(C)  (OR CORRECTION OF THE UPWIND
!                           MISTAKE DONE ABOVE)
!
        FI1=0.D0
        FI2=0.D0
        FI3=0.D0
        IF(FLOP1(I).LT.0.D0) THEN
          FI1=FI1-FP1*(F%R(I2)-F%R(I1))
        ELSE
          FI2=FI2+FP1*(F%R(I1)-F%R(I2))
        ENDIF
        IF(FLOP2(I).LT.0.D0) THEN
          FI2=FI2-FP2*(F%R(I3)-F%R(I2))
        ELSE
          FI3=FI3+FP2*(F%R(I2)-F%R(I3))
        ENDIF
        IF(FLOP3(I).LT.0.D0) THEN
          FI3=FI3-FP3*(F%R(I1)-F%R(I3)) 
        ELSE
          FI1=FI1+FP3*(F%R(I3)-F%R(I1))
        ENDIF
!
!       PSI LIMITATION
!
        FITOT=FI1+FI2+FI3
!
        IF(FITOT.GT.EPS_FLUX) THEN
!         PSI REDUCTION
          BETA1=MAX(FI1,0.D0)
          BETA2=MAX(FI2,0.D0)
          BETA3=MAX(FI3,0.D0)
          COEF=FITOT/(BETA1+BETA2+BETA3)
          FI1=BETA1*COEF
          FI2=BETA2*COEF
          FI3=BETA3*COEF
        ELSEIF(FITOT.LT.-EPS_FLUX) THEN
!         PSI REDUCTION
          BETA1=MIN(FI1,0.D0)
          BETA2=MIN(FI2,0.D0)
          BETA3=MIN(FI3,0.D0)
          COEF=FITOT/(BETA1+BETA2+BETA3)
          FI1=BETA1*COEF
          FI2=BETA2*COEF
          FI3=BETA3*COEF
        ELSE
!         NO REDUCTION
        ENDIF
!
        T5%R(I1)=T5%R(I1)+FI1
        T5%R(I2)=T5%R(I2)+FI2
        T5%R(I3)=T5%R(I3)+FI3
!
      ENDDO
!
!     ADDING THE EVOLUTIONS TO THE DEPTHS AND TRACERS
!     AFTER ASSEMBLY AT INTERFACES AND AFTER
!     CHANGING VOLUMES INTO DEPTHS.
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(T4,2,MESH)
        CALL PARCOM(T5,2,MESH)
      ENDIF
      CALL OS('X=XY    ',X=T4,Y=UNSV2D)
      CALL OS('X=XY    ',X=T5,Y=UNSV2D)
!     NEW VALUE OF DEPTH AND PREDICTOR VALUE OF TRACER F IN T3
      DO I=1,F%DIM1
        HT%R(I)=HT%R(I)+T4%R(I)
        IF(HT%R(I).GT.1.D-15) THEN
          T3%R(I)=(T6%R(I)+T5%R(I))/HT%R(I)
        ENDIF
!       TO COPE WITH TRUNCATION ERRORS AND AVOID NEGATIVE VALUES
!       IF HT OR T3 NEGATIVE FOR OTHER REASONS IT WILL MAKE MASS ERRORS
        HT%R(I)=MAX(0.D0,HT%R(I))
        T3%R(I)=MAX(0.D0,T3%R(I))
      ENDDO
!
!     CORRECTOR
!
      IF(NCO_DIST.GT.0) THEN
      DO N=1,NCO_DIST
!
      CALL OS('X=0     ',X=T5)
!     
      DO I=1,F%DIM1
!      THIS ALLOWS TO CANCEL THE PART "FIRST C * DELTA(H)" BELOW
       T6%R(I)=F%R(I)*HT%R(I)
!      T7: DEMAND
       T7%R(I)=0.D0
!      T1: OFFER
       T1%R(I)=0.D0
      ENDDO
!
!     EVALUATING OFFER AND DEMAND
!
      DO IR=1,REMAIN
        I=INDIC(IR)
!
        I1=MESH%IKLE%I(I        )
        I2=MESH%IKLE%I(I  +NELEM)
        I3=MESH%IKLE%I(I+2*NELEM)
!       LIMITED VOLUMES BETWEEN POINTS
        FP1=FLOP1(I)*DTLIM1(I)
        FP2=FLOP2(I)*DTLIM2(I)
        FP3=FLOP3(I)*DTLIM3(I)
!                             IF USED
!                             HN TO BE SAVE IN T4
!       VOL1=MESH%SURFAC%R(I)*T4%R(I1)*TIERS-( FP1-FP3)
!       VOL2=MESH%SURFAC%R(I)*T4%R(I2)*TIERS-(-FP1+FP2)
!       VOL3=MESH%SURFAC%R(I)*T4%R(I3)*TIERS-(-FP2+FP3)
!
        VOL1=SVOL1(I)-( FP1-FP3)
        VOL2=SVOL2(I)-(-FP1+FP2)
        VOL3=SVOL3(I)-(-FP2+FP3)
!
        IF(VOL1.LT.MESH%SURFAC%R(I)*HT%R(I1)*TIERS) THEN
          T7%R(I1)=T7%R(I1)+MESH%SURFAC%R(I)*HT%R(I1)*TIERS-VOL1
        ELSE
          T1%R(I1)=T1%R(I1)+VOL1+MIN( FP1,0.D0)+MIN(-FP3,0.D0)
        ENDIF
        IF(VOL2.LT.MESH%SURFAC%R(I)*HT%R(I2)*TIERS) THEN
          T7%R(I2)=T7%R(I2)+MESH%SURFAC%R(I)*HT%R(I2)*TIERS-VOL2
        ELSE
          T1%R(I2)=T1%R(I2)+VOL2+MIN(-FP1,0.D0)+MIN( FP2,0.D0)
        ENDIF
        IF(VOL3.LT.MESH%SURFAC%R(I)*HT%R(I3)*TIERS) THEN
          T7%R(I3)=T7%R(I3)+MESH%SURFAC%R(I)*HT%R(I3)*TIERS-VOL3
        ELSE
          T1%R(I3)=T1%R(I3)+VOL3+MIN(-FP2,0.D0)+MIN( FP3,0.D0)
        ENDIF
      ENDDO
      IF(NCSIZE.GT.1) THEN
!       GLOBAL OFFER AND DEMAND
        CALL PARCOM(T1,2,MESH)
        CALL PARCOM(T7,2,MESH)
      ENDIF
!
!     NOW THE REAL CORRECTOR
!
      DO IR=1,REMAIN
!
        I=INDIC(IR)
!
        I1=MESH%IKLE%I(I        )
        I2=MESH%IKLE%I(I  +NELEM)
        I3=MESH%IKLE%I(I+2*NELEM)
!
!       LIMITED VOLUMES BETWEEN POINTS
!
        FP1=FLOP1(I)*DTLIM1(I)
        FP2=FLOP2(I)*DTLIM2(I)
        FP3=FLOP3(I)*DTLIM3(I)
!
!       VERSION WITH MONOTONICITY
!       VOL1=MESH%SURFAC%R(I)*T4%R(I1)*TIERS-( FP1-FP3)
!       VOL2=MESH%SURFAC%R(I)*T4%R(I2)*TIERS-(-FP1+FP2)
!       VOL3=MESH%SURFAC%R(I)*T4%R(I3)*TIERS-(-FP2+FP3)
        VOL1=SVOL1(I)-( FP1-FP3)
        VOL2=SVOL2(I)-(-FP1+FP2)
        VOL3=SVOL3(I)-(-FP2+FP3)
!
!       VOLUMES THAT WILL BE USED FOR THE DERIVATIVE IN TIME
!
        IF(T1%R(I1).GE.T7%R(I1)) THEN
          VOL1=MESH%SURFAC%R(I)*HT%R(I1)*TIERS
        ELSEIF(T7%R(I1).GT.1.D-15) THEN
          IF(VOL1.LT.MESH%SURFAC%R(I)*HT%R(I1)*TIERS) THEN
            VOL1=VOL1+(MESH%SURFAC%R(I)*HT%R(I1)*TIERS-VOL1)
     &               *(T1%R(I1)/T7%R(I1))
          ELSE
            VOL1=-(MIN( FP1,0.D0)+MIN(-FP3,0.D0))
          ENDIF
        ENDIF
        IF(T1%R(I2).GE.T7%R(I2)) THEN
          VOL2=MESH%SURFAC%R(I)*HT%R(I2)*TIERS
        ELSEIF(T7%R(I2).GT.1.D-15) THEN
          IF(VOL2.LT.MESH%SURFAC%R(I)*HT%R(I2)*TIERS) THEN
            VOL2=VOL2+(MESH%SURFAC%R(I)*HT%R(I2)*TIERS-VOL2)
     &               *(T1%R(I2)/T7%R(I2))
          ELSE
            VOL2=-(MIN(-FP1,0.D0)+MIN( FP2,0.D0))
          ENDIF
        ENDIF
        IF(T1%R(I3).GE.T7%R(I3)) THEN
          VOL3=MESH%SURFAC%R(I)*HT%R(I3)*TIERS
        ELSEIF(T7%R(I3).GT.1.D-15) THEN
          IF(VOL3.LT.MESH%SURFAC%R(I)*HT%R(I3)*TIERS) THEN
            VOL3=VOL3+(MESH%SURFAC%R(I)*HT%R(I3)*TIERS-VOL3)
     &               *(T1%R(I3)/T7%R(I3))
          ELSE
            VOL3=-(MIN(-FP2,0.D0)+MIN( FP3,0.D0))
          ENDIF
        ENDIF
!       SIMPLE VERSION WITHOUT MONOTONICITY
!       VOL1=MESH%SURFAC%R(I)*HT%R(I1)*TIERS
!       VOL2=MESH%SURFAC%R(I)*HT%R(I2)*TIERS
!       VOL3=MESH%SURFAC%R(I)*HT%R(I3)*TIERS
!
        FI1=(F%R(I1)-T3%R(I1))*VOL1 
        FI2=(F%R(I2)-T3%R(I2))*VOL2
        FI3=(F%R(I3)-T3%R(I3))*VOL3
!
!       ADDING THE DERIVATIVE THAT WILL BE REMOVED IN UPWIND FORM
!
        T5%R(I1)=T5%R(I1)-FI1
        T5%R(I2)=T5%R(I2)-FI2
        T5%R(I3)=T5%R(I3)-FI3
!
!       FIRST C * DELTA(H)
!
!       T5%R(I1)=T5%R(I1)-( FP1-FP3)*F%R(I1)
!       T5%R(I2)=T5%R(I2)-(-FP1+FP2)*F%R(I2)
!       T5%R(I3)=T5%R(I3)-(-FP2+FP3)*F%R(I3)
!
!       THEN H * DELTA(C)  (OR CORRECTION OF THE UPWIND
!                           MISTAKE DONE ABOVE)
!
        FIP1=0.D0
        FIP2=0.D0
        FIP3=0.D0
        IF(FLOP1(I).LT.0.D0) THEN
          FIP1=FIP1-FP1*(F%R(I2)-F%R(I1))
        ELSE
          FIP2=FIP2+FP1*(F%R(I1)-F%R(I2))
        ENDIF
        IF(FLOP2(I).LT.0.D0) THEN
          FIP2=FIP2-FP2*(F%R(I3)-F%R(I2))
        ELSE
          FIP3=FIP3+FP2*(F%R(I2)-F%R(I3))
        ENDIF
        IF(FLOP3(I).LT.0.D0) THEN
          FIP3=FIP3-FP3*(F%R(I1)-F%R(I3))
        ELSE
          FIP1=FIP1+FP3*(F%R(I3)-F%R(I1))
        ENDIF
        FITOT=FIP1+FIP2+FIP3
        IF(FITOT.GT.EPS_FLUX) THEN
!         PSI REDUCTION
          BETA1=MAX(FIP1,0.D0)
          BETA2=MAX(FIP2,0.D0)
          BETA3=MAX(FIP3,0.D0)
          COEF=FITOT/(BETA1+BETA2+BETA3)
          FIP1=BETA1*COEF
          FIP2=BETA2*COEF
          FIP3=BETA3*COEF
        ELSEIF(FITOT.LT.-EPS_FLUX) THEN
!         PSI REDUCTION
          BETA1=MIN(FIP1,0.D0)
          BETA2=MIN(FIP2,0.D0)
          BETA3=MIN(FIP3,0.D0)
          COEF=FITOT/(BETA1+BETA2+BETA3)
          FIP1=BETA1*COEF
          FIP2=BETA2*COEF
          FIP3=BETA3*COEF
        ELSE
!         NO REDUCTION
        ENDIF
!
!       ADDING TO FINAL CONTRIBUTIONS THAT WILL BE REDUCED AGAIN   
!
        FI1=FI1+FIP1
        FI2=FI2+FIP2
        FI3=FI3+FIP3
!
!       PSI LIMITATION
!
        FITOT=FI1+FI2+FI3
!
        IF(FITOT.GT.EPS_FLUX) THEN
!         PSI REDUCTION
          BETA1=MAX(FI1,0.D0)
          BETA2=MAX(FI2,0.D0)
          BETA3=MAX(FI3,0.D0)
          COEF=FITOT/(BETA1+BETA2+BETA3)
          FI1=BETA1*COEF
          FI2=BETA2*COEF
          FI3=BETA3*COEF
        ELSEIF(FITOT.LT.-EPS_FLUX) THEN
!         PSI REDUCTION
          BETA1=MIN(FI1,0.D0)
          BETA2=MIN(FI2,0.D0)
          BETA3=MIN(FI3,0.D0)
          COEF=FITOT/(BETA1+BETA2+BETA3)
          FI1=BETA1*COEF
          FI2=BETA2*COEF
          FI3=BETA3*COEF
        ELSE
!         NO REDUCTION
        ENDIF
!
        T5%R(I1)=T5%R(I1)+FI1
        T5%R(I2)=T5%R(I2)+FI2
        T5%R(I3)=T5%R(I3)+FI3
!
      ENDDO
!
!     ADDING THE EVOLUTIONS TO THE TRACERS
!     AFTER ASSEMBLY AT INTERFACES AND AFTER
!     CHANGING VOLUMES INTO DEPTHS.
!
      IF(NCSIZE.GT.1) CALL PARCOM(T5,2,MESH)
      CALL OS('X=XY    ',X=T5,Y=UNSV2D)
!     NEW AVERAGE VALUE OF TRACER
      DO I=1,F%DIM1
        IF(HT%R(I).GT.1.D-15) THEN
          T3%R(I)=(T6%R(I)+T5%R(I))/HT%R(I)
          T3%R(I)=MAX(0.D0,T3%R(I))
        ENDIF
      ENDDO
!
!     END OF CORRECTOR
!
      ENDDO
      ENDIF
!
!     SETTING THE FINAL VALUE OF DEPTH AND THE TRACER FOR THIS SUB-ITERATION
      DO I=1,F%DIM1
        F%R(I)=T3%R(I)
      ENDDO
!
!     IF REMAINING FLUXES, THE ELEMENT IS KEPT IN THE LIST
      NEWREMAIN=0
      C=0.D0
!
      DO IR=1,REMAIN
        I=INDIC(IR)
        IF(DTLIM1(I).EQ.DT.AND.DTLIM2(I).EQ.DT.AND.
     &     DTLIM3(I).EQ.DT) THEN
          FLOP1(I)=0.D0
          FLOP2(I)=0.D0
          FLOP3(I)=0.D0
        ELSE
          NEWREMAIN=NEWREMAIN+1
!         BEFORE NEWREMAIN: FOR NEXT ITERATION
!         AFTER  NEWREMAIN: STILL VALID FOR NEXT ITERATION
          INDIC(NEWREMAIN)=I
          FLOP1(I)=FLOP1(I)*(1.D0-DTLIM1(I)*SURDT)
          FLOP2(I)=FLOP2(I)*(1.D0-DTLIM2(I)*SURDT)    
          FLOP3(I)=FLOP3(I)*(1.D0-DTLIM3(I)*SURDT) 
          C=C+ABS(FLOP1(I))+ABS(FLOP2(I))+ABS(FLOP3(I))
        ENDIF
      ENDDO
!
      ELSEIF(OPTION.EQ.2) THEN
!
      C=0.D0
      NEWREMAIN=0
!
      DO IR=1,REMAIN
        I=INDIC(IR)
        I1=GLOSEG1(I)
        I2=GLOSEG2(I)
        IF(FXMAT(I).GT.EPS_FLUX) THEN
!         SHARING ON DEMAND: FRACTION OF DEPTH TAKEN
!         T4 IS THE STORED DEPTH
!         1.D-20 ADDED HERE OTHERWISE IT OCCURED THAT THE PRODUCT OF 2
!         STRICTLY POSITIVE NUMBERS GAVE 0.D0
          IF(T4%R(I1).GT.1.D-20) THEN
            HSEG1=T4%R(I1)*FXMAT(I)/T1%R(I1)
!           END OF SHARING ON DEMAND
            HFL1= DT*UNSV2D%R(I1)*FXMAT(I)
            IF(HFL1.GT.HSEG1) THEN
              TET=HSEG1/HFL1
!             HSEG2 AND THUS HT WILL BE STRICTLY POSITIVE
              HSEG2=DT*UNSV2D%R(I2)*FXMAT(I)*TET
              HT%R(I2)=HT%R(I2)+HSEG2
!             GROUPING H*F
              T5%R(I2)=T5%R(I2)+HSEG2*T6%R(I1)
!             RECOMPUTING F (AS WEIGHTED AVERAGE)
!             THIS MAY BE DONE SEVERAL TIMES FOR THE SAME POINT
!             BUT THE LAST ONE WILL BE THE GOOD ONE
              F%R(I2)=T5%R(I2)/HT%R(I2)
              FXMAT(I)=FXMAT(I)*(1.D0-TET)
              C=C+FXMAT(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC(NEWREMAIN)=I
            ELSE
              HSEG1=HSEG1-HFL1
              HSEG2=DT*UNSV2D%R(I2)*FXMAT(I)
              HT%R(I2)=HT%R(I2)+HSEG2
              T5%R(I2)=T5%R(I2)+HSEG2*T6%R(I1)
!             THE LAST ONE WILL BE THE GOOD ONE
              F%R(I2)=T5%R(I2)/HT%R(I2)
              IF(HSEG1.GT.0.D0) THEN
                HT%R(I1)=HT%R(I1)+HSEG1
                T5%R(I1)=T5%R(I1)+HSEG1*T6%R(I1)
!               THE LAST ONE WILL BE THE GOOD ONE
                F%R(I1)=T5%R(I1)/HT%R(I1)
              ENDIF
            ENDIF
          ELSE
!           NO WATER NO FLUX TRANSMITTED, NOTHING CHANGED
            C=C+FXMAT(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ENDIF
        ELSEIF(FXMAT(I).LT.-EPS_FLUX) THEN
!         SHARING ON DEMAND
          IF(T4%R(I2).GT.1.D-20) THEN
!         1.D-20 ADDED HERE OTHERWISE IT OCCURED THAT THE PRODUCT OF 2
!         STRICTLY POSITIVE NUMBERS GAVE 0.D0
            HSEG2=-T4%R(I2)*FXMAT(I)/T1%R(I2)
!           END OF SHARING ON DEMAND
            HFL2=-DT*UNSV2D%R(I2)*FXMAT(I)
            IF(HFL2.GT.HSEG2) THEN
              TET=HSEG2/HFL2
!             HSEG1 AND THUS HT WILL BE STRICTLY POSITIVE
              HSEG1=-DT*UNSV2D%R(I1)*FXMAT(I)*TET
              HT%R(I1)=HT%R(I1)+HSEG1
              T5%R(I1)=T5%R(I1)+HSEG1*T6%R(I2)
!             THE LAST ONE WILL BE THE GOOD ONE
              F%R(I1)=T5%R(I1)/HT%R(I1)
              FXMAT(I)=FXMAT(I)*(1.D0-TET)
              C=C-FXMAT(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC(NEWREMAIN)=I
            ELSE
              HSEG1=-DT*UNSV2D%R(I1)*FXMAT(I)
              HSEG2=HSEG2-HFL2
              HT%R(I1)=HT%R(I1)+HSEG1
              T5%R(I1)=T5%R(I1)+HSEG1*T6%R(I2)
              F%R(I1)=T5%R(I1)/HT%R(I1)
              IF(HSEG2.GT.0.D0) THEN
                HT%R(I2)=HT%R(I2)+HSEG2
                T5%R(I2)=T5%R(I2)+HSEG2*T6%R(I2)
!               THE LAST ONE WILL BE THE GOOD ONE
                F%R(I2)=T5%R(I2)/HT%R(I2)
              ENDIF
            ENDIF
          ELSE
!           NO WATER NO FLUX TRANSMITTED, NOTHING CHANGED
            C=C-FXMAT(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ENDIF
        ENDIF
      ENDDO
!
!     MERGING DEPTHS AND F AT INTERFACE POINTS
!
      IF(NCSIZE.GT.1) THEN
        DO IPTFR=1,NPTIR
!         ARRAY WITH HT*F AT INTERFACE POINTS
          I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          T1%R(I)=HT%R(I)*F%R(I)
        ENDDO
!       SUMMING HT*F AT INTERFACE POINTS
        CALL PARCOM(T1,2,MESH)
!       SUMMING THE NEW POSITIVE PARTIAL DEPTHS OF INTERFACE POINTS
        CALL PARCOM(HT,2,MESH)
!       AVERAGE F AT INTERFACE POINTS
        DO IPTFR=1,NPTIR
          I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          IF(HT%R(I).GT.0.D0) F%R(I)=T1%R(I)/HT%R(I)
        ENDDO
      ENDIF
!
      ENDIF
!
      IF(NCSIZE.GT.1) C=P_DSUM(C)
      IF(TESTING) WRITE(LU,*) 'FLUX NON PRIS EN COMPTE=',C
!
      REMAIN=NEWREMAIN
!
      IF(C.NE.CPREV.AND.ABS(C-CPREV).GT.CINIT*1.D-9
     &             .AND.C.NE.0.D0) THEN
        CPREV=C
        IF(NITER.LT.NITMAX) GO TO 777
      ENDIF
!
!     RAIN-EVAPORATION: RAIN DONE ABOVE, NOW EVAPORATION
!
      IF(RAIN) THEN
        DO I=1,NPOIN
          C=MIN(PLUIE%R(I),0.D0)
!         POSITIVITY NOT TESTED HERE, WOULD REQUIRE C=MAX(C,-HT%R(I)/DT)
!         BUT THEN MASS-BALANCE WOULD NOT BE CORRECT,
          HT%R(I)=HT%R(I)+DT*C
!                                                VALUE IN VAPOR
!         F%R(I)=F%R(I)+DT/MAX(HT%R(I),1.D-4)*C*(0.D0-F%R(I))
          F%R(I)=F%R(I)-DT/MAX(HT%R(I),1.D-4)*C*F%R(I)
        ENDDO
      ENDIF
!
!     BOUNDARY FLUXES : ADDING THE EXITING (POSITIVE) FLUXES
!                       WITH A POSSIBLE LIMITATION
!
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
!                               T2 = // ASSEMBLED FLBOR
        HFL1=DT*UNSV2D%R(I)*MAX(T2%R(I),0.D0)
        TET=1.D0
        IF(HFL1.GT.HT%R(I)) TET=HT%R(I)/HFL1
!       MAX IS ONLY TO PREVENT TRUNCATION ERROR
        HT%R(I)=MAX(HT%R(I)-HFL1*TET,0.D0)
!       LIMITATION OF FLBOR (MUST HAVE BEEN DONE ALREADY
!                            IN POSITIVE_DEPTHS)
!       FLBOR%R(IPTFR)=FLBOR%R(IPTFR)*TET
        IF(LIMTRA(IPTFR).EQ.KDIR) THEN
          F%R(I)=F%R(I)-HFL1*TET/MAX(HT%R(I),1.D-4)*
     &           (FBOR%R(IPTFR)-F%R(I))
          FLBORTRA%R(IPTFR)=FLBOR%R(IPTFR)*FBOR%R(IPTFR)
        ELSEIF(LIMTRA(IPTFR).EQ.KDDL) THEN
          FLBORTRA%R(IPTFR)=FLBOR%R(IPTFR)*F%R(I)
        ELSE
          FLBORTRA%R(IPTFR)=0.D0
        ENDIF
      ENDDO
!
      IF(TESTING) THEN
        C=0.D0
        DO I=1,NPOIN
          C=C+(HT%R(I)-H%R(I))**2
        ENDDO
!                       FAUX MAIS PAS GRAVE SI 0.
        IF(NCSIZE.GT.1) C=P_DSUM(C)
        WRITE(LU,*) 'DIFFERENCE ENTRE H ET HT =',C
!
        C=1.D99
        DO I=1,NPOIN
          C=MIN(C,F%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_DMIN(C)
        WRITE(LU,*) 'APRES TRAITEMENT TRACEUR MIN=',C
        C=-1.D99
        DO I=1,NPOIN
          C=MAX(C,F%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_DMAX(C)
        WRITE(LU,*) 'APRES TRAITEMENT TRACEUR MAX=',C
      ENDIF
!
!-----------------------------------------------------------------------
!
!     SOURCE TERMS
!
      IF(YASMI) THEN
!
!       IMPLICIT AND EXPLICIT SOURCE TERM
!
        IF(IOPT2.EQ.0) THEN
          DO I = 1,MESH%NPOIN
            F%R(I)=(F%R(I)+DT*SM%R(I))/
     &           (1.D0-DT*SMI%R(I)/MAX(H%R(I),1.D-15))
!           COULD BE DONE LIKE THIS...
!           F%R(I)=H%R(I)*(F%R(I)+DT*SM%R(I))/(H%R(I)-DT*SMI%R(I))
          ENDDO
        ELSEIF(IOPT2.EQ.1) THEN
!         HERE WE ASSUME THAT SMI WILL PREVENT A DIVISION BY ZERO
!         THIS IS THE CASE WITH SETTLING VELOCITY IN SISYPHE
          DO I = 1,MESH%NPOIN
          F%R(I)=(F%R(I)*HT%R(I)+DT*SM%R(I)*H%R(I))/(H%R(I)-DT*SMI%R(I))
          ENDDO
        ENDIF
!
      ELSE
!
!       EXPLICIT SOURCE TERM ONLY (AND IOPT2=1 NOT TREATED !!!)
!
        DO I = 1,MESH%NPOIN
          F%R(I) = F%R(I)+DT*SM%R(I)
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(ENTET) THEN
        IF(LNG.EQ.1) WRITE(LU,101) NITER
        IF(LNG.EQ.2) WRITE(LU,102) NITER
      ENDIF
!
101   FORMAT(' CVTRVF_POS (SCHEMA 13, 14 OU 15) : ',1I3,' ITERATIONS')
102   FORMAT(' CVTRVF_POS (SCHEME 13, 14 OR 15): ',1I3,' ITERATIONS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END


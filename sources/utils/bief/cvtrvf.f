!                    *****************
                     SUBROUTINE CVTRVF
!                    *****************
!
     &(F,FN,FSCEXP,DIFT,CONV,H,HN,HPROP,UCONV,VCONV,DM1,ZCONV,SOLSYS,
     & VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,FBOR,MASKTR,MESH,
     & T1,T2,T3,T4,T5,T6,T7,T8,HNT,HT,AGGLOH,TE1,DT,ENTET,BILAN,
     & OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,LIMTRA,KDIR,KDDL,NPTFR,FLBOR,
     & YAFLBOR,VOLU2D,V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT,
     & RAIN,PLUIE,TRAIN,OPTADV)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    FINITE VOLUMES, UPWIND, EXPLICIT ADVECTOR.
!
!warning  AFBOR AND BFBOR MUST BE 0 FOR THE BOUNDARY ELEMENTS
!+            WITH NO FRICTION
!warning  DISCRETISATION OF VISC
!
!history  CHI-TUAN PHAM  (LNHE)  
!+        09/10/09
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
!history  J-M HERVOUET (LNHE)
!+        24/02/2012
!+        V6P2
!+   Rain and evaporation added (after initiative by O. Boutron, from
!+   Tour du Valat, and O. Bertrand, Artelia group)
!
!history  SARA PAVAN & J-M HERVOUET (LNHE)
!+        18/06/2013
!+        V6P3
!+   New call to CFLVF, for new monotonicity criterion.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        16/05/2014
!+        V7P0
!+   Boundary conditions LIMTRA redone (they may have been done with
!+   U.N in diffin.f, which is different from flbor here. Argument
!+   VOLU2D added.
!
!history  SARA PAVAN & J-M HERVOUET (EDF LAB, LNHE)
!+        03/10/2014
!+        V7P0
!+   New predictor-corrector PSI scheme (OPTADV=2). Security coefficient
!+   on maximum time step to avoid truncation errors that would give
!+   negative values.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AGGLOH         |-->| MASS-LUMPING IN CONTINUITY EQUATION
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
!| FN             |-->| F AT TIME T(N)
!| FSCEXP         |-->| EXPLICIT PART OF THE SOURCE TERM
!|                |   | EQUAL TO ZERO EVERYWHERE BUT ON SOURCES
!|                |   | WHERE THERE IS FSCE - (1-TETAT) FN
!|                |   | SEE DIFSOU
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
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPDADV         |-->| SCHEME OPTION FOR THE ADVECTION OF TRACERS
!|                |   | WITH N SCHEME:
!|                |   |  1: EXPLICIT   
!|                |   |  2: IMPLICIT
!|                |   |  3: PREDICTOR-CORRECTOR 2ND ORDER IN TIME (MONOTONICITY NOT PROVED)
!|                |   |  4: IMPLICIT PREDICTOR EXPLICIT CORRECTOR 2ND ORDER IN TIME
!|                |   | WITH PSI SCHEME:
!|                |   |  1: EXPLICIT   
!|                |   |  2: PREDICTOR-CORRECTOR 1ST ORDER IN TIME
!|                |   |  3: PREDICTOR-CORRECTOR 2ND ORDER IN TIME (MONOTONICITY NOT PROVED)
!|                |   |  4: IMPLICIT PREDICTOR EXPLICIT CORRECTOR 2ND ORDER IN TIME
!| OPDTRA         |-->| OPTION FOR THE DIFFUSION OF TRACERS
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
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
!| UCONV,VCONV    |-->| ADVECTION VELOCITY FIELD
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS, NOT ASSEMBLED IN PARALLEL
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
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
      USE BIEF, EX_CVTRVF => CVTRVF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: OPDTRA,OPTSOU,KDIR,NPTFR,SOLSYS
      INTEGER, INTENT(IN)             :: KDDL,IOPT,OPTADV
      INTEGER, INTENT(INOUT)          :: LIMTRA(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,AGGLOH,TRAIN
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      LOGICAL, INTENT(IN)             :: BILAN,CONV,YASMH,YAFLBOR
      LOGICAL, INTENT(IN)             :: DIFT,MSK,ENTET,YASMI,RAIN
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,H,HN,DM1,ZCONV,MASKPT
      TYPE(BIEF_OBJ), INTENT(IN)      :: VOLU2D,V2DPAR,UNSV2D,HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: F,SM,HNT,HT
      TYPE(BIEF_OBJ), INTENT(IN)      :: UCONV,VCONV,FN,SMI,SMH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FBOR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TE1,FLBORTRA
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ), INTENT(IN)      :: FSCEXP,S,MASKTR
      TYPE(BIEF_OBJ), INTENT(IN)      :: VISC_S,VISC,PLUIE
      TYPE(BIEF_MESH) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN),    TARGET :: FLBOR
      TYPE(BIEF_OBJ), INTENT(INOUT), TARGET :: T3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMF,I,IOPT1,IOPT2,N,VARIANT,IELEM,I1,I2,I3,ITER
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION DT_REMAIN,DDT,TDT,SECU,COEMIN,COESOU
!
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVE_HT,SAVE_HNT
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: FXMAT,FXMATPAR
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: FMIN,FMAX
      TYPE(BIEF_OBJ), POINTER                 :: FLBOUND
!
      DOUBLE PRECISION P_DMIN,C
      EXTERNAL         P_DMIN
!
      INTEGER NITMAX,NIT
      DATA NITMAX/200/
!
!-----------------------------------------------------------------------
!
      SAVE_HT =>HT%R
      SAVE_HNT=>HNT%R
      FXMAT=>MESH%MSEG%X%R(1:MESH%NSEG)
!     IN PARALLEL MODE, ASSEMBLED AND NON ASSEMBLED VERSIONS ARE DIFFERENT
      IF(NCSIZE.GT.1) THEN
        FXMATPAR=>MESH%MSEG%X%R(MESH%NSEG+1:2*MESH%NSEG)
      ELSE
        FXMATPAR=>MESH%MSEG%X%R(1:MESH%NSEG)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXTRACTS THE OPTIONS
!
      IOPT2=IOPT/10
      IOPT1=IOPT-10*IOPT2
!
!-----------------------------------------------------------------------
!
!     IELMF = F%ELM
!     FORCED TO LINEAR
      IELMF=11
!
!     TAKES MASS-LUMPING INTO ACCOUNT IN THE CONTINUITY EQUATION
!
      IF(ABS(1.D0-AGGLOH).GT.1.D-8) THEN
        CALL VECTOR(HT ,'=','MASVEC          ',IELMF,
     &              1.D0-AGGLOH,H ,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(HNT,'=','MASVEC          ',IELMF,
     &              1.D0-AGGLOH,HN,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(HT ,2,MESH)
          CALL PARCOM(HNT,2,MESH)
        ENDIF
        CALL OS('X=YZ    ',X=HT ,Y=HT ,Z=UNSV2D)
        CALL OS('X=YZ    ',X=HNT,Y=HNT,Z=UNSV2D)
        CALL OS('X=X+CY  ',X=HT ,Y=H  ,C=AGGLOH)
        CALL OS('X=X+CY  ',X=HNT,Y=HN ,C=AGGLOH)
      ELSE
!       CALL OS('X=Y     ',X=HT ,Y=H )
!       CALL OS('X=Y     ',X=HNT,Y=HN)
        HT%R =>H%R
        HNT%R=>HN%R
      ENDIF
!
!     IF NO FLBOR IS GIVEN, IT IS COMPUTED HERE IN T3
!
      IF(YAFLBOR) THEN
        FLBOUND => FLBOR
      ELSE
!       MASK=5 FOR NON NEUMANN BOUNDARIES IN DIFFIN
        CALL VECTOR(T3,'=','FLUBDF          ',1,1.D0,HPROP,HPROP,HPROP,
     &              UCONV,VCONV,VCONV,MESH,.TRUE.,MASKTR%ADR(5)%P)
        FLBOUND => T3
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CORRECTION OF THE BOUNDARY CONDITIONS
!
!-----------------------------------------------------------------------
!
!     A SIMILAR CORRECTION IS DONE IN DIFFIN, BUT IT MAY BE INCOMPATIBLE
!     AS U*N MAY NOT BE OF THE SAME SIGN AS FLBOR HERE (RARE BUT ALREADY
!     SEEN).
!
      CALL CPSTVC(HN,T7)
!     JUST IN CASE INTERNAL POINTS HAVE NON INITIALISED VALUES
      CALL OS('X=0     ',X=T7)
      DO I=1,MESH%NPTFR
        N=MESH%NBOR%I(I)
        T7%R(N)=FLBOUND%R(I)
      ENDDO
      IF(NCSIZE.GT.1) CALL PARCOM(T7,2,MESH)
      DO I=1,MESH%NPTFR
        N=MESH%NBOR%I(I)
        IF(LIMTRA(I).EQ.KDIR.AND.T7%R(N).GT.0.D0) THEN
          LIMTRA(I)=KDDL
        ELSEIF(LIMTRA(I).EQ.KDDL.AND.T7%R(N).LT.0.D0) THEN
          LIMTRA(I)=KDIR
          FBOR%R(I)=FN%R(N)
        ENDIF
      ENDDO
!
!     T7 MAY NOW BE REUSED
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE TRACER FLUX AT THE BOUNDARY
!

      DO I=1,MESH%NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
!         FLBOR IS NOT ASSEMBLED IN PARALLEL MODE
          FLBORTRA%R(I)=FLBOUND%R(I)*FBOR%R(I)
        ELSE
!         FOR KDDL, WILL BE DONE IN TVF OR TVF_2 OR TVF_IMP
          FLBORTRA%R(I)=0.D0
        ENDIF
      ENDDO
!
!     COMPUTES THE FLUXES PHIIJ = FXMAT
!
      FORMUL='HUGRADP         '
      IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
      CALL VECTOR(T2,'=',FORMUL,IELMF,-1.D0,
     &            HPROP,DM1,ZCONV,UCONV,VCONV,VCONV,MESH,MSK,MASKEL)
!                 T2 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
!                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
      NIT=0
      DT_REMAIN=DT
      TDT=0.D0
      CALL CPSTVC(H ,T5)
      CALL CPSTVC(H ,T4)
      CALL CPSTVC(F,T8)
!
!     T4 WILL BE F PROGRESSIVELY UPDATED
!     T5 WILL BE THE DEPTH AT THE END OF THE SUB-TIMESTEP
!     (INITIALISED HERE TO CALL  CFLVF)
!
      DO I=1,HN%DIM1
        T4%R(I)=FN%R(I)
        T5%R(I)=HNT%R(I)
      ENDDO
!
!     T1 WILL BE THE DEPTH ACCORDING TO THE CONTINUITY EQUATION
!
      IF(IOPT2.EQ.1) THEN
        DO I=1,HN%DIM1
          T1%R(I)=HNT%R(I)
        ENDDO
      ENDIF
!
100   CONTINUE
      NIT=NIT+1
!
!---------------------------------------
! VARIOUS OPTIONS TO COMPUTE THE FLUXES
!---------------------------------------
!
      IF(NIT.EQ.1.OR.IOPT1.EQ.3) THEN
        CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,2    ,T4)
!                                          IOPT1 HERE FORCED TO N SCHEME
!       CANCELS FLUXES TO AND FROM MASKED POINTS
        IF(MSK) THEN
          CALL FLUX_MASK(FXMAT,MESH%NSEG,
     &                   MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MASKPT%R)
        ENDIF
!       ASSEMBLES THE FLUXES AT INTERFACES IN PARALLEL MODE, THIS
!       IS FOR UPWINDING (STORED IN SECOND DIMENSION OF MESH%MSEG)
        IF(NCSIZE.GT.1) THEN
          CALL OV('X=Y     ',FXMATPAR,FXMAT,FXMAT,0.D0,MESH%NSEG)
          CALL PARCOM2_SEG(FXMATPAR,FXMATPAR,FXMATPAR,
     &                     MESH%NSEG,1,2,1,MESH,1,11)
        ENDIF
      ENDIF
!
!--------------------------------------------
! DETERMINES THE LARGEST ADMISSIBLE TIMESTEP
!--------------------------------------------
!
!     THIS COULD BE PUT OUTSIDE THE LOOP, BUT T7 USED LATER IN THE LOOP...
!
!     IN CFLVF, T7 WILL BE FLBOR WITH A DIMENSION NPOIN
      CALL OS('X=0     ',X=T7)
      DO I=1,MESH%NPTFR
        N=MESH%NBOR%I(I)
        T7%R(N)=FLBOUND%R(I)
      ENDDO
      IF(NCSIZE.GT.1) CALL PARCOM(T7,2,MESH)
!
!     MASKS FLBOR IF(MSK)
!
      IF(MSK) CALL OS('X=XY    ',X=T7,Y=MASKPT)
!
!     COMPUTES THE MAXIMUM TIMESTEP ENSURING MONOTONICITY
!     ACCORDING TO THEORY
!
      COESOU=0.D0
      COEMIN=0.D0
      SECU=0.99D0
!
      IF(IOPT1.EQ.3.AND.OPTADV.EQ.2) THEN
        SECU=1.D0
        COEMIN=-1.D0
        COESOU=COEMIN
      ENDIF
!
      CALL CFLVF(DDT,T5%R,HT%R,FXMAT,FXMATPAR,
!                               FLBOR%R(NPOIN)
     &           V2DPAR%R,DT_REMAIN,T7%R   ,SMH%R,
     &           YASMH,T8,MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &           MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MESH,MSK,MASKPT,
     &           RAIN,PLUIE%R,T4%R,MESH%NELEM,MESH%IKLE%I,
     &           LIMTRA,KDIR,FBOR%R,FSCEXP%R,TRAIN,MESH%NBOR%I,
     &           T2,T6,SECU,COEMIN,COESOU)
!
!     NOW RECOMPUTING THE PSI FLUXES (THE N FLUXES HAVE BEEN
!     USED FOR THE STABILITY CRITERION).
!
      IF(IOPT1.EQ.3) THEN
        CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,IOPT1,T4)
!       CANCELS FLUXES TO AND FROM MASKED POINTS
        IF(MSK) THEN
          CALL FLUX_MASK(FXMAT,MESH%NSEG,
     &                   MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MASKPT%R)
        ENDIF
!       ASSEMBLES THE FLUXES AT INTERFACES IN PARALLEL MODE, THIS
!       IS FOR UPWINDING (STORED IN SECOND DIMENSION OF MESH%MSEG)
        IF(NCSIZE.GT.1) THEN
          CALL OV('X=Y     ',FXMATPAR,FXMAT,FXMAT,0.D0,MESH%NSEG)
          CALL PARCOM2_SEG(FXMATPAR,FXMATPAR,FXMATPAR,
     &                     MESH%NSEG,1,2,1,MESH,1,11)
        ENDIF
      ENDIF
!
!
      IF(NCSIZE.GT.1) DDT=P_DMIN(DDT)
      DDT=MIN(DDT,DT_REMAIN) 
!
!     T2 WILL TAKE THE SUCCESSIVE VALUES OF H
!     AT THE BEGINNING OF THE SUB-TIMESTEP
!     WARNING: T2 ALSO USED WITH IOPT2=1, BUT SO FAR PREDICTOR-CORRECTOR
!              NOT USED WITH THIS OPTION
!
      IF(IOPT1.EQ.3.AND.OPTADV.EQ.2) THEN
        DO I=1,HN%DIM1
          T2%R(I)=HNT%R(I)+TDT*(HT%R(I)-HNT%R(I))/DT
        ENDDO
      ENDIF
!
      TDT=TDT+DDT
!
!     T5 WILL TAKE THE SUCCESSIVE VALUES OF H
!     AT THE END OF THE SUB-TIMESTEP
!
      DO I=1,HN%DIM1
        T5%R(I)=HNT%R(I)+TDT*(HT%R(I)-HNT%R(I))/DT
      ENDDO
!
!     IN TVF FACTOR HT/HLIN MAY TRIGGER DIVERGENCE FOR DRY POINTS
!
      IF(MSK) THEN
        DO I=1,HN%DIM1
          IF(MASKPT%R(I).LT.0.5D0) T5%R(I)=HT%R(I)
        ENDDO
      ENDIF
!
!------------------------------------
!  FINAL RESOLUTION OR PREDICTOR STEP
!------------------------------------
!
      CALL TRACVF(F,FN,FSCEXP,HT,HNT,FXMAT,FXMATPAR,V2DPAR,UNSV2D,
     &            DDT,FLBOUND,FBOR,SMH,YASMH,T1,T2,T4,T5,T6,T7,T8,
     &            MESH,LIMTRA,KDIR,KDDL,OPTSOU,IOPT2,FLBORTRA,MSK,
     &            DT,RAIN,PLUIE,TRAIN)
!
!--------------------------------
!  CORRECTOR STEP FOR PSI SCHEME
!--------------------------------
!
      IF(IOPT1.EQ.3.AND.OPTADV.EQ.2) THEN
!
        CALL FLUX_EF_VF_2(MESH%W%R,MESH%NELEM,
     &                    MESH%IKLE%I,IOPT1,MESH%NPOIN,T4,
!    &                    FI_I,FSTAR,
     &                    T8%R,F%R,T5%R,MESH%SURFAC%R,DDT)
        IF(NCSIZE.GT.1) CALL PARCOM(T8,2,MESH)  
!
!       COMPUTE THE SECOND ORDER CORRECTION FORM
!
        CALL TVF_2(F%R,FN%R,T4%R,UNSV2D%R,DDT,
     &             FLBOUND%R,T7%R,FBOR%R,SMH%R,YASMH,FSCEXP%R,
     &             MESH%NPOIN,MESH%NPTFR,MESH%NBOR%I,
     &             LIMTRA,KDIR,KDDL,
     &             OPTSOU,T5%R,IOPT2,FLBORTRA%R,DDT/DT,RAIN,
     &             PLUIE%R,TRAIN,T8%R)
!                                FI_I
!
      ENDIF
!
!-----------------
! END CORRECTOR STEP
!-----------------
!
      DO I=1,HN%DIM1
!       T4 IS F(N+1)
        T4%R(I)=F%R(I)
      ENDDO
      IF(IOPT2.EQ.1) THEN
        DO I=1,HN%DIM1
          T1%R(I)=T2%R(I)
        ENDDO
      ENDIF
!
      DT_REMAIN=DT_REMAIN-DDT
!
      IF(DT_REMAIN.NE.0.D0.AND.NIT.LT.NITMAX) GO TO 100
!
      IF(NIT.GE.NITMAX) THEN
        IF(LNG.EQ.1) WRITE(LU,900) NIT
        IF(LNG.EQ.2) WRITE(LU,901) NIT
900     FORMAT(1X,'CVTRVF : ',1I6,' SOUS-ITERATIONS DEMANDEES POUR LE'
     &   ,/,1X,   '         SCHEMA VF. DIMINUER LE PAS DE TEMPS')
901     FORMAT(1X,'CVTRVF: ',1I6,' SUB-ITERATIONS REQUIRED FOR THE'
     &   ,/,1X,   '         VF SCHEME. DECREASE THE TIME-STEP')
        CALL PLANTE(1)
        STOP
      ELSEIF(ENTET) THEN
        IF(LNG.EQ.1) WRITE(LU,902) NIT
        IF(LNG.EQ.2) WRITE(LU,903) NIT
902     FORMAT(1X,'CVTRVF (BIEF) : ',1I6,' SOUS-ITERATIONS')
903     FORMAT(1X,'CVTRVF (BIEF): ',1I6,' SUB-ITERATIONS')
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERM
!
      DO I = 1,MESH%NPOIN
        F%R(I) = F%R(I)+DT*SM%R(I)
      ENDDO
!
!     IMPLICIT SOURCE TERM
!
      IF(YASMI) THEN
        DO I = 1,MESH%NPOIN
          F%R(I) = F%R(I)/(1.D0-DT*SMI%R(I)/MAX(H%R(I),1.D-15))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     LOCAL MASS BALANCE (FOR CHECKING PURPOSES)
!
!     CALL OS('X=Y-Z   ',X=T7,Y=T5,Z=HT)
!     WRITE(LU,*)'DIFFERENCE ENTRE H RECALCULE ET H : ',DOTS(T7,T7)
!     CHECKS THE TRACER EQUATION
!     CALL CPSTVC(FBOR,T4)
!     T4 : F AT THE BOUNDARIES AS TAKEN FOR THE BOUNDARY FLUXES
!     DO I=1,NPTFR
!       IF(LIMTRA(I).EQ.KDIR) THEN
!         T4%R(I)=FBOR%R(I)
!       ELSE
!         T4%R(I)=FN%R(MESH%NBOR%I(I))
!       ENDIF
!     ENDDO
!     CALL OS('X=YZ    ',X=T6,Y=FN,Z=HNT)
!     CALL OS('X=YZ    ',X=T7,Y=F ,Z=HT )
!     MASSETN=P_DOTS(V2DPAR,T6,MESH)
!     MASSET =P_DOTS(V2DPAR,T7,MESH)
!     FXT2   =P_DOTS(FLBOR,T4,MESH)
!     WRITE(LU,*)'MASSE INIT: ',MASSETN,' MASSE FINALE: ',MASSET
!     WRITE(LU,*)'FLUX: ',FXT2
!     MASSETN = MASSETN - FXT2*DT
!     TSOU=0.D0
!     IF(YASMH) THEN
!       IF(OPTSOU.EQ.1) THEN
!         DO I=1,MESH%NPOIN
!           MASSETN=MASSETN
!    &             +DT*V2DPAR%R(I)*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
!           TSOU=TSOU+DT*V2DPAR%R(I)*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
!         ENDDO
!       ELSEIF(OPTSOU.EQ.2) THEN
!         DO I=1,MESH%NPOIN
!           MASSETN=MASSETN
!    &             +DT*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
!           TSOU=TSOU+DT*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
!         ENDDO
!       ENDIF
!     ENDIF
!     WRITE(LU,*)'CREATION PAR SOURCE : ',TSOU
!     WRITE(LU,*)'ERREUR DE MASSE DE TRACEUR VF : ',MASSETN-MASSET
!     CHECKS THE CONTINUITY EQUATION
!     DO I = 1,MESH%NPOIN
!       T5%R(I)=V2DPAR%R(I)*(HT%R(I)-HNT%R(I))
!     ENDDO
!     DO I = 1,MESH%NSEG
!       T5%R(MESH%GLOSEG%I(I)) =
!    &  T5%R(MESH%GLOSEG%I(I)) + DT*MESH%MSEG%X%R(I)
!       T5%R(MESH%GLOSEG%I(I+MESH%NSEG)) =
!    &  T5%R(MESH%GLOSEG%I(I+MESH%NSEG)) - DT*MESH%MSEG%X%R(I)
!     ENDDO
!     DO I = 1,MESH%NPTFR
!       T5%R(MESH%NBOR%I(I))=T5%R(MESH%NBOR%I(I))+DT*FLBOR%R(I)
!     ENDDO
!     IF(YASMH) THEN
!       IF(OPTSOU.EQ.1) THEN
!         DO I = 1,MESH%NPOIN
!           T5%R(I)=T5%R(I)-DT*V2DPAR%R(I)*SMH%R(I)
!         ENDDO
!       ELSEIF(OPTSOU.EQ.2) THEN
!         DO I = 1,MESH%NPOIN
!           T5%R(I)=T5%R(I)-DT*SMH%R(I)
!         ENDDO
!       ENDIF
!     ENDIF
!     MASSET=0.D0
!     MASSETN = 0.D0
!     DO I = 1,MESH%NPOIN
!       MASSET=MASSET+T5%R(I)
!       MASSETN=MAX(MASSETN,ABS(T5%R(I)))
!     ENDDO
!     WRITE(LU,*)'ERREUR DE MASSE GLOBALE : ',MASSET,' LOCALE : ',MASSETN
!
!-----------------------------------------------------------------------
!
!     RETURNS POINTERS HT AND HNT
!
      HT%R =>SAVE_HT
      HNT%R=>SAVE_HNT
!
!-----------------------------------------------------------------------
!
      RETURN
      END


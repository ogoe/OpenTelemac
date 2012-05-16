!                    *****************
                     SUBROUTINE CVDFTR
!                    *****************
!
     &(F,FTILD,FN,FSCEXP,DIFT,ICONVF,CONV,
     & H,HN,HPROP,TETAH,UCONV,VCONV,DM1,ZCONV,SOLSYS,
     & VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,AM1,AM2,
     & ZF,FBOR,AFBOR,BFBOR,LIMTRA,MASKTR,MESH,W,TB,
     & T1,T2,T3,T4,T5,T6,T7,T10,TE1,TE2,TE3,KDIR,KDDL,KENT,DT,ENTET,
     & TETAT,AGGLOT,INFOGT,BILAN,OPTSUP,
     & ISOUSI,LT,NIT,OPDTRA,OPTBAN,MSK,MASKEL,MASKPT,MBOR,
     & S,MASSOU,OPTSOU,SLVTRA,FLBOR,V2DPAR,UNSV2D,OPTVF,FLBORTRA,
     & FLULIM,YAFLULIM,DIRFLU)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DIFFUSION, ADVECTION AND SOURCE TERMS FOR A TRACER.
!code
!+  THE EQUATION SOLVED IS :
!+
!+
!+          N+1                                            TILD
!+         F           1                                  F   + DT*SM
!+      ---------  -  ---  DIV ( H VISC * GRAD ( F   )) = ____________
!+         DT          H                                      DT
!+
!+                                                      N+1
!+                                  + SOURCES  + SMI * F
!+                                                     ___
!+                                                     H
!+
!+     WITH :    N+1  TILD   N
!+              F   ,F     ,F  =    DIFFUSED FUNCTION
!+              VISC           =    TURBULENT VISCOSITY
!+              SM             =    SECOND MEMBER (SOURCE TERMS)
!+              TETAT          =    IMPLICITATION COEFFICIENT
!+              DT             =    TIME STEP
!+                                         N+1              N
!+              F              =    TETAT F  + (1-TETAT) * F
!+              SMI            =    IMPLICIT SOURCE TERM
!+
!+
!+                    TILD       N
!+     DISTINGUISHES F     FROM F   IN CASE A FRACTIONAL STEPS METHOD
!+
!+     HAD BEEN PREVIOUSLY PERFORMED (ADVECTION FOR EXAMPLE) GIVING
!+
!+      TILD       N
!+     F     FROM F
!+
!+-----------------------------------------------------------------------
!+
!+      BOUNDARY CONDITIONS :
!+
!+      ==>   NEUMANN CONDITION
!+
!+      VISC DF/DN = AFBOR . F  +  BFBOR
!+
!+
!+      ==>   DIRICHLET CONDITION
!+
!+            TREATED BY MODIFICATION OF THE EQUATIONS IN THE
!+            SUBROUTINE DIRICH
!
!note     JMH : W IS NOT USED.
!
!warning  MATDIF DOES NOT GIVE THE DIFFUSION MATRIX. IT MISSES THE
!+            BOUNDARY TERMS AND THERE IS A MINUS SIGN WHICH IS TAKEN
!+            INTO ACCOUNT HERE
!warning  AFBOR AND BFBOR MUST BE 0 FOR THE BOUNDARY ELEMENTS
!+            WITH NO FRICTION
!warning  BEWARE DISCRETISATION OF VISC
!
!history  JMH
!+        27/02/2009
!+
!+   CALLS CVTFVF_POS, OPTION 14
!
!history  J-M HERVOUET (LNHE)     ; C MOULIN (LNH)
!+        29/12/2009
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
!| AFBOR,BFBOR    |-->| COEFFICIENTS OF NEUMANN CONDITION
!|                |   | VISC*DF/DN = AFBOR*F + BFBOR
!|                |   | GIVEN FOR EVERY BOUNDARY POINT
!| AGGLOT         |-->| MASS-LUMPING COEFFICIENT FOR T.
!| AM1            |<->| MATRIX.
!| AM2            |<->| MATRIX.
!| BILAN          |-->| LOGICAL, IF YES A BALANCE OF MASS EXCHANGES HAS
!|                |   | TO BE DONE.
!| CONV           |-->| IF YES ADVECTION OF F
!| DIFT           |-->| IF YES, DIFFUSION IS DONE
!| DIRFLU         |-->| 1: PRIORITY TO DIRICHLET VALUES (CALL DIRICH DONE)
!|                |   | 2: PRIORITY TO FLUXES (CALL DIRICH NOT DONE)
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
!| FTILD          |-->| F AFTER ADVECTION
!| HPROP          |-->| WORK ARRAY
!| ICONVF         |-->| OPTION FOR ADVECTION TERMS
!|                |   | ICONVF = 1 : CHARACTERISTICS.
!|                |   | ICONVF = 2 : S.U.P.G.
!|                |   | ICONVF = 3 : CONSERVATIVE FINITE VOLUMES
!|                |   | ICONVF = 4 : IDEM
!|                |   | ICONVF = 6 : NON CONSERVATIVE PSI SCHEME.
!|                |   | ICONVF = 7 : NON CONSERVATIVE N SCHEME.
!|                |   | ICONVF =13 : EDGE BY EDGE FORM OF 3
!|                |   | ICONVF =14 : IDEM
!| INFOGT         |-->| LOGICAL, IF YES INFORMATION ON SOLVER WILL BE 
!|                |   | PRINTED.
!| ISOUSI         |-->| SUB-ITERATION NUMBER
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR INFLOW POINT
!| LIMTRA         |-->| BOUNDARY CONDITIONS ON BOOUNDARY POINTS
!| LT,NIT         |-->| CURRENT TIME-STEP, TOTAL NUMBER OF STEPS
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!| MASSOU         |-->| MASS OF TRACER ADDED BY SOURCE TERM
!|                |   | SEE DIFSOU
!| MBOR           |-->| BOUNDARY MATRIX
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| OPDTRA         |-->| OPTION FOR THE DIFFUSION OF TRACERS
!| OPTBAN         |-->| OPTION FOR THE TREATMENT OF TIDAL FLATS
!|                |   | 1:NORMAL   2:WITH MASKING
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| OPTSUP         |-->| SUPG OPTION
!|                |   | 1: CLASSIC SUPG
!|                |   | 2: MODIFIED SUPG
!| OPTVF          |-->| OPTIONS FOR FINITE VOLUMES (SEE CVTRVF)
!| S              |-->| VOID STRUCTURE
!| SLVTRA         |-->| SOLVER CONFIGURATION (SLVCFG) STRUCTURE 
!|                |   | CONTAINING DATA FOR CALLING SOLVE
!| SM             |-->| SOURCE TERMS.
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SMI            |-->| IMPLICIT SOURCE TERM
!| SOLSYS         |-->| 1 OR 2. IF 2 ADVECTION FIELD IS UCONV + DM1*GRAD(ZCONV)
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| TB             |<->| BLOCK OF WORK BIEF_OBJ STRUCTURES (CONTAINS T1,...)
!| TE1,TE2,TE3    |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TETAH          |-->| IMPLICITATION BETWEEN H AND HN
!| TETAT          |-->| IMPLICITATION COEFFICIENT OF ADVECTION
!| UCONV,VCONV    |-->| ADVECTION VELOCITY FIELD
!| UNSV2D         |-->| =1/V2DPAR
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS (ASSEMBLED IN PARALLEL)
!| VISC           |-->| VISCOSITY COEFFICIENTS ALONG X,Y AND Z .
!|                |   | IF P0 : PER ELEMENT
!|                |   | IF P1 : PERR POINT
!| VISC_S         |<->| WORK ARRAY FOR SAVING VISC
!| W              |-->| WORK ARRAY OF DIMENSION :
!|                |   | NELMAX * (NOMBER OF POINTS IN AN ELEMENT)
!| YASMH          |-->| IF YES SMH TAKEN INTO ACCOUNT
!| YASMI          |-->| IF YES SMI TAKEN INTO ACCOUNT
!| ZCONV          |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| ZF             |-->| BOTTOM ELEVATION.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CVDFTR => CVDFTR
      USE DECLARATIONS_TELEMAC, ONLY : ADV_CAR,ADV_SUP,ADV_NSC,ADV_PSI,
     &   ADV_PSI_NC,ADV_NSC_NC,ADV_LPO,ADV_NSC_TF,ADV_PSI_TF,ADV_LPO_TF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: ICONVF,ISOUSI,OPTSUP,OPDTRA,KENT
      INTEGER, INTENT(IN)           :: LT,NIT,OPTBAN,OPTSOU,KDIR,SOLSYS
      INTEGER, INTENT(IN)           :: KDDL,OPTVF,DIRFLU
      DOUBLE PRECISION, INTENT(IN)  :: TETAT,AGGLOT,TETAH,DT
      DOUBLE PRECISION, INTENT(INOUT)  :: MASSOU
      LOGICAL, INTENT(IN)           :: INFOGT,BILAN,CONV,YASMH
      LOGICAL, INTENT(IN)           :: DIFT,MSK,ENTET,YASMI,YAFLULIM
      TYPE(SLVCFG), INTENT(INOUT)   :: SLVTRA
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,MASKPT,H,HN,AFBOR,BFBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: F,SM,FLBORTRA
      TYPE(BIEF_OBJ), INTENT(IN)    :: FBOR,UCONV,VCONV,ZF
      TYPE(BIEF_OBJ), INTENT(IN)    :: FTILD,FN,SMI,FLULIM
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SMH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TE1,TE2,TE3,W
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T10
      TYPE(BIEF_OBJ), INTENT(IN)    :: FSCEXP,DM1,ZCONV
      TYPE(BIEF_OBJ), INTENT(IN)    :: S,LIMTRA,FLBOR,V2DPAR,UNSV2D
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VISC_S,VISC
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AM1,AM2,MBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TB
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKTR
      TYPE(BIEF_MESH) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION C,CFLMAX
!
      INTEGER IELMF,IELMH,IELMS,IELMU,MSKNEU,I,N,IOPT
!
      LOGICAL MSQ,FV_SCHEME
!
      CHARACTER*16 FORMUL
!
!-----------------------------------------------------------------------
!
      IELMF = F%ELM
      IELMH = H%ELM
      IELMS = SM%ELM
      IELMU = UCONV%ELM
!
!-----------------------------------------------------------------------
!
!     IS IT A FINITE VOLUME SCHEME FOR ADVECTION ?
!
      FV_SCHEME=.FALSE.
      IF(  ICONVF.EQ.ADV_LPO.OR.ICONVF.EQ.ADV_LPO_TF.OR.
     &     ICONVF.EQ.ADV_NSC.OR.ICONVF.EQ.ADV_NSC_TF.OR.
     &     ICONVF.EQ.ADV_PSI.OR.ICONVF.EQ.ADV_PSI_TF     ) THEN
        FV_SCHEME=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CASE WHERE H AND T DON'T HAVE THE SAME DISCRETISATION
!
      IF(IELMF.NE.IELMS) THEN
        CALL CHGDIS(SM ,IELMS,IELMF,MESH)
      ENDIF
      IF(IELMF.NE.IELMH.AND.YASMH) THEN
        CALL CHGDIS(SMH ,IELMH,IELMF,MESH)
      ENDIF
!
!
!-----------------------------------------------------------------------
!
!     SEMI-IMPLICITATION OF THE DEPTH
!     WITH SCHEME 5 FOR H, TETAH=0
!
      CALL OS( 'X=CY    ' , X=HPROP , Y=H     , C=      TETAH )
      CALL OS( 'X=X+CY  ' , X=HPROP , Y=HN    , C= 1.D0-TETAH )
      CALL OS( 'X=Y     ' , X=T10   , Y=HPROP )
      IF(IELMF.NE.IELMH) THEN
        CALL CHGDIS(T10,IELMH,IELMF,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE VARIABLES
!
!     SOLUTION INITIALISED TO F AT TIME N
      IF(ISOUSI.EQ.1) CALL OS( 'X=Y     ' , X=F , Y=FN )
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!  IF SUPG, BUILDS THE SEMI-IMPLICIT MATRIX + SUPG IN AM2
!
      IF(ICONVF.EQ.ADV_SUP.AND.CONV) THEN
!
!       TERM IN U.GRAD(T) CENTERED:
!
        CALL MATRIX(AM2,'M=N     ','MATVGR          ',IELMF,IELMF,
     &              1.D0,S,S,S,UCONV,VCONV,S,
     &              MESH,MSK,MASKEL)
!
!       ADDS SUPG CONTRIBUTION TO AM2
!
        IF(OPTSUP.EQ.1) THEN
!         CLASSICAL SUPG
          CALL KSUPG(TE1,TE2,1.D0,UCONV,VCONV,MESH)
          CALL MATRIX(AM2,'M=M+N   ','MASUPG          ',IELMF,IELMF,
     &                1.D0,TE1,TE2,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
!
        ELSEIF(OPTSUP.EQ.2) THEN
!         MODIFIED SUPG
          CALL MATRIX(AM2,'M=M+N   ','MAUGUG          ',IELMF,IELMF,
     &                0.5D0*DT,S,S,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
        ENDIF
!
!     NON CONSERVATIVE, IMPLICIT N-SCHEME EQUATION
      ELSEIF(ICONVF.EQ.ADV_NSC_NC) THEN
!
!       TERM IN U.GRAD(T) (IMPLICIT N-SCHEME)
!
        CALL MATRIX(AM2,'M=N     ','MATVGR         N',IELMF,IELMF,
     &              1.D0,S,S,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   COMPUTES AM1: MASS MATRIX MULTIPLIED BY 1/DT
!
      IF(DIFT.OR..NOT.FV_SCHEME.OR..NOT.CONV.OR.BILAN) THEN
!
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
        CALL OS('X=Y+Z   ',T2,ZF,HN,C)
        CALL DECVRT(TE3,T2,ZF,MESH)
!       MASS MATRIX LOCALLY LUMPED ON THE TIDAL FLATS
        FORMUL='MSLUMP          '
!       WILL USE MSQ TO MASK THE DIFFUSION
        MSQ=.TRUE.
        IF(MSK) CALL OS('X=XY    ',TE3,MASKEL,MASKEL,C)
      ELSE
!       NORMAL MASS MATRIX
        FORMUL='MATMAS          '
!       MASK FOR THE DIFFUSION = MASKEL
        IF(MSK) CALL OS('X=Y     ',TE3,MASKEL,MASKEL,C)
        MSQ=MSK
      ENDIF
      CALL MATRIX(AM1,'M=N     ',FORMUL,IELMF,IELMF,
     &            1.D0/DT,TE3,S,S,S,S,S,MESH,MSK,MASKEL)
!
!   POSSIBLE MASS-LUMPING
!
      IF(AGGLOT.GT.0.001D0) THEN
        CALL LUMP(T1,AM1,MESH,AGGLOT)
        CALL OM( 'M=CN    ' , AM1 , AM1 , S  , 1.D0-AGGLOT , MESH )
        CALL OM( 'M=M+D   ' , AM1 , AM1 , T1 , C           , MESH )
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(BILAN) THEN
!
        CALL MATVEC( 'X=AY    ',T2,AM1,SM,C,MESH)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(T2,2,MESH)
          MASSOU = MASSOU + P_DOTS(T2,T10,MESH)
        ELSE
          MASSOU = MASSOU + DOTS(T2,T10)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   COMPUTES THE SECOND MEMBERS
!
!     COMPUTES DT * SM IN T2
!
!     CVTRVF AND CVTRVF_POS WILL TREAT SM IN A DIFFERENT WAY
      IF(.NOT.FV_SCHEME) THEN
        CALL OS( 'X=CY    ' , X=T2 , Y=SM , C=DT )
      ENDIF
!
!=======================================================================
! TREATS THE VARIOUS TYPES OF ADVECTION:
!-----------------------------------------------------------------------
!
      IF(ICONVF.EQ.ADV_CAR.OR..NOT.CONV) THEN
!
        CALL OS( 'X=X+Y   ' , X=T2 , Y=FTILD )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
!
!-----------------------------------------------------------------------
!
      ELSEIF(ICONVF.EQ.ADV_SUP.AND.CONV) THEN
!
!       AM1 MADE NONSYMMETRICAL IF IT WAS NOT ALREADY
!
        IF(AM1%TYPEXT.NE.'Q') THEN
          CALL OM( 'M=X(M)  ' , AM1 , AM1 , S , C , MESH )
        ENDIF
!
!       SUPG CONTRIBUTION TO THE MASS MATRIX
!
        IF(OPTSUP.EQ.1) THEN
!         CLASSICAL SUPG
!         TE1 AND TE2 ALREADY COMPUTED
          CALL MATRIX(AM1,'M=M+TN    ','MATVGR          ',IELMF,IELMF,
     &                1.D0/DT,S,S,S,TE1,TE2,S,MESH,MSK,MASKEL)
!
        ELSEIF(OPTSUP.EQ.2) THEN
!         MODIFIED SUPG
          CALL MATRIX(AM1,'M=M+TN    ','MATVGR          ',IELMF,IELMF,
     &                0.5D0,S,S,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
        ENDIF
!
!       END OF THE SUPG CONTRIBUTION TO THE MASS MATRIX
!
        CALL OS( 'X=X+Y   ' , T2 , FN , FN , C )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
!
! EXPLICIT ADVECTION TERM:
!
        CALL MATVEC( 'X=X+CAY ',SM,AM2,FN,TETAT-1.D0,MESH)
!
! ADDS THE IMPLICIT ADVECTION PART IN AM2 TO AM1
!
        CALL OM( 'M=M+CN  ' , AM1,AM2 , S , TETAT , MESH )
!
!-----------------------------------------------------------------------
!
      ELSEIF(ICONVF.EQ.ADV_NSC_NC.AND.CONV) THEN
!
!       AM1 MADE NONSYMMETRICAL IF IT WAS NOT ALREADY
!
        IF(AM1%TYPEXT.NE.'Q') THEN
          CALL OM( 'M=X(M)  ' , AM1 , AM1 , S , C , MESH )
        ENDIF
!
        CALL OS( 'X=X+Y   ' , T2 , FN , FN , C )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
!
! EXPLICIT ADVECTION TERM:
!
        CALL MATVEC( 'X=X+CAY ',SM,AM2,FN,TETAT-1.D0,MESH)
!
! ADDS THE IMPLICIT ADVECTION PART IN AM2 TO AM1
!
        CALL OM( 'M=M+CN  ' , AM1,AM2 , S , TETAT , MESH )
!
!-----------------------------------------------------------------------
!
      ELSEIF(ICONVF.EQ.ADV_PSI_NC.AND.CONV) THEN
!
! PSI SCHEME
!
!       TRADITIONAL AM1 * FN TERM
!
        CALL OS( 'X=X+Y   ' , T2 , FN , FN , C )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
!
!       EXPLICIT ADVECTION TERM (PSI SCHEME)
!
        CALL VGFPSI(T5,IELMF,UCONV,VCONV,FN,DT,-1.D0,CFLMAX,
     &              T6,T7,MESH,MSK,MASKEL)
        CALL OS( 'X=X+Y   ' , SM , T5 , T5 , C )
!
!-----------------------------------------------------------------------
!
      ELSEIF( (ICONVF.EQ.ADV_LPO.OR.
     &         ICONVF.EQ.ADV_NSC.OR.
     &         ICONVF.EQ.ADV_PSI    ).AND.CONV ) THEN
!
! CONSERVATIVE EQUATION, DISTRIBUTIVE SCHEMES (LEO POSTMA, N AND PSI)
!                        LEO POSTMA AND N-SCHEME ARE THE SAME IN 2D
!
!       TO BE REMOVED WHEN ALL CALLS TO CVDFTR ARE CHECKED
!       OPTVF SHOULD BE 0 (VELOCITY FIELD OBEYS THE CONTINUITY EQUATION)
!       OR 10 (VELOCITY FIELD DOES NOT OBEY THE CONTINUITY EQUATION)
        IOPT=10*(OPTVF/10)
!       OPTION TO DISTRIBUTE THE FLUXES (HERE 2 OR 3)
        IF(ICONVF.EQ.ADV_LPO) IOPT=IOPT+2
        IF(ICONVF.EQ.ADV_NSC) IOPT=IOPT+2
        IF(ICONVF.EQ.ADV_PSI) IOPT=IOPT+3
!
        IF(TB%N.LT.22) THEN
          WRITE(LU,*) 'SIZE OF TB TOO SMALL IN CVDFTR'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL CVTRVF(F,FN,FSCEXP,DIFT,CONV,H,HN,HPROP,UCONV,VCONV,
     &              DM1,ZCONV,SOLSYS,VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,
     &              FBOR,MASKTR,MESH,
     &              TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &              TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &              TB%ADR(19)%P,TB%ADR(20)%P,TB%ADR(21)%P,
     &              TB%ADR(22)%P,
     &              AGGLOT,TE1,DT,ENTET,BILAN,
     &              OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,
!                                                       YAFLBOR
     &              LIMTRA%I,KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &              V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT)
!       IF EXITS AT THIS POINT, THE DIRICHLET ARE NOT DONE, ALSO WORKS
!       CAN THEN CHECK THE MASS CONSERVATION EXACTLY
        IF(.NOT.DIFT) RETURN
        CALL MATVEC( 'X=AY    ',SM,AM1,F,C,MESH)
!
!-----------------------------------------------------------------------
!
      ELSEIF( (ICONVF.EQ.ADV_LPO_TF.OR.
     &         ICONVF.EQ.ADV_NSC_TF.OR.
     &         ICONVF.EQ.ADV_PSI_TF    ).AND.CONV ) THEN
!
! EDGE-BASED VERSIONS, FOR TIDAL FLATS
! CONSERVATIVE EQUATION, DISTRIBUTIVE SCHEMES (LEO POSTMA, N AND PSI)
!                        LEO POSTMA AND N-SCHEME ARE THE SAME IN 2D
!
!       OPTION TO DISTRIBUTE THE FLUXES (HERE 2 OR 3 AND 12 OR 13)
        IOPT=10*(OPTVF/10)
        IF(ICONVF.EQ.ADV_LPO_TF) IOPT=IOPT+2
        IF(ICONVF.EQ.ADV_NSC_TF) IOPT=IOPT+2
        IF(ICONVF.EQ.ADV_PSI_TF) IOPT=IOPT+3
        IF(TB%N.LT.22) THEN
          WRITE(LU,*) 'SIZE OF TB TOO SMALL IN CVDFTR'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL CVTRVF_POS(F,FN,FSCEXP,DIFT,CONV,H,HN,HPROP,UCONV,VCONV,
     &              DM1,ZCONV,SOLSYS,VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,
     &              FBOR,MASKTR,MESH,
     &              TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &              TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &              TB%ADR(19)%P,TB%ADR(20)%P,TB%ADR(21)%P,
     &              TB%ADR(22)%P,
     &              AGGLOT,TE1,DT,ENTET,BILAN,
     &              OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,
!                                                       YAFLBOR
     &              LIMTRA%I,KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &              V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT,
     &            MESH%GLOSEG%I(                 1:  MESH%GLOSEG%DIM1),
     &            MESH%GLOSEG%I(MESH%GLOSEG%DIM1+1:2*MESH%GLOSEG%DIM1),
     &            MESH%NBOR%I,2,FLULIM%R,YAFLULIM)
!                             2:HARDCODED OPTION FOR ALGORITHM
!                               INDEPENDENT OF SEGMENT NUMBERING.
!       IF EXITS AT THIS POINT, THE DIRICHLET ARE NOT DONE, ALSO WORKS
!       CAN THEN CHECK THE MASS CONSERVATION EXACTLY
        IF(.NOT.DIFT) RETURN
        CALL MATVEC( 'X=AY    ',SM,AM1,F,C,MESH)
      ELSE
!
!-----------------------------------------------------------------------
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CVDFTR : OPTION DE CONVECTION INCONNUE : ',ICONVF
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CVDFTR: UNKNOWN ADVECTION OPTION : ',ICONVF
        ENDIF
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!
      ENDIF
!
! END OF TREATMENT OF THE VARIOUS TYPES OF ADVECTION:
!=======================================================================
!
!                   COMPUTES THE MATRICES
!
!-----------------------------------------------------------------------
!   COMPUTES AM2 : - DIFFUSION MATRIX, AND BOUNDARY TERMS
!
      IF(DIFT) THEN
!
        IF(OPDTRA.EQ.2) THEN
!             SAVES THE DIFFUSION
              CALL OS('X=Y     ',VISC_S,VISC,VISC,C)
!             MULTIPLIES THE DIFFUSION BY HPROP
           CALL OV_2('X=XY    ',VISC%R,1,T10%R,1,T10%R,1,C,
     &                          VISC%MAXDIM1,VISC%DIM1)
           IF(VISC%DIM2.EQ.3) THEN
           CALL OV_2('X=XY    ',VISC%R,2,T10%R,1,T10%R,1,C,
     &                          VISC%MAXDIM1,VISC%DIM1)
           CALL OV_2('X=XY    ',VISC%R,3,T10%R,1,T10%R,1,C,
     &                          VISC%MAXDIM1,VISC%DIM1)
           ENDIF
        ENDIF
!
!       COMPUTES THE DIFFUSION MATRIX (OPTION WITH MONOTONICITY)
!
        CALL MATRIX(AM2,'M=N     ','MATDIF       MON',IELMF,IELMF,
     &              1.D0,S,S,S,VISC,S,S,MESH,MSQ,TE3)
!
        IF(OPDTRA.EQ.2) THEN
!         MULTIPLIES THE MATRIX BY 1/HPROP
          CALL OS( 'X=1/Y   ',T4,T10,T10,C,
     &             IOPT=2,INFINI=0.D0,ZERO=1.D-2)
          CALL OM( 'M=X(M)  ' , AM2 , AM2 , S  , C , MESH )
          CALL OM( 'M=DM    ' , AM2 , AM2 , T4 , C , MESH )
!         RETURNS THE DIFFUSION
          CALL OS('X=Y     ',VISC,VISC_S,VISC_S,C)
        ENDIF
!
!   TAKES THE BOUNDARY TERMS INTO ACCOUNT IN THE DIFFUSION MATRIX
!
        MSKNEU=3
        CALL MATRIX(MBOR,'M=N     ','FMATMA          ',
     &              IELBOR(IELMF,1),IELBOR(IELMF,1),
     &              -1.D0,AFBOR,S,S,S,S,S,
     &              MESH,.TRUE.,MASKTR%ADR(MSKNEU)%P)
        CALL OM( 'M=M+N   ' , AM2 , MBOR , S , C , MESH )
!
!       EXPLICIT DIFFUSION TERM
!
        CALL MATVEC( 'X=AY    ',T1,AM2,FN,C,MESH)
        CALL OS( 'X=X+CY  ' , SM , T1 , T1 , TETAT-1.D0 )
!
!       IMPLICIT DIFFUSION TERM ( AM1 + TETAT * AM2 )
!
        IF(AM1%TYPEXT.NE.'Q'.AND.AM2%TYPEXT.EQ.'Q') THEN
          CALL OM( 'M=X(M)  ' , AM1 , AM1 , S , C , MESH )
        ENDIF
        CALL OM( 'M=M+CN  ' , AM1,AM2 , S , TETAT , MESH )
!
!       BOUNDARY STRESS TERMS
!
        CALL VECTOR(T2,'=','MASVEC          ',IELBOR(IELMF,1),
     &              1.D0,BFBOR,S,S,S,S,S,MESH,
     &              .TRUE.,MASKTR%ADR(MSKNEU)%P)
        CALL OSDB( 'X=X+Y   ' , SM , T2 , T2 , C , MESH )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   TAKES INTO ACCOUNT THE IMPLICIT TERM RESULTING FROM THE POINT SOURCES:
!
      IF(YASMH.AND..NOT.(FV_SCHEME.AND.CONV)) THEN
!
        IF(OPTSOU.EQ.1) THEN
!         JMH MODIFICATION 23/09/98
          CALL VECTOR(T2,'=','MASVEC          ',IELMF,
     &                1.D0,SMH,S,S,S,S,S,MESH,MSK,MASKEL)
          CALL OS( 'X=Y/Z   ' ,T1,T2,T10,C,
     &              IOPT=2,INFINI=0.D0,ZERO=1.D-3)
!         IMPLICIT PART OF THE POINT SOURCE TERM
!         - TETAT T 1/HPROP SUM ( SCE PSI D(OMEGA)
          CALL OS( 'X=CX    ' , T1 , T1 , T1 , TETAT )
          CALL OM( 'M=M+D   ' , AM1 , AM1 , T1 , TETAT , MESH )
!         PREPARES THE EXPLICIT PART
          CALL OS( 'X=YZ    ' , T1 , SMH , FSCEXP , C )
          CALL VECTOR(T2,'=','MASVEC          ',IELMF,
     &                1.D0,T1,S,S,S,S,S,MESH,MSK,MASKEL)
          CALL OS( 'X=Y/Z   ' ,T1,T2,T10,C,
     &             IOPT=2,INFINI=0.D0,ZERO=1.D-3)
          CALL OS( 'X=X+Y   ' , SM , T1 , T1 , C )
        ELSEIF(OPTSOU.EQ.2) THEN
          CALL OS( 'X=Y/Z   ' ,T1,SMH,T10,C,
     &              IOPT=2,INFINI=0.D0,ZERO=1.D-3)
!         EXPLICIT PART OF THE POINT SOURCE TERM
!         1/HPROP (FSCE-(1-TETAT)FN) SMH
          CALL OS( 'X=X+YZ  ' , SM , T1 , FSCEXP , C )
!         IMPLICIT PART OF THE POINT SOURCE TERM
!         - TETAT T 1/HPROP SUM ( SCE PSI D(OMEGA)
          CALL OS( 'X=CX    ' , T1 , T1 , T1 , TETAT )
          CALL OM( 'M=M+D   ' , AM1 , AM1 , T1 , TETAT , MESH )
        ENDIF
!
      ENDIF
!
!   IMPLICIT TERM IF THERE IS ONE :
!
!   THE TREATMENT BELOW ENSURES THAT IF THE EXPLICIT SOURCE TERM
!   IS IN THE FORM  K*FN/H AND SMI EQUALS -K THEN THE TWO TERMS
!   WILL BE BALANCED (CASE OF EROSION AND DEPOSITION)
!
!                  FV_SCHEME : IMPLICIT SOURCE TERM HAS BEEN TREATED
!                  AND WILL NOT BE DONE TWICE
      IF(YASMI.AND..NOT.(FV_SCHEME.AND.CONV)) THEN
        CALL MATRIX(AM2,'M=N     ','MATMAS          ',IELMF,IELMF,
     &              -1.D0,S,S,S,S,S,S,MESH,MSK,MASKEL)
!       POSSIBLE MASS-LUMPING
        IF(AGGLOT.GT.0.001D0) THEN
          CALL LUMP(T1,AM2,MESH,AGGLOT)
          CALL OM( 'M=CN    ' , AM2 , AM2 , S  , 1.D0-AGGLOT , MESH )
          CALL OM( 'M=M+D   ' , AM2 , AM2 , T1 , C           , MESH )
        ENDIF
!       COMPUTES SMI/H (DOES NOT CHECK IF H SIZE IS SUFFICIENT!!)
        IF(OPTBAN.GT.0) THEN
!         DIVIDES BY H WITH HARD-CODED CLIPPING AT 0.01
          CALL CPSTVC(SMI,T4)
          DO I=1,SMI%DIM1
!           CORRECTION JMH 26/04/2012
!           IF(T10%R(I).LT.1.D-2) THEN
            IF(H%R(I).LT.1.D-4) THEN
              T4%R(I)=0.D0
            ELSE
              T4%R(I)=SMI%R(I)/H%R(I)
            ENDIF
          ENDDO
        ELSE
!         DIVIDES WITHOUT CHECKING
          CALL OS( 'X=Y/Z   ',X=T4,Y=SMI,Z=H)
        ENDIF
        CALL OM( 'M=X(M)  ' , AM2 , AM2 , S  , C , MESH )
        CALL OM( 'M=MD    ' , AM2 , AM2 , T4 , C , MESH )
!       ADDS TO MATRIX AM1
        IF(AM1%TYPEXT.NE.'Q') THEN
          CALL OM( 'M=X(M)  ' , AM1 , AM1 , S , C , MESH )
        ENDIF
        CALL OM( 'M=M+N   ' , AM1 , AM2 , S , C , MESH )
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(ICONVF.EQ.ADV_CAR.AND..NOT.DIFT) THEN
        CALL OS( 'X=Y     ' , F , FTILD , FTILD , C )
      ENDIF
      IF(ICONVF.EQ.ADV_PSI_NC.AND.CONV) THEN
        CALL LUMP(T1,AM1,MESH,1.D0)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(T1,2,MESH)
          CALL OS( 'X=Y     ' , T2 , SM , SM , C )
          CALL PARCOM(T2,2,MESH)
          CALL OS( 'X=Y/Z   ' , F , T2 , T1 , C ,2,0.D0,1.D-6)
        ELSE
          CALL OS( 'X=Y/Z   ' , F , SM , T1 , C ,2,0.D0,1.D-6)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!   BOUNDARY CONDITIONS (POINTS OF THE TYPE DIRICHLET)
!
!     DIRICHLET BOUNDARY CONDITIONS ARE TREATED IN A WEAK FORM BY
!     FINITE VOLUME SCHEMES, TO ENSURE THE CORRECT FLUX
!     THEN CALL DIRICH HERE WOULD SPOIL MASS CONSERVATION. HOWEVER
!     FORCING THE CALL IS ALLOWED IN THIS CASE WITH PARAMETER DIRFLU
!
!
      IF(.NOT.FV_SCHEME.OR.DIRFLU.EQ.1) THEN
        CALL DIRICH(F, AM1, SM,FBOR,LIMTRA%I,TB,MESH,KDIR,MSK,MASKPT)
      ENDIF
!
!-----------------------------------------------------------------------
!
!   SOLVES THE LINEAR SYSTEM:
!
      CALL SOLVE(F,AM1,SM,TB,SLVTRA,INFOGT,MESH,AM2)
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE TRACER FLUX AT THE BOUNDARY
!
      IF(.NOT.FV_SCHEME) THEN
        DO I=1,MESH%NPTFR
          N=MESH%NBOR%I(I)
          FLBORTRA%R(I)=FLBOR%R(I)*(TETAT*F%R(N)+(1.D0-TETAT)*FN%R(N))
        ENDDO
!     ELSE
!       FLBORTRA ALREADY COMPUTED
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IELMF.NE.IELMS) CALL CHGDIS(SM  ,IELMF,IELMS,MESH)
      IF(IELMF.NE.IELMH.AND.YASMH) CALL CHGDIS(SMH ,IELMF,IELMH,MESH)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

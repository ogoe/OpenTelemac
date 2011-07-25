!                    *****************
                     SUBROUTINE KEPSIL
!                    *****************
!
     &(AK,EP,AKTILD,EPTILD,AKN,EPN,VISC,CF,U,V,HN,UCONV,VCONV,
     & KBOR,EBOR,LIMKEP,IELMK,IELME,
     & SMK,SME,TM1,MAK,MAE,CM2,TE1,TE2,NPTFR,DT,
     & MESH,T1,T2,T3,TB,CMU,C1,C2,SIGMAK,SIGMAE,ESTAR,SCHMIT,
     & KMIN,KMAX,EMIN,EMAX,
     & INFOKE,KDIR,MSK,MASKEL,MASKPT,S,SLVK,SLVEP,ICONV,OPTSUP)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DIFFUSION STEP FOR SOURCE TERMS (K-EPSILON MODEL).
!
!history  J-M HERVOUET (LNH)
!+        27/11/1992
!+        V5P5
!+
!
!history  L. VAN HAREN (LNH)
!+        30/05/1994
!+
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
!| AK             |<--| TURBULENT KINETIC ENERGY K AT TIME T(N+1)
!| AKN            |-->| TURBULENT KINETIC ENERGY K AT TIME T(N)
!| AKTILD         |-->| TURBULENT KINETIC ENERGY AFTER ADVECTION
!| C1             |-->| CONSTANT OF K-EPSILON MODEL
!| C2             |-->| CONSTANT OF K-EPSILON MODEL
!| CF             |-->| ADIMENSIONAL FRICTION COEFFICIENT
!| CM2            |<->| MATRIX
!| CMU            |-->| CONSTANT OF K-EPSILON MODEL
!| DT             |-->| TIME STEP
!| EBOR           |<--| TURBULENT ENERGY DISSIPATION AT BOUNDARY
!| EMIN           |-->| MINIMUM EPSILON IF CLIPPING
!| EMAX           |-->| MAXIMUM EPSILON IF CLIPPING
!| EP             |<--| TURBULENT ENERGY DISSIPATION AT TIME T(N+1)
!| EPN            |-->| TURBULENT ENERGY DISSIPATION AT TIME T(N)
!| EPTILD         |-->| TURBULENT ENERGY DISSIPATION AFTER ADVECTION
!| ESTAR          |-->| CONSTANT OF K-EPSILON MODEL
!| HN             |-->| WATER DEPTH AT TIME T(N)
!| ICONV          |-->| TYPE OF ADVECTION ON K AND EPSILON
!|                |   | 1 : CHARACTERISTICS
!|                |   | 2 : SUPG, ...
!| IELME          |-->| TYPE OF ELEMENT FOR K
!| IELMK          |-->| TYPE OF ELEMENT FOR EPSILON
!| INFOKE         |-->| IF YES, INFORMATION ON LINEAR SYSTEMS
!| KBOR           |<--| TURBULENTE KINETIC ENERGY ON BOUNDARIES
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KMIN           |-->| MINIMUM K IF CLIPPING
!| KMAX           |-->| K MINIMUM ET MAXIMUM EN CAS DE CLIPPING
!| LIMKEP         |-->| BOUNDARY CONDITIONS ON K AND EPSILON
!| MAE            |<->| MATRIX FOR EPSILON EQUATION
!| MAK            |<->| MATRIX FOR K EQUATION
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPTSUP         |-->| SUPG OPTION
!| S              |-->| VOID STRUCTURE
!| SCHMIT         |-->| CONSTANT OF K-EPSILON MODEL
!| SIGMAE         |-->| CONSTANT OF K-EPSILON MODEL
!| SIGMAK         |-->| CONSTANT OF K-EPSILON MODEL
!| SLVEP          |-->| STRUCTURE WITH SOLVER OPTIONS FOR E
!| SLVK           |-->| STRUCTURE WITH SOLVER OPTIONS FOR K
!| SME            |<--| RIGHT-HAND SIDE OF EPSILON EQUATION
!| SMK            |<--| RIGHT-HAND SIDE OF K EQUATION
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| TB             |<->| BLOCK OF WORK ARRAYS
!| TE1            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE2            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TM1            |<->| DIFFUSION MATRIX
!| UCONV          |-->| X-COMPONENT OF ADVECTION VELOCITY FIELD
!| VCONV          |-->| Y-COMPONENT OF ADVECTION VELOCITY FIELD
!| VISC           |-->| TURBULENT DIFFUSION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SLVCFG), INTENT(INOUT)  :: SLVK,SLVEP
      INTEGER, INTENT(IN)          :: ICONV,NPTFR,KDIR,LIMKEP(NPTFR,2)
      INTEGER, INTENT(IN)          :: OPTSUP,IELMK,IELME
      LOGICAL, INTENT(IN)          :: INFOKE,MSK
      DOUBLE PRECISION, INTENT(IN) :: KMIN,KMAX,EMIN,EMAX,SCHMIT
      DOUBLE PRECISION, INTENT(IN) :: CMU,C1,C2,SIGMAK,SIGMAE,ESTAR
      DOUBLE PRECISION, INTENT(IN) :: DT
!     MATRIX STRUCTURES
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TM1,MAK,MAE,CM2
!     VECTOR STRUCTURES
      TYPE(BIEF_OBJ), INTENT(IN)    :: UCONV,VCONV,AKN,EPN,AKTILD,EPTILD
      TYPE(BIEF_OBJ), INTENT(IN)    :: HN,VISC,U,V,MASKEL,S,MASKPT,CF
      TYPE(BIEF_OBJ), INTENT(IN)    :: KBOR,EBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,AK,EP,SMK,SME,TE1,TE2
!     MESH STRUCTURE
      TYPE(BIEF_MESH) :: MESH
!     BLOCK STRUCTURE
      TYPE(BIEF_OBJ) :: TB
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION C,SL1,CEPS,USTAR,AGGLOK,AGGLOE,TETAK
!
      INTEGER N
!
!-----------------------------------------------------------------------
!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE MASS MATRIX AND THE DIFFUSION MATRIX
!
      SL1 = 1.D0/DT
!
!     -----------------------------
!     COMPUTES THE MASS MATRIX
!     -----------------------------
!
      CALL MATRIX(MAK,'M=N     ','MATMAS          ',IELMK,IELMK,
     &            SL1,S,S,S,S,S,S,MESH,MSK,MASKEL)
      CALL MATRIX(MAE,'M=N     ','MATMAS          ',IELME,IELME,
     &            SL1,S,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     MASS-LUMPING TEST
!
      AGGLOK = 1.D0
      AGGLOE = 1.D0
      IF(AGGLOK.GT.0.001D0) THEN
        CALL LUMP(T1,MAK,MESH,AGGLOK)
        CALL OM( 'M=CN    ' , MAK , MAK , S  , 1.D0-AGGLOK , MESH )
        CALL OM( 'M=M+D   ' , MAK , MAK , T1 , C           , MESH )
      ENDIF
      IF(AGGLOE.GT.0.001D0) THEN
        CALL LUMP(T1,MAE,MESH,AGGLOE)
        CALL OM( 'M=CN    ' , MAE , MAE , S  , 1.D0-AGGLOE , MESH )
        CALL OM( 'M=M+D   ' , MAE , MAE , T1 , C           , MESH )
      ENDIF
!
!     --------------------------------------------------
!     CONCATENATES THE MASS MATRIX: IN T3
!     --------------------------------------------------
!
      CALL LUMP(T3,MAK,MESH,DT)
!
!     ---------------------
!     DIFFUSION MATRIX
!     ---------------------
!
      CALL MATRIX(TM1,'M=N     ','MATDIF          ',IELMK,IELMK,
     &            1.D0,S,S,S,VISC,VISC,VISC,MESH,MSK,MASKEL)
!
!***********************************************************************
!
!     EXPLICIT SOURCE TERMS: T1 FOR K, T2 FOR EPSILON                  *
!                                                                      *
!     EXPLICIT TERM FOR K :                                            *
!                                            3                         *
!                               N           U                          *
!                              K             *
!                              --   +  C  * --  +  PROD
!                              DT       K   H
!
!
!     EXPLICIT TERM FOR EPSILON:
!
!                                            4
!                                N          U              N
!                              EP            *           EP
!                              --   +  C  * --  +  C   * -- * PROD
!                              DT       E    2      E1    N
!                                           H            K
!
!
!                     2        2           2
!                  DU       DV     DU   DV           N
!      PROD = ( 2*(--) + 2*(--) + (-- + --)  ) * VISC
!                  DX       DY     DY   DX
!
!
!                           N
!                         EP
!      THE TERM  +  C1  * -- * PROD   IS WRITTEN AS :
!                          N
!                         K
!
!                               N
!                   C1 * CMU * K * PROD / VISC
!
!***********************************************************************
!
!     --------------------------------
!     TAKES ADVECTION INTO ACCOUNT
!     --------------------------------
!
      IF(ICONV.EQ.1) THEN
!
        CALL MATVEC('X=AY    ',SMK,MAK,AKTILD,C,MESH)
        CALL MATVEC('X=AY    ',SME,MAE,EPTILD,C,MESH)
!
      ELSEIF(ICONV.EQ.2) THEN
!
        CALL MATVEC('X=AY    ',SMK,MAK,AKN,C,MESH)
        CALL MATVEC('X=AY    ',SME,MAE,EPN,C,MESH)
!       CENTERED SEMI-IMPLICIT ADVECTION TERM : MATRIX
        CALL MATRIX(CM2,'M=N     ','MATVGR          ',IELMK,IELMK,
     &              1.D0,S,S,S,UCONV,VCONV,VCONV,MESH,MSK,MASKEL)
!       SUPG CONTRIBUTION
        IF(OPTSUP.EQ.1) THEN
!         CLASSICAL SUPG
          CALL KSUPG(TE1,TE2,1.D0,UCONV,VCONV,MESH)
          CALL MATRIX(CM2,'M=M+N   ','MASUPG          ',IELMK,IELMK,
     &                1.D0,TE1,TE2,S,UCONV,VCONV,VCONV,
     &                MESH,MSK,MASKEL)
        ELSEIF(OPTSUP.EQ.2) THEN
!         MODIFIED SUPG
          CALL MATRIX(CM2,'M=M+N   ','MAUGUG          ',IELMK,IELMK,
     &                0.5D0*DT,S,S,S,UCONV,VCONV,VCONV,
     &                MESH,MSK,MASKEL)
        ENDIF
!       END OF SUPG CONTRIBUTION
!       EXPLICIT RIGHT HAND SIDES
        TETAK=0.6
        CALL MATVEC( 'X=X+CAY ',SMK,CM2,AKN,TETAK-1.D0,MESH)
        CALL MATVEC( 'X=X+CAY ',SME,CM2,EPN,TETAK-1.D0,MESH)
!       ADDS SUPG MATRIX TO MAK AND MAE
        CALL OM( 'M=X(M)  ' , MAK , MAK , S , C , MESH )
        CALL OM( 'M=M+CN  ' , MAK , CM2 , S , TETAK , MESH )
        CALL OM( 'M=X(M)  ' , MAE , MAE , S , C , MESH )
        CALL OM( 'M=M+CN  ' , MAE , CM2 , S , TETAK , MESH )
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,100) ICONV
        IF(LNG.EQ.2) WRITE(LU,101) ICONV
100     FORMAT(1X,'KEPSIL : FORME DE LA CONVECTION INCONNUE : ',1I4)
101     FORMAT(1X,'KEPSIL: UNKNOWN TYPE OF ADVECTION:',1I4)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     ------------------------------------------------
!     CREATION TERM - BOTTOM FRICTION (P)
!     ------------------------------------------------
!
!     BEWARE : MISSES 1.D0/(0.5D0*CF)**0.75 TO GET TRUE CEPS
!     (TAKEN INTO ACCOUNT AFTERWARD)
!
      CEPS = C2 * SQRT(CMU) / SQRT( ESTAR*SCHMIT )
!
      CALL CPSTVC(SMK,T1)
      CALL CPSTVC(SMK,T2)
      DO N=1,T1%DIM1
         USTAR = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2))
!        T1 : PKV TERM OF THE EQUATION FOR K
         T1%R(N)=USTAR**3/MAX(SQRT(0.5D0*CF%R(N))*HN%R(N),1.D-6)
!        LIMITS THE GROWTH OF K DUE TO BOTTOM FRICTION
!        TO 50% PER TIMESTEP
         T1%R(N) = MIN (T1%R(N) , EP%R(N) + 0.5D0*AK%R(N)/DT )
!        T2 : PEV TERM OF THE EQUATION FOR EPSILON
         T2%R(N) = CEPS * USTAR**4 /
     &           MAX(((0.5D0*CF%R(N))**0.75D0)*HN%R(N)**2,1.D-6)
!        LIMITS THE GROWTH OF E TO 50% PER TIMESTEP
         T2%R(N) = MIN(T2%R(N),C2*EP%R(N)**2/MAX(AK%R(N),KMIN)
     &           +0.5D0*EP%R(N)/DT )
      ENDDO
!
      CALL OS( 'X=XY    ' , T1  , T3 , T3 , C )
      CALL OS( 'X=X+Y   ' , SMK , T1 , T1 , C )
      CALL OS( 'X=XY    ' , T2  , T3 , T3 , C )
      CALL OS( 'X=X+Y   ' , SME , T2 , T2 , C )
!
!     -----------------------------------
!     CREATION TERM - SHEAR
!     -----------------------------------
!
      CALL VECTOR(T1,'=','PRODF           ',IELMK,
     &            1.D0,S,S,S,U,V,S,MESH,MSK,MASKEL)
      CALL OS( 'X=XY    ' , T1  , VISC , VISC , C )
!
! TEST JMH : LIMITS TURBULENCE FOR SHALLOW DEPTHS ( < 2CM )
!
      DO N=1,SMK%DIM1
        IF(HN%R(N).LT.0.02D0) T1%R(N)=0.D0
      ENDDO
!
! END OF TEST
!
      CALL OS( 'X=X+Y   ' , SMK , T1   , T1   , C )
!
      CALL VECTOR(T2,'=','PRODF           ',IELMK,
     &            CMU*C1,S,S,S,U,V,S,MESH,MSK,MASKEL)
      CALL OS( 'X=XY    ' , T2  , AK , AK , C )
      CALL OS( 'X=X+Y   ' , SME , T2 , T2 , C )
!
! TEST JMH : LIMITS TURBULENCE FOR SHALLOW DEPTHS ( < 2CM )
!
      DO N=1,SMK%DIM1
        SMK%R(N) = SMK%R(N) * (MIN(HN%R(N),0.02D0)/0.02D0)**2
      ENDDO
!
! END OF TEST
!
!***********************************************************************
!     IMPLICIT SOURCE TERMS : T1 FOR K , T2 FOR EPSILON                *
!                                                                      *
!     IMPLICIT TERM FOR K :           +      EP(N)/K(N) * K (N+1)      *
!     IMPLICIT TERM FOR EPSILON:      + C2 * EP(N)/K(N) * EP(N+1)      *
!***********************************************************************
!
      CALL OS( 'X=Y/Z   ',T1,EP,AK,C  ,IOPT=2,INFINI=0.D0,ZERO=KMIN)
      CALL OS( 'X=CY    ',T2,T1,T1,C2 )
!
!     ---------------------------------------------------
!     INTEGRATES THESE SOURCE TERMS IN THE MATRICES
!     ---------------------------------------------------
!
      CALL OS( 'X=XY    ' , T1 , T3 , T3 , C )
      CALL OS( 'X=XY    ' , T2 , T3 , T3 , C )
!
!     -------------------------------------------
!     ADDS TO THE DIAGONAL OF THE MASS MATRIX
!     -------------------------------------------
!
      CALL OM( 'M=M+D   ' , MAK , MAK , T1 , C , MESH )
      CALL OM( 'M=M+D   ' , MAE , MAE , T2 , C , MESH )
!
!***********************************************************************
!
!     COMBINES THE MASS AND DIFFUSION MATRICES                         *
!                                                                      *
!     MAK = MAK + TM1/SIGMAK                                           *
!     MAE = MAE + TM1/SIGMAE                                           *
!
!***********************************************************************
!
      CALL OM( 'M=M+CN  ' , MAK , TM1 , S , 1.D0/SIGMAK , MESH )
      CALL OM( 'M=M+CN  ' , MAE , TM1 , S , 1.D0/SIGMAE , MESH )
!
!***********************************************************************
!     DIRICHLET TYPE BOUNDARY CONDITIONS
!***********************************************************************
!
      CALL DIRICH(AK,MAK,SMK,KBOR,LIMKEP(1,1),TB,MESH,KDIR,MSK,MASKPT)
      CALL DIRICH(EP,MAE,SME,EBOR,LIMKEP(1,2),TB,MESH,KDIR,MSK,MASKPT)
!
!***********************************************************************
!     SOLVES THE TWO OBTAINED SYSTEMS
!***********************************************************************
!
      CALL SOLVE(AK,MAK,SMK,TB,SLVK ,INFOKE,MESH,TM1)
      CALL SOLVE(EP,MAE,SME,TB,SLVEP,INFOKE,MESH,TM1)
!
!***********************************************************************
!     CLIPS SMALL VALUES                                               *
!***********************************************************************
!
      CALL CLIP(AK,0.D0,.TRUE.,KMAX,.FALSE.,0)
      CALL CLIP(EP,EMIN,.TRUE.,EMAX,.FALSE.,0)
!
!***********************************************************************
!
      RETURN
      END

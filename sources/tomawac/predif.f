!                       *****************
                        SUBROUTINE PREDIF
!                       *****************
!
     &(CX    , CY    , CT    , DT    , X     , Y     , TETA  , COSTET,
     & SINTET, FREQ  , IKLE2 , IFABOR, ETAP1 , TRA01 , SHP   ,
     & SHZ   , ELT   , ETA   , DEPTH , DZX   , DZY   , XK    ,
     & CG    , ITR01 , NPOIN3, NPOIN2, NELEM2, NPLAN , NF    , SURDET,
     & COURAN, SPHE  , PROINF, A     , DFREQ , F     , CCG   , DIV   ,
     & DELTA , DDX   , DDY   , EPS   , NBOR  , NPTFR , XKONPT, RK    ,
     & RX    , RY    , RXX   , RYY   , NEIGB , NB_CLOSE,DIFFRA,MAXNSP,
     & FLTDIF, MESH3D, MESH  , IELM3 , TB    , ISUB  , SIKLE2)
!
!***********************************************************************
! TOMAWAC   V6P3                                   25/06/2012
!***********************************************************************
!
!brief    PREPARES DIFFRACTION.
!+
!+            COMPUTES THE ADVECTION FIELD; TRACES BACK THE
!+                CHARACTERISTICS.
!
!history  E. KRIEZI (LNH)
!+        04/12/2006
!+        V5P5
!+
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+   Modification for V6P2
!+   Taking into account both Mean Sloe Equation model (Berkhoff,1972)
!+      and Revised Mild Slope Equation model (Porter,2003)
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        21/03/2013
!+        V6P3
!+   Call CONWAC added before call to DIFFRAC and DIFFRAC does only the
!+   modification of the velocities.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |<--| AMPLITUDE OF DIRECTIONAL SPECTRUM
!| CCG            |<--| GROUP VELOCITY TIMES PHASE VELOCITY
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COSTET         |-->| COSINE OF TETA ANGLE
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| CX             |<->| ADVECTION FIELD ALONG X(OR PHI)
!| CY             |<->| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| CT             |<->| ADVECTION FIELD ALONG TETA
!| DDX            |<--| X-DERIVATIVE OF A VARIABLE
!| DDY            |<--| Y-DERIVATIVE OF A VARIABLE
!| DELTA          |<--| DIFFRACTION PARAMETER
!| DEPTH          |-->| WATER DEPTH
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| DIFFRA         |-->| IF >0 DIFFRACTION IS CONSIDERED
!|                      IF = 1 MSE FORMULATION
!|                      IF = 2 RMSE FORMULATION
!| DIV            |<--| DIVERGENCE OF FUNCTION USED FOR DELTA COMPUT.
!| DT             |-->| TIME STEP
!| DZX            |-->| BOTTOM SLOPE ALONG X
!| DZY            |-->| BOTTOM SLOPE ALONG Y
!| ELT            |<->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| EPS            |-->| VARIANCE THRESHOLD FOR DIFFRACTION
!| ETA            |<->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETAP1          |<->| HIGHER LAYERS TABLE
!| F              |<->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FLTDIF         |-->| IF TRUE, LOCAL AMPLITUDES ARE FILTERED
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!|                |   | SOLID OR PERIODIC BOUNDARY
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| ISUB           |<->| ARRIVAL SUB-DOMAIN (IN PARALLEL)
!| ITR01          |<->| WORK TABLE
!| MAXNSP         |-->| CONSTANT FOR MESHFREE TECHNIQUE
!| NB_CLOSE       |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NEIGB          |-->| NEIGHBOUR POINTS FOR MESHFREE METHOD
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN3         |-->| NPOIN2*NPLAN
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| RK             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RX             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RXX            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RY             |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| RYY            |-->| ARRAY USED IN THE MESHFREE TECHNIQUE
!| SHP            |<->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |<->| BARYCENTRIC COORDINATES ALONG TETA OF THE
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| SINTET         |-->| SINE OF TETA ANGLE
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| SURDET         |-->| 1/DET. OF ELEMENTS 2D FOR ISOPARAM. TRANSF.
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TRA01          |<->| WORK TABLE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKONPT         |<--| ARRAY USED FOR COMPUTING DIFFRACTION PARAMETER
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC, ONLY : PROMIN,OPTDER,COSF,TGF
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(IN)    :: NPOIN3,NPOIN2,NELEM2,NPLAN,NF,NPTFR
      INTEGER,INTENT(IN)    :: NBOR(NPTFR),DIFFRA ,MAXNSP
      INTEGER,INTENT(IN)    :: ETAP1(NPLAN)
      INTEGER,INTENT(IN)    :: IKLE2(NELEM2,3)
      INTEGER,INTENT(IN)    :: NB_CLOSE(NPOIN2),NEIGB(NPOIN2,MAXNSP)
      INTEGER,INTENT(INOUT) :: ISUB(NPOIN3,NF),IFABOR(NELEM2,7),IELM3
      INTEGER,INTENT(INOUT) :: ELT(NPOIN3,NF), ETA(NPOIN3,NF)
      INTEGER,INTENT(INOUT) :: ITR01(NPOIN3,3)
      DOUBLE PRECISION,INTENT(IN) :: RK(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: RX(MAXNSP,NPOIN2),RY(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: RXX(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: RYY(MAXNSP,NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: XK(NPOIN2,NF),CG(NPOIN2,NF)
      DOUBLE PRECISION,INTENT(IN) :: FREQ(NF)
      DOUBLE PRECISION,INTENT(IN) :: SINTET(NPLAN),COSTET(NPLAN)
      DOUBLE PRECISION,INTENT(IN) :: SURDET(NELEM2)
      DOUBLE PRECISION,INTENT(IN) :: DT,EPS
      DOUBLE PRECISION,INTENT(IN) :: TRA01(NPOIN3,8)
      DOUBLE PRECISION,INTENT(IN) :: DEPTH(NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: DZX(NPOIN2),DZY(NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: DDX(NPOIN2),DDY(NPOIN2),A(NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: DFREQ(NF)
      DOUBLE PRECISION,INTENT(IN) :: F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION,INTENT(IN) :: CCG(NPOIN2),DIV(NPOIN2)
      DOUBLE PRECISION,INTENT(IN) :: DELTA(NPOIN2),XKONPT(NPOIN2)
      LOGICAL,INTENT(IN) :: COURAN,SPHE,PROINF,FLTDIF
      TYPE(BIEF_OBJ), INTENT(IN)    :: TETA,SIKLE2
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SHP,SHZ,CX,CY,CT,TB
      TYPE(BIEF_MESH),INTENT(INOUT) :: MESH3D,MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFF,IEL,I1,I2,I3
      TYPE(BIEF_OBJ) :: BID
      TYPE(SLVCFG)   :: SLVBID
!
!----------------------------------------------------------------------
!
      IF (.NOT.COURAN) THEN
!
!       RELATIVE = ABSOLUTE => ADVECTION IN 3D
!       SEPARATES OUT THE FREQUENCIES
!
        DO IFF=1,NF
!
!         COMPUTES THE ADVECTION FIELD
!
          CALL CONWAC
     &( CX%R  , CY%R  , CT%R , XK    , CG    , COSF  , TGF   , DEPTH ,
     &  DZX   , DZY   , FREQ , COSTET, SINTET, NPOIN2, NPLAN , IFF  ,
     &  NF    , PROINF, SPHE , PROMIN, TRA01)
!
!         MODIFIESS THE ADVECTION FIELD WITH DIFFRACTION
!
          CALL DIFFRAC
     &  (CX%R,CY%R,CT%R,XK,CG,DEPTH,DZX,DZY,FREQ,COSTET,SINTET,
     &   NPOIN2,NPLAN,IFF,NF,PROINF,SPHE,A,DFREQ,
     &   F,CCG,DIV,DELTA,DDX,DDY,EPS,NBOR,NPTFR,XKONPT,
     &   RK,RX,RY,RXX,RYY,NEIGB,NB_CLOSE,DIFFRA,MAXNSP,FLTDIF,OPTDER)
!
          DO IEL=1,NELEM2
            I1=IKLE2(IEL,1)
            I2=IKLE2(IEL,2)
            I3=IKLE2(IEL,3)
            IF(DEPTH(I1).LT.PROMIN.AND.DEPTH(I2).LT.PROMIN.AND.
     &         IFABOR(IEL,1).GT.0) IFABOR(IEL,1)=-1
            IF(DEPTH(I2).LT.PROMIN.AND.DEPTH(I3).LT.PROMIN.AND.
     &         IFABOR(IEL,2).GT.0) IFABOR(IEL,2)=-1
            IF(DEPTH(I3).LT.PROMIN.AND.DEPTH(I1).LT.PROMIN.AND.
     &         IFABOR(IEL,3).GT.0) IFABOR(IEL,3)=-1
          ENDDO
!
          WRITE(LU,*) 'FREQUENCE :',IFF
!
          CALL CHARAC(SHZ%ADR(IFF)%P,SHZ%ADR(IFF)%P,0,
     &                CX,CY,CT,CT,TETA,TETA,DT,MESH3D%IFABOR,IELM3,
     &                NPOIN2,NPLAN,1,1,.FALSE.,BID,SHP%ADR(IFF)%P,
     &                SHZ%ADR(IFF)%P,SHZ%ADR(IFF)%P,TB,
     &                ELT(1,IFF),ETA(1,IFF),ETA(1,IFF),ITR01,
     &                ISUB(1,IFF),ITR01(1,2),MESH3D,NELEM2,NELEM2,
     &                SIKLE2,
     &                MESH%SURDET,BID,BID,SLVBID,0.D0,.FALSE.,3,BID,1,
!                     A POSTERIORI INTERPOLATION
     &                .TRUE.,
!                     AND PERIODICITY
     &                .TRUE.)
!
        ENDDO !  IFF
!
      ELSE
!
!   ---------------------------------------------------------------
!
!   IN A RELATIVE REFERENCE SYSTEM => ADVECTION IN 4D
!   IT IS NO LONGER POSSIBLE TO SEPARATE THE FREQUENCIES OUT
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) ''
          WRITE(LU,*) '***************************************'
          WRITE(LU,*) ' ATTENTION : LA DIFFRACTION N''EST PAS '
          WRITE(LU,*) ' PRISE EN COMPTE EN PRESENCE D''UN     '
          WRITE(LU,*) ' COURANT OU D''UNE HAUTEUR D''EAU      '
          WRITE(LU,*) ' VARIABLE                              '
          WRITE(LU,*) '***************************************'
        ELSE
          WRITE(LU,*) ''
          WRITE(LU,*) '***************************************'
          WRITE(LU,*) ' ATTENTION : DIFFRACTION IS NOT TAKEN  '
          WRITE(LU,*) ' INTO ACCOUNT IF CURRENTS OF VARYING   '
          WRITE(LU,*) ' WATER LEVELS ARE CONSIDERED           '
          WRITE(LU,*) '***************************************'
        ENDIF
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END

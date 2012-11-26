!                    *****************
                     SUBROUTINE PREPRO
!                    *****************
!
     &( CX    , CY    , CT    , CF    , DT    , X     , Y     ,
     &  TETA  , COSTET, SINTET, FREQ  , IKLE2 , IFABOR, ETAP1 , TRA01 ,
     &  SHP   , SHZ   , SHF   , ELT   , ETA   , FRE   ,
     &  DEPTH , DZHDT , DZX   , DZY   , U     , V     , DUX   , DUY   ,
     &  DVX   , DVY   , XK    , CG    , COSF  , TGF   , ITR01 , NPOIN3,
     &  NPOIN2, NELEM2, NPLAN , NF    , SURDET, COURAN, SPHE  ,
     &  PROINF, PROMIN, MESH  , MESH3D, SIKLE2, TB,IELM3, DIFFRA, ISUB)
!
!***********************************************************************
! TOMAWAC   V6P3                                   25/06/2012
!***********************************************************************
!
!brief    PREPARES ADVECTION.
!+
!+            COMPUTES THE ADVECTION FIELD; TRACES BACK THE
!+                CHARACTERISTICS.
!
!history  F. MARCOS (LNH)
!+        04/12/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+   Modifications : possibility of taking into account diffraction
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |<->| ADVECTION FIELD ALONG FREQUENCY
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COSF           |-->| COSINE OF THE LATITUDES OF THE POINTS 2D
!| COSTET         |-->| COSINE OF TETA ANGLE
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| CX             |<->| ADVECTION FIELD ALONG X(OR PHI)
!| CY             |<->| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| CT             |<->| ADVECTION FIELD ALONG TETA
!| DEPTH          |-->| WATER DEPTH
!| DIFFRA         |-->| 0: NO DIFFRACTION  1: DIFFRACTION
!| DT             |-->| TIME STEP
!| DUX            |-->| DERIVATIVE OF CURRENT SPEED DU/DX
!| DUY            |-->| DERIVATIVE OF CURRENT SPEED DU/DY
!| DVX            |-->| DERIVATIVE OF CURRENT SPEED DV/DX
!| DVY            |-->| DERIVATIVE OF CURRENT SPEED DV/DY
!| DZHDT          |-->| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| DZX            |-->| BOTTOM SLOPE ALONG X
!| DZY            |-->| BOTTOM SLOPE ALONG Y
!| ELT            |<->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |<->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETAP1          |<->| HIGHER LAYERS TABLE
!| FRE            |<->| NUMBER OF THE FREQUENCIES OF THE
!|                |   | POINTS TO BE ADVECTED
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| IELM3          |-->| TYPE OF 3D ELEMENT
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!|                |   | SOLID OR PERIODIC BOUNDARY
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| ISUB           |<->| ARRIVAL SUB-DOMAIN OF CHARACTERISTICS
!| ITR01          |<->| WORK TABLE
!| MESH           |-->| 2D MESH
!| MESH3D         |-->| 3D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN3         |-->| NPOIN2*NPLAN
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| PROMIN         |-->| MINIMUM VALUE OF WATER DEPTH
!| SHF            |<->| BARYCENTRIC COORDINATES ALONG F OF THE 
!|                |   | NODES IN THEIR ASSOCIATED FREQUENCIES "FRE"
!| SHP            |<->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |<->| BARYCENTRIC COORDINATES ALONG TETA OF THE 
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| SIKLE2         |-->| IKLE2 IN A BIEF_OBJ STRUCTURE
!| SINTET         |-->| SINE OF TETA ANGLE
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| SURDET         |-->| 1/DET. OF ELEMENTS 2D FOR ISOPARAM. TRANSF.
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TGF            |-->| TANGENT OF THE LATITUDES OF THE POINTS 2D
!| TRA01          |<->| WORK TABLE
!| U              |-->| CURRENT SPEED ALONG X
!| V              |-->| CURRENT SPEED ALONG Y
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_PREPRO => PREPRO
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPOIN3,NPOIN2,NELEM2,NPLAN,NF,DIFFRA
      INTEGER, INTENT(INOUT) :: IELM3
      DOUBLE PRECISION, INTENT(IN) :: DT,PROMIN
      DOUBLE PRECISION DZHDT(NPOIN2)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION XK(NPOIN2,NF),CG(NPOIN2,NF)
      DOUBLE PRECISION SINTET(NPLAN),COSTET(NPLAN)
      DOUBLE PRECISION COSF(NPOIN2),TGF(NPOIN2)
      DOUBLE PRECISION DEPTH(NPOIN2),DZX(NPOIN2),DZY(NPOIN2)
      DOUBLE PRECISION U(NPOIN2),DUX(NPOIN2),DUY(NPOIN2)
      DOUBLE PRECISION V(NPOIN2),DVX(NPOIN2),DVY(NPOIN2)
      DOUBLE PRECISION SURDET(NELEM2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3,8)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN3,NF),ETA(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: ISUB(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: FRE(NPOIN3,NF)
      INTEGER, INTENT(IN) :: IKLE2(NELEM2,3)
      INTEGER, INTENT(IN) :: ETAP1(NPLAN)
      INTEGER, INTENT(INOUT) :: ITR01(NPOIN3,3),IFABOR(NELEM2,7)
      LOGICAL, INTENT(IN) :: COURAN,SPHE,PROINF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SHP,SHZ,SHF,CX,CY,CT,CF,TB
      TYPE(BIEF_OBJ), INTENT(IN)    :: SIKLE2,TETA,FREQ
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH,MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JF,IEL,I1,I2,I3
      TYPE(BIEF_OBJ) :: BID
!
!     JUST FOR CALL INIPIE FOR DIFFRACTION, TO BE REMOVED
!
      INTEGER,DIMENSION(:,:,:), ALLOCATABLE :: GOODELT
!
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!
      SAVE
!
!----------------------------------------------------------------------
!
      IF (.NOT.COURAN) THEN
!
!   -------------------------------------------------------------------
!
!   RELATIVE = ABSOLUTE => ADVECTION IN 3D
!   SEPARATES OUT THE FREQUENCIES
!
        DO 200 JF=1,NF
!
!      ---------------------------------------------------------------
!
!      COMPUTES THE ADVECTION FIELD
!
         CALL CONWAC
     &( CY%R  , CX%R  , CT%R , XK    , CG    , COSF  , TGF   , DEPTH ,
     &  DZY   , DZX   , FREQ%R , COSTET, SINTET, NPOIN2, NPLAN , JF  ,
     &  NF    , PROINF, SPHE , PROMIN, TRA01 , TRA01(1,2) )
!
!      ----------------------------------------------------------------         
!
      DO IEL=1,NELEM2
        I1=IKLE2(IEL,1)
        I2=IKLE2(IEL,2)
        I3=IKLE2(IEL,3)
        IF((DEPTH(I1).LT.PROMIN).AND.(DEPTH(I2).LT.PROMIN).AND.
     &     (IFABOR(IEL,1).GT.0)) IFABOR(IEL,1)=-1
        IF((DEPTH(I2).LT.PROMIN).AND.(DEPTH(I3).LT.PROMIN).AND.
     &     (IFABOR(IEL,2).GT.0)) IFABOR(IEL,2)=-1
        IF((DEPTH(I3).LT.PROMIN).AND.(DEPTH(I1).LT.PROMIN).AND.
     &     (IFABOR(IEL,3).GT.0)) IFABOR(IEL,3)=-1
      ENDDO
!
      IF(DIFFRA.EQ.0) THEN      
!
!         CALLING CHARAC WITH NOMB=0, FN AND FTILD, THOUGH NOT USED
!         WILL GIVE THE NUMBER OF POINTS, HENCE SHZ
!
          CALL CHARAC(SHZ%ADR(JF)%P,SHZ%ADR(JF)%P,0,
     &                CX,CY,CT,CT,TETA,TETA,DT,MESH3D%IFABOR,IELM3,
     &                NPOIN2,NPLAN,1,1,.FALSE.,BID,SHP%ADR(JF)%P,
     &                SHZ%ADR(JF)%P,SHZ%ADR(JF)%P,TB,
     &                ELT(1,JF),ETA(1,JF),ETA(1,JF),ITR01,
     &                ISUB(1,JF),ITR01(1,2),MESH3D,NELEM2,NELEM2,
     &                SIKLE2,
     &                MESH%SURDET,
!                     A POSTERIORI INTERPOLATION
     &                .TRUE.,
!                     AND PERIODICITY 
     &                .TRUE.)
!    
      ELSE
!
          IF(.NOT.DEJA) THEN
            ALLOCATE(GOODELT(NPOIN2,NPLAN,NF))
            DEJA=.TRUE.
          ENDIF
!
          CALL INIPIE
     &( CX%R,CY%R,CT%R,X,Y,
     &  SHP%ADR(JF)%P%R,
     &  SHZ%ADR(JF)%P%R,
     &  ELT(1,JF),ETA(1,JF),
     &  TRA01,TRA01(1,2),TRA01(1,3),TETA%R,IKLE2,NPOIN2,NELEM2,NPLAN,
     &  ITR01,ITR01,ITR01,NELEM2,NPOIN2,IFABOR,GOODELT(1,1,JF))
!
          CALL MPOINT
     &  (CX%R,CY%R,CT%R,
     &   DT,X,Y,TETA%R,IKLE2,IFABOR,ETAP1,TRA01,TRA01(1,2),
     &   TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),
     &   SHP%ADR(JF)%P%R(         1:  NPOIN3),
     &   SHP%ADR(JF)%P%R(  NPOIN3+1:2*NPOIN3),
     &   SHP%ADR(JF)%P%R(2*NPOIN3+1:3*NPOIN3),
     &   SHZ%ADR(JF)%P%R,ELT(1,JF),ETA(1,JF),
     &   ITR01(1,1),NPOIN3,
     &   NPOIN2,NELEM2,NPLAN,JF,SURDET,-1,ITR01(1,2))
!
      ENDIF   
!
200   CONTINUE
!
      ELSE
!
!   ---------------------------------------------------------------
!
!   IN A RELATIVE REFERENCE SYSTEM => ADVECTION IN 4D
!   IT IS NO LONGER POSSIBLE TO SEPARATE THE FREQUENCIES OUT
!
      DO JF=1,NF
!
        CALL CONW4D(CY%R,CX%R,CT%R,CF%R,
     &              V,U,XK,CG,COSF,TGF,DEPTH,DZHDT,DZY,DZX,DVY,DVX,
     &              DUY,DUX,FREQ%R,COSTET,SINTET,NPOIN2,NPLAN,
     &              JF,NF,PROINF,SPHE,COURAN,TRA01,TRA01(1,2))
!
      ENDDO
!
      DO JF=1,NF       
!
        CALL CHARAC(SHZ%ADR(JF)%P,SHZ%ADR(JF)%P,0,
     &              CX,CY,CT,CF,TETA,FREQ,DT,MESH3D%IFABOR,IELM3,
     &              NPOIN2,NPLAN,JF,NF,.FALSE.,BID,SHP%ADR(JF)%P,
     &              SHZ%ADR(JF)%P,SHF%ADR(JF)%P,TB,
     &              ELT(1,JF),ETA(1,JF),FRE(1,JF),ITR01,
     &              ISUB(1,JF),ITR01(1,2),MESH3D,NELEM2,NELEM2,
     &              SIKLE2,MESH%SURDET,
!                   A POSTERIORI INTERPOLATION
     &              .TRUE.,
!                   AND PERIODICITY 
     &              .TRUE.)   
!
        WRITE(LU,*) 'FREQUENCE :',JF
!
      ENDDO
!
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END

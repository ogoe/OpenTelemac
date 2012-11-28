!                    *****************
                     SUBROUTINE CONWAC
!                    *****************
!
     &( CX    , CY    , CT    , XK    , CG    , COSF  , TGF   , DEPTH ,
     &  DZX   , DZY   , FREQ  , COSTET, SINTET, NPOIN2, NPLAN , JF    ,
     &  NF    , PROINF, SPHE  , PROMIN, TRA01 )
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/068/2011
!***********************************************************************
!
!brief    COMPUTES THE ADVECTION FIELD (3D WITHOUT CURRENT).
!
!warning  IN THIS CASE THE X AXIS IS VERTICAL ORIENTED UPWARDS AND
!+            THE Y AXIS IS HORIZONTAL ORIENTED TOWARDS THE RIGHT;
!+            TETA IS THE DIRECTION WRT NORTH, CLOCKWISE
!
!history  M. BENOIT (EDF LNHE)
!+        19/01/2004
!+        V5P4
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
!+        14/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF-LNHE)
!+        27/11/2012
!+        V6P3
!+   Optimisation (loops on NPOIN2 and NPLAN swapped to get smaller 
!+   strides, work array TRA01 differently used, etc.)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COSF           |-->| COSINE OF THE LATITUDES OF THE POINTS 2D
!| COSTET         |-->| COSINE OF TETA ANGLE
!| CX             |<--| ADVECTION FIELD ALONG X(OR PHI)
!| CY             |<--| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| CT             |<--| ADVECTION FIELD ALONG TETA
!| DEPTH          |-->| WATER DEPTH
!| DZX            |-->| SEA BOTTOM SLOPE ALONG X
!| DZY            |-->| SEA BOTTOM SLOPE ALONG Y
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| JF             |-->| INDEX OF THE FREQUENCY
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| PROMIN         |-->| MINIMUM VALUE OF WATER DEPTH
!| SINTET         |-->| SINE OF TETA ANGLE
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| TGF            |-->| TANGENT OF THE LATITUDES OF THE POINTS 2D
!| TRA01          |<->| WORK TABLE
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NPLAN,NPOIN2,JF
      DOUBLE PRECISION, INTENT(IN)    :: PROMIN
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: DZX(NPOIN2),DZY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: COSF(NPOIN2),TGF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: FREQ(NF)
      DOUBLE PRECISION, INTENT(IN)    :: COSTET(NPLAN),SINTET(NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: CX(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: CY(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: CT(NPOIN2,NPLAN)
      LOGICAL, INTENT(IN)             :: PROINF,SPHE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JP,IP
      DOUBLE PRECISION GSQP,SR,R,SRCF,TFSR
      DOUBLE PRECISION DDDN,GRADEG,DEUKD,DEUPI,TR1,TR2
!
      GSQP=0.780654996D0
      R=6400.D3
      DEUPI=6.283185307D0
!
      IF(PROINF) THEN
!
!-----------------------------------------------------------------------
!     INFINITE WATER DEPTH ...
!-----------------------------------------------------------------------
!
        IF(.NOT.SPHE) THEN
!
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
          DO JP=1,NPLAN
            TR1=GSQP/FREQ(JF)*COSTET(JP)
            TR2=GSQP/FREQ(JF)*SINTET(JP)
            DO IP=1,NPOIN2
              CX(IP,JP)=TR1
              CY(IP,JP)=TR2
              CT(IP,JP)=0.D0
            ENDDO
          ENDDO
!
        ELSE
!
!       ----------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
          SR=1.D0/R
          GRADEG=180.D0/3.1415926D0
          DO JP=1,NPLAN
            TR1=GSQP/FREQ(JF)*COSTET(JP)
            TR2=GSQP/FREQ(JF)*SINTET(JP)
            DO IP=1,NPOIN2
              SRCF=SR/COSF(IP)
              TFSR=TGF(IP)*SR
              CX(IP,JP)=TR1*SR*GRADEG
              CY(IP,JP)=TR2*SRCF*GRADEG
              CT(IP,JP)=TR2*TFSR
            ENDDO
          ENDDO
!
        ENDIF
!
      ELSE
!
!-----------------------------------------------------------------------
!     FINITE WATER DEPTH ....
!-----------------------------------------------------------------------
!
        DO IP=1,NPOIN2
          DEUKD=2.D0*XK(IP,JF)*DEPTH(IP)
          IF(DEUKD.GT.7.D2) THEN
            TRA01(IP)=0.D0
          ELSE
            TRA01(IP)=DEUPI*FREQ(JF)/SINH(DEUKD)
          ENDIF
        ENDDO
!
        IF(.NOT.SPHE) THEN
!
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              IF(DEPTH(IP).GT.PROMIN) THEN
                DDDN=-SINTET(JP)*DZX(IP)+COSTET(JP)*DZY(IP)
                CX(IP,JP)=CG(IP,JF)*COSTET(JP)
                CY(IP,JP)=CG(IP,JF)*SINTET(JP)
                CT(IP,JP)=-TRA01(IP)*DDDN
              ELSE
                CX(IP,JP)=0.D0
                CY(IP,JP)=0.D0
                CT(IP,JP)=0.D0
              ENDIF
            ENDDO
          ENDDO
!
        ELSE
!
!       ----------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
          GRADEG=180.D0/3.1415926D0
          SR=1.D0/R
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              IF(DEPTH(IP).GT.PROMIN) THEN
                SRCF=SR/COSF(IP)
                TFSR=SR*TGF(IP)              
                DDDN=-SINTET(JP)*DZX(IP)*SR+COSTET(JP)*DZY(IP)*SRCF
                CX(IP,JP)=(CG(IP,JF)*COSTET(JP))*SR*GRADEG
                CY(IP,JP)=(CG(IP,JF)*SINTET(JP))*SRCF*GRADEG
                CT(IP,JP)=CG(IP,JF)*SINTET(JP)*TFSR
     &                   -TRA01(IP)*DDDN*GRADEG
              ELSE
                CX(IP,JP)=0.0D0
                CY(IP,JP)=0.0D0
                CT(IP,JP)=0.0D0
              ENDIF
            ENDDO
          ENDDO
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

!                    *****************
                     SUBROUTINE CONW4D
!                    *****************
!
     &(CX,CY,CT,CF,U,V,XK,CG,COSF,TGF,DEPTH,DZHDT,DZY,DZX,DVY,DVX,
     & DUY,DUX,FREQ,COSTET,SINTET,NPOIN2,NPLAN,JF,NF,PROINF,SPHE,
     & MAREE,TRA01)
!
!***********************************************************************
! TOMAWAC   V7P0                                   14/06/2011
!***********************************************************************
!
!brief    COMPUTES THE ADVECTION FIELD.
!
!warning  IN THIS CASE THE X AXIS IS VERTICAL ORIENTED UPWARDS AND
!+            THE Y AXIS IS HORIZONTAL ORIENTED TOWARDS THE RIGHT;
!+            TETA IS THE DIRECTION WRT NORTH, CLOCKWISE
!
!history  F MARCOS (LNH)
!+        01/02/95
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
!history  J-M HERVOUET (EDF-LNHE)
!+        08/01/2014
!+        V7P0
!+   Was always called with COURAN=.TRUE., with heavy tests on this
!+   variable. COURAN now considered=.TRUE. and suppressed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COSF           |-->| COSINE OF THE LATITUDES OF THE POINTS 2D
!| COSTET         |-->| COSINE OF TETA ANGLE
!| CT             |<--| ADVECTION FIELD ALONG TETA
!| CY             |<--| ADVECTION FIELD ALONG X(OR PHI)
!| CX             |<--| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| CF             |<--| ADVECTION FIELD ALONG FREQUENCX
!| DEPTH          |-->| WATER DEPTH
!| DVY            |-->| DERIVATIVE OF CURRENT SPEED DU/DX
!| DVX            |-->| DERIVATIVE OF CURRENT SPEED DU/DY
!| DUY            |-->| DERIVATIVE OF CURRENT SPEED DV/DX
!| DUX            |-->| DERIVATIVE OF CURRENT SPEED DV/DY
!| DZHDT          |-->| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| DZY            |-->| SEA BOTTOM SLOPE ALONG X
!| DZX            |-->| SEA BOTTOM SLOPE ALONG Y
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| JF             |-->| INDEX OF THE FREQUENCX
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| SINTET         |-->| SINE OF TETA ANGLE
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| TGF            |-->| TANGENT OF THE LATITUDES OF THE POINTS 2D
!| TRA01          |<->| WORK TABLE
!| V           |-->| CURRENT SPEED ALONG X
!| U           |-->| CURRENT SPEED ALONG Y
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,USDPI,SR,GRADEG,GRAVIT
!
      USE INTERFACE_TOMAWAC, EX_CONW4D => CONW4D
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NPLAN,NPOIN2,JF
!
      DOUBLE PRECISION, INTENT(INOUT) :: CY(NPOIN2,NPLAN,JF)
      DOUBLE PRECISION, INTENT(INOUT) :: CX(NPOIN2,NPLAN,JF)
      DOUBLE PRECISION, INTENT(INOUT) :: CT(NPOIN2,NPLAN,JF)
      DOUBLE PRECISION, INTENT(INOUT) :: CF(NPOIN2,NPLAN,JF)
      DOUBLE PRECISION, INTENT(IN)    :: FREQ(NF)
      DOUBLE PRECISION, INTENT(IN)    :: CG(NPOIN2,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH(NPOIN2),DZHDT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: V(NPOIN2),U(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: DZY(NPOIN2),DZX(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: DVY(NPOIN2),DVX(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: DUY(NPOIN2),DUX(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: COSTET(NPLAN),SINTET(NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: COSF(NPOIN2),TGF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN2)
      LOGICAL, INTENT(IN)             :: PROINF,SPHE,MAREE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,IPOIN
      DOUBLE PRECISION GSQP,SRCF,TFSR,DDDN,LSDUDN,LSDUDS
      DOUBLE PRECISION USGD,DEUKD,TR1,TR2
!
!***********************************************************************
!
      GSQP=GRAVIT/(2.D0*DEUPI)
!
!-----------------------------------------------------------------------
!     INFINITE WATER DEPTH ...
!-----------------------------------------------------------------------
!
      IF(PROINF) THEN
!
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
        IF(.NOT.SPHE) THEN
!
          DO IP=1,NPLAN
            TR1=GSQP/FREQ(JF)*COSTET(IP)
            TR2=GSQP/FREQ(JF)*SINTET(IP)
            DO IPOIN=1,NPOIN2
              LSDUDN= SINTET(IP)*
     &              (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &              + COSTET(IP)*
     &              ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              LSDUDS= COSTET(IP)*
     &               (COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &              + SINTET(IP)*
     &               (COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              CY(IPOIN,IP,JF)=TR1+V(IPOIN)
              CX(IPOIN,IP,JF)=TR2+U(IPOIN)
              CT(IPOIN,IP,JF)=-LSDUDN
              CF(IPOIN,IP,JF)=-CG(IPOIN,JF)*XK(IPOIN,JF)*LSDUDS*USDPI
            ENDDO
          ENDDO
!
!       ----------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
        ELSE
!
          DO IP=1,NPLAN
            TR1=GSQP/FREQ(JF)*COSTET(IP)
            TR2=GSQP/FREQ(JF)*SINTET(IP)
            DO IPOIN=1,NPOIN2
              SRCF=SR/COSF(IPOIN)
              LSDUDN= SINTET(IP)*SR*
     &               (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &               + COSTET(IP)*SRCF*
     &               ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              LSDUDS= COSTET(IP)*SR*
     &               (COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &              + SINTET(IP)*SRCF*
     &               (COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
              CY(IPOIN,IP,JF)=(TR1+V(IPOIN))*GRADEG*SR
              CX(IPOIN,IP,JF)=(TR2+U(IPOIN))*GRADEG*SRCF
              CT(IPOIN,IP,JF)=TR2*TGF(IPOIN)*SR - LSDUDN*GRADEG
              CF(IPOIN,IP,JF)= - LSDUDS*GRADEG*
     &                        CG(IPOIN,JF)*XK(IPOIN,JF)*USDPI
            ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!     FINITE WATER DEPTH ....
!-----------------------------------------------------------------------
!
      ELSE
!
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
        IF(.NOT.SPHE) THEN
!
          DO IPOIN=1,NPOIN2
            DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
            IF(DEUKD.GT.7.D2) THEN
              TRA01(IPOIN) = 0.D0
            ELSE
              TRA01(IPOIN) = DEUPI*FREQ(JF)/SINH(DEUKD)
            ENDIF
          ENDDO
!
          DO IP=1,NPLAN
            DO IPOIN=1,NPOIN2
              DDDN=-SINTET(IP)*DZY(IPOIN)+COSTET(IP)*DZX(IPOIN)
              CY(IPOIN,IP,JF)=CG(IPOIN,JF)*COSTET(IP)
              CX(IPOIN,IP,JF)=CG(IPOIN,JF)*SINTET(IP)
              CT(IPOIN,IP,JF)=-TRA01(IPOIN)*DDDN
            ENDDO
          ENDDO
!
          DO IPOIN=1,NPOIN2
            DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
            IF(DEUKD.GT.7.D2) THEN
              TRA01(IPOIN)=0.D0
            ELSE
              TRA01(IPOIN)=XK(IPOIN,JF)*DEUPI*FREQ(JF)/SINH(DEUKD)
            ENDIF
          ENDDO
!
          IF(MAREE) THEN
            DO IP=1,NPLAN
              DO IPOIN=1,NPOIN2
                LSDUDN= SINTET(IP)*
     &               (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &              + COSTET(IP)*
     &               ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
                LSDUDS= COSTET(IP)*
     &               (COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &              + SINTET(IP)*
     &               (COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
                USGD=V(IPOIN)*DZY(IPOIN)+U(IPOIN)*DZX(IPOIN)
                CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF) + V(IPOIN)
                CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF) + U(IPOIN)
                CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF) - LSDUDN
                CF(IPOIN,IP,JF)= (TRA01(IPOIN)*(USGD+DZHDT(IPOIN))
     &                 - LSDUDS*CG(IPOIN,JF)*XK(IPOIN,JF))*USDPI
              ENDDO
            ENDDO
          ELSE
!           IDEM BUT DZHDT=0.D0
            DO IP=1,NPLAN
              DO IPOIN=1,NPOIN2
                LSDUDN= SINTET(IP)*
     &               (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &              + COSTET(IP)*
     &               ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
                LSDUDS= COSTET(IP)*
     &               (COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &              + SINTET(IP)*
     &               (COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
                USGD=V(IPOIN)*DZY(IPOIN)+U(IPOIN)*DZX(IPOIN)
                CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF) + V(IPOIN)
                CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF) + U(IPOIN)
                CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF) - LSDUDN
                CF(IPOIN,IP,JF)= (TRA01(IPOIN)*USGD
     &             - LSDUDS*CG(IPOIN,JF)*XK(IPOIN,JF))*USDPI
              ENDDO
            ENDDO
          ENDIF
!
!       --------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       --------------------------------------------------------------
!
        ELSE
!
          DO IPOIN=1,NPOIN2
            DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
            IF(DEUKD.GT.7.D2) THEN
              TRA01(IPOIN) = 0.D0
            ELSE
              TRA01(IPOIN) = DEUPI*FREQ(JF)/SINH(DEUKD)
            ENDIF
          ENDDO
!
          DO IP=1,NPLAN
            DO IPOIN=1,NPOIN2
             SRCF=SR/COSF(IPOIN)
             TFSR=TGF(IPOIN)*SR
             DDDN=-SINTET(IP)*DZY(IPOIN)*SR+COSTET(IP)*DZX(IPOIN)*SRCF
             CY(IPOIN,IP,JF)=(CG(IPOIN,JF)*COSTET(IP))*SR*GRADEG
             CX(IPOIN,IP,JF)=(CG(IPOIN,JF)*SINTET(IP))*SRCF*GRADEG
             CT(IPOIN,IP,JF)=CG(IPOIN,JF)*SINTET(IP)*TFSR
     &                                  -TRA01(IPOIN)*DDDN*GRADEG
            ENDDO
          ENDDO
!
          DO IPOIN=1,NPOIN2
            DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
            IF(DEUKD.GT.7.D2) THEN
              TRA01(IPOIN)=0.D0
            ELSE
              TRA01(IPOIN)=XK(IPOIN,JF)*DEUPI*FREQ(JF)/SINH(DEUKD)
            ENDIF
          ENDDO
!
          IF(MAREE) THEN
            DO IP=1,NPLAN
              DO IPOIN=1,NPOIN2
                SRCF=SR/COSF(IPOIN)
                LSDUDN= SINTET(IP)*SR*
     &               (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &              + COSTET(IP)*SRCF*
     &               ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
                LSDUDS= COSTET(IP)*SR*
     &               ( COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &              + SINTET(IP)*SRCF*
     &               ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
                USGD=V(IPOIN)*DZY(IPOIN)*SR
     &              +U(IPOIN)*DZX(IPOIN)*SRCF
                CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF)+V(IPOIN)*SR*GRADEG
                CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF)+U(IPOIN)*SRCF*GRADEG
                CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF)-LSDUDN*GRADEG
                CF(IPOIN,IP,JF)=
     &           (TRA01(IPOIN)*(USGD*GRADEG+DZHDT(IPOIN))
     &          -LSDUDS*GRADEG*CG(IPOIN,JF)*XK(IPOIN,JF))*USDPI
              ENDDO
            ENDDO
          ELSE
!           IDEM BUT DZHDT=0.D0
            DO IP=1,NPLAN
              DO IPOIN=1,NPOIN2
                SRCF=SR/COSF(IPOIN)
                LSDUDN= SINTET(IP)*SR*
     &               (-COSTET(IP)*DVY(IPOIN)-SINTET(IP)*DUY(IPOIN))
     &              + COSTET(IP)*SRCF*
     &               ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
                LSDUDS= COSTET(IP)*SR*
     &               ( COSTET(IP)*DVY(IPOIN)+SINTET(IP)*DUY(IPOIN))
     &              + SINTET(IP)*SRCF*
     &               ( COSTET(IP)*DVX(IPOIN)+SINTET(IP)*DUX(IPOIN))
                USGD=V(IPOIN)*DZY(IPOIN)*SR
     &              +U(IPOIN)*DZX(IPOIN)*SRCF
                CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF)+V(IPOIN)*SR*GRADEG
                CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF)+U(IPOIN)*SRCF*GRADEG
                CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF)-LSDUDN*GRADEG
                CF(IPOIN,IP,JF)=(TRA01(IPOIN)*USGD
     &              -LSDUDS*CG(IPOIN,JF)*XK(IPOIN,JF))*GRADEG*USDPI
              ENDDO
            ENDDO
          ENDIF
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

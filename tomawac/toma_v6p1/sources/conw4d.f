!                    *****************
                     SUBROUTINE CONW4D
!                    *****************
!
     &(CX,CY,CT,CF,U,V,XK,CG,COSF,TGF,DEPTH,DZHDT,DZX,DZY,DUX,DUY,
     & DVX,DVY,FREQ,COSTET,SINTET,NPOIN2,NPLAN,JF,NF,PROINF,SPHE,
     & COURAN,TRA01,TRA02)
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/06/2011
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COSF           |-->| COSINE OF THE LATITUDES OF THE POINTS 2D
!| COSTET         |-->| COSINE OF TETA ANGLE
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| CT             |<--| ADVECTION FIELD ALONG TETA
!| CX             |<--| ADVECTION FIELD ALONG X(OR PHI)
!| CY             |<--| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| CF             |<--| ADVECTION FIELD ALONG FREQUENCY
!| DEPTH          |-->| WATER DEPTH
!| DUX            |-->| DERIVATIVE OF CURRENT SPEED DU/DX
!| DUY            |-->| DERIVATIVE OF CURRENT SPEED DU/DY
!| DVX            |-->| DERIVATIVE OF CURRENT SPEED DV/DX
!| DVY            |-->| DERIVATIVE OF CURRENT SPEED DV/DY
!| DZHDT          |-->| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| DZX            |-->| SEA BOTTOM SLOPE ALONG X
!| DZY            |-->| SEA BOTTOM SLOPE ALONG Y
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| JF             |-->| INDEX OF THE FREQUENCY
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| SINTET         |-->| SINE OF TETA ANGLE
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| TGF            |-->| TANGENT OF THE LATITUDES OF THE POINTS 2D
!| TRA01          |<->| WORK TABLE
!| TRA02          |<->| WORK TABLE
!| U              |-->| CURRENT SPEED ALONG X
!| V              |-->| CURRENT SPEED ALONG Y
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NF,NPLAN,NPOIN2
      INTEGER JF,IP,IPOIN
!
      DOUBLE PRECISION CX(NPOIN2,NPLAN,JF),CY(NPOIN2,NPLAN,JF)
      DOUBLE PRECISION CT(NPOIN2,NPLAN,JF),CF(NPOIN2,NPLAN,JF)
      DOUBLE PRECISION FREQ(NF)
      DOUBLE PRECISION CG(NPOIN2,NF),XK(NPOIN2,NF)
      DOUBLE PRECISION DEPTH(NPOIN2),DZHDT(NPOIN2)
      DOUBLE PRECISION U(NPOIN2),V(NPOIN2),DZX(NPOIN2),DZY(NPOIN2)
      DOUBLE PRECISION DUX(NPOIN2),DUY(NPOIN2),DVX(NPOIN2),DVY(NPOIN2)
      DOUBLE PRECISION COSTET(NPLAN),SINTET(NPLAN)
      DOUBLE PRECISION COSF(NPOIN2),TGF(NPOIN2)
!
      DOUBLE PRECISION TRA01(NPLAN),TRA02(NPLAN)
      DOUBLE PRECISION GSQP,SR,R,SRCF,TFSR
      DOUBLE PRECISION DDDN,DSDNSK,LSDUDN,GRADEG,LSDUDS
      DOUBLE PRECISION DSDD,USGD,USDPI,DEUPI,DEUKD
!
      LOGICAL PROINF,SPHE,COURAN
!
!***********************************************************************
!
      GSQP=0.780654996D0
      R=6400.D3
      USDPI=0.159154943D0
      DEUPI=6.283185307D0
!
!-----------------------------------------------------------------------
!     INFINITE WATER DEPTH ...
!-----------------------------------------------------------------------
!
      IF (PROINF) THEN
!
        DO IP=1,NPLAN
          TRA01(IP)=GSQP/FREQ(JF)*COSTET(IP)
          TRA02(IP)=GSQP/FREQ(JF)*SINTET(IP)
        ENDDO
!
!       ----------------------------------------------------------------
!       ... AND IN CARTESIAN COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
        IF (.NOT.SPHE) THEN
!
          DO IPOIN=1,NPOIN2
            DO IP=1,NPLAN
              CX(IPOIN,IP,JF)=TRA01(IP)
              CY(IPOIN,IP,JF)=TRA02(IP)
              CT(IPOIN,IP,JF)=0.D0
            ENDDO
          ENDDO
!
          IF (COURAN) THEN
            DO IPOIN=1,NPOIN2
             DO IP=1,NPLAN
              LSDUDN= SINTET(IP)*
     &                 (-COSTET(IP)*DUX(IPOIN)-SINTET(IP)*DVX(IPOIN))
     &              + COSTET(IP)*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
              LSDUDS= COSTET(IP)*
     &                 (COSTET(IP)*DUX(IPOIN)+SINTET(IP)*DVX(IPOIN))
     &              + SINTET(IP)*
     &                 (COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
              CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF)+U(IPOIN)
              CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF)+V(IPOIN)
              CT(IPOIN,IP,JF)= -LSDUDN
              CF(IPOIN,IP,JF)= -CG(IPOIN,JF)*XK(IPOIN,JF)*
     &                          LSDUDS*USDPI
             ENDDO
            ENDDO
          ENDIF
!
!       ----------------------------------------------------------------
!       ... AND IN SPHERICAL COORDINATE SYSTEM
!       ----------------------------------------------------------------
!
        ELSE
!
          SR=1.D0/R
          GRADEG=180.D0/3.1415926D0
          DO IPOIN=1,NPOIN2
            SRCF=SR/COSF(IPOIN)
            TFSR=TGF(IPOIN)*SR
            DO IP=1,NPLAN
              CX(IPOIN,IP,JF)=TRA01(IP)*SR*GRADEG
              CY(IPOIN,IP,JF)=TRA02(IP)*SRCF*GRADEG
              CT(IPOIN,IP,JF)=TRA02(IP)*TFSR
            ENDDO
          ENDDO
!
          IF (COURAN) THEN
            DO IPOIN=1,NPOIN2
             SRCF=SR/COSF(IPOIN)
             DO IP=1,NPLAN
              LSDUDN= SINTET(IP)*SR*
     &                 (-COSTET(IP)*DUX(IPOIN)-SINTET(IP)*DVX(IPOIN))
     &                 + COSTET(IP)*SRCF*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
              LSDUDS= COSTET(IP)*SR*
     &                 (COSTET(IP)*DUX(IPOIN)+SINTET(IP)*DVX(IPOIN))
     &                + SINTET(IP)*SRCF*
     &                 (COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
              CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF) + U(IPOIN)*SR*GRADEG
              CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF) + V(IPOIN)*SRCF*GRADEG
              CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF) - LSDUDN*GRADEG
              CF(IPOIN,IP,JF)= - LSDUDS*GRADEG*
     &                          CG(IPOIN,JF)*XK(IPOIN,JF)*USDPI
             ENDDO
            ENDDO
          ENDIF
        ENDIF
!
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
        IF (.NOT.SPHE) THEN
!
          DO IPOIN=1,NPOIN2
            DO IP=1,NPLAN
              DDDN=-SINTET(IP)*DZX(IPOIN)+COSTET(IP)*DZY(IPOIN)
              CX(IPOIN,IP,JF)=CG(IPOIN,JF)*COSTET(IP)
              CY(IPOIN,IP,JF)=CG(IPOIN,JF)*SINTET(IP)
              DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
              IF (DEUKD.GT.7.D2) THEN
                DSDNSK = 0.D0
              ELSE
                DSDNSK = DEUPI*FREQ(JF)/SINH(DEUKD)
              ENDIF
              CT(IPOIN,IP,JF)=-DSDNSK*DDDN
            ENDDO
          ENDDO
!
          IF (COURAN) THEN
            DO IPOIN=1,NPOIN2
              DO IP=1,NPLAN
                LSDUDN= SINTET(IP)*
     &                 (-COSTET(IP)*DUX(IPOIN)-SINTET(IP)*DVX(IPOIN))
     &                + COSTET(IP)*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
                LSDUDS= COSTET(IP)*
     &                 (COSTET(IP)*DUX(IPOIN)+SINTET(IP)*DVX(IPOIN))
     &                + SINTET(IP)*
     &                 (COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
                DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
                IF (DEUKD.GT.7.D2) THEN
                  DSDD = 0.D0
                ELSE
                  DSDD = XK(IPOIN,JF)*DEUPI*FREQ(JF)/SINH(DEUKD)
                ENDIF
                USGD=U(IPOIN)*DZX(IPOIN)+V(IPOIN)*DZY(IPOIN)
      	        CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF) + U(IPOIN)
                CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF) + V(IPOIN)
                CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF) - LSDUDN
                CF(IPOIN,IP,JF)= (DSDD*(USGD+DZHDT(IPOIN))
     &                 - LSDUDS*CG(IPOIN,JF)*XK(IPOIN,JF))*USDPI
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
          SR=1.D0/R
          GRADEG=180.D0/3.1415926D0
          DO IPOIN=1,NPOIN2
            SRCF=SR/COSF(IPOIN)
            TFSR=TGF(IPOIN)*SR
            DO IP=1,NPLAN
             DDDN=-SINTET(IP)*DZX(IPOIN)*SR+COSTET(IP)*DZY(IPOIN)*SRCF
             CX(IPOIN,IP,JF)=(CG(IPOIN,JF)*COSTET(IP))*SR*GRADEG
             CY(IPOIN,IP,JF)=(CG(IPOIN,JF)*SINTET(IP))*SRCF*GRADEG
             DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
             IF (DEUKD.GT.7.D2) THEN
               DSDNSK = 0.D0
             ELSE
               DSDNSK = DEUPI*FREQ(JF)/SINH(DEUKD)
             ENDIF
             CT(IPOIN,IP,JF)=CG(IPOIN,JF)*SINTET(IP)*TFSR
     &                                  -DSDNSK*DDDN*GRADEG
            ENDDO
          ENDDO
!
          IF (COURAN) THEN
            DO IPOIN=1,NPOIN2
              SRCF=SR/COSF(IPOIN)
              DO IP=1,NPLAN
                LSDUDN= SINTET(IP)*SR*
     &                 (-COSTET(IP)*DUX(IPOIN)-SINTET(IP)*DVX(IPOIN))
     &                + COSTET(IP)*SRCF*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
                LSDUDS= COSTET(IP)*SR*
     &                 ( COSTET(IP)*DUX(IPOIN)+SINTET(IP)*DVX(IPOIN))
     &                + SINTET(IP)*SRCF*
     &                 ( COSTET(IP)*DUY(IPOIN)+SINTET(IP)*DVY(IPOIN))
                DEUKD=2.D0*XK(IPOIN,JF)*DEPTH(IPOIN)
                IF (DEUKD.GT.7.D2) THEN
                  DSDD = 0.D0
                ELSE
                  DSDD = XK(IPOIN,JF)*DEUPI*FREQ(JF)/SINH(DEUKD)
                ENDIF
                USGD=U(IPOIN)*DZX(IPOIN)*SR+V(IPOIN)*DZY(IPOIN)*SRCF
                CX(IPOIN,IP,JF)=CX(IPOIN,IP,JF) + U(IPOIN)*SR*GRADEG
                CY(IPOIN,IP,JF)=CY(IPOIN,IP,JF) + V(IPOIN)*SRCF*GRADEG
                CT(IPOIN,IP,JF)=CT(IPOIN,IP,JF) - LSDUDN*GRADEG
                CF(IPOIN,IP,JF)=  (DSDD*(USGD*GRADEG+DZHDT(IPOIN))
     &            -LSDUDS*GRADEG*CG(IPOIN,JF)*XK(IPOIN,JF))*USDPI
              ENDDO
            ENDDO
          ENDIF
!
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

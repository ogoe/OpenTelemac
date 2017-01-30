!                    *****************
                     SUBROUTINE QVEG1
!                    *****************
!
     &( TSTOT , TSDER , F , VARIAN , DEPTH, FMOY ,
     &  XKMOY , NF    , NPLAN  , NPOIN2   , BETA  )
!
!***********************************************************************
! TOMAWAC   V6P3                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE WHITECAPPING
!+                SOURCE TERM BASED ON KOMEN ET AL. (1984).
!
!note     CMOUT1 (USED IN WAM-CYCLE 4) EQUALS 4.5.
!note   CMOUT2 (USED IN WAM-CYCLE 4) EQUALS 0.5.
!
!reference  KOMEN G.J., HASSELMANN S., HASSELMANN K. (1984) :
!+                     "ON THE EXISTENCE OF A FULLY DEVELOPED WINDSEA
!+                      SPECTRUM". JPO, VOL 14, PP 1271-1285.
!
!history  P. THELLIER; M. BENOIT (EDF/DER/LNH)
!+        06/04/95
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETA           |<--| WORK TABLE
!| CMOUT1         |-->| WHITE CAPPING DISSIPATION COEFFICIENT
!| CMOUT2         |-->| WHITE CAPPING WEIGHTING COEFFICIENT
!| ENRJ           |-->| SPECTRUM VARIANCE
!| F              |-->| DIRECTIONAL SPECTRUM
!| FMOY           |-->| MEAN SPECTRAL FRQUENCY FMOY (relative frequency)
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| TAUX1          |<--| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKMOY          |-->| AVERAGE WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT,PI
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NPLAN,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: XKMOY(NPOIN2),VARIAN(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH(NPOIN2),FMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NPLAN,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION C1,CVEG,CD,NV,BV,ALFA,KH,AKH,RO
      DOUBLE PRECISION AUX,NUM,DENUM,SHAKH
!
!     COEFFICIENTS DE DISSIPATION LIES À LA VEGETATION
      RO = 1.D0
      NV = 10
      BV = 0.04
      CD = 1.D0
      ALFA = 1.D0
      C1 = - DSQRT(2.0D0/PI)*GRAVIT**2
      CVEG = C1*CD*BV*NV/(DEUPI**3)
!
!       VEGETATION OVER A CONSTAT DEPTHV
!       COMPUTES THE BETA COEFFICIENT : QVEG1 = BETA * F
!
        DO IP=1,NPOIN2
          KH = XKMOY(IP)*DEPTH(IP)
          AKH = ALFA*KH
          SHAKH = SINH(AKH)
          NUM = SHAKH*(SHAKH**2 + 3)
          DENUM = 3.0D0*XKMOY(IP)*COSH(KH)**3
          AUX = (XKMOY(IP)/FMOY(IP))**3
          BETA(IP) = RO*CVEG*AUX*(NUM/DENUM)*DSQRT(VARIAN(IP))
        ENDDO
        WRITE(LU,*)'VARIAN(IP) =,FMOY =',VARIAN(10)
!
!       LOOP OVER THE DISCRETISED FREQUENCIES
!
!         TAKES THE SOURCE TERM INTO ACCOUNT
!
        DO JF=1,NF
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETA(IP)*F(IP,JP,JF)
              TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETA(IP)
            ENDDO
          ENDDO
        ENDDO
!        WRITE(6,*) 'TSDER=,BETA=,F=',TSDER(10,2,1),BETA(10),F(10,2,1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END


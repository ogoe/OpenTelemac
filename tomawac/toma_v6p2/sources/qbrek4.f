!                    *****************
                     SUBROUTINE QBREK4
!                    *****************
!
     &( TSTOT , TSDER , F     , FCAR  , VARIAN, DEPTH , BETAIH, EM2SIH,
     &  GRAVIT, NF    , NPLAN , NPOIN2, BETA  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE DEPTH-INDUCED
!+                BREAKING SOURCE TERM BASED ON IZUMIYA ET HORIKAWA (1984).
!
!note     THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!+          COEFFICIENT DOES NOT VARY WITH TIME.
!
!reference  IZUMIYA T., HORIKAWA K. (1984) :
!+                     "WAVE ENERGY EQUATION APPLICABLE IN AND OUTSIDE
!+                      THE SURF ZONE". COASTAL ENGINEERING IN JAPAN, VOL 17, PP 119-137.
!
!history  F. BECQ; M. BENOIT (EDF/DER/LNH)
!+        26/03/96
!+        V1P1
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
!| BETA           |<--| BREAKING WAVES COEFFICIENT
!| BETAIH         |-->| BETA0 CONSTANT OF WAVE BREAKING IH MODEL
!| DEPTH          |-->| WATER DEPTH
!| EM2SIH         |-->| M2* CONSTANT OF WAVE BREAKING IH MODEL
!| F              |-->| DIRECTIONAL SPECTRUM
!| FCAR           |-->| CHARACTERISTIC FREQUENCY
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| VARIAN         |-->| SPECTRUM VARIANCE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER          NF    , NPLAN , NPOIN2
      DOUBLE PRECISION BETAIH, EM2SIH, GRAVIT
      DOUBLE PRECISION DEPTH(NPOIN2), BETA(NPOIN2), FCAR(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF), VARIAN(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , IFF   , IP
      DOUBLE PRECISION COEF  , XKCAR , DEUKD , GG1   , GG2
!
!
      COEF   = -DSQRT(GRAVIT)*BETAIH
!
!.....COMPUTES THE LINEAR COEFFICIENT BETA : QBREK4 = BETA * F
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""
      DO 40 IP = 1,NPOIN2
        CALL WNSCOU( XKCAR, FCAR(IP), DEPTH(IP) )
        DEUKD=2.D0*XKCAR*DEPTH(IP)
        IF (DEUKD.GT.7.D2) THEN
          GG1 = 0.D0
          GG2 = 0.5D0
        ELSE
          GG1 = DEUKD/SINH(DEUKD)
          GG2 = 0.5D0*(1.D0+GG1)
        ENDIF
        BETA(IP) = COEF/DEPTH(IP)**1.5*DSQRT(VARIAN(IP)*GG1)
     &               *DSQRT(DMAX1(0.D0,GG2*VARIAN(IP)
     &               /(DEPTH(IP)*DEPTH(IP))-EM2SIH))
   40 CONTINUE
!
!
!.....TAKES THE SOURCE TERM INTO ACCOUNT
!     """"""""""""""""""""""""""""""""
      DO 10 IFF = 1,NF
        DO 20 JP = 1,NPLAN
          DO 30 IP = 1,NPOIN2
            TSTOT(IP,JP,IFF) = TSTOT(IP,JP,IFF)+BETA(IP)*F(IP,JP,IFF)
!            TSDER(IP,JP,IFF) = TSDER(IP,JP,IFF)+BETA(IP)
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
!
      RETURN
      END

!                    *****************
                     SUBROUTINE QBREK2
!                    *****************
!
     &( TSTOT , TSDER , F     , FCAR  , VARIAN, DEPTH , BORETG, GAMATG,
     &  IWHTG , NF    , NPLAN , NPOIN2, BETA  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE DEPTH-INDUCED
!+                BREAKING SOURCE TERM BASED ON THORNTON AND GUZA (1983).
!
!note     THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!+          COEFFICIENT DOES NOT VARY WITH TIME.
!
!reference  THORNTON AND GUZA (1983) :
!+                     "TRANSFORMATION OF WAVE HEIGHT DISTRIBUTION".
!
!history  F. BECQ; M. BENOIT (EDF/DER/LNH)
!+        14/02/96
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
!| BORETG         |-->| COEFFICIENT B OF WAVE BREAKING TG MODEL
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| DIRECTIONAL SPECTRUM
!| FCAR           |-->| CHARACTERISTIC FREQUENCY
!| GAMATG         |-->| GAMMA CONSTANT OF WAVE BREAKING TG MODEL
!| IWHTG          |-->| WEIGHT. FUN.SELECTION OF WAVE BREAKING TG MODEL
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
      INTEGER  NF    , NPLAN , NPOIN2, IWHTG
      DOUBLE PRECISION BORETG, GAMATG
      DOUBLE PRECISION DEPTH(NPOIN2), BETA(NPOIN2), FCAR(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF), VARIAN(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , IFF   , IP
      DOUBLE PRECISION COEF  , GAMMA2, SEUIL , DEUPI
!
!
      DEUPI  = 6.283185307D0
      SEUIL  = 1.D-6
      GAMMA2 = GAMATG*GAMATG
      COEF   = -24.D0*DSQRT(DEUPI)*BORETG**3/GAMMA2
!
      IF (IWHTG.EQ.1) THEN
!
!.......COMPUTES THE LINEAR COEFFICIENT BETA : QBREK2 = BETA * F
!       WITH THE WEIGHT FUNCTION W(H) = CONSTANT
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO 25 IP = 1,NPOIN2
          BETA(IP) = COEF*8.D0*VARIAN(IP)**2.5D0*FCAR(IP)
     &             /(GAMMA2*DEPTH(IP)**5)
   25   CONTINUE
!
      ELSEIF (IWHTG.EQ.2) THEN
!
!.......COMPUTES THE LINEAR COEFFICIENT BETA : QBREK2 = BETA * F
!       WITH THE WEIGHT FUNCTION W(H) != CONSTANT
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO 35 IP = 1,NPOIN2
          BETA(IP) = (COEF*VARIAN(IP)**1.5D0*FCAR(IP)/
     &                DEPTH(IP)**3)*(1.D0-1.D0/(1.D0+VARIAN(IP)*8.D0
     &                /(GAMMA2*DEPTH(IP)*DEPTH(IP)))**2.5D0)
  35    CONTINUE
      ENDIF
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

!                    *****************
                     SUBROUTINE QBREK2
!                    *****************
!
     &( TSTOT , F     , FCAR  , VARIAN, DEPTH , BORETG, GAMATG,
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
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| VARIAN         |-->| SPECTRUM VARIANCE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI
!
      USE INTERFACE_TOMAWAC, EX_QBREK2 => QBREK2
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)            :: NF, NPLAN, NPOIN2, IWHTG
      DOUBLE PRECISION, INTENT(IN)   :: BORETG, GAMATG
      DOUBLE PRECISION, INTENT(IN)   :: DEPTH(NPOIN2), FCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)   :: VARIAN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: BETA(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NPLAN,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , IFF   , IP
      DOUBLE PRECISION COEF  , GAMMA2
!
!
      GAMMA2 = GAMATG**2
      COEF   = -24.D0*SQRT(DEUPI)*BORETG**3/GAMMA2
!
      IF (IWHTG.EQ.1) THEN
!
!.......COMPUTES THE LINEAR COEFFICIENT BETA : QBREK2 = BETA * F
!       WITH THE WEIGHT FUNCTION W(H) = CONSTANT
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP = 1,NPOIN2
          BETA(IP) = COEF*8.D0*SQRT(VARIAN(IP)**5)*FCAR(IP)
     &             /(GAMMA2*DEPTH(IP)**5)
        ENDDO
!
      ELSEIF (IWHTG.EQ.2) THEN
!
!.......COMPUTES THE LINEAR COEFFICIENT BETA : QBREK2 = BETA * F
!       WITH THE WEIGHT FUNCTION W(H) != CONSTANT
!       """""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP = 1,NPOIN2
          BETA(IP) = (COEF*SQRT(VARIAN(IP)**3)*FCAR(IP)/
     &              DEPTH(IP)**3)*(1.D0-1.D0/(1.D0+VARIAN(IP)*8.D0
     &              /(GAMMA2*DEPTH(IP)*DEPTH(IP)))**2.5D0)
        ENDDO ! IP
      ENDIF
!
!.....TAKES THE SOURCE TERM INTO ACCOUNT
!     """"""""""""""""""""""""""""""""
      DO IFF = 1,NF
        DO JP = 1,NPLAN
          DO IP = 1,NPOIN2
            TSTOT(IP,JP,IFF) = TSTOT(IP,JP,IFF)+BETA(IP)*F(IP,JP,IFF)
          ENDDO ! IP
        ENDDO ! JP
      ENDDO ! IFF
!
      RETURN
      END

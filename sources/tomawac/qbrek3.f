!                    *****************
                     SUBROUTINE QBREK3
!                    *****************
!
     &( TSTOT , F     , FCAR  , VARIAN, DEPTH , ALFARO, GAMARO,
     &  GAM2RO, IEXPRO, IDISRO, NF    , NPLAN , NPOIN2, BETA  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE DEPTH-INDUCED
!+                BREAKING SOURCE TERM BASED ON ROELVINK (1993).
!
!note     THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!+          COEFFICIENT DOES NOT VARY WITH TIME.
!
!reference  ROELVINK (1993) :
!+                     "DISSIPATION IN RANDOM WAVE GROUPS INCIDENT ON A
!+                      BEACH". COASTAL ENG. VOL 19, PP 127-150.
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
!| ALFARO         |-->| COEFFICIENT ALPHA OF RO WAVE BREAKING MODEL
!| BETA           |<--| BREAKING WAVES COEFFICIENT
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| DIRECTIONAL SPECTRUM
!| FCAR           |-->| CHARACTERISTIC FREQUENCY
!| GAM2RO         |-->| GAMMA2 CONSTANT OF WAVE BREAKING RO MODEL
!| GAMARO         |-->| GAMMA CONSTANT OF WAVE BREAKING RO MODEL
!| IDISRO         |-->| WAVE HEIGHT DISTRIBUTION SLECTION FOR RO MODEL
!| IEXPRO         |-->| EXPONENT OF WAVE HEIGHT DISTR. FOR RO MODEL
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| VARIAN         |-->| SPECTRUM VARIANCE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : PISUR2,DEUPI
      USE INTERFACE_TOMAWAC, EX_QBREK3 => QBREK3
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)   ::  IEXPRO, IDISRO, NF    , NPLAN , NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: ALFARO, GAMARO, GAM2RO
      DOUBLE PRECISION, INTENT(IN)   :: DEPTH(NPOIN2), FCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)   :: VARIAN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: BETA(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NPLAN,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , IFF   , IP
      DOUBLE PRECISION COEF1 , COEF2 , SEUIL
      DOUBLE PRECISION A     , XM    , SIGMA , BX    , FN
!
!.....EXTERNAL FUNCTIONS
!     """"""""""""""""""
!      DOUBLE PRECISION   GAMMLN, QGAUSS
!      EXTERNAL           GAMMLN, QGAUSS
!
!
      SEUIL  = 1.D-6
      COEF1  = -2.D0*ALFARO
      COEF2  = 8.D0/(GAMARO**2)
!
      IF(IDISRO.EQ.1) THEN
!
!.......COMPUTES THE LINEAR COEFFICIENT BETA (WEIBULL FIT)
!       """""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP = 1,NPOIN2
          IF (VARIAN(IP).GT.SEUIL) THEN
            BX    = COEF2*VARIAN(IP)/(DEPTH(IP)*DEPTH(IP))
            SIGMA = SQRT(8.D0*VARIAN(IP))/DEPTH(IP)
            XM    = 1.D0 + 0.7D0*(TAN(PISUR2*SIGMA/GAM2RO))**2
            A     = EXP(XM*(GAMMLN(1.D0+1.D0/XM,DEUPI)))
            IF(XM.GT.98.D0) THEN
              FN = 1.D0
            ELSE
              FN = QGAUSS(BX,IEXPRO,A,XM)
            ENDIF
            BETA(IP) = COEF1*FCAR(IP)*FN
          ELSE
            BETA(IP) = 0.D0
          ENDIF
        ENDDO ! IP
!
      ELSE
!
!.......COMPUTES THE LINEAR COEFFICIENT BETA (RAYLEIGH FIT)
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP = 1,NPOIN2
          BX = COEF2*VARIAN(IP)/(DEPTH(IP)**2)
          XM = 1.D0
          A  = 1.D0
          FN = QGAUSS(BX,IEXPRO,A,XM)
          BETA(IP) = COEF1*FCAR(IP)*FN
        ENDDO ! IP
      ENDIF
!
!     TAKES THE SOURCE TERM INTO ACCOUNT
!
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

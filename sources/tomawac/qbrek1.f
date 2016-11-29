!                    *****************
                     SUBROUTINE QBREK1
!                    *****************
!
     &( TSTOT , F     , FCAR  , VARIAN, DEPTH , ALFABJ, GAMBJ1,
     &  GAMBJ2, IQBBJ , IHMBJ , NF    , NPLAN , NPOIN2, BETA  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE DEPTH-INDUCED
!+                BREAKING SOURCE TERM BASED ON BATTJES AND JANSSEN (1978).
!
!note     THIS SOURCE TERM IS LINEAR IN F(FREQ,TETA), AND THE LINEAR
!+          COEFFICIENT DOES NOT VARY WITH TIME.
!
!reference  BATTJES AND JANSSEN (1978) :
!+                     "ENERGY LOSS AND SET-UP DUE TO BREAKING
!+                      OF RANDOM WAVES". ICCE'78.
!
!history  F. BECQ; M. BENOIT (EDF/DER/LNH)
!+        14/02/96
!+        V1P1
!+
!
!history  OPTIMER
!+        14/06/2001
!+        V5P2
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
!| ALFABJ         |-->| COEFFICIENT ALPHA OF BJ MODEL
!| BETA           |<--| BREAKING WAVES COEFFICIENT
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| DIRECTIONAL SPECTRUM
!| FCAR           |-->| CHARACTERISTIC FREQUENCY
!| GAMBJ1         |-->| GAMMA1 CONSTANT OF WAVE BREAKING BJ MODEL
!| GAMBJ2         |-->| GAMMA2 CONSTANT OF WAVE BREAKING BJ MODEL
!| IHMBJ          |-->| DEPTH-INDUCED BREAKING CRITERIUM GIVING THE
!|                |   | BREAKING WAVE HEIGHT
!| IQBBJ          |-->| SELECTED QB COMPUTATION METHOD FOR BJ MODEL
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| VARIAN         |-->| SPECTRUM VARIANCE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_QBREK1 => QBREK1
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)            :: IQBBJ, IHMBJ, NF, NPLAN, NPOIN2
      DOUBLE PRECISION, INTENT(IN)   :: ALFABJ, GAMBJ1, GAMBJ2
      DOUBLE PRECISION, INTENT(IN)   :: F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)   :: VARIAN(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: DEPTH(NPOIN2),FCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: BETA(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT):: TSTOT(NPOIN2,NPLAN,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER          JP   , IFF , IP
      DOUBLE PRECISION COEF , HM  , XK8 , XKCAR , B , QB , SEUIL
!
!.....EXTERNAL FUNCTIONS
!     """"""""""""""""""
!      DOUBLE PRECISION   QBBJ78
!      EXTERNAL           QBBJ78
!
!
      SEUIL=1.D-6
      COEF =-.25D0*ALFABJ
!
!.....COMPUTES THE LINEAR COEFFICIENT BETA: QBREK1 = BETA * F
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""
      DO IP = 1,NPOIN2
        IF (VARIAN(IP).GT.SEUIL) THEN
!
!..........COMPUTES THE MAXIMUM WAVE HEIGHT
!          """""""""""""""""""""""""""""""""""""""
          IF(IHMBJ.EQ.1) THEN
            HM  = GAMBJ2*DEPTH(IP)
          ELSEIF(IHMBJ.EQ.2) THEN
            CALL WNSCOU(XKCAR,FCAR(IP),DEPTH(IP))
            XK8 = GAMBJ1/XKCAR
            HM  = XK8*TANH(GAMBJ2*DEPTH(IP)/XK8)
          ENDIF
!
!..........COMPUTES THE FRACTION OF BREAKING WAVES
!          """"""""""""""""""""""""""""""""""""""""""""
          B   = SQRT(8.D0*VARIAN(IP))/HM
          QB  = QBBJ78(B,IQBBJ)
!
          BETA(IP) = COEF*QB*FCAR(IP)*HM**2/VARIAN(IP)
        ELSE
          BETA(IP) = 0.D0
        ENDIF
      ENDDO ! IP
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

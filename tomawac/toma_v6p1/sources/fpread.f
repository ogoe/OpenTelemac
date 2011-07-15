!                    *****************
                     SUBROUTINE FPREAD
!                    *****************
!
     &( FREAD , F     , FREQ  , DFREQ , NF    , NPLAN , NPOIN2, EXPO  ,
     &  TAILF , DENOM , E     )
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE PEAK FREQUENCY OF THE VARIANCE SPECTRUM
!+                USING THE SO-CALLED READ METHOD.
!
!history  M. BENOIT
!+        30/01/96
!+        V1P1
!+   CREATED
!
!history  M. BENOIT
!+        05/07/96
!+        V1P2
!+   MODIFIED
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
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DENOM          |<->| WORK TABLE
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| E              |<->| WORK TABLE
!| EXPO           |-->| EXPONENT OF READ METHOD
!| F              |---| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREAD          |<--| PEAK FREQUENCY (READ METHOD)
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION EXPO  , TAILF
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), FREQ(NF), DFREQ(NF)
      DOUBLE PRECISION DENOM(NPOIN2), E(NPOIN2), FREAD(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION SEUIL , AUXI  , COEFN  , COEFD , DTETAR
!
!
      SEUIL =1.D-20
      DTETAR=2.D0*3.141592654D0/DBLE(NPLAN)
      DO 10 IP = 1,NPOIN2
        FREAD(IP)=0.D0
        DENOM(IP)=0.D0
   10 CONTINUE
!
!-----C-------------------------------------------------------C
!-----C SUMS UP THE CONTRIBUTIONS FOR THE DISCRETISED PART OF THE SPECTRUM     C
!-----C-------------------------------------------------------C
      DO 20 JF=1,NF
!
!.......INTEGRATES WRT DIRECTIONS TO GET E(F)
!       """""""""""""""""""""""""""""""""""""""""""""""""
        DO 60 IP=1,NPOIN2
          E(IP) = 0.D0
   60   CONTINUE
        DO 30 JP=1,NPLAN
          DO 40 IP=1,NPOIN2
                 E(IP) = E(IP) + F(IP,JP,JF)*DTETAR
   40     CONTINUE
   30   CONTINUE
!
!.......SUMS UP THE CONTRIBUTION OF THE FREQUENCY F
!       """""""""""""""""""""""""""""""""""""""""""
        DO 50 IP=1,NPOIN2
          IF (E(IP).GT.SEUIL) THEN
            AUXI = E(IP)**EXPO*DFREQ(JF)
            FREAD(IP) = FREAD(IP)+AUXI*FREQ(JF)
            DENOM(IP) = DENOM(IP)+AUXI
          ENDIF
   50   CONTINUE
!
   20 CONTINUE
!
!-----C-------------------------------------------------------------C
!-----C (OPTIONALLY) TAKES INTO ACCOUNT THE HIGH-FREQUENCY PART     C
!-----C-------------------------------------------------------------C
      IF (TAILF.GT.1.D0) THEN
        COEFN=FREQ(NF)**2/(TAILF*EXPO-2.D0)
        COEFD=FREQ(NF)   /(TAILF*EXPO-1.D0)
        DO 55 IP=1,NPOIN2
          AUXI=E(IP)**EXPO
          FREAD(IP) = FREAD(IP)+AUXI*COEFN
          DENOM(IP) = DENOM(IP)+AUXI*COEFD
   55   CONTINUE
      ENDIF
!
!-----C-------------------------------------------------------------C
!-----C COMPUTES THE PEAK FREQUENCY                                 C
!-----C-------------------------------------------------------------C
      DO 70 IP=1,NPOIN2
        IF (DENOM(IP).LT.1.D-90) THEN
          FREAD(IP) = SEUIL
        ELSE
          FREAD(IP) = FREAD(IP)/DENOM(IP)
        ENDIF
   70 CONTINUE
!
      RETURN
      END

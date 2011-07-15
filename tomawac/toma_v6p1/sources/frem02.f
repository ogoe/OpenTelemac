!                    *****************
                     SUBROUTINE FREM02
!                    *****************
!
     &( FM02  , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  AUX1  , AUX2  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE MEAN SPECTRAL FREQUENCY FM02 FOR ALL
!+                THE NODES IN THE 2D MESH. THIS FREQUENCY IS DEFINED
!+                FROM THE M0 AND M2 SPECTRAL MOMENTUM.
!code
!+                  (  SOMME(  F(FREQ,TETA)*FREQ**2 DFREQ DTETA  )  )
!+       FM02 = SQRT(  -------------------------------------------  )
!+                  (  SOMME(  F(FREQ,TETA) DFREQ DTETA  )          )
!
!note     THE HIGH-FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!+         IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 3.
!
!history  P. THELLIER; M. BENOIT
!+        09/02/95
!+        V1P0
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
!| AUX1           |<->| WORK TABLE
!| AUX2           |<->| WORK TABLE
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FM02           |<--| MEAN FREQUENCIES F02
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
      DOUBLE PRECISION TAILF
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION FREQ(NF), DFREQ(NF), FM02(NPOIN2)
      DOUBLE PRECISION AUX1(NPOIN2), AUX2(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION SEUIL , DTETAR, AUX3  , AUX4
!
!
      SEUIL = 1.D-20
      DTETAR= 2.D0*3.141592654D0/DBLE(NPLAN)
      DO 30 IP = 1,NPOIN2
        AUX1(IP) = 0.D0
        AUX2(IP) = 0.D0
   30 CONTINUE
!
!-----C-------------------------------------------------------C
!-----C SUMS UP THE CONTRIBUTIONS FOR THE DISCRETISED PART OF THE SPECTRUM     C
!-----C-------------------------------------------------------C
      DO 20 JF = 1,NF-1
        AUX3=DTETAR*DFREQ(JF)
        AUX4=AUX3*FREQ(JF)**2
        DO 10 JP = 1,NPLAN
          DO 5 IP=1,NPOIN2
            AUX1(IP) = AUX1(IP) + F(IP,JP,JF)*AUX4
            AUX2(IP) = AUX2(IP) + F(IP,JP,JF)*AUX3
    5     CONTINUE
   10   CONTINUE
   20 CONTINUE
!
!-----C-------------------------------------------------------------C
!-----C (OPTIONALLY) TAKES INTO ACCOUNT THE HIGH-FREQUENCY PART     C
!-----C-------------------------------------------------------------C
      IF (TAILF.GT.3.D0) THEN
        AUX3=DTETAR*(DFREQ(NF)+FREQ(NF)/(TAILF-1.D0))
        AUX4=DTETAR*(DFREQ(NF)+FREQ(NF)/(TAILF-3.D0))*FREQ(NF)**2
      ELSE
        AUX3=DTETAR*DFREQ(NF)
        AUX4=AUX3*FREQ(NF)**2
      ENDIF
      DO 40 JP = 1,NPLAN
        DO 45 IP=1,NPOIN2
          AUX1(IP) = AUX1(IP) + F(IP,JP,NF)*AUX4
          AUX2(IP) = AUX2(IP) + F(IP,JP,NF)*AUX3
   45   CONTINUE
   40 CONTINUE
!
!-----C-------------------------------------------------------------C
!-----C COMPUTES THE MEAN FREQUENCY                                 C
!-----C-------------------------------------------------------------C
      DO 50 IP=1,NPOIN2
        IF (AUX2(IP).LT.SEUIL) THEN
          FM02(IP) = SEUIL
        ELSE
          FM02(IP) = SQRT(AUX1(IP)/AUX2(IP))
        ENDIF
   50 CONTINUE
!
      RETURN
      END

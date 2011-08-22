!                    *****************
                     SUBROUTINE TOTNRJ
!                    *****************
!
     &( VARIAN, F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   28/06/2011
!***********************************************************************
!
!brief    COMPUTES THE VARIANCE OF THE DIRECTIONAL SPECTRUM
!+                FOR ALL THE NODES IN THE 2D MESH. IT IS COMPUTED BY
!+                INTEGRATION OVER FREQUENCIES AND DIRECTIONS AND CAN
!+                TAKE THE HIGH FREQUENCY PART OF THE SPECTRUM INTO
!+                ACCOUNT.
!
!note     THE HIGH FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!+          IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 1.
!
!history  P. THELLIER; M. BENOIT
!+        09/02/95
!+        V1P0
!+   CREATED
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
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |---| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!| VARIAN         |<--| SPECTRUM VARIANCE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER          NF    , NPLAN , NPOIN2
      DOUBLE PRECISION TAILF , VARIAN(NPOIN2), FREQ(NF), DFREQ(NF)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER          IP    , JP    , JF
      DOUBLE PRECISION AUX1  , DTETAR
!
!
      DTETAR=2.D0*3.14159265D0/FLOAT(NPLAN)
      DO 30 IP = 1,NPOIN2
        VARIAN(IP) = 0.D0
30    CONTINUE
!
!-----C-------------------------------------------------------C
!-----C  SUMS UP THE DISCRETISED PART OF THE SPECTRUM         C
!-----C-------------------------------------------------------C
      DO 20 JF = 1,NF-1
        AUX1=DFREQ(JF)*DTETAR
        DO 10 JP = 1,NPLAN
          DO 5 IP=1,NPOIN2
            VARIAN(IP) = VARIAN(IP) + F(IP,JP,JF)*AUX1
    5     CONTINUE
   10   CONTINUE
   20 CONTINUE
!
!-----C-------------------------------------------------------------C
!-----C  TAKES THE HIGH FREQUENCY PART INTO ACCOUNT (OPTIONAL)      C
!-----C-------------------------------------------------------------C
      IF (TAILF.GT.1.D0) THEN
        AUX1=DTETAR*(DFREQ(NF)+FREQ(NF)/(TAILF-1.D0))
      ELSE
        AUX1=DTETAR*DFREQ(NF)
      ENDIF
      DO 40 JP = 1,NPLAN
        DO 45 IP=1,NPOIN2
          VARIAN(IP) = VARIAN(IP) + F(IP,JP,NF)*AUX1
   45   CONTINUE
   40 CONTINUE
!
      RETURN
      END

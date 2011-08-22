!                    *****************
                     SUBROUTINE KMOYEN
!                    *****************
!
     &( XKMOY , XK    , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN ,
     &  NPOIN2, AUX1  , AUX2  , AUX3  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   20/06/2011
!***********************************************************************
!
!brief    COMPUTES THE AVERAGE WAVE NUMBER FOR ALL THE NODES
!+                IN THE 2D MESH.
!
!note     THE HIGH-FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!+          IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 1.
!
!history  P. THELLIER; M. BENOIT (EDF/DER/LNH)
!+        04/04/95
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
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AUX1           |<->| WORK TABLE
!| AUX2           |<->| WORK TABLE
!| AUX3           |<->| WORK TABLE
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |---| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKMOY          |<--| AVERAGE WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION TAILF
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), XK(NPOIN2,NF)
      DOUBLE PRECISION FREQ(NF)  , DFREQ(NF) , XKMOY(NPOIN2)
      DOUBLE PRECISION AUX1(NPOIN2) , AUX2(NPOIN2) , AUX3(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IPLAN , JF    , IP
      DOUBLE PRECISION COEFF , PI    , SEUIL , CTE1  , CTE2  , AUX4
!
!
      PI = 3.141592654D0
      SEUIL = 1.D-20
      COEFF = SQRT(9.806D0)/(2.D0*PI)
      DO 30 IP = 1,NPOIN2
        AUX1(IP) = 0.D0
        AUX2(IP) = 0.D0
   30 CONTINUE
!
!.....SUMS UP THE CONTRIBUTIONS FOR THE DISCRETISED PART OF THE SPECTRUM
!     """"""""""""""""""""""""""""""""""""""""""""""""
      DO 20 JF = 1,NF
        AUX4=DFREQ(JF)
!
        DO 15 IP=1,NPOIN2
          AUX3(IP) = 0.D0
   15   CONTINUE
        DO 10 IPLAN = 1,NPLAN
          DO 5 IP=1,NPOIN2
            AUX3(IP) = AUX3(IP) + F(IP,IPLAN,JF)
    5     CONTINUE
   10   CONTINUE
!
        DO 25 IP = 1,NPOIN2
          AUX1(IP)=AUX1(IP)+AUX3(IP)*AUX4
          AUX2(IP)=AUX2(IP)+AUX3(IP)/SQRT(XK(IP,JF))*AUX4
   25   CONTINUE
!
   20 CONTINUE
!
!.....(OPTIONALLY) TAKES INTO ACCOUNT THE HIGH-FREQUENCY PART
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (TAILF.GT.1.D0) THEN
        CTE1=FREQ(NF)/(TAILF-1.D0)
        CTE2=COEFF/TAILF
      ELSE
        CTE1=0.D0
        CTE2=0.D0
      ENDIF
      DO 45 IP=1,NPOIN2
        AUX1(IP) = AUX1(IP) + AUX3(IP)*CTE1
        AUX2(IP) = AUX2(IP) + AUX3(IP)*CTE2
   45 CONTINUE
!
!.....COMPUTES THE AVERAGE WAVE NUMBER
!     """"""""""""""""""""""""""""""
      DO 50 IP=1,NPOIN2
        IF (AUX2(IP).LT.SEUIL) THEN
          XKMOY(IP) = 1.D0
        ELSE
          XKMOY(IP) = (AUX1(IP)/AUX2(IP))**2
        ENDIF
   50 CONTINUE
!
      RETURN
      END

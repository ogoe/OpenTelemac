!                    *****************
                     SUBROUTINE TETMOY
!                    *****************
!
     &( TETAM , F     , COSTET, SINTET, NPLAN , FREQ  , DFREQ , NF    ,
     &  NPOIN2, TAILF , COSMOY, SINMOY, TAUXC , TAUXS )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE MEAN DIRECTION OF A DIRECTIONAL SPECTRUM
!+               (BY COMPUTING THE ARC TANGENT OF THE MEAN SINE AND
!+                COSINE). THE RESULT IS IN RADIANS.
!
!history  P. THELIIER; M. BENOIT
!+        01/02/95
!+        V1P0
!+
!
!history  M. BENOIT
!+        05/07/96
!+        V1P2
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COSMOY         |---|
!| COSTET         |---|
!| DFREQ          |---|
!| F              |---|
!| FREQ           |---|
!| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
!| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
!| SINMOY         |---|
!| SINTET         |---|
!| TAILF          |-->| FACTEUR DE QUEUE DU SPECTRE
!| TAUXC          |---|
!| TAUXS          |---|
!| TETAM          |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION TAILF
      DOUBLE PRECISION TETAM(NPOIN2) , DFREQ(NF)    , COSMOY(NPOIN2)
      DOUBLE PRECISION COSTET(NPLAN) , SINTET(NPLAN), SINMOY(NPOIN2)
      DOUBLE PRECISION TAUXC(NPOIN2) , TAUXS(NPOIN2)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), FREQ(NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP    , JP    , JF
      DOUBLE PRECISION AUXC  , AUXS  , DEUPI , SEUIL , COEFT , DFDTET
      DOUBLE PRECISION DTETAR
!
!
      DEUPI =2.D0*3.14159265D0
      DTETAR=DEUPI/DBLE(NPLAN)
      SEUIL =1.D-10
      DO 10 IP=1,NPOIN2
        COSMOY(IP)=0.D0
        SINMOY(IP)=0.D0
   10 CONTINUE
!
!-----C-------------------------------------------------------C
!-----C  SUMS UP THE DISCRETISED PART OF THE SPECTRUM         C
!-----C-------------------------------------------------------C
      DO 30 JF=1,NF
!
        DFDTET=DFREQ(JF)*DTETAR
!
        DO 35 IP=1,NPOIN2
          TAUXC(IP)=0.D0
          TAUXS(IP)=0.D0
   35   CONTINUE
!
        DO 20 JP=1,NPLAN
          AUXC=COSTET(JP)*DFDTET
          AUXS=SINTET(JP)*DFDTET
          DO 40 IP=1,NPOIN2
            TAUXC(IP)=TAUXC(IP)+F(IP,JP,JF)*AUXC
            TAUXS(IP)=TAUXS(IP)+F(IP,JP,JF)*AUXS
   40     CONTINUE
   20   CONTINUE
!
        DO 45 IP=1,NPOIN2
          COSMOY(IP)=COSMOY(IP)+TAUXC(IP)
          SINMOY(IP)=SINMOY(IP)+TAUXS(IP)
   45   CONTINUE
!
   30 CONTINUE
!
!-----C-------------------------------------------------------------C
!-----C  TAKES THE HIGH FREQUENCY PART INTO ACCOUNT (OPTIONAL)      C
!-----C-------------------------------------------------------------C
      IF (TAILF.GT.1.D0) THEN
        COEFT=FREQ(NF)/((TAILF-1.D0)*DFREQ(NF))
        DO 55 IP=1,NPOIN2
          COSMOY(IP)=COSMOY(IP)+TAUXC(IP)*COEFT
          SINMOY(IP)=SINMOY(IP)+TAUXS(IP)*COEFT
   55   CONTINUE
      ENDIF
!
!-----C-------------------------------------------------------------C
!-----C  COMPUTES THE MEAN DIRECTION                                C
!-----C  (IN RADIANS BETWEEN 0 AND 2.PI)                            C
!-----C-------------------------------------------------------------C
      DO 60 IP=1,NPOIN2
        IF ((ABS(SINMOY(IP)).LT.SEUIL).AND.
     &      (ABS(COSMOY(IP)).LT.SEUIL)) THEN
          TETAM(IP) = 0.D0
        ELSE
          TETAM(IP)=ATAN2(SINMOY(IP),COSMOY(IP))
          IF (TETAM(IP).LT.0.D0) TETAM(IP)=TETAM(IP)+DEUPI
        ENDIF
   60 CONTINUE
!
      RETURN
      END
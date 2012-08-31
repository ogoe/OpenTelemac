!                    *****************
                     SUBROUTINE VITFON
!                    *****************
!
     &( UWBM  , F     , XK    , DEPTH , DFREQ , NF    , NPOIN2, NPLAN ,
     &      GRAVIT, BETA  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   29/06/2011
!***********************************************************************
!
!brief    COMPUTES THE MAXIMUM ORBITAL VELOCITY NEAR THE BOTTOM
!+               (AVERAGE VELOCITY ON THE SPECTRUM).
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
!history  G.MATTAROLO (EDF - LNHE)
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETA           |<->| WORK TABLE
!| DEPTH          |-->| WATER DEPTH
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| UWBM           |<--| MAXIMUM ORBITAL VELOCITY NEAR THE BOTTOM
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION GRAVIT
      DOUBLE PRECISION UWBM(NPOIN2), DEPTH(NPOIN2), BETA(NPOIN2)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), XK(NPOIN2,NF), DFREQ(NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP    , JP    , JF
      DOUBLE PRECISION DEUPI , DTETAR, DEUKD , COEF
!
!
      DEUPI=2.D0*3.14159265D0
      DTETAR=DEUPI/FLOAT(NPLAN)
!
      DO 30 IP = 1,NPOIN2
        UWBM(IP) = 0.D0
   30 CONTINUE
!
!.....SUMS UP THE DISCRETISED PART OF THE SPECTRUM
!     """"""""""""""""""""""""""""""""""""""""""""""""
      DO 20 JF = 1,NF
        COEF=2.D0*GRAVIT*DFREQ(JF)*DTETAR
        DO 25 IP = 1,NPOIN2
          DEUKD = MIN(2.D0*DEPTH(IP)*XK(IP,JF),7.D2)
          BETA(IP) = COEF*XK(IP,JF)/SINH(DEUKD)
   25   CONTINUE
        DO 10 JP = 1,NPLAN
          DO 5 IP=1,NPOIN2
            UWBM(IP) = UWBM(IP) + F(IP,JP,JF)*BETA(IP)
    5     CONTINUE
   10   CONTINUE
   20 CONTINUE
!
      DO 35 IP=1,NPOIN2
        UWBM(IP) = DSQRT(UWBM(IP))
   35 CONTINUE
!
      RETURN
      END

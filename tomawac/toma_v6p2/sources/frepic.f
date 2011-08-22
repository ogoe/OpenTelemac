!                    *****************
                     SUBROUTINE FREPIC
!                    *****************
!
     &( FPIC  , F     , FREQ  , NF    , NPLAN , NPOIN2, EMAX  , E     )
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE PEAK FREQUENCY FOR ALL THE NODES IN THE
!+                2D MESH. THIS PEAK FREQUENCY IS DEFINED AS THE
!+                DISCRETISED FREQUENCY FOR WHICH E(F) IS GREATEST.
!
!history  P. THELLIER; M. BENOIT
!+        09/02/95
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
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| E              |<->| WORK TABLE
!| EMAX           |<->| WORK TABLE
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FPIC           |<--| PEAK FREQUENCIES
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), FREQ(NF)  , FPIC(NPOIN2)
      DOUBLE PRECISION EMAX(NPOIN2),E(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
!
!
      DO 10 IP = 1,NPOIN2
        FPIC(IP) = 1.D-20
        EMAX(IP) = 0.D0
   10 CONTINUE
!
!.....LOOP OVER DISCRETISED FREQUENCIES
!     """""""""""""""""""""""""""""""""""""""""""""
      DO 20 JF = 1,NF
!
!.......INTEGRATES WRT DIRECTIONS TO GET E(F)
!       """""""""""""""""""""""""""""""""""""""""""""""""
        DO 60 IP=1,NPOIN2
          E(IP) = 0.D0
   60   CONTINUE
        DO 30 JP = 1,NPLAN
          DO 40 IP=1,NPOIN2
                 E(IP) = E(IP) + F(IP,JP,JF)
   40     CONTINUE
   30   CONTINUE
!
!.......KEEPS THE MAXIMUM VALUE FOR E(F) AND ASSOCIATED FREQUENCY
!       """""""""""""""""""""""""""""""""""""""""""""""""""""
        DO 50 IP=1,NPOIN2
          IF (E(IP).GT.EMAX(IP)) THEN
            EMAX(IP) = E(IP)
            FPIC(IP) = FREQ(JF)
          ENDIF
   50   CONTINUE
!
   20 CONTINUE
!
      RETURN
      END

!                    *****************************
                     DOUBLE PRECISION FUNCTION SPE
!                    *****************************
!
     &(F)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE ENERGY DENSITY BASED ON GODA.
!
!reference  "RANDOM SEA AND DESIGN OF MARITIME STRUCTURES",
!+                       UNIVERSITY OF TOKYO PRESS - 1985
!
!history  F. LEPEINTRE (LNH)
!+        01/06/1993
!+        V2P0
!+
!
!history  D. AELBRECHT (LNH)
!+        04/06/1999
!+        V5P1
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
!| F             |-->| FREQUENCY FOR WHICH ENERGY DENSITY IS CALCULATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_SPE => SPE
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS, ONLY: FP,GAM,DELTA
      IMPLICIT NONE
!
      DOUBLE PRECISION F,SIGMA
!
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
!      FP=1/1.68
!      GAM=3.3
!
      IF (F.LE.FP) THEN
        SIGMA = 0.07D0
      ELSE
        SIGMA = 0.09D0
      ENDIF
!
      DELTA = 0.0624D0 * FP**4 /
     &       ( 0.230D0 + 0.0336D0*GAM - 0.185D0 / (1.9D0+GAM) )
!
!     DELTA IS COMPUTED IN PERALE
!
      IF ( F.GE.1.D-4*FP) THEN
        SPE = DELTA/F**5 * EXP(-1.25D0*(FP/F)**4) *
     &         GAM** ( EXP( -0.5D0*( ( (F-FP)/(SIGMA*FP) ) **2 ) ) )
      ELSE
        SPE = 0D0
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

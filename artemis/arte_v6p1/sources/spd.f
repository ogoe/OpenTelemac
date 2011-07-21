!                    *****************************
                     DOUBLE PRECISION FUNCTION SPD
!                    *****************************
!
     &(TETA)
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
!code
!+ SPD(TETA) = COS( (TETA)/2 )**(2*EXPO)
!+
!+
!+ WHERE TETA IS THE WAVE PROPAGATION ANGLE
!+       (THE MAIN PROPAGATION DIRECTION IS TETA=0)
!+       EXPO IS AN EXPONENT SPECIFIED BY THE USER
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
!| TETA           |-->| ANGLE FOR CONSIDERED WAVE PROPAGATION 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION TETA,PI,DEGRAD
!
      DOUBLE PRECISION EXPO
      COMMON /COEFHD/ EXPO
!
      INTRINSIC COS
!
!-----------------------------------------------------------------------
!
      PARAMETER( PI = 3.1415926535897932384626433D0 ,
     &           DEGRAD = PI/180.D0 )
!
!-----------------------------------------------------------------------
!
      SPD = COS ( TETA*DEGRAD / 2.D0 )**(2*EXPO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

!                    ***************************
                     MODULE DECLARATIONS_SPECIAL
!                    ***************************
!
!
!***********************************************************************
! SPECIAL                                             09/05/2014
!***********************************************************************
!
!brief    Defining parameters that need to be created first because they
!+        could be used by any other library.
!
!history  Y. AUDOUIN & J-M HERVOUET (EDF LAB, LNHE)
!+        09/05/2014
!+        V7P0
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!----------------------------------------------------------------------
!
!     Integer values to allow the allocation of I4 and I8 integers.
!
!     The syntax will be :
!
!     INTEGER(KIND=K4) :: I (I WILL BE A 4-BYTE INTEGER)
!     INTEGER(KIND=K8) :: I (I WILL BE A 8-BYTE INTEGER)
!
!     SELECTED_INT_KIND(R):
!
!     Returns the kind type for integers N such that : -10**R<N<10**R
!
      INTEGER, PARAMETER :: K4  =  SELECTED_INT_KIND(9)
      INTEGER, PARAMETER :: K8  =  SELECTED_INT_KIND(15)
!
!-----------------------------------------------------------------------
!
      END MODULE DECLARATIONS_SPECIAL

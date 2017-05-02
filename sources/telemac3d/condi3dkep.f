!                    *********************
                     SUBROUTINE CONDI3DKEP
!                    *********************
!
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    INITIALISES K AND EPSILON
!
!history  C.-T. PHAM (LNHE)
!+        24/03/2017
!+        V7P3
!+   Creation from not splitted CONDIM
!+   Called by CONDIM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC3D, EX_CONDI3DKEP => CONDI3DKEP
      USE DECLARATIONS_TELEMAC3D
!
!     USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!     INTEGER I
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE K-EPSILON MODEL (OPTIONAL)
!     WHEN DONE: AKEP = .FALSE.
!
      AKEP=.TRUE.
!
!     IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
!
!       HERE INITIALISES K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

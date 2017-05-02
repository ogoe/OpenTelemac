!                    *******************
                     SUBROUTINE CONDI3DP
!                    *******************
!
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    INITIALISES PRESSURE
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
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_CONDI3DP => CONDI3DP
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!     INTEGER I
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE PRESSURE FIELDS TO 0.
!
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

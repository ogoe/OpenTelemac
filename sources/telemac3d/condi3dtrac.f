!                    **********************
                     SUBROUTINE CONDI3DTRAC
!                    **********************
!
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    INITIALISES TRACER(S)
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
      USE INTERFACE_TELEMAC3D, EX_CONDI3DTRAC => CONDI3DTRAC
      USE DECLARATIONS_TELEMAC3D
!
!     USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

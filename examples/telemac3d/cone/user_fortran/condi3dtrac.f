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
      INTEGER I,J
      DOUBLE PRECISION EIKON
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
! BEGIN OF PART SPECIFIC TO THIS CASE
!         CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
          DO J=1,NPOIN3
            EIKON = ( (X(J)-15.D0)**2 + (Y(J)-10.2D0)**2 ) / 2.D0
            TA%ADR(I)%P%R(J) = EXP(-EIKON)
          ENDDO
! END OF PART SPECIFIC TO THIS CASE
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

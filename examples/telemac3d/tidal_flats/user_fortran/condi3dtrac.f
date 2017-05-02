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
      INTEGER I,J,IPLAN
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
!###>TBE modification for tidal test case, apply susp sediment
!    in the right hand side of the model (where the intertidals are)
      DO I = 1,NPOIN2
        DO IPLAN = 1,NPLAN
          J = (IPLAN-1)*NPOIN2 + I
          IF (X(I).GT.15000.D0) THEN
            TA%ADR(1)%P%R(J) = 0.3D0
          ELSE
            TA%ADR(1)%P%R(J) = 0.D0
          ENDIF
        ENDDO
      ENDDO
!###<TBE end of modification
!
!-----------------------------------------------------------------------
!
      RETURN
      END

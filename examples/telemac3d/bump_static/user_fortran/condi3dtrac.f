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
      INTEGER IPLAN,I,J
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
        DO IPLAN=1,NPLAN
          DO J=1,NPOIN2
            I = (IPLAN-1)*NPOIN2+J
            IF((Z(I)-ZF%R(J)).GT.0.66D0) THEN
              TA%ADR(1)%P%R(I) = 25.D0
            ELSE
              TA%ADR(1)%P%R(I) = 7.D0
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

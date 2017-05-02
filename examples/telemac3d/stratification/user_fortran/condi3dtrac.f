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
      INTEGER I,J,IPLAN,ITRAC
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
! BEGIN OF PART SPECIFIC TO THIS CASE
        DO ITRAC=1,NTRAC
!         CALL OS( 'X=C     ', X=TA%ADR(ITRAC)%P, C=TRAC0(ITRAC))
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              J=NPOIN2*(IPLAN-1)+I
              IF(IPLAN.GT.18) THEN
                TA%ADR(ITRAC)%P%R(J) = 28.D0
              ELSE
                TA%ADR(ITRAC)%P%R(J) = 38.D0
              ENDIF
            ENDDO
          ENDDO
! END OF PART SPECIFIC TO THIS CASE
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

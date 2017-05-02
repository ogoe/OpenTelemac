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
      INTEGER IPLAN,I,IPOIN2,IPOIN3
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
! BEGIN OF PART SPECIFIC TO THIS CASE
        DO IPLAN=1,NPLAN
          DO IPOIN2=1,NPOIN2
            IPOIN3 = IPOIN2 + (IPLAN-1)*NPOIN2
            IF(X(IPOIN3).LE.15.D0) TA%ADR(1)%P%R(IPOIN3) = 1.D0
          ENDDO
        ENDDO
! END OF PART SPECIFIC TO THIS CASE
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

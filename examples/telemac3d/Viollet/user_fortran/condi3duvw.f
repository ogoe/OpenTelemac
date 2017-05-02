!                    *********************
                     SUBROUTINE CONDI3DUVW
!                    *********************
!
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    INITIALISES VELOCITY
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
      USE INTERFACE_TELEMAC3D, EX_CONDI3DUVW => CONDI3DUVW
      USE DECLARATIONS_TELEMAC3D
!
!     USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,J
!
!-----------------------------------------------------------------------
!
!     INITIALISES VELOCITIES
!
      IF(SUIT2) THEN
        DO I=1,NPLAN
          DO J=1,NPOIN2
            U%R((I-1)*NPOIN2+J)=U2D%R(J)
            V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR.
     &       CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN
!
!     U2D, V2D PREVIOUSLY COMPUTED IN CONDI3DH
!
        DO I=1,NPLAN
          DO J=1,NPOIN2
            U%R((I-1)*NPOIN2+J)=U2D%R(J)
            V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSE
! BEGIN OF PART SPECIFIC TO THIS CASE
!       CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=C     ' , X=U, C=0.05D0 )
! END OF PART SPECIFIC TO THIS CASE
        CALL OS( 'X=0     ' , X=V )
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
      RETURN
      END

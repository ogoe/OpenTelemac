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
      INTEGER I,J,IPLAN
      DOUBLE PRECISION DELTAZ,AUX
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
!     BEGINNING OF SPECIFIC TO THIS CASE
!       CALL OS( 'X=0     ' , X=U )
        DO IPLAN=1,NPLAN
          DO I=1,NPOIN2
            IF(IPLAN.EQ.1) THEN
              DELTAZ=(MESH3D%Z%R(I+NPOIN2)-MESH3D%Z%R(I))
     &                /2.71828182845D0**2
            ELSE
              DELTAZ=MESH3D%Z%R(I+(IPLAN-1)*NPOIN2)-MESH3D%Z%R(I)
            ENDIF
            AUX=MAX(30.D0*DELTAZ/0.0162D0,1.D0)
            U%R(I+(IPLAN-1)*NPOIN2)=(0.0703D0/0.41D0)*LOG(AUX)
          ENDDO
        ENDDO
!     END OF SPECIFIC TO THIS CASE
        CALL OS( 'X=0     ' , X=V )
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
      RETURN
      END

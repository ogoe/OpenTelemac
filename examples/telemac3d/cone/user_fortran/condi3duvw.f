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
        CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=0     ' , X=V )
! BEGIN OF PART SPECIFIC TO THIS CASE
        DO I=1,NPOIN3
          U%R(I) = -(Y(I)-10.05D0)
          V%R(I) =  (X(I)-10.05D0)
        ENDDO
! END OF PART SPECIFIC TO THIS CASE
      ENDIF
!
! BEGIN OF PART SPECIFIC TO THIS CASE
      DO I=1,NPTFR3
        UBORL%R(I)=U%R(MESH3D%NBOR%I(I))
        VBORL%R(I)=V%R(MESH3D%NBOR%I(I))
      ENDDO
! END OF PART SPECIFIC TO THIS CASE
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
      RETURN
      END

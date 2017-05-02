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
      INTEGER IPLAN,IPOIN2,IPOIN3
!     FOR SOLITARY WAVE
      DOUBLE PRECISION WX,WH,HV1,HV2,HV3,HV4,X0
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
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
! BEGIN OF PART SPECIFIC TO THIS CASE
! SOLITARY WAVE INITIAL VELOCITY - ANALYTICAL SOLUTION
!
! *** SOLITARY WAVE ***
!
! WH WAVE HEIGHT
! WX WQUILIBRIUM WATER DEPTH
! X0 CREST INITIAL POSITION
!
!       WH=2.D0
        WH=1.D0
        WX=10.D0
!       X0=80.D0
        X0=150.D0
!
      DO IPLAN=1,NPLAN
        DO IPOIN2=1,NPOIN2
          IPOIN3 = (IPLAN-1)*NPOIN2 + IPOIN2
!
          HV1=SQRT(3.D0/4.D0*WH/WX**3)*(X(IPOIN3)-X0)
          HV2=2.D0/(EXP(HV1)+EXP(-HV1))
          U%R(IPOIN3)=SQRT(GRAV*WX)*WH/WX*HV2**2

          HV3=(EXP(HV1)-EXP(-HV1))/(EXP(HV1)+EXP(-HV1))
          HV4=SQRT(3.D0*GRAV*WX)
     &       *((SQRT(WH/WX))**3)*(Z(IPOIN3)+WX)/WX
          W%R(IPOIN3)=(HV4*HV2**2)*HV3
        ENDDO
      ENDDO
! END OF PART SPECIFIC TO THIS CASE
!
!-----------------------------------------------------------------------
!
      RETURN
      END

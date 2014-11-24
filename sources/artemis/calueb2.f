!                    ******************
                     SUBROUTINE CALUEB2
!                    ******************
!
!
!***********************************************************************
! ARTEMIS   V7P0                                   06/2014
!***********************************************************************
!
!brief    COMPUTES AN EFFECTIVE SPEED UE FOR THE ESTIMATION
!+        OF THE FRICTION DISSIPATION UNDER IRREGULAR SEA STATES
!+               
!
!history  C.PEYRARD (LNHE)
!+        06/2014
!+        V7P0
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I
!
      DOUBLE PRECISION PI,RADDEG,DEUKD,KD,UI
      DOUBLE PRECISION ZERO, BID
!
      INTRINSIC SQRT, ATAN2, DMOD, ABS, COS, SIN
!
!-----------------------------------------------------------------------
!
!
      PARAMETER (ZERO = 1.D-10)
      PARAMETER (PI = 3.1415926535897932384626433D0)
      PARAMETER (RADDEG = 57.29577951D0)
      
!
!     
!=======================================================================
! CASE 1 : USE OF BOTTOM FRICTION COMPUTED FROM THE POTENTIAL
!=======================================================================
! VELOCITY IN Z=-D FOR RANDOM WAVES (see TOMAWAC vitfon.f)
!    UE^2 = UE^2 + 2 * Sp(f,teta) df dteta * K g / SINH(DEUKD) = UE^2 + Ai²*K g / SINH(DEUKD)
!    Ue :Ai = Hi/2 = (HHO/2)  
!    But here we look to RMS value : Ue => Ue/sqrt(2) and we write Ai²=HHO/8
!=======================================================================
!
!=======================================================================
! CASE 2 : USE OF BOTTOM FRICTION COMPUTED FROM THE STOKES LINEAR THEORY
!          + linear hypothesis abs(U) * U = 8/3PI U
!=======================================================================
! VELOCITY IN Z=-D FOR RANDOM WAVES 
!   Ui = H/2 * (gk/w)*1/ch(kD) 
!=======================================================================
!
      IF (FORMFR.EQ.1) THEN
        DO I=1,NPOIN
          DEUKD=2D0*K%R(I)*H%R(I)
!         UPDATE OF UEB WITH ACTUAL WAVE COMPONENT  
          UEB%R(I)=UEB%R(I)+HHO%R(I)**2/8D0*K%R(I)*GRAV/SINH(DEUKD)
        ENDDO
      ELSEIF (FORMFR.EQ.2) THEN
        DO I=1,NPOIN
          KD=K%R(I)*H%R(I)
!         UPDATE OF UEB WITH ACTUAL WAVE COMPONENT  
          UI      =(8D0/(3D0*PI))*(HHO%R(I)/2D0)*GRAV*K%R(I)
          UI      =UI/(OMEGA*COSH(KD))
          UEB%R(I)=UEB%R(I)+UI**2
        ENDDO  
      ELSE
        WRITE(6,*) 'YOUR OPTION FOR BOTTOM FRICTION IS NOT CORRECT'
        WRITE(6,*) 'CALUEB2 : PLEASE USE FORMFR = 1 OR FORMFR = 2 '
        WRITE(6,*) 'THE CODE IS GOING TO STOP.....................'
        STOP
      ENDIF
!
      RETURN
      END SUBROUTINE

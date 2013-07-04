!                    ****************************
                     SUBROUTINE TOMAWAC_CONSTANTS
!                    ****************************
!
!
!***********************************************************************
! TOMAWAC   V6P3                                   14/06/2011
!***********************************************************************
!
!brief    Sets a number of constants used by Tomawac, like PI, etc.
!
!
!history  J-M HERVOUET (EDF-LNHE)
!+        27/11/2012
!+        V6P3
!+   First version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC ACOS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VARIOUS CONSTANTS LINKED TO PI
!
      PI     = ACOS (-1.D0)
      DEUPI  = 2.D0*PI
      PISUR2 = PI/2.D0
      USDPI  = 1.D0/DEUPI
      GRADEG = 180.D0/PI
      DEGRAD = PI/180.D0
!
!     GRAVITY ACCELERATION
!
      GRAVIT = 9.81D0
!
!     SQUARE AND INVERSE OF EARTH RADIUS
!
      R2 = (6400.D3)**2
      SR = 1.D0/6400.D3
!
!-----------------------------------------------------------------------
!
      RETURN
      END

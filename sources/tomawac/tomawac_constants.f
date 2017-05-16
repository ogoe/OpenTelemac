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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC ACOS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VARIOUS CONSTANTS LINKED TO PI
!
!> SEB @ HRW: ALGORITHMIC DIFFERENTIATION
      PI = 4.D0 * ATAN( 1.D0 )
!      PI  = ACOS (-1.D0)
!< SEB @ HRW
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

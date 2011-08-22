!                    ************************
                     SUBROUTINE FRICTION_USER
!                    ************************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DEFINES FRICTION ZONES (BY NODE).
!
!history  F. HUVELIN
!+        15/04/2004
!+
!+
!
!history  J-M HERVOUET (LNHE)
!+
!+        V5P4
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      !1/ GLOBAL VARIABLES
      !-------------------
      !2/ LOCAL VARIABLES
      !------------------
      INTEGER :: I, NPOIN_CF
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
! EXAMPLE :
! ---------
!
!      NPOIN_CF = CF%DIM1 ! NUMBER OF POINT FOR THE DISCRETIZATION OF CF
!
!      DO I = 1, NPOIN_CF
!         KFROPT%I(I) = 1
!      ENDDO
!=======================================================================!
!=======================================================================!
      RETURN
      END SUBROUTINE FRICTION_USER

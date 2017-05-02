!                    *****************
                     SUBROUTINE CONDIM
!                    *****************
!
!
!***********************************************************************
! TELEMAC3D   V7P3
!***********************************************************************
!
!brief    INITIALISES VELOCITY, DEPTH AND TRACERS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET(LNH)
!+        11/12/2000
!+        V5P1
!+   TELEMAC 3D VERSION 5.1
!
!history
!+        20/04/2007
!+
!+   ADDED INITIALISATION OF DPWAVE
!
!history
!+        23/01/2009
!+
!+   ADDED CHECK OF ZSTAR
!
!history
!+        16/03/2010
!+
!+   NEW OPTIONS FOR BUILDING THE MESH IN CONDIM, SEE BELOW
!
!history  J-M HERVOUET(LNHE)
!+        05/05/2010
!+        V6P0
!+   SUPPRESSED INITIALISATION OF DPWAVE
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
!history  M.S.TURNBULL (HRW), N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        C.-T. PHAM (LNHE)
!+        19/07/2012
!+        V6P2
!+   Addition of the TPXO tidal model by calling CONDI_TPXO
!+   (the TPXO model being coded in module TPXO)
!
!history  C.-T. PHAM (LNHE), M.S.TURNBULL (HRW)
!+        02/11/2012
!+        V6P3
!+   Correction of bugs when initialising velocity with TPXO
!+   or when sea levels are referenced with respect to Chart Datum (CD)
!
!history  C.-T. PHAM (LNHE)
!+        03/09/2015
!+        V7P1
!+   Change in the number of arguments when calling CONDI_TPXO
!
!history  C.-T. PHAM (LNHE)
!+        24/03/2017
!+        V7P3
!+   Split of CONDIM: calls of subroutines to define initial conditions
!    for different variables, one subroutine pro variable
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC3D, EX_CONDIM => CONDIM
      USE DECLARATIONS_TELEMAC3D
!
!     USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!     ORIGIN OF TIME
!
      IF(.NOT.SUIT2) AT = 0.D0
!
!     INITIALISES H, THE WATER DEPTH
!
      CALL CONDI3DH
!
!-----------------------------------------------------------------------
!
!     DATA TO BUILD VERTICAL COORDINATES IN CALCOT
!
      CALL MESH_TRANSF
!
!***********************************************************************
!
!     COMPUTES ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISES VELOCITIES
!
      CALL CONDI3DUVW
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      CALL CONDI3DTRAC
!
!-----------------------------------------------------------------------
!     INITIALISES THE K-EPSILON MODEL (OPTIONAL)
!     WHEN DONE: AKEP = .FALSE.
!
      CALL CONDI3DKEP
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE PRESSURE FIELDS TO 0. IF NON-HYDROSTATIC VERSION
!
      CALL CONDI3DP
!
!-----------------------------------------------------------------------
!
      RETURN
      END

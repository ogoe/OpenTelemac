!                    *****************
                     SUBROUTINE COSAKE
!                    *****************
!
     &(KARMAN,CMU,C1,C2,SIGMAK,SIGMAE,ESTAR,SCHMIT,KMIN,KMAX,EMIN,EMAX)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SETS THE CONSTANTS FOR THE K-EPSILON MODEL.
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
!+        V5P2
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
!| C1             |<--| CONSTANTE DU MODELE K-EPSILON
!| C2             |<--| CONSTANTE DU MODELE K-EPSILON
!| CMU            |<--| CONSTANTE DU MODELE K-EPSILON
!| EMAX           |<--| EPSILON MAXIMUM EN CAS DE CLIPPING
!| EMIN           |<--| EPSILON MINIMUM EN CAS DE CLIPPING
!| ESTAR          |<--| CONSTANTE DU MODELE K-EPSILON
!| KARMAN         |<--| CONSTANTE DE KARMAN
!| KMAX           |<--| K MAXIMUM EN CAS DE CLIPPING
!| KMIN           |<--| K MINIMUM EN CAS DE CLIPPING
!| SCHMIT         |---|
!| SIGMAE         |<--| CONSTANTE DU MODELE K-EPSILON
!| SIGMAK         |<--| CONSTANTE DU MODELE K-EPSILON
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(OUT) :: KMIN,KMAX,EMIN,EMAX
      DOUBLE PRECISION, INTENT(OUT) :: KARMAN,CMU,C1,C2
      DOUBLE PRECISION, INTENT(OUT) :: SIGMAK,SIGMAE,ESTAR,SCHMIT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! KARMAN CONSTANT
!
!     UP TO VERSION 6.0 : 0.41, FROM VERSION 6.1 ON : 0.40
      KARMAN = 0.40D0
      CMU    = 0.09D0
      C1     = 1.44D0
      C2     = 1.92D0
      SIGMAK = 1.00D0
      SIGMAE = 1.30D0
      ESTAR  = 0.15D0
!
! SCHMIDT NUMBER
!
      SCHMIT = 0.50D0
!
! RANGE OF VALUES USED TO CLIP K AND EPSILON
!
      KMIN = 1.D-8
      EMIN = 1.D-8
      KMAX = 1.D10
      EMAX = 1.D10
!
!-----------------------------------------------------------------------
!
      RETURN
      END
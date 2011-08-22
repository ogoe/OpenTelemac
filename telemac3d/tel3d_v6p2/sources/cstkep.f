!                    *****************
                     SUBROUTINE CSTKEP
!                    *****************
!
     & (KARMAN,CMU,C1,C2,SIGMAK,SIGMAE,VIRT,SCHMIT,
     &  KMIN,KMAX,EMIN,EMAX,PRANDTL,ALPHA,BETA,BETAS,OMSTAR,ITURBV)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SETS CONSTANTS OF K-EPSILON AND K-OMEGA MODELS.
!
!history  VINCENT BOYER
!+        01/02/01
!+
!+
!
!history  OLIVER GOETHEL
!+        18/03/04
!+
!+
!
!history  J-M HERVOUET(LNH)
!+        14/12/09
!+        V6P0
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
!| ALPHA          |<->| K-OMEGA CONSTANT
!| BETA           |<->| K-OMEGA CONSTANT
!| BETAS          |<->| K-OMEGA CONSTANT
!| C1             |<->| K-EPSILON CONSTANT
!| C2             |<->| K-EPSILON CONSTANT
!| CMU            |<->| K-EPSILON CONSTANT
!| EMAX           |<->| EPSILON MAXIMUM
!| EMIN           |<->| EPSILON MINIMUM
!| ITURBV         |-->| TURBULENCE MODEL (3:K-EPSILON 7:K-OMEGA)
!| KARMAN         |<->| VON KARMAN CONSTANT
!| KMAX           |<->| K MAXIMUM
!| KMIN           |<->| K MINIMUM
!| OMSTAR         |<->| K-OMEGA CONSTANT
!| PRANDTL        |<->| PRANDTL NUMBER
!| SCHMIT         |<->| SCHMIT NUMBER
!| SIGMAE         |<->| K-EPSILON OR K-OMEGA CONSTANT
!| SIGMAK         |<->| K-EPSILON OR K-OMEGA CONSTANT
!| VIRT           |<->| VIRTUAL ORIGIN FOR EPSILON
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN )   :: ITURBV
      DOUBLE PRECISION, INTENT(INOUT) :: KMIN,KMAX,EMIN,EMAX
      DOUBLE PRECISION, INTENT(INOUT) :: KARMAN,CMU,C1,C2,SIGMAK,SIGMAE
      DOUBLE PRECISION, INTENT(INOUT) :: VIRT,PRANDTL,SCHMIT
      DOUBLE PRECISION, INTENT(INOUT) :: ALPHA,BETA,BETAS,OMSTAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VON KARMAN CONSTANT
!
!     UP TO VERSION 6.0, 0.41  FROM NOW ON : 0.40
!
      KARMAN = 0.40D0
!
!     SCHMIDT NUMBER
!
      SCHMIT = 1.D0
!
!     PRANDTL NUMBER (BETWEEN 0.8 AND 0.9 FOR TEMPERATURE)
!
      PRANDTL = 1.D0
!
!     OTHER CONSTANTS
!
      CMU    = 0.09D0
      C1     = 1.44D0
      C2     = 1.92D0
!
      IF(ITURBV.EQ.3) THEN
        SIGMAK = 1.D0
        SIGMAE = 1.3D0
      ELSEIF(ITURBV.EQ.7) THEN
        SIGMAK = 2.D0
        SIGMAE = 2.D0
      ENDIF
!
!     K-OMEGA MODEL
!
      ALPHA  = 5.D0/9.D0
      BETA   = 3.D0/40.D0
      BETAS  = 0.09D0
!     TO COMPUTE THE FREE SURFACE VALUE OF OMEGA
      OMSTAR  = 100.D0
!     VIRTUAL ORIGIN FOR EPSILON
      VIRT = 0.07D0
!
!     MINIMA AND MAXIMA FOR CLIPPING
!
      IF(ITURBV.EQ.3) THEN
        KMIN = 1.D-10
        EMIN = 1.D-10
        KMAX = 1.D4
        EMAX = 1.D10
      ELSEIF(ITURBV.EQ.7) THEN
        KMIN = 1.D-8
        EMIN = 1.D-3
        KMAX = 1.D-1
        EMAX = 1.D4
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

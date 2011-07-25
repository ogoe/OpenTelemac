!                    *****************************
                     SUBROUTINE BUILD_GLOBAL_BOUND
!                    *****************************
!
     &(KNOLG,NPOIN,NPOIN_TOT,X,Y,K,C,CG,XT,YT,KT,CTT,CGT)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    REBUILDS THE BOUNDARY OF THE MESH (GLOBAL NODES).
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
!
!history  C. DENIS (SINETICS)
!+        16/01/2010
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
!| C              |---|
!| CG             |---|
!| CGT            |---|
!| CTT            |---|
!| K              |---|
!| KNOLG          |---|
!| KT             |---|
!| NPOIN          |---|
!| NPOIN_TOT      |---|
!| X              |---|
!| XT             |---|
!| Y              |---|
!| YT             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: NPOIN_TOT
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN) :: NPTFR
      INTEGER, INTENT(IN) :: NPTFR_TOT
!
      INTEGER, INTENT(IN), DIMENSION(NPOIN) :: KNOLG
      DOUBLE PRECISION, INTENT(IN), DIMENSION(NPOIN)  :: X, Y, K, C,CG
      DOUBLE PRECISION , INTENT(IN), DIMENSION(NPOIN_TOT) :: XT,YT,KT,
     &                                                       CTT,CGT
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR)
      INTEGER, INTENT(IN) :: LIHBORT(NPTFR_TOT)
      INTEGER, INTENT(IN) :: NBOR(NPTFR), NBOR_TOT(NPTFR_TOT)
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'BUILD_GLOBAL_BOUND VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'BUILD_GLOBAL_BOUND IN ITS VOID VERSION'
!
!-----------------------------------------------------------------------
!
      RETURN
      END

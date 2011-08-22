!                    *****************************
                     SUBROUTINE BUILD_GLOBAL_BOUND
!                    *****************************
!
     &(KNOLG,NPOIN,NPOIN_TOT,X,Y,K,C,CG,XT,YT,KT,CTT,CGT)
!
!***********************************************************************
! PARAVOID   V6P1                                   21/08/2010
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
!| C              |-->| LOCAL PHASE VELOCITIES
!| CG             |-->| LOCAL GROUP VELOCITIES
!| CGT            |<--| GLOBAL GROUP VELOCITIES
!| CTT            |<--| GLOBAL PHASE VELOCITIES
!| K              |-->| LOCAL WAVE NUMBER
!| KNOLG          |<--| GLOBAL NUMBER OF A LOCAL POINT IN PARALLEL
!| KT             |<--| GLOBAL WAVE NUMBER
!| NPOIN          |-->| NUMBER OF LOCAL POINTS
!| NPOIN_TOT      |<--| NUMBER OF GLOBAL POINTS
!| X              |-->| LOCAL X VECTOR
!| XT             |<--| GLOBAL X VECTOR
!| Y              |-->| LOCAL X VECTOR
!| YT             |<--| GLOBAL Y VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: NPOIN_TOT
      INTEGER, INTENT(IN) :: NPOIN
!
      INTEGER, INTENT(IN), DIMENSION(NPOIN) :: KNOLG
      DOUBLE PRECISION, INTENT(IN), DIMENSION(NPOIN)  :: X, Y, K, C,CG
      DOUBLE PRECISION , INTENT(IN), DIMENSION(NPOIN_TOT) :: XT,YT,KT,
     &                                                       CTT,CGT
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

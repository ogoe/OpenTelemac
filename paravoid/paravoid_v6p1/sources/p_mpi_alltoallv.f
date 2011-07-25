!                    ***************************
                     SUBROUTINE  P_MPI_ALLTOALLV
!                    ***************************
!
     &(I1,I2,I3,I4,I5,I6,I7,I8,I9,I10)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_ALLTOALLV.
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
!
!history  C. DENIS (SINETICS)
!+        27/10/2009
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
!| I1             |---|
!| I10            |---|
!| I2             |---|
!| I3             |---|
!| I4             |---|
!| I5             |---|
!| I6             |---|
!| I7             |---|
!| I8             |---|
!| I9             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: I1(*),I2(*),I3(*),I4,I5(*),I6(*),I7(*)
      INTEGER, INTENT(IN) :: I8,I9,I10
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE  P_MPI_ALLTOALLV  VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_MPI_ALLTOALLV VOID VERSION'
!
!-----------------------------------------------------------------------
!
      STOP
      END

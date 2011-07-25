!                    *****************************
                     SUBROUTINE GET_MPI_PARAMETERS
!                    *****************************
!
     &(P_INTEGER,P_REAL8,P_UB,P_COMM_WORLD,P_SUCCESS)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
!
!history  J-M HERVOUET (LNHE)
!+        02/02/2009
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
!| P_COMM_WORLD   |---|
!| P_INTEGER      |---|
!| P_REAL8        |---|
!| P_SUCCESS      |---|
!| P_UB           |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(OUT) :: P_INTEGER,P_REAL8,P_UB
      INTEGER, INTENT(OUT) :: P_COMM_WORLD,P_SUCCESS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     HERE VALUES OF MPI 1, NOT USED ANYWAY
!
      P_INTEGER=28
      P_REAL8=11
      P_UB=16
      P_COMM_WORLD=91
      P_SUCCESS=0
!
!-----------------------------------------------------------------------
!
      RETURN
      END

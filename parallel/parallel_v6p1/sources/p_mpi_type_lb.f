!                    *************************
                     SUBROUTINE  P_MPI_TYPE_LB
!                    *************************
!
     &(I1,I2,I3)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_TYPE_LB.
!
!history  C. DENIS (SINETICS)
!+        27/10/2009
!+        V5P6
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
!| I2             |---| 
!| I3             |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I1,I2,I3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL MPI_TYPE_LB(I1,I2,I3)
!
      IF(I3.NE.0) THEN
        WRITE(LU,*) 'P_MPI_TYPE_LB:'
        WRITE(LU,*) 'MPI ERROR ',I3
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
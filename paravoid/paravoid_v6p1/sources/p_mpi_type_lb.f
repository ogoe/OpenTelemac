!                    *************************
                     SUBROUTINE  P_MPI_TYPE_LB
!                    *************************
!
     &(I1,I2,I3)
!
!***********************************************************************
! PARAVOID   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_TYPE_LB.
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
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
!| I1             |-->| DATATYPE 
!| I2             |<--| LOWER BOUND OF THE DATATYPE
!| I3             |<--| ERROR VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: I1,I2,I3
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_MPI_TYPE_LB VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_MPI_TYPE_LB VOID VERSION'
!
!-----------------------------------------------------------------------
!
      STOP
      END

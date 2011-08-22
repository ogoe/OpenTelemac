!                    ************************************
                     SUBROUTINE  P_MPI_TYPE_CREATE_STRUCT
!                    ************************************
!
     &(I1,I2,I3,I4,I5,I6)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_TYPE_STRUCT.
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
!| I1             |-->| NUMBER OF BLOCKS 
!| I2             |-->| NUMBER OF ELEMENTS IN EACH BLOCK
!| I3             |-->| BYTE DISLACEMENT   IN EACH BLOCK
!| I4             |-->| TYPE OF ELEMENTS   IN EACH BLOCK 
!| I5             |<--| NEW DATATYPE
!| I6             |<--| ERROR VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
       include "mpif.h"

      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I1,I5,I6
      INTEGER, INTENT(IN) :: I2(I1),I4(I1)
      INTEGER  (KIND=MPI_ADDRESS_KIND) :: I3(I1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL MPI_TYPE_CREATE_STRUCT(I1,I2,I3,I4,I5,I6)
!
      IF(I6.NE.0) THEN
        WRITE(LU,*) 'P_MPI_TYPE_STRUCT:'
        WRITE(LU,*) 'MPI ERROR ',I6
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

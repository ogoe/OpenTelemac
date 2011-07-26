!                    *************************
                     SUBROUTINE P_MPI_ALLTOALL
!                    *************************
!
     &(I1,I2,I3,I4,I5,I6,I7,I8)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_ALLTOALL.
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
!| I1             |-->| SEND BUFFER  
!| I2             |-->| SPECIFIES THE NUMBER OF ELEMENTS TO SEND TO EACH
!|                |   | PROCESSOR  
!| I3             |-->| DATA TYPE OF SEND BUFFER ELEMENTS
!| I4             |-->| RECEIVE BUFFER
!| I5             |-->| SPECIFIES THE MAXIMUM NUMBER OF ELEMENTS THAT 
!|                |   | CAN BE RECEIVED FROM EACH PROCESSOR 
!| I6             |-->| DATA TYPE OF RECEIVE BUFFER ELEMENTS
!| I7             |-->| COMMUNICATOR 
!| I8             |-->| ERROR VALUE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I1(*),I2,I3,I4(*),I5,I6,I7,I8
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL MPI_ALLTOALL(I1,I2,I3,I4,I5,I6,I7,I8)
!
      IF(I8.NE.0) THEN
        WRITE(LU,*) 'P_MPI_ALLTOALL:'
        WRITE(LU,*) 'MPI ERROR ',I8
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

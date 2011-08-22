!                    ************************
                     SUBROUTINE P_WAIT_PARACO
!                    ************************
!
     &(IBUF,NB)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WAITS AT THE END OF PARACO.
!
!history  PASCAL VEZOLLE (IBM)
!+        23/06/2008
!+        V5P9
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
!| IBUF           |--->| ARRAY OF REQUESTS 
!| NB             |--->| LISTS LENGHT 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INCLUDE 'mpif.h'
!
      INTEGER IBUF(*), NB, IER
      INTEGER WAIT_REQ(MPI_STATUS_SIZE,100)
      SAVE
!
!-----------------------------------------------------------------------
!
      IF(NB.GT.100) THEN
        WRITE(LU,*) 'WAIT_PARACO:'
        WRITE(LU,*) 'DIMENSION OF WAIT_REQ TOO SMALL'
        STOP
      ENDIF
!
      CALL MPI_WAITALL(NB,IBUF,WAIT_REQ,IER)
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'WAIT_PARACO:'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END

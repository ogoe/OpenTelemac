!                    *****************
                     SUBROUTINE P_EXIT
!                    *****************
!
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    END OF MPI.
!
!history  RAINER JOHANNI (SGI MUNICH)
!+        **/10/1999
!+
!+   ADAPTED FOR MPI
!
!history  J.A. JANKOWSKI (BAW KARLSRUHE)
!+        28/12/1999
!+
!+   RELEASE 5.0 MODIFIED
!
!history  J-M HERVOUET (LNHE)
!+        16/06/2009
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INCLUDE 'mpif.h'
!
      INTEGER IER
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) WRITE(LU,*) 'SORTIE DE MPI'
      IF(LNG.EQ.2) WRITE(LU,*) 'EXITING MPI'
      WRITE(LU,*) ' '
!
!     AVOIDS EXITING BEFORE EVERYTHING IS DONE IN OTHER PROCESSORS
!
      CALL MPI_BARRIER(MPI_COMM_WORLD,IER)
!
!     EXITS
!
      CALL MPI_FINALIZE(IER)
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_EXIT: ERREUR DANS MPI_FINALIZE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_EXIT: ERROR IN MPI_FINALIZE'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

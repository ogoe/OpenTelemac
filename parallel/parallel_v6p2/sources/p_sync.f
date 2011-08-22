!                    *****************
                     SUBROUTINE P_SYNC
!                    *****************
!
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SYNCHRONISES ALL THE PROCESSORS.
!
!warning  THIS ROUTINE MUST BE CALLED BY ALL THE NODES/PROCESSORS,
!+            OTHERWISE THE PROGRAM WILL "HANG"
!
!history  REINHARD HINKELMANN (HANOVER)
!+        08/06/1996
!+
!+   MODIFIED
!
!history  J-M HERVOUET (LNH)
!+        17/12/1996
!+
!+   MODIFIED
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
!history  HANS HERRMANN (HANOVER)
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
      CALL MPI_BARRIER(MPI_COMM_WORLD,IER)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_SYNC : ERREUR DANS MPI_BARRIER'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_SYNC: ERROR IN MPI_BARRIER'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

!                    ***********************
                     INTEGER FUNCTION P_ISUM
!                    ***********************
!
     &(MYPART)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SUM OF VALUES FROM ALL THE PROCESSORS.
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
!+        10/06/2005
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
!| MYPART         |-->| SEND BUFFER.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INCLUDE 'mpif.h'
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: MYPART
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IER
!
!-----------------------------------------------------------------------
!
      CALL MPI_ALLREDUCE(MYPART,P_ISUM,1,MPI_INTEGER,MPI_SUM,
     &                   MPI_COMM_WORLD,IER)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_ISUM: ERREUR DANS MPI_ALLREDUCE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_ISUM: ERROR IN MPI_ALLREDUCE'
        WRITE (LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

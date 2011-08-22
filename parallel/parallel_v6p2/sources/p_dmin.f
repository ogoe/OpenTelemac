!                    ********************************
                     DOUBLE PRECISION FUNCTION P_DMIN
!                    ********************************
!
     &(MYPART)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MINIMUM VALUE FROM ALL THE PROCESSORS.
!
!history  J-M HERVOUET (LNHE)
!+        08/01/1997
!+
!+
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
!| MYPART         |-->| SEND BUFFER
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
      DOUBLE PRECISION, INTENT(IN) :: MYPART
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IER
!
!-----------------------------------------------------------------------
!
      CALL MPI_ALLREDUCE(MYPART,P_DMIN,1,MPI_DOUBLE_PRECISION,MPI_MIN,
     &                   MPI_COMM_WORLD,IER)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_DMIN: ERREUR DANS MPI_ALLREDUCE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_DMIN: ERROR IN MPI_ALLREDUCE'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

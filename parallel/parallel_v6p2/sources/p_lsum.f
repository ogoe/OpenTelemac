!                    *****************
                     SUBROUTINE P_LSUM
!                    *****************
!
     &(IARG1,LARG2)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    REDUCTION OF A VECTOR OF LOGICALS WITH DIFFUSION OF
!+                THE RESULT TO ALL THE PROCESSORS.
!
!history  O.BOITEAU (SINETICS)
!+        01/07/2006
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
!| IARG1          |-->| BUFFER SIZE
!| LARG2          |<->| SEND BUFFER
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
      INTEGER, INTENT(IN) :: IARG1
      LOGICAL, DIMENSION(IARG1), INTENT(INOUT) :: LARG2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, DIMENSION(:), ALLOCATABLE :: LAUX
      INTEGER IER,I
!
!-----------------------------------------------------------------------
!
      ALLOCATE(LAUX(IARG1),STAT=IER)
      IF (IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*)'P_LSUM: ERREUR DANS ALLOCATION MEMOIRE'
        IF(LNG.EQ.2) WRITE(LU,*)'P_LSUM: ERROR IN MEMORY ALLOCATION'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,IARG1
        LAUX(I)=LARG2(I)
      ENDDO
!
      CALL MPI_ALLREDUCE(LAUX,LARG2,IARG1,MPI_LOGICAL,
     &                   MPI_LOR,MPI_COMM_WORLD,IER)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_LSUM: ERREUR DANS MPI_ALLREDUCE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_LSUM: ERROR IN MPI_ALLREDUCE'
        WRITE(LU,*) 'MPI ERROR: ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DEALLOCATE(LAUX)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

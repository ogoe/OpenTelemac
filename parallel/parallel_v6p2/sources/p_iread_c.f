!                    ********************
                     SUBROUTINE P_IREAD_C
!                    ********************
!
     &(BUFFER,NBYTES,SOURCE,ITAG,IREQ)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  PASCAL VEZOLLES (IBM)
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
!| BUFFER         |-->| ZONE TAMPON POUR LES DONNEES
!|                |   | BUFFER / PUFFERFELD
!| IREQ           |-->| NUMERO DE REQUEST POUR MPI_IRECV
!| ITAG           |-->| MESSAGE TAG
!| NBYTES         |-->| NOMBRE DE BYTES A TRANSMETTRE
!|                |   | LENGTH IN BYTES / LAENGE IN BYTES
!| SOURCE         |-->| ORIGINE DES DONNEES
!|                |   | TID OF THE SENDER / KNOTEN-ID DES SENDER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INCLUDE 'mpif.h'
!
      INTEGER NBYTES,SOURCE,ITAG,IREQ,IER
      CHARACTER(LEN=*) BUFFER
!
!-----------------------------------------------------------------------
!     RECEIVES DATA
!-----------------------------------------------------------------------
!
      CALL MPI_IRECV(BUFFER,NBYTES,MPI_BYTE,SOURCE,ITAG,
     &               MPI_COMM_WORLD,IREQ,IER)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_IREAD: ERREUR IN MPI_IRECV'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_IREAD: ERROR IN MPI_IRECV'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

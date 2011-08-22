!                    ******************
                     SUBROUTINE P_IWRIT
!                    ******************
!
     &(BUFFER,NBYTES,DEST,ITAG,IREQ)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXCHANGES VALUES BETWEEN PROCESSORS.
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
!| DEST           |-->| DESTINATION DES DONNEES
!|                |   | TID OF THE DEST.  / KNOTEN-ID DES EMPFAENGERS
!| IREQ           |-->| NUMERO DE REQUEST POUR MPI_ISEND
!| ITAG           |-->| MESSAGE TAG
!| NBYTES         |-->| NOMBRE DE BYTES A TRANSMETTRE
!|                |   | LENGTH IN BYTES / LAENGE IN BYTES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INCLUDE 'mpif.h'
!
      INTEGER NBYTES,DEST,ITAG,IREQ,IER
      DOUBLE PRECISION BUFFER(*)
!
!-----------------------------------------------------------------------
!
      CALL MPI_ISEND(BUFFER,NBYTES,MPI_BYTE,DEST,ITAG,
     &               MPI_COMM_WORLD,IREQ,IER)
!
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_IWRIT: ERREUR IN MPI_ISEND'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_IWRIT: ERROR IN MPI_ISEND'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END

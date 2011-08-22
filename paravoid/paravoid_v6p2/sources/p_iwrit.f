!                    ******************
                     SUBROUTINE P_IWRIT
!                    ******************
!
     &(BUFFER,NBYTES,DEST,ITAG,IREQ)
!
!***********************************************************************
! PARAVOID   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXCHANGES VALUES BETWEEN PROCESSORS.
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
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
      INTEGER NBYTES,DEST,ITAG,IREQ,IER
      DOUBLE PRECISION BUFFER(*)
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_IWRIT VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_IWRIT IN ITS VOID VERSION'
!
!----------------------------------------------------------------------
!
      RETURN
      END

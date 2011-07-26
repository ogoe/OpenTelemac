!                    *****************
                     SUBROUTINE P_WRIT
!                    *****************
!
     &(BUFFER,NBYTES,DEST,TYPE)
!
!***********************************************************************
! PARAVOID   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXCHANGES VALUES BETWEEN PROCESSORS.
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
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
!history  HANS HERRMANN (HANOVRE)
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
!| NBYTES         |-->| NOMBRE DE BYTES A TRANSMETTRE
!|                |   | LENGTH IN BYTES / LAENGE IN BYTES
!| TYPE           |-->| TYPE DES DONNEES (MSGTAG DE PVM)
!|                |   | 0 - STRING
!|                |   | 1 - BYTE1
!|                |   | 2 - INTEGER2
!|                |   | 3 - INTEGER4
!|                |   | 4 - REAL4
!|                |   | 5 - COMPLEX8
!|                |   | 6 - REAL8
!|                |   | 7 - COMPLEX16
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NBYTES,DEST,TYPE
      DOUBLE PRECISION BUFFER(*)
!
!----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_WRIT VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_WRIT IN ITS VOID VERSION'
!
!----------------------------------------------------------------------
!
      STOP
      END

!                    *****************
                     SUBROUTINE P_READ
!                    *****************
!
     &(BUFFER,NBYTES,SOURCE,TYPE)
!
!***********************************************************************
! PARAVOID   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    RECEIVES DATA.
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
!
!history  REINHARD HINKELMANN (HANOVER)
!+        08/06/1996
!+
!+   MODIFIED
!
!history  J-M MERVOUET (LNH)
!+        17/12/1996
!+
!+   MODIFIED
!
!history  HANS HERRMANN (HANOVRE)
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
!| BUFFER         |-->| ZONE TAMPON POUR LES DONNEES
!|                |   | BUFFER / PUFFERFELD
!| NBYTES         |-->| NOMBRE DE BYTES A TRANSMETTRE
!|                |   | LENGTH IN BYTES / LAENGE IN BYTES
!| SOURCE         |-->| ORIGINE DES DONNEES
!|                |   | TID OF THE SENDER / KNOTEN-ID DES SENDER
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
      INTEGER NBYTES, SOURCE, TYPE
      DOUBLE PRECISION BUFFER(*)
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_READ VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_READ IN ITS VOID VERSION'
!
!-----------------------------------------------------------------------
!
      STOP
      END

!                    *****************
                     SUBROUTINE P_INIT
!                    *****************
!
     &(CHAINE,NCAR,IPID,NCSIZE)
!
!***********************************************************************
! PARAVOID   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES.
!+                REGISTERS PROGRAM WITH PARASTATION.
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
!
!history  HANS HERRMANN (HANOVER)
!+        **/06/1996
!+
!+
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
!history  P. VEZOLLE (IBM)
!+        16/05/2008
!+        V5P9
!+   MODIFIED (SIZE OF EXTENSION)
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
!| CHAINE         |---| WORKING DIRECTORY
!| IPID           |---| PROCESSUS ID
!| NCAR           |---| SIZE OF THE CHARACTER STRING
!| NCSIZE         |---| NUMBER OF MPI PROCESSUS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER IPID,NCSIZE
!
      CHARACTER*144 CHAINE
      INTEGER NCAR
!
      NCAR = 0
      CHAINE =' '
      IPID=0
!
!     IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_INIT VERSION VIDE'
!     IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_INIT IN ITS VOID VERSION'
!
!-----------------------------------------------------------------------
!
      RETURN
      END

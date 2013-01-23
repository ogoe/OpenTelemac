!                    *****************
                     SUBROUTINE P_LSUM
!                    *****************
!
     &(IARG1,LARG2)
!
!***********************************************************************
! PARAVOID   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    REDUCTION OF A VECTOR OF LOGICALS WITH DIFFUSION OF
!+                THE RESULT TO ALL THE PROCESSORS.
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IARG1
      LOGICAL, DIMENSION(IARG1), INTENT(OUT) :: LARG2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_LSUM VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_LSUM IN ITS VOID VERSION'
!
!     JMH 14/01/2008, TO AVOID A WARNING CAUSED BY INTENT(OUT)
      LARG2 = .FALSE.
!
      RETURN
      END

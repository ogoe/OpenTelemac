!                    *****************
                     SUBROUTINE PTTOEL
!                    *****************
!
     &(XEL,X,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GOES FROM A VECTOR BY POINTS TO A VECTOR BY ELEMENTS.
!
!history  J-M HERVOUET (LNH)
!+        10/01/95
!+        V5P6
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
!| MESH           |-->| MESH STRUCTURE
!| X              |-->| VECTOR DEFINED PER POINT
!| XEL            |<--| SAME VECTOR DEFINED PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PTTOEL => PTTOEL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  VECTORS STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: X
      TYPE(BIEF_OBJ), INTENT(INOUT) :: XEL
!
!-----------------------------------------------------------------------
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(X%ELM.EQ.11) THEN
!
        CALL PTEL11(XEL%R,X%R,MESH%IKLE%I,MESH%NELMAX,MESH%NELEM)
!
      ELSE
       IF (LNG.EQ.1) WRITE(LU,100) X%ELM
       IF (LNG.EQ.2) WRITE(LU,101) X%ELM
100    FORMAT(1X,'PTTOEL (BIEF) : IELM = ',1I6,' ELEMENT NON PREVU')
101    FORMAT(1X,'PTTOEL (BIEF) : IELM = ',1I6,' ELEMENT NOT AVAILABLE')
       CALL PLANTE(1)
       STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

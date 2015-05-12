!                    **********************
                     SUBROUTINE BIEF_DEALLOBJ
!                    **********************
!
     &(OBJ)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    DEALLOCATES MEMORY FOR A BIEF_OBJ STRUCTURE.
!
!history  J-M HERVOUET (LNH)
!+        25/05/2013
!+        V6P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MAT            |<->| THE MATRIX TO BE ALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_DEALLOBJ => BIEF_DEALLOBJ
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: OBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ! VECTOR
      IF(OBJ%TYPE.EQ.2) THEN
        CALL BIEF_DEALLVEC(OBJ)
      ! MATRICE
      ELSEIF(OBJ%TYPE.EQ.3) THEN
        CALL BIEF_DEALLMAT(OBJ)
      ! BLOCK
      ELSEIF(OBJ%TYPE.EQ.4) THEN
        CALL DEALLBLO(OBJ)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END


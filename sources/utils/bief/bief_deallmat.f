!                    **********************
                     SUBROUTINE BIEF_DEALLMAT
!                    **********************
!
     &(MAT)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    DEALLOCATES MEMORY FOR A REAL MATRIX STRUCTURE.
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
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: MAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL BIEF_DEALLVEC(MAT%D)
      DEALLOCATE(MAT%D)
      CALL BIEF_DEALLVEC(MAT%X)
      DEALLOCATE(MAT%X)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
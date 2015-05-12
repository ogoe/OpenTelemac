!                    **********************
                     SUBROUTINE BIEF_DEALLVEC
!                    **********************
!
     &(VEC)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DEALLOCATES MEMORY FOR A VECTOR STRUCTURE.
!
!history  Y AUDOUIN (LNHE)
!+        23/05/2013
!+        V6P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| VEC            |<--| VECTOR TO BE ALLOCATED
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
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: VEC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(VEC%NAT.EQ.1) THEN
        IF(ASSOCIATED(VEC%R)) DEALLOCATE(VEC%R)
      ELSEIF(VEC%NAT.EQ.2) THEN
        IF(ASSOCIATED(VEC%I)) DEALLOCATE(VEC%I)
      ELSE
        WRITE(LU,*) 'UNKNOWN NAT IN DEALLVEC FOR :', VEC%NAME
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BIEF_DEALLVEC

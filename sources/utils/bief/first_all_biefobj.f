!                    ****************************
                     SUBROUTINE FIRST_ALL_BIEFOBJ
!                    ****************************
!
     &(OBJ)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    Allocates a BIEF_OBJ object which is itself a component of a 
!+        BIEF_OBJ. Nullifies all pointers in this BIEF_OBJ structure.
!+        This is not done by compilers.
!
!history  J-M HERVOUET (jubilado)
!+        04/11/2016
!+        V7P3
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| OBJ            |<->| BIEF_OBJ TO BE ALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FIRST_ALL_BIEFOBJ => FIRST_ALL_BIEFOBJ
!
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), POINTER, INTENT(INOUT) :: OBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC ASSOCIATED
!
!-----------------------------------------------------------------------
!
      IF(.NOT.ASSOCIATED(OBJ)) THEN
        ALLOCATE(OBJ)
!       NULLIFYING ALL POINTERS OF BIEF_OBJ STRUCTURE
        NULLIFY(OBJ%R)
        NULLIFY(OBJ%D)
        NULLIFY(OBJ%X)
        NULLIFY(OBJ%I)
        NULLIFY(OBJ%ADR)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

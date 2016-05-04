!                    *****************
                     SUBROUTINE DEALLBLO
!                    *****************
!
     &( BLO )
!
!***********************************************************************
! BIEF   V6P3
!***********************************************************************
!
!brief    DEALLOCATES MEMORY FOR A BLOCK STRUCTURE.
!
!history  Y AUDOUIN (LNHE)
!+        25/05/2015
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BLO            |-->| THE BLOCK TO BE DEALLOCATED
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
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, J, NLEN
      LOGICAL INVEC
!
!-----------------------------------------------------------------------
!
      ! If the block contains nothing ... do nothing
      IF(BLO%N.GT.0) THEN
        DO I=1,BLO%N
            ! If the element of the block was added using bief_all_in_vec
            ! This means we have to deallocate the vector and the pointer
            IF(BLO%ADR(I)%P%TYPDIA(1:1).EQ.'W') THEN
              CALL BIEF_DEALLVEC(BLO%ADR(I)%P)
              BLO%ADR(I)%P%TYPDIA = ' '
              DEALLOCATE(BLO%ADR(I)%P)
            ENDIF
          NULLIFY(BLO%ADR(I)%P)
        ENDDO
        DEALLOCATE(BLO%ADR)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

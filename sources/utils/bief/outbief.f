!                    *****************
                     SUBROUTINE OUTBIEF
!                    *****************
     &(MESH)
!
!***********************************************************************
! BIEF   V7P0                                   28/03/2014
!***********************************************************************
!
!brief    CLEAN UP THE DATA FROM MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(MODASS.EQ.2) THEN
        DEALLOCATE(MESH%WI8)
        DEALLOCATE(MESH%TI8)
      ENDIF 
!
!     THESE STUCTURES ARE ALLOCATED IN PARINI
      CALL BIEF_DEALLVEC(MESH%NB_NEIGHB_PT    )
      CALL BIEF_DEALLVEC(MESH%LIST_SEND       )
      CALL BIEF_DEALLVEC(MESH%NH_COM          )
      CALL BIEF_DEALLVEC(MESH%NB_NEIGHB_PT_SEG)
      CALL BIEF_DEALLVEC(MESH%LIST_SEND_SEG   )
      CALL BIEF_DEALLVEC(MESH%NH_COM_SEG      )
      CALL BIEF_DEALLVEC(MESH%BUF_SEND        )
      CALL BIEF_DEALLVEC(MESH%BUF_RECV        )
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END

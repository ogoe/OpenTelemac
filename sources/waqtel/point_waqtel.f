!                    ***********************
                     SUBROUTINE POINT_WAQTEL
!                    ***********************
     & (WAQPROCESS,MESH,IELM1,VENT,WINDX,WINDY)
!
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    Memory allocation of structures, aliases, blocks...
!
!history  R ATA (LNHE)
!+        13/05/2015
!+        V7P1
!+   
!+        Creation of the file   
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_WAQTEL
      USE INTERFACE_WAQTEL, EX_POINT_WAQTEL => POINT_WAQTEL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL,         INTENT(IN   ) :: VENT
      INTEGER,         INTENT(IN   ) :: IELM1,WAQPROCESS
      TYPE(BIEF_OBJ ), INTENT(INOUT) :: WINDX,WINDY
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     IF WAQTEL
      IF(WAQPROCESS.EQ.1.OR.WAQPROCESS.EQ.3) THEN
        CALL BIEF_ALLVEC(1,K2   ,'K2    ',IELM1,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,K2   ,'K2    ',    0,1,0,MESH)
      ENDIF
!     WIND GIVEN IN P1
      IF(.NOT.VENT) THEN
        CALL BIEF_ALLVEC(1,WINDX,'WINDX ',IELM1,1,1,MESH)
        CALL BIEF_ALLVEC(1,WINDY,'WINDY ',IELM1,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,WINDX,'WINDX ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,WINDY,'WINDY ',    0,1,0,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

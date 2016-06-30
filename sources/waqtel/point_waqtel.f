!                    ***********************
                     SUBROUTINE POINT_WAQTEL
!                    ***********************
!
     &(WAQPROCESS,MESH2D,IELM1,VENT,WINDX,WINDY,MESH3D,IELM3)
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
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_WAQTEL,ONLY:K2,RAYEFF
      USE INTERFACE_WAQTEL, EX_POINT_WAQTEL => POINT_WAQTEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL,         INTENT(IN   ) :: VENT
      INTEGER,         INTENT(IN   ) :: IELM1,WAQPROCESS
      TYPE(BIEF_OBJ ), INTENT(INOUT) :: WINDX,WINDY
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE(BIEF_MESH), INTENT(INOUT),OPTIONAL :: MESH3D
      INTEGER,         INTENT(IN   ),OPTIONAL :: IELM3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      IF(WAQPROCESS.EQ.1.OR.WAQPROCESS.EQ.3) THEN
        CALL BIEF_ALLVEC(1,K2   ,'K2    ',IELM1,1,1,MESH2D)
      ELSE
        CALL BIEF_ALLVEC(1,K2    ,'K2    ',    0,1,0,MESH2D)
      ENDIF
!
!     SUN RAY EFFECTS  CAN BE 2D OR 3D
!
      IF(WAQPROCESS.EQ.2.OR.WAQPROCESS.EQ.3) THEN
        IF(PRESENT(MESH3D))THEN
          CALL BIEF_ALLVEC(1,RAYEFF,'RAYEFF',IELM3,1,1,MESH3D)
        ELSE
          CALL BIEF_ALLVEC(1,RAYEFF,'RAYEFF',IELM1,1,1,MESH2D)
        ENDIF
      ELSE
        IF(PRESENT(MESH3D))THEN
          CALL BIEF_ALLVEC(1,RAYEFF,'RAYEFF',    0,1,0,MESH3D)
        ELSE
          CALL BIEF_ALLVEC(1,RAYEFF,'RAYEFF',    0,1,0,MESH2D)
        ENDIF
      ENDIF
!
!     WIND GIVEN IN P1
!
      IF(.NOT.VENT) THEN
        CALL BIEF_ALLVEC(1,WINDX,'WINDX ',IELM1,1,1,MESH2D)
        CALL BIEF_ALLVEC(1,WINDY,'WINDY ',IELM1,1,1,MESH2D)
      ELSE
        CALL BIEF_ALLVEC(1,WINDX,'WINDX ',    0,1,0,MESH2D)
        CALL BIEF_ALLVEC(1,WINDY,'WINDY ',    0,1,0,MESH2D)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

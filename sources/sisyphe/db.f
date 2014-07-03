!                        *******************
                         LOGICAL FUNCTION DB
!                        *******************
!
     &(J_GLOBAL, TIMESTAMP)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!BRIEF    CHECKS IF A CERTAIN POINT SHOULD BE DEBUGGED FOR THIS TIMESTEP
!+        ROUTINE FOR USER DEFINED SUPERVISION OF POINTS
!+
!
!HISTORY  UWE MERKEL
!+        2011
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J_GLOBAL              |<--| POINT ID
!| T                     |<--| TIMESTEP ID
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE, ONLY: MESH, CVSM_OUT, LT, ENTET
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)    :: J_GLOBAL
      INTEGER, INTENT(IN)    :: TIMESTAMP
!
      INTEGER J

      J = J_GLOBAL
      DB = .FALSE.

      !IF (J.EQ.1) DB = .TRUE.

      !USE THIS TO LIMIT OUTPUT TO CAS_FILE DEFINED TIMESTEPS
      IF(CVSM_OUT) THEN
        !IF ( (J.EQ.502) )DB = .TRUE.
        !IF ( (J.EQ.1156) )DB = .TRUE.
        !IF ( (J.EQ.4899) )DB = .TRUE.
        !IF ( (J.EQ.4561) )DB = .TRUE.
        !IF ( (J.EQ.4813) )DB = .TRUE.
        !IF ( (J.EQ.5198) )DB = .TRUE.
        !IF ( (J.EQ.4851) )DB = .TRUE.
        !IF ( (J.EQ.4273) )DB = .TRUE.
        !IF ( (J.EQ.4956) )DB = .TRUE.
        !IF ( (J.EQ.4862) )DB = .TRUE.
        !IF ( (J.EQ.3949) )DB = .TRUE.
        !IF ( (J.EQ.513) )DB = .TRUE.
      ENDIF

      !USE THIS TO SET AN OUTPUT FOR A CERTAIN TIMESTEP, INDEPENDENT OF POINT NUMBER AND CAS FILE
      !IF ( (J.EQ.-1).AND.(T.GE.-1)) DB = .TRUE.
      !IF ( (J.EQ.1553)) DB = .TRUE.

      !USE THIS TO SET AN OUTPUT FOR A CERTAIN NODE, WITH TIMESTEP RULES
      !IF ( (J.EQ.948).AND.(AT.GE.0.D0)) DB = .TRUE.


      !USE THIS TO SET AN OUTPUT FOR A CERTAIN NODE, AT A SPECIFIC TIMESTEP
      !IF ( (J.EQ.27).AND.(LT.GE.TIMESTAMP)) DB = .TRUE.
      !IF ( (J.EQ.1974).AND.(LT.GE.TIMESTAMP)) DB = .TRUE.
      !IF ( (J.EQ.1129).AND.(LT.GE.TIMESTAMP)) DB = .TRUE.
      !IF ( (J.EQ.2032).AND.(LT.GE.TIMESTAMP)) DB = .TRUE.
      !IF ( (J.EQ.186).AND.(LT.GE.TIMESTAMP)) DB = .TRUE.

      END FUNCTION DB

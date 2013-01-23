!        ************************************************
         LOGICAL FUNCTION DB(J_GLOBAL, TIMESTAMP)
!        ************************************************
!
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    Checks if a certain Point should be debugged for this timestep
!+        Routine for USER DEFINED SUPERVISION OF POINTS
!+
!
!history  UWE MERKEL
!+        2011
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J_Global              |<--| Point ID
!| T                     |<--| Timestep ID
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!



      USE DECLARATIONS_SISYPHE, ONLY: MESH, CVSM_OUT, LT, ENTET


      implicit none

      integer, INTENT(IN)    :: J_Global
      integer, INTENT(IN)    :: TimeStamp

      integer J

            J = J_Global
            db = .false.

                if (J.eq.1) db = .true.

         !Use this to limit output to CAS_FILE DEFINED TIMESTEPS
            if(CVSM_OUT) THEN
              !if ( (J.eq.502) )db = .true.
              !if ( (J.eq.1156) )db = .true.
              !if ( (J.eq.4899) )db = .true.
              !if ( (J.eq.4561) )db = .true.
              !if ( (J.eq.4813) )db = .true.
              !if ( (J.eq.5198) )db = .true.
              !if ( (J.eq.4851) )db = .true.
              !if ( (J.eq.4273) )db = .true.
              !if ( (J.eq.4956) )db = .true.
              !if ( (J.eq.4862) )db = .true.
              !if ( (J.eq.3949) )db = .true.
              !if ( (J.eq.513) )db = .true.
            endif

         !Use this to set an output for a certain timestep, independent of point number and cas file
            !if ( (J.eq.-1).and.(T.ge.-1)) db = .true.
            !if ( (J.eq.1553)) db = .true.

         !Use this to set an output for a certain node, with timestep rules
            !if ( (J.eq.948).and.(AT.ge.0D0)) db = .true.


         !Use this to set an output for a certain node, at a specific timestep
            !if ( (J.eq.27).and.(LT.ge.TimeStamp)) db = .true.
            !if ( (J.eq.1974).and.(LT.ge.TimeStamp)) db = .true.
            !if ( (J.eq.1129).and.(LT.ge.TimeStamp)) db = .true.
            !if ( (J.eq.2032).and.(LT.ge.TimeStamp)) db = .true.
            !if ( (J.eq.186).and.(LT.ge.TimeStamp)) db = .true.

        END FUNCTION DB

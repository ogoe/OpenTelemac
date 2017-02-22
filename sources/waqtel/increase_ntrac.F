!                       *************************
                        SUBROUTINE INCREASE_NTRAC
!                       *************************
     &  (NTRAC,IND_T,WAQPROCESS,ADDTR)
!
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brieF INCREASE NTRAC DEPENDING ON THE WAQ QUALITY MODULES
!
!
!history  R.ATA
!+        12/09/2014
!+        V7P0
!+        CREATION
!+
!history  M.JODEAU
!+        05/2016
!+        V7P3
!+        AED2 coupling
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ADDTR          |<->| NUMBER OF TRACERS TO BE INCREASED
!| NTRAC          |<--| TOTAL NUMBER OF TRACERS
!| IND_T          |<--| INDEX OF TEMPERATURE
!| WAQPROCESS     |-->| WAQ PROCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER, INTENT(INOUT)::  IND_T,NTRAC,ADDTR
      INTEGER, INTENT(IN)   ::  WAQPROCESS
!
      INTEGER I
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     FOR THE CASE AED2 (6), ADDTR HAS BEEN CALCULATED BEFORE, WHEN
!     CALLING INIT_AED2_MODELS IN LECDON_TELEMAC3D
!     ADDTR = 0
!
      SELECT CASE(WAQPROCESS)
!       O2 MODULE
        CASE(1)
          ADDTR = 3
!       BIOMASS MODULE
        CASE(2)
          ADDTR = 5
!       EUTRO MODULE
        CASE(3)
          ADDTR = 8
!       MICROPOL MODULE
        CASE(4)
          ADDTR = 5
!       THERMIC MODULE
        CASE(5)
          IF(IND_T.EQ.0)THEN
            ADDTR = 1
            IND_T = NTRAC+1
          ENDIF
#if defined (HAVE_AED2)
        CASE(6)
#endif
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,10) WAQPROCESS
          ELSE
            WRITE(LU,20) WAQPROCESS
          ENDIF
          CALL PLANTE(1)
          STOP
!
      END SELECT
      NTRAC=NTRAC+ADDTR
!
      RETURN
!-----------------------------------------------------------------------
!     MESSAGES
10    FORMAT(1X,'INCREASE_NTRAC: MODULE WAQ INCONNU : ',I4)
20    FORMAT(1X,'INCREASE_NTRAC: UNKNOWN WAQ MODULE : ',I4)
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE

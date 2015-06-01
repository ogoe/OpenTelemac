!                       **************************
                        SUBROUTINE INCREASE_NTRAC
!                       **************************
     &  (ADDTR,WAQ,WAQPROCESS,SECCURRENT,MOTCAR,NTRTOT,NTRAC)
!
!
!***********************************************************************
! TELEMAC2D   V7P0
!***********************************************************************
!
!brieF INCREASE NTRAC DEPENDING ON THE WAQ QUALITY MODULES
!
!
!history  R.ATA
!+        12/09/2014
!+        V7P0
!+        CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ADDTR          |<--| NUMBER OF TRACERS TO BE INCREASED
!| MOTCAR         |-->| KEY-WORDS OF TYPE CHARACTER
!| NTRAC          |-->| NUMBER OF TRACERS
!| NTRTOT         |-->| TOTAL NUMBER OF TRACERS
!| WAQ            |-->| THERE IS WAQ OR NOT
!| SECCURRENT     |-->| THERE IS SECONDARY CURRENTS OR NOT
!| WAQPROCESS     |-->| WAQ PROCESS ()
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC2D, ONLY: MAXKEY,IND_T,IND_SEC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER, INTENT(INOUT)::  ADDTR
      INTEGER, INTENT(IN)   ::  WAQPROCESS,NTRTOT,NTRAC
      LOGICAL, INTENT(IN)   ::  WAQ, SECCURRENT
      CHARACTER(LEN=144), INTENT(IN) :: MOTCAR(MAXKEY)
!
      INTEGER I
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      ADDTR   = 0
      IND_T   = 0
      IND_SEC = 0
!     IF SECONDARY CUURRENT
      IF(SECCURRENT)THEN
        ADDTR   = ADDTR +1
        IND_SEC = NTRAC + 1
      ENDIF
!     WATER QUALAITY
      IF(WAQ)THEN
!       LOOKS FOR TEMPERATURE IN TRACER NAMES
        IF(WAQPROCESS.EQ.5)THEN
          IF(NTRTOT.GT.0.AND.NTRAC.GT.0) THEN
            DO I=1,NTRTOT
              IF(MOTCAR(NTRTOT+I-1)(1:8).EQ.'TEMPERAT')THEN
                IND_T = I
              ENDIF
            ENDDO
          ENDIF
        ENDIF
!
        SELECT CASE(WAQPROCESS)
!         O2 MODULE
          CASE(1)
            ADDTR = ADDTR + 3
!         BIOMASS MODULE
          CASE(2)
            ADDTR = ADDTR + 5
!         EUTRO MODULE
          CASE(3)
            ADDTR = ADDTR + 8
!         MICROPOL MODULE
          CASE(4)
            ADDTR = ADDTR + 5
!         THERMIC MODULE
          CASE(5)
            IF(IND_T.EQ.0)THEN
              ADDTR = ADDTR + 1
              IND_T = NTRAC + 1
            ENDIF
          CASE DEFAULT
            IF(LNG.EQ.1) THEN
              WRITE(LU,10)WAQPROCESS
            ELSE
              WRITE(LU,20)WAQPROCESS
            ENDIF
            CALL PLANTE(1)
            STOP
!
        END SELECT
      ENDIF
!
      RETURN
!-----------------------------------------------------------------------
!     MESSAGES
10    FORMAT(1X,'INCREASE_NTRAC: MODULE WAQ INCONNU : ',I4)
20    FORMAT(1X,'INCREASE_NTRAC: UNKNOWN WAQ MODULE : ',I4)
!-----------------------------------------------------------------------
!
      RETURN
      END

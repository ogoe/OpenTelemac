!                       **********************
                        SUBROUTINE INIT_WATQUA
!                       **********************
!
!
!***********************************************************************
! TELEMAC2D   V7P0
!***********************************************************************
!
!brieF INITIALISATION OF WATER QUALITY PARAMETERS
!
!
!history  R.ATA
!+        12/09/2014
!+        V7P0
!+        CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_WAQTEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      CHARACTER(LEN=250) PATH
      INTEGER :: NCAR=250
      CHARACTER(LEN=24), PARAMETER :: CODE='WAQTEL                  '
      CHARACTER(LEN=144) FILE_DESC(4,MAXKEY)
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     1-INITIALISATION OF FILES
!
!      CALL BIEF_INIT(CODE,PATH,NMAX,.TRUE.)
!
!     2-READ THE WAQ PARAMETERS FILE
!     ===========================
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING LECWAQTEL'
      CALL LECDON_WAQTEL(FILE_DESC,PATH,NCAR,CODE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM LECWAQTEL'
      IF(LNG.EQ.1) WRITE (LU,*) 'LECTURE DES PARAMETRES PHYSIQUES WAQ'
      IF(LNG.EQ.2) WRITE (LU,*) 'READING WAQ PHYSICAL PARAMETERS'
!
!
!     IF WATER QUALITY, INCREASE NTRAC DEPENDING ON WHICH PROCESS
!
!      CALL INCREASE_NTRAC(NTRAC,WAQPROCESS,MAX_KEY,IND_T)
!
!-----------------------------------------------------------------------
!     MESSAGES
!-----------------------------------------------------------------------
!
      RETURN
      END

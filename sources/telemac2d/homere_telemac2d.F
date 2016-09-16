!                    ************************
                     PROGRAM HOMERE_TELEMAC2D
!                    ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    1) READS ALL NECESSARY DATA.
!+
!+            2) CALLS TELEMAC2D AND SISYPHE IN CASE OF COUPLING.
!
!note     IN CASE OF PARAMETER ESTIMATION, HOMERE_ADJ_T2D IS
!+            CALLED INSTEAD OF HOMERE_TELEMAC2D.
!
!history  R. ATA
!+        10/11/2014
!+        V7P0
!+       add waq variables for lecdon_telemac2d
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES,MAXLU_SIS
      USE DECLARATIONS_TOMAWAC, ONLY : WAC_FILES,MAXLU_WAC
      USE DECLARATIONS_WAQTEL,  ONLY : WAQ_FILES,MAXLU_WAQ,WAQPROCESS
      USE INTERFACE_TELEMAC2D
      USE INTERFACE_WAQTEL
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
      INTEGER TDEB(8),TFIN(8),NCAR,IFLOT
!
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC2D               '
      CHARACTER(LEN=24), PARAMETER :: CODE2='SISYPHE                 '
      CHARACTER(LEN=24), PARAMETER :: CODE3='TOMAWAC                 '
      CHARACTER(LEN=24), PARAMETER :: CODE4='WAQTEL                  '
!
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) MOTCAR(MAXKEYWORD),FILE_DESC(4,MAXKEYWORD)
      CHARACTER(LEN=144) DUMMY
!
!======================================================================
!
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
#ifdef COMPAD
      CALL AD_TELEMAC2D_MAIN_INIT
#endif
!< JR @ RWTH
!
!-----------------------------------------------------------------------
!
!     INITIALISES FILES (NAMES OF FILES=' ' AND LOGICAL UNITS =0)
!     GETTING NCSIZE BY CALLING P_INIT
!
      CALL BIEF_INIT(CODE1,PATH,NCAR,.TRUE.)
!
!     INITIAL TIME FOR COMPUTATION DURATION
!
      CALL DATE_AND_TIME(VALUES=TDEB)
!
!     PRINTS BANNER TO LISTING
!
      CALL PRINT_HEADER(CODE1,'                        ')
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
!
      DUMMY = ' '
!
      CALL LECDON_TELEMAC2D(MOTCAR,FILE_DESC,
     &                      PATH,NCAR,DUMMY,DUMMY)
!
!-----------------------------------------------------------------------
!
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
#ifdef COMPAD
      CALL AD_TELEMAC2D_MAIN_AFTER_LECDON_TELEMAC2D
#endif
!< JR @ RWTH
!
!-----------------------------------------------------------------------
!
!     OPENS THE FILES FOR TELEMAC2D
!
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE1,T2D_FILES,MAXLU_T2D,PATH,NCAR,
     &                     1,.FALSE.)
!
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY
!
      CALL POINT_TELEMAC2D
!
!-----------------------------------------------------------------------
!
!     INITIALISES SISYPHE
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
        CALL PRINT_HEADER(CODE2,CODE1)
!
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,
     &                    CODE1,DUMMY,DUMMY)
!
!-----------------------------------------------------------------------
!
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
#ifdef COMPAD
      CALL AD_TELEMAC2D_MAIN_AFTER_LECDON_SISYPHE
#endif
!< JR @ RWTH
!
!-----------------------------------------------------------------------
!
      CALL BIEF_OPEN_FILES(CODE2,SIS_FILES,MAXLU_SIS,PATH,NCAR,
     &                     2,.FALSE.)
!
!     RESETS TELEMAC2D CONFIGURATION
!
      CALL CONFIG_CODE(1)
!
!     MEMORY ORGANISATION
!
      CALL POINT_SISYPHE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES TOMAWAC
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!
        CALL PRINT_HEADER(CODE3,CODE1)
!
      CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,CODE3)
      CALL BIEF_OPEN_FILES(CODE3,WAC_FILES,MAXLU_WAC,PATH,NCAR,
     &                     3,.FALSE.)
!
!     RESETS TELEMAC2D CONFIGURATION
!
      CALL CONFIG_CODE(1)
!
!     MEMORY ORGANISATION
!
      CALL POINT_TOMAWAC
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES WAQTEL
!
      IF(INCLUS(COUPLING,'WAQTEL')) THEN
!
!
        CALL PRINT_HEADER(CODE4,CODE1)
!
        CALL LECDON_WAQTEL(FILE_DESC,PATH,NCAR,CODE4)
        CALL BIEF_OPEN_FILES(CODE4,WAQ_FILES,MAXLU_WAQ,PATH,NCAR,
     &                       4,.FALSE.)
!
!       UPDATING TRACER INFORMATION OF WAQTEL
!
        CALL NAMETRAC_WAQ(TEXTE,TEXTPR,NAMETRAC,NTRAC,IND_T,
     &                    MAXTRA,ICONVFT,VISCT,2)
!
!       RESETS TELEMAC2D CONFIGURATION
!
        CALL CONFIG_CODE(1)
!
!       MEMORY ORGANISATION
!
        CALL POINT_WAQTEL(WAQPROCESS,MESH,IELM1,VENT,WINDX,WINDY,ATMOS,
     &                    PATMOS)
!
      ENDIF
!
!=======================================================================
!
      IF(ESTIME.EQ.' ') THEN
!
!-----------------------------------------------------------------------
!
!     STANDARD MODE: ONE TELEMAC2D CALL
!
      CALL TELEMAC2D(PASS=-1,ATDEP=0.D0,NITER=0,CODE='       ')
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
!       PARAMETER ESTIMATION MODE : CALLS HOMERE_ADJ_T2D
!
        CALL HOMERE_ADJ_T2D
!
      ENDIF
!
!=======================================================================
!
!     CLOSES FILES
!
      CALL BIEF_CLOSE_FILES(CODE1,T2D_FILES,MAXLU_T2D,.TRUE.)
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
        CALL CONFIG_CODE(2)
        CALL BIEF_CLOSE_FILES(CODE2,SIS_FILES,MAXLU_SIS,.FALSE.)
      ENDIF
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
        CALL CONFIG_CODE(3)
        CALL BIEF_CLOSE_FILES(CODE3,WAC_FILES,MAXLU_WAC,.FALSE.)
      ENDIF
      IF(INCLUS(COUPLING,'WAQTEL')) THEN
        CALL CONFIG_CODE(4)
        CALL BIEF_CLOSE_FILES(CODE3,WAQ_FILES,MAXLU_WAQ,.FALSE.)
      ENDIF
!
!-----------------------------------------------------------------------
!
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
#ifdef COMPAD
      CALL AD_TELEMAC2D_MAIN_FINALIZE
#endif
!< JR @ RWTH
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
10    FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
!
!     TIME OF END OF COMPUTATION
!
      CALL DATE_AND_TIME(VALUES=TFIN)
      CALL ELAPSE(TDEB,TFIN)
!
!-----------------------------------------------------------------------
!
      STOP 0
      END

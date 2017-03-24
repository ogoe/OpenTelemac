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
      USE INTERFACE_STBTEL
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
      CHARACTER(LEN=MAXLENTMPDIR) PATH
      CHARACTER(LEN=144) MOTCAR(MAXKEYWORD),FILE_DESC(4,MAXKEYWORD)
      CHARACTER(LEN=144) DUMMY
!
      INTEGER IERR,ITRAC
      INTEGER, DIMENSION(:,:), TARGET, ALLOCATABLE :: CORRESP
!
!======================================================================
!
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
#if defined(COMPAD)
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
#if defined(COMPAD)
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
#if defined(COMPAD)
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
!
!-----------------------------------------------------------------------
!
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
#if defined(COMPAD)
      CALL AD_TELEMAC2D_MAIN_AFTER_LECDON_TOMAWAC
#endif
!< JR @ RWTH
!
!-----------------------------------------------------------------------
!
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
      IF(ESTIME.EQ.' '.AND..NOT.CONVERGENCE) THEN
!
!-----------------------------------------------------------------------
!
!     STANDARD MODE: ONE TELEMAC2D CALL
!
      CALL TELEMAC2D(PASS=-1,ATDEP=0.D0,NITER=0,CODE='       ')
!
!-----------------------------------------------------------------------
!
      ELSE IF (ESTIME.EQ.' '.AND.CONVERGENCE) THEN
!
!       CONVERGENCE MODE
!       MESH REFINEMENT WITH RLEVELS DIVISIONS
!
        WRITE(LU,*) "TELEMAC2D LAUNCHED IN CONVERGENCE MODE"
        WRITE(LU,*) "--------------------------------------"
        WRITE(LU,*) "GENERATE THE FINEST MESH"
        WRITE(LU,*) "WITH ", FINEMESH%NELMAX, "ELEMENTS MAX"
        WRITE(LU,*) "WITH ", FINEMESH%NELEM, "ELEMENTS AT THE BEGINNING"
        WRITE(LU,*) "WITH ", FINEMESH%NPMAX, "POINTS MAX"
        WRITE(LU,*) "WITH ", FINEMESH%NPOIN, "POINTS AT THE BEGINNING"
        ALLOCATE(CORRESP(FINEMESH%NELMAX,RLEVELS+1))
        CALL REFINE_MESH(RLEVELS+1,FINEMESH,FINEMESH%NELMAX,
     &                   FINEMESH%NPTFRX,NTRAC,.FALSE.,CORRESP=CORRESP)
        WRITE(LU,*) "END OF THE FINE MESH GENERATION"
!
!       RLEVELS TELEMAC2D CALLS WITH SUCCESSIVE REFINEMENTS
!
        WRITE(LU,*) "ENTERING THE LOOP OF TELEMAC2D CALLS"
        DO IERR = 1,RLEVELS+1
!
          WRITE(LU,*) "CALL ", IERR, " TO TELEMAC2D"
!          WRITE(T2D_FILES(T2DRES)%NAME, FMT='(A,I6,A)')
!     &           'level_', IERR, '.slf'

          CALL TELEMAC2D(PASS=-1,ATDEP=0.D0,NITER=0,CODE='       ',
     &                   CONVERGENCE_LEVEL=IERR)
!
          DO ITRAC=1,NTRAC
          WRITE(LU,*) "ERROR ON THE TRACER", ITRAC
            CALL ERROR_COMPUTATION(T%ADR(ITRAC)%P%R,MESH,FINEMESH,
     &                             FINEMESH%NELMAX,MESH%NPOIN,
     &                             CORRESP,RLEVELS+1,IERR,MESH%IKLE%I,
     &                             FINEMESH%IKLE%I)
          ENDDO
!
          IF(IERR.EQ.RLEVELS+1) EXIT
          CALL REFINE_MESH(1,MESH,MESH%NELMAX,MESH%NPTFRX,
     &                     NTRAC,.TRUE.,LIHBOR=LIHBOR%I,LIUBOR=LIUBOR%I,
     &                     LIVBOR=LIVBOR%I,LITBOR=LITBOR,HBOR=HBOR%R,
     &                     UBOR=UBOR%R,VBOR=VBOR%R,CHBORD=CHBORD%R,
     &                     TBOR=TBOR,ATBOR=ATBOR,BTBOR=BTBOR,ZF=ZF,
     &                     H=H,TEXP=TEXP,TTILD=TTILD,TN=TN)
!
        ENDDO
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
#if defined(COMPAD)
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

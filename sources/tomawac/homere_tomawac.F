!                    **********************
                     PROGRAM HOMERE_TOMAWAC
!                    **********************
!
!
!***********************************************************************
! TOMAWAC   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    MAIN PROGRAM FOR TOMAWAC.
!+                SOLVES THE EQUATION FOR THE
!+                MULTIDIRECTIONAL WAVE SPECTRUM.
!+
!+     1) READS IN THE NECESSARY INFORMATION FOR MEMORY ALLOCATION,
!+
!+     2) ALLOCATES THE MEMORY,
!+
!+     3) CALLS THE REAL MAIN PROGRAM WAC.
!
!history  OPTIMER
!+        12/01/01
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  C VILLARET (HRW+EDF) & J-M HERVOUET (EDF - LNHE)
!+        18/09/2014
!+        V7P0
!+   Adding STRA01 for the variable ORBVEL_TEL in wac arguments
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      INTEGER TDEB(8),TFIN(8),NCAR
!
      CHARACTER(LEN=24), PARAMETER :: CODE='TOMAWAC                '
!
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) FILE_DESC(4,MAXKEYWORD)
!
!======================================================================
!
!> SEB @ HRW: ALGORITHMIC DIFFERENTIATION
#if defined(COMPAD)
      CALL AD_TOMAWAC_MAIN_INIT
#endif
!< SEB @ HRW
!
!-----------------------------------------------------------------------
!
      CALL BIEF_INIT(CODE,PATH,NCAR,.TRUE.)
!
!     INITIAL TIME FOR COMPUTATION DURATION
!
      CALL DATE_AND_TIME(VALUES=TDEB)
!
!-----------------------------------------------------------------------
!     EN TETE   -  HEADING
!-----------------------------------------------------------------------
!
      CALL PRINT_HEADER(CODE,'                        ')
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
!
      CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,CODE)
!
!-----------------------------------------------------------------------
!
!> SEB @ HRW: ALGORITHMIC DIFFERENTIATION
#if defined(COMPAD)
      CALL AD_TOMAWAC_MAIN_AFTER_LECDON_TOMAWAC
#endif
!< SEB @ HRW
!
!-----------------------------------------------------------------------
!
!     OPENS THE FILES
!
      CALL BIEF_OPEN_FILES(CODE,WAC_FILES,MAXLU_WAC,PATH,NCAR,
     &                     1,.FALSE.)
!
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY
!
!      write(*,*) 'POINT_TOMAWAC'
      CALL POINT_TOMAWAC
!      write(*,*) 'APRES POINT_TOMAWAC'
      !
!-----------------------------------------------------------------------
!
!     CALLS THE REAL MAIN PROGRAM
!
!      write(*,*) 'call wac'
      CALL WAC(-1,STRA01,STRA01,STRA01,STRA01,STRA01,STRA01,STRA01,
     &         CODE,0.D0,0.D0,0,1,STRA01,STRA01,STRA01,STRA01)
!
!-----------------------------------------------------------------------
!
!     CLOSES THE FILES
!
      CALL BIEF_CLOSE_FILES(CODE,WAC_FILES,MAXLU_WAC,.TRUE.)
!
!-----------------------------------------------------------------------
!
!> SEB @ HRW: ALGORITHMIC DIFFERENTIATION
#if defined(COMPAD)
      CALL AD_TOMAWAC_MAIN_FINALIZE
#endif
!< SEB @ HRW
!
!-----------------------------------------------------------------------
!
      IF (LNG.EQ.1) WRITE(LU,10)
      IF (LNG.EQ.2) WRITE(LU,20)
10    FORMAT(1X,////,1X,'FIN NORMALE DU PROGRAMME',/////)
20    FORMAT(1X,////,1X,'CORRECT END OF RUN',/////)
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

!                    **********************
                     PROGRAM HOMERE_TOMAWAC
!                    **********************
!
!
!***********************************************************************
! TOMAWAC   V6P1                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NCAR,IFLOT
!
      CHARACTER(LEN=24), PARAMETER :: CODE='TOMAWAC                '
!
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) FILE_DESC(4,300)
!
      CALL BIEF_INIT(CODE,PATH,NCAR,.TRUE.)
!
!-----------------------------------------------------------------------
!     EN TETE   -  HEADING
!-----------------------------------------------------------------------
!
      WRITE(LU,100)
      WRITE(LU,110)
100   FORMAT(100(1H-),////////,
     &16X,
     &'TTTTT  OOOOO  M   M  AAAAA  W   W  AAAAA  CCCCC '
     &,/,16X,
     &'  T    O   O  MM MM  A   A  W   W  A   A  C     '
     &,/,16X,
     &'  T    O   O  M W M  AAAAA  W W W  AAAAA  C     '
     &,/,16X,
     &'  T    O   O  M   M  A   A  WW WW  A   A  C     '
     &,/,16X,
     &'  T    OOOOO  M   M  A   A  W   W  A   A  CCCCC '
     &,//)
110   FORMAT(15X,
     &'               |    |    |                 '
     &,/,15X,
     &'              )_)  )_)  )_) _              '
     &,/,15X,
     &'             )___))___))___)\              '
     &,/,15X,
     &'             )____)____)_____)\\           '
     &,/,15X,
     &'           _____|____|____|____\\\__       '
     &,/,15X,
     &'  ---------\               6.1  /---------  '
     &,/,15X,
     &'    ^^^^^^^^^^^^^^^^^^^^^^^^^^^             '
     &,/,15X,
     &'         ^^^^      ^^^^     ^^^    ^^      '
     &,/,15X,
     &'             ^^^^      ^^^                 '
     &,///)
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
!
      CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,CODE)
!
!-----------------------------------------------------------------------
!
!     OPENS THE FILES
!
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE,WAC_FILES,MAXLU_WAC,PATH,NCAR,
     &                     .FALSE.,IFLOT,1)
!
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY
!
      CALL POINT_TOMAWAC
!
!-----------------------------------------------------------------------
!
!     CALLS THE REAL MAIN PROGRAM
!
      CALL WAC(-1,STRA01,STRA01,STRA01,STRA01,STRA01,STRA01,STRA01,
     &         CODE,0.D0,0.D0,0,1)
!
!-----------------------------------------------------------------------
!
!     CLOSES THE FILES
!
      CALL BIEF_CLOSE_FILES(CODE,WAC_FILES,MAXLU_WAC,.TRUE.)
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
      STOP
      END

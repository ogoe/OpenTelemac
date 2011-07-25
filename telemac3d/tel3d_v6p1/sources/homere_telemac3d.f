!                    ************************
                     PROGRAM HOMERE_TELEMAC3D
!                    ************************
!
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    1) OPENS FILES, SETS POINTERS ACCORDING TO THE
!+                   PARAMETERS IMPOSED IN THE STEERING FILE AND
!+                   THE GIVEN GEOMETRY.
!+
!+            2) CALLS THE MAIN SUBROUTINE.
!+
!+            3) MEASURES CPU TIME.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history
!+        10/04/2009
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
      USE INTERFACE_TELEMAC3D
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES
      USE DECLARATIONS_TOMAWAC, ONLY : WAC_FILES,MAXLU_WAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER TDEB(8),TFIN(8),NCAR,IFLOT
!
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC3D               '
      CHARACTER(LEN=24), PARAMETER :: CODE2='SISYPHE                 '
      CHARACTER(LEN=24), PARAMETER :: CODE3='TOMAWAC                 '
!
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) MOTCAR(300),FILE_DESC(4,300)
!
!======================================================================
!
! STARTS COUNTING CPU TIME
!
      CALL DATE_AND_TIME(VALUES=TDEB)
!
! INITIALISES FILES (ESPECIALLY IMPORTANT FOR A PARALLEL MACHINE)
!
      CALL BIEF_INIT(CODE1,PATH,NCAR,.TRUE.)
!
! WRITES A BANNER TO THE LISTING
!
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,102)
100   FORMAT(///,78('-'),/,1X,'LISTING DE TELEMAC-3D ',/)
101   FORMAT(///,78('-'),/,1X,'LISTING OF TELEMAC-3D ',/)
102   FORMAT(/////,
     &14X,'   TTTTT  EEEEE  L      EEEEE  M   M  AAAAA  CCCCC',/,
     &14X,'     T    E      L      E      MM MM  A   A  C    ',/,
     &14X,'     T    EEE    L      EEE    M M M  AAAAA  C    ',/,
     &14X,'     T    E      L      E      M   M  A   A  C    ',/,
     &14X,'     T    EEEEE  LLLLL  EEEEE  M   M  A   A  CCCCC',/,
     &14X,'                                                  ',/,
     &14X,'            3D   VERSION 6.1   FORTRAN 90    ',/,
     &14X,/////)
!
!-----------------------------------------------------------------------
! READS THE STEERING FILE
!
      CALL LECDON_TELEMAC3D(MOTCAR,FILE_DESC,PATH,NCAR)
!
!-----------------------------------------------------------------------
! OPENS THE FILES
!
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE1,T3D_FILES,MAXLU_T3D,PATH,NCAR,
     &                     INCLUS(COUPLING,'SISYPHE'),IFLOT,1)
!
!-----------------------------------------------------------------------
!
! ALLOCATES VECTORS, MATRICES AND BLOCKS
!
      CALL POINT_TELEMAC3D
!
!-----------------------------------------------------------------------
!
! INITIALISES SISYPHE IF COUPLING THE 2 MODELS
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
        IF(LNG.EQ.1) WRITE(LU,103)
        IF(LNG.EQ.2) WRITE(LU,104)
        WRITE(LU,105)
103     FORMAT(/////,1X,'LISTING DE SISYPHE AVEC COUPLAGE',78('-'))
104     FORMAT(/////,1X,'LISTING OF SISYPHE WITH COUPLING',78('-'))
105     FORMAT(/////,
     &  14X,'    SSSS I   SSSS Y   Y PPPP  H   H EEEEE' ,/,
     &  14X,'   S     I  S      Y Y  P   P H   H E    ' ,/,
     &  14X,'    SSS  I   SSS    Y   PPPP  HHHHH EEEE  ',/,
     &  14X,'       S I      S   Y   P     H   H E     ',/,
     &  14X,'   SSSS  I  SSSS    Y   P     H   H EEEEE' ,/,
     &  14X,'                                          ',/,
     &  14X,'                VERSION 6.1               ',/,
     &  14X,'      COUPLED WITH TELEMAC-3D INTERNALLY  ',/,
     &  14X,/////)
!
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE1)
      CALL BIEF_OPEN_FILES(CODE2,SIS_FILES,MAXLU_T3D,PATH,NCAR,
     &                     INCLUS(COUPLING,'SISYPHE'),IFLOT,2)
      CALL CONFIG_CODE(1)
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
        WRITE(LU,106)
        WRITE(LU,107)
106     FORMAT(100(1H-),////////,
     &  16X,
     &  'TTTTT  OOOOO  M   M  AAAAA  W   W  AAAAA  CCCCC '
     &  ,/,16X,
     &  '  T    O   O  MM MM  A   A  W   W  A   A  C     '
     &  ,/,16X,
     &  '  T    O   O  M W M  AAAAA  W W W  AAAAA  C     '
     &  ,/,16X,
     &  '  T    O   O  M   M  A   A  WW WW  A   A  C     '
     &  ,/,16X,
     &  '  T    OOOOO  M   M  A   A  W   W  A   A  CCCCC '
     &  ,//)
107     FORMAT(15X,
     &  '               |    |    |                 '
     &  ,/,15X,
     &  '              )_)  )_)  )_) _              '
     &  ,/,15X,
     &  '             )___))___))___)\              '
     &  ,/,15X,
     &  '             )____)____)_____)\\           '
     &  ,/,15X,
     &  '           _____|____|____|____\\\__       '
     &  ,/,15X,
     &  '  ---------\               6.1  /---------  '
     &  ,/,15X,
     & '    ^^^^^^^^^^^^^^^^^^^^^^^^^^^             '
     &  ,/,15X,
     &  '         ^^^^      ^^^^     ^^^    ^^      '
     &  ,/,15X,
     &  '             ^^^^      ^^^                 '
     &,///)
!
      CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,CODE3)
      CALL BIEF_OPEN_FILES(CODE3,WAC_FILES,MAXLU_WAC,PATH,NCAR,
     &                     .TRUE.,IFLOT,3)
      CALL CONFIG_CODE(1)
      CALL POINT_TOMAWAC
!
      ENDIF
!
!=======================================================================
!
      CALL TELEMAC3D
!
!-----------------------------------------------------------------------
!
      CALL BIEF_CLOSE_FILES(CODE1,T3D_FILES,MAXLU_T3D,.TRUE.)
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
        CALL CONFIG_CODE(2)
        CALL BIEF_CLOSE_FILES(CODE2,SIS_FILES,MAXLU_T3D,.FALSE.)
      ENDIF
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
        CALL CONFIG_CODE(3)
        CALL BIEF_CLOSE_FILES(CODE3,WAC_FILES,MAXLU_WAC,.FALSE.)
      ENDIF
!
!-----------------------------------------------------------------------
! HOPEFULLY GOOD NEWS
!
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
10    FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
! PRINTS THE CPU TIME CONSUMED
!
      CALL DATE_AND_TIME(VALUES=TFIN)
      CALL ELAPSE(TDEB,TFIN)
!
!-----------------------------------------------------------------------
!
      STOP
      END

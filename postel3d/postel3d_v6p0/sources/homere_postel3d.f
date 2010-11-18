C                          ***********************
                           PROGRAM HOMERE_POSTEL3D
C                          ***********************
C
C***********************************************************************
C  POSTEL3D VERSION 6.0
C***********************************************************************
C
C-----------------------------------------------------------------------
C
C                       PROGRAMME PRINCIPAL DE
C
C       PPPP    OOO    SSSS  TTTTT  EEEEE  L         33333  DDDD
C       P   P  O   O  S        T    E      L             3  D   D
C       PPPP   O   O   SSS     T    EEEE   L     ---   333  D   D
C       P      O   O      S    T    E      L             3  D   D
C       P       OOO   SSSS     T    EEEEE  LLLLL     33333  DDDD
C
C
C                INTERFACE ENTRE TELEMAC-3D ET RUBENS
C
C-----------------------------------------------------------------------
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_POSTEL3D 
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER TDEB(8),TFIN(8),NCAR,IFLOT
C
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) MOTCAR(4000),FILE_DESC(4,4000)
C
      CHARACTER(LEN=24), PARAMETER :: CODE='POSTEL3D                '
C
      CALL BIEF_INIT(CODE,PATH,NCAR,.TRUE.)
C
C     INITIAL TIME FOR COMPUTATION DURATION
C
      CALL DATE_AND_TIME(VALUES=TDEB)
C
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,10)
100   FORMAT(/////,1X,'LISTING DE POSTEL-3D ',78('-'))
101   FORMAT(/////,1X,'LISTING OF POSTEL-3D ',78('-'))
10    FORMAT('1',1X,100(1H-),////////,
     *12X,'PPPP    OOO    SSSS  TTTTT  EEEEE  L         3333   DDDD ',/,
     *12X,'P   P  O   O  S        T    E      L             3  D   D',/,
     *12X,'PPPP   O   O   SSS     T    EEEE   L     ---  333   D   D',/,
     *12X,'P      O   O      S    T    E      L             3  D   D',/,
     *12X,'P       OOO   SSSS     T    EEEEE  LLLLL     3333   DDDD ',
     *////////)
C
      CALL LECDON_POSTEL3D(MOTCAR,FILE_DESC,PATH,NCAR)
C
      CALL BIEF_OPEN_FILES(CODE,POS_FILES,100,PATH,NCAR,.FALSE.,IFLOT,1)
C
      CALL POINT_POSTEL3D
      CALL POSTEL3D
      CALL BIEF_CLOSE_FILES(CODE,POS_FILES,100,.FALSE.)
C
C
      IF (LNG.EQ.1) WRITE(LU,11)
      IF (LNG.EQ.2) WRITE(LU,12)
C
11    FORMAT(////,' FIN NORMALE DU PROGRAMME',/////)
12    FORMAT(////,' CORRECT END OF RUN',/////)
C
C-----------------------------------------------------------------------
C
      STOP
      END

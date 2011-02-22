C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       1) READS ALL NECESSARY DATA.
!><br>            2) CALLS TELEMAC2D AND SISYPHE IN CASE OF COUPLING.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     IN CASE OF PARAMETER ESTIMATION, HOMERE_ADJ_T2D IS
!>            CALLED INSTEAD OF HOMERE_TELEMAC2D.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        PROGRAM HOMERE_TELEMAC2D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES,MAXLU_SIS
      USE DECLARATIONS_TOMAWAC, ONLY : WAC_FILES,MAXLU_WAC
      USE INTERFACE_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER TDEB(8),TFIN(8),NCAR,IFLOT
C
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC2D               '
      CHARACTER(LEN=24), PARAMETER :: CODE2='SISYPHE                 '
      CHARACTER(LEN=24), PARAMETER :: CODE3='TOMAWAC                 '
C
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) MOTCAR(300),FILE_DESC(4,300)
C
C======================================================================
C
C     INITIALISES FILES (NAMES OF FILES=' ' AND LOGICAL UNITS =0)
C     GETTING NCSIZE BY CALLING P_INIT
C
      CALL BIEF_INIT(CODE1,PATH,NCAR,.TRUE.)
C
C     INITIAL TIME FOR COMPUTATION DURATION
C
      CALL DATE_AND_TIME(VALUES=TDEB)
C
C     PRINTS BANNER TO LISTING
C
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,102)
100   FORMAT(/////,1X,'LISTING DE TELEMAC-2D ',78('-'))
101   FORMAT(/////,1X,'LISTING OF TELEMAC-2D ',78('-'))
102   FORMAT(/////,
     &14X,'   TTTTT  EEEEE  L      EEEEE  M   M  AAAAA  CCCCC',/,
     &14X,'     T    E      L      E      MM MM  A   A  C    ',/,
     &14X,'     T    EEE    L      EEE    M M M  AAAAA  C    ',/,
     &14X,'     T    E      L      E      M   M  A   A  C    ',/,
     &14X,'     T    EEEEE  LLLLL  EEEEE  M   M  A   A  CCCCC',/,
     &14X,'                                                  ',/,
     &14X,'           2D    VERSION 6.1    FORTRAN 90        ',/,
     &14X,'                 WITH SEVERAL TRACERS             ',/,
     &14X,'           COUPLED WITH SISYPHE AND TOMAWAC       ',/,
     &14X,/////)
C
C-----------------------------------------------------------------------
C
C     READS THE STEERING FILE
C
      CALL LECDON_TELEMAC2D(MOTCAR,FILE_DESC,PATH,NCAR)
C
C-----------------------------------------------------------------------
C
C     OPENS THE FILES FOR TELEMAC2D
C
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE1,T2D_FILES,MAXLU_T2D,PATH,NCAR,
     &                     INCLUS(COUPLING,'SISYPHE').OR.
     &                     INCLUS(COUPLING,'TOMAWAC')         ,IFLOT,1)
C
C-----------------------------------------------------------------------
C
C     ALLOCATES MEMORY
C
      CALL POINT_TELEMAC2D
C
C-----------------------------------------------------------------------
C
C     INITIALISES SISYPHE
C
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
C
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
     &  14X,'      COUPLED WITH TELEMAC-2D INTERNALLY  ',/,
     &  14X,/////)
C
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE1)
C
      CALL BIEF_OPEN_FILES(CODE2,SIS_FILES,MAXLU_SIS,PATH,NCAR,
     &                     .TRUE.,IFLOT,2)
C
C     RESETS TELEMAC2D CONFIGURATION
C
      CALL CONFIG_CODE(1)
C
C     MEMORY ORGANISATION
C
      CALL POINT_SISYPHE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     INITIALISES TOMAWAC
C
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
C
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
C
      CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,CODE3)
      CALL BIEF_OPEN_FILES(CODE3,WAC_FILES,MAXLU_WAC,PATH,NCAR,
     &                     .TRUE.,IFLOT,3)
C
C     RESETS TELEMAC2D CONFIGURATION
C
      CALL CONFIG_CODE(1)
C
C     MEMORY ORGANISATION
C
      CALL POINT_TOMAWAC
C
      ENDIF
C
C=======================================================================
C
      IF(ESTIME.EQ.' ') THEN
C
C-----------------------------------------------------------------------
C
C     STANDARD MODE: ONE TELEMAC2D CALL
C
      CALL TELEMAC2D(PASS=-1,ATDEP=0.D0,NITER=0,CODE='       ')
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
C       PARAMETER ESTIMATION MODE : CALLS HOMERE_ADJ_T2D
C
        CALL HOMERE_ADJ_T2D
C
      ENDIF
C
C=======================================================================
C
C     CLOSES FILES
C
      CALL BIEF_CLOSE_FILES(CODE1,T2D_FILES,MAXLU_T2D,.TRUE.)
C
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
        CALL CONFIG_CODE(2)
        CALL BIEF_CLOSE_FILES(CODE2,SIS_FILES,MAXLU_SIS,.FALSE.)
      ENDIF
C
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
        CALL CONFIG_CODE(3)
        CALL BIEF_CLOSE_FILES(CODE3,WAC_FILES,MAXLU_WAC,.FALSE.)
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
10    FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
C
C-----------------------------------------------------------------------
C
C     TIME OF END OF COMPUTATION
C
      CALL DATE_AND_TIME(VALUES=TFIN)
      CALL ELAPSE(TDEB,TFIN)
C
C-----------------------------------------------------------------------
C
      STOP
      END

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       1) READS ALL NECESSARY DATA.
!><br>            2) CALLS TELEMAC2D AND SISYPHE IN CASE OF COUPLING.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     IN CASE OF PARAMETER ESTIMATION, HOMERE_ADJ_T2D IS
!>            CALLED INSTEAD OF HOMERE_TELEMAC2D.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::MAXLU_SIS MAXLU_SIS@endlink, 
!> @link DECLARATIONS_SISYPHE::SIS_FILES SIS_FILES@endlink<hr>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::ESTIME ESTIME@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAXLU_T2D MAXLU_T2D@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2D_FILES T2D_FILES@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::COUPLING COUPLING@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CODE1, CODE2, FILE_DESC, IFLOT, MOTCAR, NCAR, PATH, TDEB, TFIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_CLOSE_FILES(), BIEF_INIT(), BIEF_OPEN_FILES(), CONFIG_CODE(), ELAPSE(), HOMERE_ADJ_T2D(), INCLUS(), LECDON_SISYPHE(), LECDON_TELEMAC2D(), PLANTE(), POINT_SISYPHE(), POINT_TELEMAC2D(), SAVE_NDS(), TELEMAC2D()
!>   </td></tr>
!>     <tr><th> Unknown(s)
!>    </th><td> DATE_AND_TIME
!>   </td></tr>
!>     </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 27/03/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

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
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES,MAXLU_SIS
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
     &14X,'           2D    VERSION 6.0    FORTRAN 90        ',/,
     &14X,'                 WITH SEVERAL TRACERS             ',/,
     &14X,'           COUPLED WITH SISYPHE INTERNALLY        ',/,
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
     &                     INCLUS(COUPLING,'INTER'),IFLOT,1)
C
C-----------------------------------------------------------------------
C
C     ALLOCATES MEMORY
C
      CALL POINT_TELEMAC2D
C
C     SAVES COMMON NDS CORRESPONDING TO TELEMAC-2D MESH
C
      CALL SAVE_NDS(1)
C
C-----------------------------------------------------------------------
C
C     INITIALISES SISYPHE
C
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
C
C                                       FALSE= P_INIT NOT CALLED
        CALL BIEF_INIT(CODE2,PATH,NCAR,.FALSE.)
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
     &  14X,'                VERSION 6.0               ',/,
     &  14X,'      COUPLED WITH TELEMAC-2D INTERNALLY  ',/,
     &  14X,/////)
C
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE1)
C
      CALL BIEF_OPEN_FILES(CODE2,SIS_FILES,MAXLU_SIS,PATH,NCAR,
     &                     INCLUS(COUPLING,'SISYPHE'),IFLOT,2)
      CALL POINT_SISYPHE
C
C     SAVES COMMON NDS CORRESPONDING TO SISYPHE MESH (SO FAR THE SAME !)
C
      CALL SAVE_NDS(2)
C
      ELSEIF(COUPLING(1:1).EQ.' ') THEN
C
C       NOTHING TO DO
C
      ELSEIF(INCLUS(COUPLING,'DELWAQ')) THEN
C
C       NOTHING TO DO
C
      ELSE
C       ERROR
        IF(LNG.EQ.1) WRITE(LU,*) 'CAS DE COUPLAGE INCONNU : ',COUPLING
        IF(LNG.EQ.2) WRITE(LU,*) 'UNEXPECTED COUPLING CASE: ',COUPLING
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     RESETS TELEMAC2D CONFIGURATION
C
      CALL CONFIG_CODE(1)
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
C
C#######################################################################
C
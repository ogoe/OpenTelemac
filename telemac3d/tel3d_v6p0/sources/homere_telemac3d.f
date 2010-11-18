C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       1) OPENS FILES, SETS POINTERS ACCORDING TO THE
!>                   PARAMETERS IMPOSED IN THE STEERING FILE AND
!>                   THE GIVEN GEOMETRY.
!><br>            2) CALLS THE MAIN SUBROUTINE.
!><br>            3) MEASURES CPU TIME.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::SIS_FILES SIS_FILES@endlink<hr>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::MAXLU_T3D MAXLU_T3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3D_FILES T3D_FILES@endlink<hr>
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
!>    </th><td> BIEF_CLOSE_FILES(), BIEF_INIT(), BIEF_OPEN_FILES(), CONFIG_CODE(), ELAPSE(), INCLUS(), LECDON_SISYPHE(), LECDON_TELEMAC3D(), POINT_SISYPHE(), POINT_TELEMAC3D(), SAVE_NDS(), TELEMAC3D()
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
!> </td><td> 10/04/2009
!> </td><td>
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
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
                        PROGRAM HOMERE_TELEMAC3D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES
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
!
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) MOTCAR(300),FILE_DESC(4,300)
!
!======================================================================
!
C STARTS COUNTING CPU TIME
!
      CALL DATE_AND_TIME(VALUES=TDEB)
!
C INITIALISES FILES (ESPECIALLY IMPORTANT FOR A PARALLEL MACHINE)
!
      CALL BIEF_INIT(CODE1,PATH,NCAR,.TRUE.)
!
C WRITES A BANNER TO THE LISTING
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
     &14X,'            3D   VERSION 6.0   FORTRAN 90    ',/,
     &14X,/////)
!
!-----------------------------------------------------------------------
C READS THE STEERING FILE
!
      CALL LECDON_TELEMAC3D(MOTCAR,FILE_DESC,PATH,NCAR)
!
!-----------------------------------------------------------------------
C OPENS THE FILES
!
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE1,T3D_FILES,MAXLU_T3D,PATH,NCAR,
     &                     INCLUS(COUPLING,'SISYPHE'),IFLOT,1)
!
!-----------------------------------------------------------------------
!
C ALLOCATES VECTORS, MATRICES AND BLOCKS
!
      CALL POINT_TELEMAC3D
      CALL SAVE_NDS(1)
!
!-----------------------------------------------------------------------
!
C INITIALISES SISYPHE IF COUPLING THE 2 MODELS
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
C                                       FALSE = P_INIT NOT CALLED
        CALL BIEF_INIT(CODE2,PATH,NCAR,.FALSE.)
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
     &  14X,'                VERSION 6.0               ',/,
     &  14X,'      COUPLED WITH TELEMAC-3D INTERNALLY  ',/,
     &  14X,/////)
!
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE1)
!
      CALL BIEF_OPEN_FILES(CODE2,SIS_FILES,MAXLU_T3D,PATH,NCAR,
     &                     INCLUS(COUPLING,'SISYPHE'),IFLOT,2)
      CALL POINT_SISYPHE
      CALL SAVE_NDS(2)
!
      ENDIF
!
!=======================================================================
!
      CALL CONFIG_CODE(1)
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
!-----------------------------------------------------------------------
C HOPEFULLY GOOD NEWS
!
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
10    FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
C PRINTS THE CPU TIME CONSUMED
!
      CALL DATE_AND_TIME(VALUES=TFIN)
      CALL ELAPSE(TDEB,TFIN)
!
!-----------------------------------------------------------------------
!
      STOP
      END
C
C#######################################################################
C
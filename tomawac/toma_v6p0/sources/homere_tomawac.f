C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MAIN PROGRAM FOR TOMAWAC.
!>                SOLVES THE EQUATION FOR THE
!>                MULTIDIRECTIONAL WAVE SPECTRUM.
!><br>     1) READS IN THE NECESSARY INFORMATION FOR MEMORY ALLOCATION,
!><br>     2) ALLOCATES THE MEMORY,
!><br>     3) CALLS THE REAL MAIN PROGRAM WAC.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::MAXLU_WAC MAXLU_WAC@endlink, 
!> @link DECLARATIONS_TOMAWAC::WAC_FILES WAC_FILES@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CODE, FILE_DESC, IFLOT, NCAR, PATH
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_CLOSE_FILES(), BIEF_INIT(), BIEF_OPEN_FILES(), LECDON_TOMAWAC(), POINT_TOMAWAC(), WAC()
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
!> </td><td> 12/01/01
!> </td><td> OPTIMER  02 98 44 24 51
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
                        PROGRAM HOMERE_TOMAWAC
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NCAR,IFLOT
C
      CHARACTER(LEN=24), PARAMETER :: CODE='TOMAWAC                '
C
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) FILE_DESC(4,300)
C
      CALL BIEF_INIT(CODE,PATH,NCAR,.TRUE.)
C
C-----------------------------------------------------------------------
C     EN TETE   -  HEADING
C-----------------------------------------------------------------------
C
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
     &'  ---------\               6.0  /---------  '
     &,/,15X,
     &'    ^^^^^^^^^^^^^^^^^^^^^^^^^^^             '
     &,/,15X,
     &'         ^^^^      ^^^^     ^^^    ^^      '
     &,/,15X,
     &'             ^^^^      ^^^                 '
     &,///)
C
C-----------------------------------------------------------------------
C     LECTURE DU FICHIER DES PARAMETRES
C     READS THE STEERING FILE
C
      CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,CODE)
C
C-----------------------------------------------------------------------
C     OUVERTURE DES FICHIERS
C     OPENS THE FILES
C
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE,WAC_FILES,MAXLU_WAC,PATH,NCAR,.FALSE.,IFLOT,1)
C
C-----------------------------------------------------------------------
C     ALLOCATION DE LA MEMOIRE
C     ALLOCATES MEMORY
C
      CALL POINT_TOMAWAC
C
C-----------------------------------------------------------------------
C     APPEL DU VRAI PROGRAMME PRINCIPAL
C     CALLS THE REAL MAIN PROGRAM
C
      CALL WAC
C
C-----------------------------------------------------------------------
C     FERMETURE DES FICHIERS
C     CLOSES THE FILES
C
      CALL BIEF_CLOSE_FILES(CODE,WAC_FILES,MAXLU_WAC,.TRUE.)
C
C-----------------------------------------------------------------------
C
      IF (LNG.EQ.1) WRITE(LU,10)
      IF (LNG.EQ.2) WRITE(LU,20)
10    FORMAT(1X,////,1X,'FIN NORMALE DU PROGRAMME',/////)
20    FORMAT(1X,////,1X,'CORRECT END OF RUN',/////)
C
C-----------------------------------------------------------------------
C
      STOP
      END
C
C#######################################################################
C
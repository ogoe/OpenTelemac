C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       1)  ACQUIRES DATA REQUIRED TO ALLOCATE MEMORY
!>                   (STEERING FILE + GEOMETRY)<br>
!><br>            2)  CALLS THE REAL MAIN PROGRAM ARTEMIS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::ART_FILES ART_FILES@endlink, 
!> @link DECLARATIONS_ARTEMIS::MAXLU_ART MAXLU_ART@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CODE, FILE_DESC, I, IFLOT, J, NCAR, PATH, TDEB, TFIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ARTEMIS(), BIEF_CLOSE_FILES(), BIEF_INIT(), BIEF_OPEN_FILES(), LECDON_ARTEMIS(), POINT_ARTEMIS(), TIME_IN_SECONDS()
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
!>  <tr>
!>    <td><center> 6.0                                    </center></td>
!>    <td> 19/04/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 24/04/1997                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.5                                      </td>
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
                        PROGRAM HOMERE_ARTEMIS
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER     LNG,LU,I,J
      COMMON/INFO/LNG,LU
C
      INTEGER TDEB,TFIN,NCAR,IFLOT
C
      CHARACTER(LEN=24), PARAMETER :: CODE='ARTEMIS                 '
C
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
C
C======================================================================
C
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) FILE_DESC(4,300)
C
C-----------------------------------------------------------------------
C
      CALL BIEF_INIT(CODE,PATH,NCAR,.TRUE.)

C
      TDEB = TIME_IN_SECONDS()
C
C     HEADING
C
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,102)
100   FORMAT(/////,1X,'LISTING DE ARTEMIS ',78('-'))
101   FORMAT(/////,1X,'LISTING OF ARTEMIS ',78('-'))
102   FORMAT(/////,
     &14X,'    AAA  RRRR  TTTTT EEEEE M   M IIIII  SSSS',/,
     &14X,'   A   A R   R   T   E     MM MM   I   S    ',/,
     &14X,'   AAAAA RRRR    T   EEEEE M M M   I    SSS ',/,
     &14X,'   A   A R   R   T   E     M   M   I       S',/,
     &14X,'   A   A R   R   T   EEEEE M   M IIIII SSSS ',/,
     &14X,'                                            ',/,
     &14X,'          VERSION 6.0      FORTRAN 90 ',/,
     &14X,/////)
C
C-----------------------------------------------------------------------
C
C     READS THE STEERING FILE
C
      CALL LECDON_ARTEMIS(FILE_DESC,PATH,NCAR,CODE)

C-----------------------------------------------------------------------
C
C     OPENS THE FILES
C
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE,ART_FILES,MAXLU_ART,PATH,NCAR,.FALSE.,IFLOT,1)

C-----------------------------------------------------------------------
C
C     ALLOCATES MEMORY FOR BIEF_OBJ STRUCTURES (VECTORS, MATRICES)


C
      CALL POINT_ARTEMIS
C
C-----------------------------------------------------------------------
C
C     CALLS REAL MAIN PROGRAM
C
      CALL ARTEMIS
C
C-----------------------------------------------------------------------
C
      CALL BIEF_CLOSE_FILES(CODE,ART_FILES,MAXLU_ART,.TRUE.)
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
      TFIN = TIME_IN_SECONDS()
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'DUREE DU CALCUL : ',TFIN-TDEB,' SECONDES'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'COMPUTER TIME: ',TFIN-TDEB,' SECONDS'
      ENDIF
C
C-----------------------------------------------------------------------
C
      STOP
      END
C
C#######################################################################
C
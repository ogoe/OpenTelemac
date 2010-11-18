C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       1) ACQUIRES THE DATA REQUIRED TO DEFINE THE POINTERS:
!>                   STEERING FILE + GEOMETRY FILE (PARTIALLY ONLY).
!><br>            2) CALLS THE SUBROUTINE SISYPHE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::MAXLU_SIS MAXLU_SIS@endlink, 
!> @link DECLARATIONS_SISYPHE::SIS_FILES SIS_FILES@endlink, 
!> @link DECLARATIONS_SISYPHE::T1 T1@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CODE, DUMINT, DUMLOG, FILE_DESC, IFLOT, MOTCAR, NCAR, PATH, TDEB, TFIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_CLOSE_FILES(), BIEF_INIT(), BIEF_OPEN_FILES(), LECDON_SISYPHE(), POINT_SISYPHE(), SISYPHE(), TIME_IN_SECONDS()
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
!> </td><td> 09/04/2009
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
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
                        PROGRAM HOMERE_SISYPHE
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      USE INTERFACE_SISYPHE
C
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER TDEB,TFIN,IFLOT,NCAR,DUMINT
      LOGICAL DUMLOG
C
      CHARACTER(LEN=24), PARAMETER :: CODE='SISYPHE                 '
C
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) MOTCAR(300),FILE_DESC(4,300)
C
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
C
C======================================================================
C
      CALL BIEF_INIT(CODE,PATH,NCAR,.TRUE.)
C
      TDEB = TIME_IN_SECONDS()
C
C  HEADING TO THE LISTING
C
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,102)
100   FORMAT(/////,1X,'LISTING DE SISYPHE ',78('-'))
101   FORMAT(/////,1X,'LISTING OF SISYPHE ',78('-'))
102   FORMAT(/////,
     &14X,'    SSSS I   SSSS Y   Y PPPP  H   H EEEEE',/,
     &14X,'   S     I  S      Y Y  P   P H   H E    ',/,
     &14X,'    SSS  I   SSS    Y   PPPP  HHHHH EEEE  ',/,
     &14X,'       S I      S   Y   P     H   H E     ',/,
     &14X,'   SSSS  I  SSSS    Y   P     H   H EEEEE',/,
     &14X,'                                          ',/,
     &14X,'                 VERSION 6.0              ',/,
     &14X,/////)
C
C-----------------------------------------------------------------------
C
C READS THE STEERING FILE
C
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,CODE)
C
C-----------------------------------------------------------------------
C
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE,SIS_FILES,MAXLU_SIS,
     &                     PATH,NCAR,.FALSE.,IFLOT,2)
C
C-----------------------------------------------------------------------
C
C ALLOCATES VECTORS, MATRICES AND BLOCKS
C
      CALL POINT_SISYPHE
C
C-----------------------------------------------------------------------
C
C  CALLS THE SUBROUTINE SISYPHE
C  -1 GOES THROUGH THE WHOLE SUBROUTINE BECAUSE THERE IS NO COUPLING
C  THE OTHER VARIABLES ARE ONLY USED WHEN COUPLING
C
C     INOUT VARIABLES IN SISYPHE CANNOT BE HARD-CODED
      DUMINT=1
      DUMLOG=.FALSE.
C
      CALL SISYPHE(-1,0,0,0,0,T1,T1,T1,T1,T1,T1,T1,
     &             DUMLOG,DUMINT,DUMLOG,CODE,1,
     &             T1,T1,0.D0,T1,0.D0,DUMLOG,DUMLOG,
     &             T1,1,T1,T1,T1,T1)
C
C-----------------------------------------------------------------------
C
      CALL BIEF_CLOSE_FILES(CODE,SIS_FILES,MAXLU_SIS,.TRUE.)
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
      END PROGRAM HOMERE_SISYPHE
C
C#######################################################################
C
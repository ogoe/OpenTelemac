C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief      'FICHIER' IS THE NAME OF A FILE EXPECTED IN THE DIRECTORY.
!>                THIS FILE EXISTS IF THERE IS AN EMPTY FILE CALLED
!>               'YAFICHIER' (NAME OF THE FILE PRECEDED WITH 'YA').
!><br>            WHEN FILE 'YAFICHIER' EXISTS, IT IS DELETED AND WE EXIT
!>                THE SUBROUTINE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DOSSIER, FICHIER
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ERR1, LAPS, NOMFIC, OUI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ATTEND(), PLANTE()
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
!>      <td><center> 5.2                                       </center>
!> </td><td> 08/02/2001
!> </td><td> NATHALY BARBRY (UNIVERSITE DE CAEN); J-M HERVOUET (LNHE) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DOSSIER
!></td><td>--></td><td>DOSSIER OU SE TROUVENT LES FICHIERS A LIRE
!>    </td></tr>
!>          <tr><td>FICHIER
!></td><td>--></td><td>FICHIER A LIRE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WAITFOR
     & (DOSSIER,FICHIER)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DOSSIER        |-->| DOSSIER OU SE TROUVENT LES FICHIERS A LIRE
C| FICHIER        |-->| FICHIER A LIRE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      CHARACTER(LEN=250), INTENT(IN) :: DOSSIER
      CHARACTER(LEN=*)  , INTENT(IN) :: FICHIER
C
      CHARACTER(LEN=270) NOMFIC
      LOGICAL OUI
      INTEGER ERR1
C
C     PARAMETER: WAIT TIME
C
      INTEGER, PARAMETER :: LAPS = 3
C
C-----------------------------------------------------------------------
C
      INTRINSIC TRIM
C
C-----------------------------------------------------------------------
C
10    CONTINUE
C
      NOMFIC=TRIM(DOSSIER)//'YA'//FICHIER
      INQUIRE(FILE=NOMFIC,EXIST=OUI,ERR=84,IOSTAT=ERR1)
C
C-----------------------------------------------------------------------
C
      IF(OUI) THEN
C
        OPEN(94,FILE=NOMFIC,
     &          STATUS='OLD',FORM='UNFORMATTED',ERR=85,IOSTAT=ERR1)
        CLOSE(94,STATUS='DELETE',ERR=86,IOSTAT=ERR1)
C
        GO TO 1000
C
      ELSE
C
        INQUIRE(FILE=TRIM(DOSSIER)//'STOP',EXIST=OUI,ERR=84,IOSTAT=ERR1)
        IF(OUI) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'WAITFOR : ARRET DU PROGRAMME, ATTENTE INUTILE'
            WRITE(LU,*) '          CAR UN FICHIER STOP A ETE CREE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'WAITFOR : PROGRAM STOPPED, WAITING IS USELESS'
            WRITE(LU,*) '          BECAUSE A FILE STOP HAS BEEN CREATED'
          ENDIF
          CALL PLANTE(1)
          STOP
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'ATTENTE DE ',LAPS,' SECONDES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'WAITING ',LAPS,' SECONDS'
          ENDIF
          CALL ATTEND(LAPS)
        ENDIF
C
        GO TO 10
C
      ENDIF
C
C-----------------------------------------------------------------------
C     ERROR MESSAGES
C-----------------------------------------------------------------------
C
84    CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'WAITFOR : ERREUR DE INQUIRE SUR LE FICHIER :'
        WRITE(LU,*) '         ',TRIM(DOSSIER)//'YA'//FICHIER
        WRITE(LU,*) '          ERREUR NUMERO : ',ERR1
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'WAITFOR : ERROR OF COMMAND INQUIRE ON FILE :'
        WRITE(LU,*) '         ',TRIM(DOSSIER)//'YA'//FICHIER
        WRITE(LU,*) '          ERROR NUMBER : ',ERR1
      ENDIF
      CALL PLANTE(1)
      STOP
85    CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'WAITFOR : ERREUR ',ERR1
        WRITE(LU,*) 'A L''OUVERTURE DU FICHIER ',NOMFIC
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'WAITFOR : ERROR ',ERR1
        WRITE(LU,*) 'WHEN OPENING THE FILE ',NOMFIC
      ENDIF
      CALL PLANTE(1)
      STOP
86    CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'WAITFOR : ERREUR ',ERR1
        WRITE(LU,*) 'A LA FERMETURE DU FICHIER ',NOMFIC
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'WAITFOR : ERROR ',ERR1
        WRITE(LU,*) 'WHEN CLOSING THE FILE ',NOMFIC
      ENDIF
      CALL PLANTE(1)
      STOP
C
C-----------------------------------------------------------------------
C
1000  CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
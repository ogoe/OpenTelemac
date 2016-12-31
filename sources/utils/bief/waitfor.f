!                    ******************
                     SUBROUTINE WAITFOR
!                    ******************
!
     & (DOSSIER,FICHIER)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    'FICHIER' IS THE NAME OF A FILE EXPECTED IN THE DIRECTORY.
!+                THIS FILE EXISTS IF THERE IS AN EMPTY FILE CALLED
!+               'YAFICHIER' (NAME OF THE FILE PRECEDED WITH 'YA').
!+
!+            WHEN FILE 'YAFICHIER' EXISTS, IT IS DELETED AND WE EXIT
!+                THE SUBROUTINE.
!
!history  NATHALY BARBRY (UNIVERSITE DE CAEN); J-M HERVOUET (LNHE)
!+        08/02/2001
!+        V5P2
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
!| DOSSIER        |-->| DIRECTORY WHERE IS THE FILE TO BE READ
!| FICHIER        |-->| FILE TO BE READ
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      CHARACTER(LEN=250), INTENT(IN) :: DOSSIER
      CHARACTER(LEN=*)  , INTENT(IN) :: FICHIER
!
      CHARACTER(LEN=270) NOMFIC
      LOGICAL OUI
      INTEGER ERR1
!
!     PARAMETER: WAIT TIME
!
      INTEGER, PARAMETER :: LAPS = 3
      INTEGER :: ID
!
!-----------------------------------------------------------------------
!
      INTRINSIC TRIM
!
!-----------------------------------------------------------------------
!
10    CONTINUE
!
      NOMFIC=TRIM(DOSSIER)//'YA'//FICHIER
      INQUIRE(FILE=NOMFIC,EXIST=OUI,ERR=84,IOSTAT=ERR1)
!
!-----------------------------------------------------------------------
!
      IF(OUI) THEN
!
        CALL GET_FREE_ID(ID)
        OPEN(ID,FILE=NOMFIC,
     &          STATUS='OLD',FORM='UNFORMATTED',ERR=85,IOSTAT=ERR1)
        CLOSE(ID,STATUS='DELETE',ERR=86,IOSTAT=ERR1)
!
        GO TO 1000
!
      ELSE
!
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
!
        GO TO 10
!
      ENDIF
!
!-----------------------------------------------------------------------
!     ERROR MESSAGES
!-----------------------------------------------------------------------
!
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
!
!-----------------------------------------------------------------------
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END

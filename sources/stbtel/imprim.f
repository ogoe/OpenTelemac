!                       *****************
                        SUBROUTINE IMPRIM
!                       *****************
!
     &(NPOIN1,NPOIN,TYPELE,NELEM,TITRE,MAILLE,PRECIS)
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2        31/08/89    J-C GALLAND
!
!     FONCTION  : IMPRESSION DANS LE LISTING D'INFORMATIONS GENERALES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | NPOIN1         | -->| NOMBRE DE POINT DONNES PAR LE MAILLEUR
! | NPOIN          | -->| NOMBRE REEL DE POINTS
! | TYPELE         | -->| TYPE DES ELEMENTS
! | NELEM          | -->| NOMBRE D'ELEMENTS
! | TITRE          | -->| TITRE DU MAILLAGE
! | MAILLE         | -->| NOM DU MAILLEUR UTILISE
! | PRECIS         | -->| FORMAT DE LECTURE DES COORDONNEES DES NOEUDS
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN1, NPOIN, NELEM
!
      CHARACTER(LEN=11), INTENT(IN) :: TYPELE
      CHARACTER(LEN=80), INTENT(IN) :: TITRE
      CHARACTER(LEN=9), INTENT(IN) ::  MAILLE
      CHARACTER(LEN=6), INTENT(IN) ::  PRECIS
!
!-----------------------------------------------------------------------
!
      IF (LNG.EQ.1) WRITE(LU,10) MAILLE
      IF (LNG.EQ.2) WRITE(LU,3010) MAILLE
!
 10   FORMAT(/,1X,'MAILLEUR : ',A9,/,
     &         1X,'--------')
 3010 FORMAT(/,1X,'MESH GENERATOR : ',A9,/,
     &         1X,'--------------')
!
      IF (LNG.EQ.1.AND.MAILLE(1:8).EQ.'SUPERTAB') WRITE(LU,  20) PRECIS
      IF (LNG.EQ.2.AND.MAILLE(1:8).EQ.'SUPERTAB') WRITE(LU,3020) PRECIS
!
 20   FORMAT(1X,'(COORDONNEES DES NOEUDS LUES EN ',A6,' PRECISION)',/)
 3020 FORMAT(1X,'(COORDINATES OF NODES ARE READ IN ',A6,' PRECISION)',/)
!
      IF (LNG.EQ.1) WRITE(LU,30) TITRE,NPOIN1,NPOIN,NELEM,TYPELE
      IF (LNG.EQ.2) WRITE(LU,3030) TITRE,NPOIN1,NPOIN,NELEM,TYPELE
!
 30   FORMAT(//,1X,'DONNEES LUES SUR LE FICHIER UNIVERSEL',
     &        /,1X,'-------------------------------------',/
     &        /,1X,'TITRE DU MAILLAGE                 : ',A72,
     &        /,1X,'NOMBRE REEL DE POINTS             : ',I11,
     &        /,1X,'NUMERO MAXI DONNE PAR LE MAILLEUR : ',I11,
     &        /,1X,'NOMBRE TOTAL D''ELEMENTS           : ',I11,
     &        /,1X,'TYPE D''ELEMENTS                   :      ',A11)
!
 3030 FORMAT(//,1X,'DATAS READ IN THE UNIVERSAL FILE',
     &        /,1X,'---------------------------------',/
     &        /,1X,'TITLE OF THE MESH                 : ',A72,
     &        /,1X,'REAL NUMBER OF POINTS             : ',I11,
     &        /,1X,'MAX. NUMBER GIVEN BY THE MESH GENERATOR: ',I11,
     &        /,1X,'TOTAL NUMBER OF ELEMENTS           : ',I11,
     &        /,1X,'TYPE OF ELEMENTS                   :      ',A11)
!
      RETURN
      END

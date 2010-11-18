C                       *****************
                        SUBROUTINE IMPRIM
C                       *****************
C
     *(NPOIN1,NPOIN,TYPELE,NELEM,TITRE,MAILLE,PRECIS)
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2        31/08/89    J-C GALLAND
C
C     FONCTION  : IMPRESSION DANS LE LISTING D'INFORMATIONS GENERALES
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C | NPOIN1         | -->| NOMBRE DE POINT DONNES PAR LE MAILLEUR
C | NPOIN          | -->| NOMBRE REEL DE POINTS
C | TYPELE         | -->| TYPE DES ELEMENTS
C | NELEM          | -->| NOMBRE D'ELEMENTS
C | TITRE          | -->| TITRE DU MAILLAGE
C | MAILLE         | -->| NOM DU MAILLEUR UTILISE
C | PRECIS         | -->| FORMAT DE LECTURE DES COORDONNEES DES NOEUDS
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C APPELE PAR : HOMERE
C APPEL DE : -
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN1, NPOIN, NELEM
C
      CHARACTER*11 TYPELE
      CHARACTER*80 TITRE
      CHARACTER*9  MAILLE
      CHARACTER*6  PRECIS
C
C-----------------------------------------------------------------------
C
      IF (LNG.EQ.1) WRITE(LU,10) MAILLE
      IF (LNG.EQ.2) WRITE(LU,3010) MAILLE
C
 10   FORMAT(/,1X,'MAILLEUR : ',A9,/,
     *         1X,'--------')
 3010 FORMAT(/,1X,'MESH GENERATOR : ',A9,/,
     *         1X,'--------------')
C
      IF (LNG.EQ.1.AND.MAILLE(1:8).EQ.'SUPERTAB') WRITE(LU,  20) PRECIS
      IF (LNG.EQ.2.AND.MAILLE(1:8).EQ.'SUPERTAB') WRITE(LU,3020) PRECIS
C
 20   FORMAT(1X,'(COORDONNEES DES NOEUDS LUES EN ',A6,' PRECISION)',/)
 3020 FORMAT(1X,'(COORDINATES OF NODES ARE READ IN ',A6,' PRECISION)',/)
C
      IF (LNG.EQ.1) WRITE(LU,30) TITRE,NPOIN1,NPOIN,NELEM,TYPELE
      IF (LNG.EQ.2) WRITE(LU,3030) TITRE,NPOIN1,NPOIN,NELEM,TYPELE
C
 30   FORMAT(//,1X,'DONNEES LUES SUR LE FICHIER UNIVERSEL',
     *        /,1X,'-------------------------------------',/
     *        /,1X,'TITRE DU MAILLAGE                 : ',A72,
     *        /,1X,'NOMBRE REEL DE POINTS             : ',I11,
     *        /,1X,'NUMERO MAXI DONNE PAR LE MAILLEUR : ',I11,
     *        /,1X,'NOMBRE TOTAL D''ELEMENTS           : ',I11,
     *        /,1X,'TYPE D''ELEMENTS                   :      ',A11)
C
 3030 FORMAT(//,1X,'DATAS READ IN THE UNIVERSAL FILE',
     *        /,1X,'---------------------------------',/
     *        /,1X,'TITLE OF THE MESH                 : ',A72,
     *        /,1X,'REAL NUMBER OF POINTS             : ',I11,
     *        /,1X,'MAX. NUMBER GIVEN BY THE MESH GENERATOR: ',I11,
     *        /,1X,'TOTAL NUMBER OF ELEMENTS           : ',I11,
     *        /,1X,'TYPE OF ELEMENTS                   :      ',A11)
C
      RETURN
      END

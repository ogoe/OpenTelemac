!                       *****************
                        SUBROUTINE LECSIM
!                       *****************
!
     &( X , Y , IKLE , NCOLOR , TITRE , NOP5 , NGEO )
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2         25/02/92    J-C GALLAND  (LNH)
!***********************************************************************
!
!     FONCTION  :  LECTURE DU FICHIER DE LA GEOMETRIE CREE PAR SIMAIL
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X,Y          |<-- | COORDONNEES DU MAILLAGE .
! |   X1,Y1        |<-- | COORDONNEES DU MAILLAGE LUES EN SIMPLE
! |                |    | PRECISION DANS LE FICHIER SIMAIL
! |   IKLE         |<-- | LISTE DES POINTS DE CHAQUE ELEMENT
! |   NCOLOR       |<-- | TABLEAU DES COULEURS DES POINTS DU MAILLAGE
! |   TITRE        |<-- | TITRE DU MAILLAGE
! |   NOP5         | -->| TABLEAU DE TRAVAIL POUR LA LECTURE DE LA SD
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |  FICH:         |    |
! |    NRES        | -->| NUMERO DU CANAL DU FICHIER DE SERAFIN
! |    NGEO       | -->| NUMERO DU CANAL DU FICHIER MAILLEUR
! |    NLIM      | -->| NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! |    NFO1      |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : -
!***********************************************************************
!
!    LISTE DES ENREGISTREMENTS DU FICHIER GEOMETRIQUE:
!             (DOCUMENTION: NOTICE SIMAIL)
!
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NGEO , NPOIN , ERR
      INTEGER NELEM , MESH , NDP , NELMAX , NPMAX
      INTEGER IKLE(NELMAX,4) , NCOLOR(*)
      INTEGER I,J,K
      INTEGER NOP5(*)
      INTEGER LONG , NTASD
      INTEGER NCGE , NMAE , NDSDE , NNO , NCOPNP ,NPO
      INTEGER INING , NBEGM , INDIC
!
      DOUBLE PRECISION X(*) , Y(*)
!
      REAL, DIMENSION(:), ALLOCATABLE :: X1,Y1
!
      CHARACTER*80 TITRE
!
      INTRINSIC DBLE
!
! COMMON
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
!
!-----------------------------------------------------------------------
!
      ALLOCATE(X1(NPOIN),STAT=ERR)
      ALLOCATE(Y1(NPOIN),STAT=ERR)
!
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,7000) ERR
        IF(LNG.EQ.2) WRITE(LU,8000) ERR
7000    FORMAT(1X,'LECSIM : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &            'CODE D''ERREUR : ',1I6)
8000    FORMAT(1X,'LECSIM: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
      ENDIF
!
!=======================================================================
!   INITIALISATION
!=======================================================================
!
      REWIND NGEO
!
      DO 5 I=1,NPOIN
         X(I) = 9999999.D0
         Y(I) = 9999999.D0
         NCOLOR(I) = 99999
 5    CONTINUE
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (1ER ENRGISTREMENT DE LA SD)
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (TABLEAU NOP0) . LECTURE DU TITRE
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,TITRE,(NOP5(I),I=1,11),NTASD
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (TABLEAUX NOP1 ET ASSOCIES)
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      IF (NTASD.GT.0) THEN
         READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
         DO 10 I=1,NTASD
            READ(NGEO,ERR=110,END=120) LONG,(NOP5(J),J=1,LONG)
 10      CONTINUE
      ENDIF
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (TABLEAU NOP2)
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
      NCOPNP = NOP5(4)
      NBEGM  = NOP5(25)
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER (TABLEAU NOP3)
! POUR LES LECTURES BIDON, ON UTILISE NOP5
!=======================================================================
!
      IF (NBEGM.NE.0) THEN
         READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
      ENDIF
!
!=======================================================================
! LECTURE SEQUENTIELLE DES COORDONNEES DES NOEUDS (TABLEAU NOP4)
!=======================================================================
!
      READ(NGEO,ERR=110,END=120) LONG,(X1(I),Y1(I),I=1,NPOIN)
      DO 20 I=1,NPOIN
         X(I) = DBLE(X1(I))
         Y(I) = DBLE(Y1(I))
 20   CONTINUE
!
!=======================================================================
! LECTURE SEQUENTIELLE DES IKLE (TABLEAU NOP5)
! POUR LES LECTURE BIDON, ON UTILISE NOP5 ET LONG
!=======================================================================
!
      INDIC = 0
      READ(NGEO,ERR=110,END=120) LONG,(NOP5(I),I=1,LONG)
      DO 30 I=1,NELEM
         INDIC = INDIC +1
         NCGE  = NOP5(INDIC)
         INDIC = INDIC +1
         NMAE  = NOP5(INDIC)
         INDIC = INDIC +1
         NDSDE = NOP5(INDIC)
         INDIC = INDIC +1
         NNO   = NOP5(INDIC)
! NNO : NOMBRE DE NOEUDS PAR ELEMENT
         IF ( (NNO.EQ.4.AND.MESH.NE.2) .OR. (NNO.EQ.3.AND.MESH.NE.3) )
     &   THEN
            IF (LNG.EQ.1) WRITE(LU,1000)
            IF (LNG.EQ.2) WRITE(LU,4000)
            STOP
         ENDIF
         DO 40 K=1,NNO
            INDIC = INDIC +1
            IKLE(I,K) = NOP5(INDIC)
 40      CONTINUE
         IF (NCOPNP.EQ.1) GOTO 50
         INDIC = INDIC +1
         NPO = NOP5(INDIC)
         DO 60 K=1,NPO
            INDIC = INDIC +1
 60      CONTINUE
 50      CONTINUE
!  NMAE :
         IF (NMAE.EQ.0) GOTO 30
         INDIC = INDIC +1
         INING = NOP5(INDIC)
         DO 70 K=2,NMAE
            IF (INING.EQ.3) THEN
               INDIC = INDIC +1
               NCOLOR(IKLE(I,K-1)) = NOP5(INDIC)
            ELSE IF(INING.EQ.2) THEN
               INDIC = INDIC +1
               IF (K.GT.NNO+1) NCOLOR(IKLE(I,K-(NNO+1))) = NOP5(INDIC)
            ELSE IF(INING.EQ.1) THEN
               INDIC = INDIC +1
               IF (K.GT.2*NNO+1)
     &         NCOLOR(IKLE(I,K-(2*NNO+1))) = NOP5(INDIC)
            ENDIF
 70      CONTINUE
 30   CONTINUE
!
      GOTO 80
!
 110  IF (LNG.EQ.1) WRITE(LU,1100)
      IF (LNG.EQ.2) WRITE(LU,4100)
 120  IF (LNG.EQ.1) WRITE(LU,1200)
      IF (LNG.EQ.2) WRITE(LU,4200)
!
 80   CONTINUE
!
!=======================================================================
!
      DEALLOCATE (X1)
      DEALLOCATE (Y1)
!
!=======================================================================
!
1000  FORMAT(//,'*********************************************',/,
     &          'LECSIM : IL N''Y A PAS DE LIEN ENTRE LE NOMBRE ',/,
     &          'DE POINTS PAR ELEMENT ET LE TYPE DES ELEMENTS ',/,
     &          '**********************************************',//)
4000  FORMAT(//,'*********************************************',/,
     &          'LECSIM : THERE IS NO LINK BETWEEN THE NUMBER  ',/,
     &          'OF POINTS BY ELEMENT AND THE TYPE OF ELEMENTS ',/,
     &          '**********************************************',//)
1100  FORMAT(//,'**********************************************',/,
     &          'LECSIM : ERREUR A LA LECTURE DU FICHIER SIMAIL',/,
     &          '**********************************************',//)
4100  FORMAT(//,'**********************************************',/,
     &          'LECSIM : ERROR IN READING FILE SIMAIL         ',/,
     &          '**********************************************',//)
1200  FORMAT(//,'*****************************************',/,
     &          'LECSIM : FIN PREMATUREE DU FICHIER SIMAIL',/,
     &          '*****************************************',//)
4200  FORMAT(//,'*************************************************',/,
     &          'LECSIM : ATTEMPT TO READ AFTER END OF FILE SIMAIL ',/,
     &          '*************************************************',//)
!
      RETURN
      END
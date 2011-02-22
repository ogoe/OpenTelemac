C                       *****************
                        SUBROUTINE INIADC
C                       *****************
C
     *(NPOIN1,TYPELE,NSFOND,IHAUT,NGEO,TITRE)
C
C***********************************************************************
C PROGICIEL : STBTEL  V5.2      13/08/01    J.M. HERVOUET 01 30 87 80 18
C***********************************************************************
C
C   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
C               MAILLAGE DANS LE FICHIER D'ENTREE ADCIRC
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C | NPOIN1         |<-- | NOMBRE REEL DE POINTS DU MAILLAGE
C |                |    | (NPOIN REPRESENTE L'INDICE MAX DES NOEUDS CAR
C |                |    | SUPERTAB LAISSE DES TROUS DANS LA NUMEROTATION
C | TYPELE         |<-- | TYPE D'ELEMENTS
C | IHAUT          |<-- | NUMERO DE LA VARIABLE HAUTEUR D'EAU 
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |    |
C |    MESH        |<-- | TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       |<-- | NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       |<-- | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C |                |    | (NPMAX = NPOIN + 0.1*NELEM)
C |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
C |  FICH:         |    |
C |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
C |    NGEO        |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
C |    NLIM        |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
C |    NFO1        |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
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
      INTEGER NGEO
      INTEGER MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
      INTEGER NPOIN1 , NSFOND
      INTEGER IHAUT
C
      CHARACTER*80 TITRE
      CHARACTER*11 TYPELE
C
C COMMON
C
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
C=======================================================================
C INITIALISATIONS
C=======================================================================
C
      NSFOND = 2
      REWIND NGEO
C
C=======================================================================
C LECTURE SEQUENTIELLE DU FICHIER ET RECHERCHE ENREGISTREMENT 5
C=======================================================================
C
      TITRE=' '
      READ(NGEO,*) TITRE(1:24)
      READ(NGEO,*) NELEM,NPOIN
      IHAUT = 0  
      NDP   = 3
      NPOIN1= NPOIN
      MESH = 3
      TYPELE = 'TRIANGLES  '
C
C-----------------------------------------------------------------------
C
      RETURN
      END

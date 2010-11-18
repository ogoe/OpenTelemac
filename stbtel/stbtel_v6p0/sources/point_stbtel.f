C                      *********************** 
                       SUBROUTINE POINT_STBTEL 
C                      *********************** 
C 
C*********************************************************************** 
C PROGICIEL : STBTEL  V5.2        09/08/89    J-C GALLAND   (LNH) 
C                                 19/02/93    J-M JANIN     (LNH) 
C                            21/08/96    P   CHAILLET  (LHF) - FASTTABS 
C                               09/98       A. CABAL / SOGREAH 
C   ORIGINE : ULYSSE 
C*********************************************************************** 
C 
C     FONCTION  : CONSTRUCTION DES POINTEURS DES TABLEAUX A ET IA 
C 
C----------------------------------------------------------------------- 
C                             ARGUMENTS 
C .________________.____.______________________________________________ 
C |      NOM       |MODE|                   ROLE 
C |________________|____|______________________________________________ 
C |   IDIMA        | -->| DIMENSION DU TABLEAU A 
C |   IDIMIA       | -->| DIMENSION DU TABLEAU IA 
C |   NBAT         | -->| NOMBRE DE POINTS DE BATHY 
C |   NBFOND       | -->| NOMBRE DE FICHIERS BATHY 
C |   MAILLE       | -->| NOM DU MAILLEUR 
C |                | -->| POUR LA LECTURE DU FICHIER SIMAIL 
C | arguments rajoutes pour l'option d'elimination des elements secs                      
C |   ELISEC      | -->| BOOLEAN INDIQUANT SI ELIMINATION DES POINTS SECS 
C |                |    | EST DEMANDEE 
C | fin arguments rajoutes pour l'option d'elimination des elements secs                      
C |________________|____|______________________________________________ 
C | COMMON         |    | 
C |    K...        |<-- | POINTEURS DU TABLEAU ENTIER 
C |  GEO:          |    | 
C |    MESH        |--> | TYPE DE MAILLAGE 
C |    NDP         |--> | NOMBRE DE NOEUDS PAR ELEMENTS 
C |    NPOIN       |--> | NOMBRE TOTAL DE POINTS DU MAILLAGE 
C |    NELEM       |--> | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE 
C |    NPMAX       |<-- | DIMENSION EFFECTIVE DES TABLEAUX X ET Y 
C |                |    | (NPMAX = NPOIN + 0.1*NELEM) 
C |    NELMAX      |<-- | DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT 
C |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM) 
C |________________|____|______________________________________________ 
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
C---------------------------------------------------------------------- 
C APPELE PAR : HOMERE 
C APPEL DE : - 
C*********************************************************************** 
C
      USE DECLARATIONS_STBTEL
C 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
C 
      INTEGER  NPMAX,NPOIN,NELMAX,NELEM,MESH 
      INTEGER  NDP 
C 
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX  
C 
C======================================================================= 
C  POUR PREVOIR L'ELIMINATION DES TRIANGLES SURCONTRAINTS , LES VALEURS 
C  DE NPOIN ET NELEM2 SONT SURDIMENSIONNEES 
C======================================================================= 
C 
      NPMAX  = NPOIN +   INT(0.1*NELEM) 
      NELMAX = NELEM + 2*INT(0.1*NELEM) 
      IF(DIV4) NPMAX  = NPMAX  + 3*NELEM 
      IF(DIV4) NELMAX = NELMAX + 3*NELEM 
C 
      RETURN 
      END

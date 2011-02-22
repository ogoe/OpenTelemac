C                       *****************
                        SUBROUTINE NOMTRA
C                       *****************

     *(NOMTAC,NOMTPA,NTRAC,NTRPA)
C
C***********************************************************************
C POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
C FORTRAN90
C***********************************************************************
C
C FONCTION  :  FIXE LES NOMS DES TRACEURS (ACTIFS OU PASSIFS) 
C              POUR LA VISUALISATION DANS RUBENS
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE |
C |________________|____|______________________________________________|
C |   NOMTAC       |<-- | NOMS DES TRACEURS ACTIFS                     |
C |   NOMTPA       |<-- | NOMS DES TRACEURS PASSIFS                    |
C |   NTRAC        |<-- | NOMBRE DE TRACEURS ACTIFS                    |
C |   NTRPA        |<-- | NOMBRE DE TRACEURS PASSIFS                   |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : ECRDEB
C
C SOUS-PROGAMME APPELE : NEANT
C
C**********************************************************************
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NTRAC,NTRPA,I
      CHARACTER*32 NOMTAC(NTRAC),NOMTPA(NTRPA)
      CHARACTER*32 TEXTE
C
C-----------------------------------------------------------------------
C
C PAR DEFAUT CETTE ROUTINE DONNE DES NOMS NEUTRES
C (TRACEUR ACTIF XX OU TRACEUR PASSIF XX)
C
C POUR DONNER SES PROPRES NOMS :
C RAJOUTER ENSUITE DES LIGNES DU TYPE DE CELLES
C QUI SONT COMMENTARISEES
C
C ATTENTION : BIEN RESPECTER LA LONGUEUR DE 32 CARACTERES
C (LES 16 PREMIERS POUR LE NOM, LE RESTE POUR L'UNITE)
C
C ********************************
C TRACEURS ACTIFS / ACTIVE TRACERS
C ********************************
C
C  ENGLISH
C
      IF(LNG.EQ.2) THEN
C
      DO 10 I=1,NTRAC
        TEXTE = 'ACTIVE TRACER '
        TEXTE = TEXTE(1:14)//CHAR(48+I)//'                 '
        NOMTAC(I) = TEXTE
10    CONTINUE
C
C  EXAMPLE
C 
C      NOMTAC(1)='TEMPERATURE     ³C              '
C      NOMTAC(2)='SALINITY        G/L             '
C
C-----------------------------------------
C
C  FRANCAIS OU AUTRE
C
      ELSE
C
      DO 20 I=1,NTRAC
        TEXTE = 'TRACEUR ACTIF '
        TEXTE = TEXTE(1:14)//CHAR(48+I)//'                 '
        NOMTAC(I) = TEXTE
20    CONTINUE
C
C  EXEMPLE
C 
C      NOMTAC(1)='TEMPERATURE     ³C              '
C      NOMTAC(2)='SALINITE        G/L             '
C
C
      ENDIF
C
C-----------------------------------------
C
C **********************************
C TRACEURS PASSIFS / PASSIVE TRACERS
C **********************************
C
C  ENGLISH
C
      IF(LNG.EQ.2) THEN
C
      DO 30 I=1,NTRPA
        TEXTE = 'PASSIVE TRACER '
        TEXTE = TEXTE(1:15)//CHAR(48+I)//'                 '
        NOMTPA(I) = TEXTE
30    CONTINUE
C
C
C  EXAMPLE
C 
C      NOMTPA(1)='OXYGEN          G/L             '
C
C-----------------------------------------
C
C  FRANCAIS OU AUTRE
C
      ELSE
C
      DO 40 I=1,NTRPA
        TEXTE = 'TRACEUR PASSIF '
        TEXTE = TEXTE(1:15)//CHAR(48+I)//'                 '
        NOMTPA(I) = TEXTE
40    CONTINUE
C
C  EXEMPLE
C 
C      NOMTPA(1)='OXYGENE         G/L             '
C
C
      ENDIF
C
C-----------------------------------------
C
C
C
C-----------------------------------------------------------------------
C
      RETURN
      END

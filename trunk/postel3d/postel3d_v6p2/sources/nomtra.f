!                       *****************
                        SUBROUTINE NOMTRA
!                       *****************
!
     &(NOMTAC,NOMTPA,NTRAC,NTRPA)
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
! FONCTION  :  FIXE LES NOMS DES TRACEURS (ACTIFS OU PASSIFS)
!              POUR LA VISUALISATION DANS RUBENS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE |
! |________________|____|______________________________________________|
! |   NOMTAC       |<-- | NOMS DES TRACEURS ACTIFS                     |
! |   NOMTPA       |<-- | NOMS DES TRACEURS PASSIFS                    |
! |   NTRAC        |<-- | NOMBRE DE TRACEURS ACTIFS                    |
! |   NTRPA        |<-- | NOMBRE DE TRACEURS PASSIFS                   |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : ECRDEB
!
! SOUS-PROGAMME APPELE : NEANT
!
!**********************************************************************
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NTRAC,NTRPA,I
      CHARACTER*32 NOMTAC(NTRAC),NOMTPA(NTRPA)
      CHARACTER*32 TEXTE
!
!-----------------------------------------------------------------------
!
! PAR DEFAUT CETTE ROUTINE DONNE DES NOMS NEUTRES
! (TRACEUR ACTIF XX OU TRACEUR PASSIF XX)
!
! POUR DONNER SES PROPRES NOMS :
! RAJOUTER ENSUITE DES LIGNES DU TYPE DE CELLES
! QUI SONT COMMENTARISEES
!
! ATTENTION : BIEN RESPECTER LA LONGUEUR DE 32 CARACTERES
! (LES 16 PREMIERS POUR LE NOM, LE RESTE POUR L'UNITE)
!
! ********************************
! TRACEURS ACTIFS / ACTIVE TRACERS
! ********************************
!
!  ENGLISH
!
      IF(LNG.EQ.2) THEN
!
      DO 10 I=1,NTRAC
        TEXTE = 'ACTIVE TRACER '
        TEXTE = TEXTE(1:14)//CHAR(48+I)//'                 '
        NOMTAC(I) = TEXTE
10    CONTINUE
!
!  EXAMPLE
!
!      NOMTAC(1)='TEMPERATURE     ³C              '
!      NOMTAC(2)='SALINITY        G/L             '
!
!-----------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
!
      DO 20 I=1,NTRAC
        TEXTE = 'TRACEUR ACTIF '
        TEXTE = TEXTE(1:14)//CHAR(48+I)//'                 '
        NOMTAC(I) = TEXTE
20    CONTINUE
!
!  EXEMPLE
!
!      NOMTAC(1)='TEMPERATURE     ³C              '
!      NOMTAC(2)='SALINITE        G/L             '
!
!
      ENDIF
!
!-----------------------------------------
!
! **********************************
! TRACEURS PASSIFS / PASSIVE TRACERS
! **********************************
!
!  ENGLISH
!
      IF(LNG.EQ.2) THEN
!
      DO 30 I=1,NTRPA
        TEXTE = 'PASSIVE TRACER '
        TEXTE = TEXTE(1:15)//CHAR(48+I)//'                 '
        NOMTPA(I) = TEXTE
30    CONTINUE
!
!
!  EXAMPLE
!
!      NOMTPA(1)='OXYGEN          G/L             '
!
!-----------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
!
      DO 40 I=1,NTRPA
        TEXTE = 'TRACEUR PASSIF '
        TEXTE = TEXTE(1:15)//CHAR(48+I)//'                 '
        NOMTPA(I) = TEXTE
40    CONTINUE
!
!  EXEMPLE
!
!      NOMTPA(1)='OXYGENE         G/L             '
!
!
      ENDIF
!
!-----------------------------------------
!
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
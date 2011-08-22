C                       ***************
                        SUBROUTINE FLOT
C                       ***************
C
     *(XFLOT,YFLOT,NFLOT,NITFLO,FLOPRD,X,Y,NPOIN,DEBFLO,FINFLO,NIT)
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.2    17/08/94    J-M JANIN    (LNH) 30 87 72 84
C
C***********************************************************************
C
C   FONCTION : L'UTILISATEUR DOIT DONNER ICI :
C
C   1) LE PAS DE TEMPS DE LARGAGE DE CHAQUE FLOTTEUR
C
C   2) LE PAS DE TEMPS DE FIN DE CALCUL DE DERIVE DE CHAQUE FLOTTEUR
C
C   3) LA POSITION DES FLOTTEURS AU MOMENT DU LARGAGE
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    XFLOT,YFLOT |<-- | POSITIONS SUCCESSIVES DES FLOTTEURS.         |
C |    NFLOT       | -->| NOMBRE DE FLOTTEURS.                         |
C |    NITFLO      | -->| NOMBRE MAXIMAL D'ENREGISTREMENTS DES         |
C |                |    | POSITIONS SUCCESSIVES DES FLOTTEURS.         |
C |    FLOPRD      | -->| NOMBRE DE PAS DE TEMPS ENTRE 2 ENREGITREMENTS|
C |                |    | DES POSITIONS SUCCESSIVES DES FLOTTEURS.     |
C |    X,Y         | -->| COORDONNEES DES POINTS DU MAILLAGE.          |
C |    NPOIN       | -->| NOMBRE DE POINTS DU MAILLAGE.                |
C |    DEBFLO      |<-- | NUMEROS DES PAS DE TEMPS DE LARGAGE DE       |
C |                |    | CHAQUE FLOTTEUR.                             |
C |    FINFLO      |<-- | NUMEROS DES PAS DE TEMPS DE FIN DE CALCUL DE |
C |                |    | DERIVE POUR CHAQUE FLOTTEUR.                 |
C |    NIT         | -->| NOMBRE DE PAS DE TEMPS.                      |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : TELMAC
C
C SOUS-PROGRAMME APPELE : NEANT
C
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NFLOT,NITFLO,FLOPRD,NPOIN,IFLOT,NIT
      INTEGER DEBFLO(NFLOT),FINFLO(NFLOT)
C
      DOUBLE PRECISION XFLOT(NITFLO,NFLOT),YFLOT(NITFLO,NFLOT)
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
C
C-----------------------------------------------------------------------
C
C     INITIALISATION DU PAS DE TEMPS DE LARGAGE DE CHAQUE FLOTTEUR
C     PAR DEFAUT, LE LARGAGE DE TOUS LES FLOTTEURS EST FAIT AU 1ER PAS
C
C-----------------------------------------------------------------------
C
C  1) PAS DE TEMPS DE LARGAGE (DEBFLO)
C  2) PAS DE TEMPS DE FIN DE CALCUL (FINFLO)
C
      DEBFLO(1) = 1
      FINFLO(1) = NIT
      DEBFLO(2) = 100
      FINFLO(2) = 600
C
C-----------------------------------------------------------------------
C
C  3) COORDONNEES DES FLOTTEURS AU DEPART
C
C     INITIALISATION DE LA POSITION DES FLOTTEURS AU MOMENT DU LARGAGE
C     PAR DEFAUT, LE FLOTTEUR "IFLOT" EST LARGUE AU POINT "IFLOT"
C
C-----------------------------------------------------------------------
C
      XFLOT(1,1)= -14.D0
      YFLOT(1,1)= 418.D0

      XFLOT(1,2)= 636.D0
      YFLOT(1,2)= 368.D0
C
C-----------------------------------------------------------------------
C
      RETURN
      END

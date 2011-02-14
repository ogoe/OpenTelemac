C                       ********************************
                        DOUBLE PRECISION FUNCTION DEBSCE
C                       ********************************
C
C
     *( TIME , I , DISCE )
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.0    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C FONCTION  : DONNE LA VALEUR DU DEBIT POUR TOUTES LES SOURCES
C
C             PERMET DE PROGRAMMER DES VARIATIONS EN FONCTION DU TEMPS
C             ET DE LA PROFONDEUR.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   TIME         | -->| TIME
C |   I            | -->| NUMBER OF THE SOURCE
C |   DISCE        | -->| ARRAY OF DISCHARGES OF SOURCES.
C |                |    | READ IN THE PARAMETER FILE.
C |                |    | NAME OF DISCE IS DSCE IN TELEMAC-2D.
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : DIFSOU ET PROSOU
C
C***********************************************************************
C
      USE BIEF
C     USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN) :: TIME,DISCE(*)
      INTEGER         , INTENT(IN) :: I
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-2D
C
      DEBSCE = DISCE(I)
C
C-----------------------------------------------------------------------
C
      RETURN
      END          
C                       *******************************
                        DOUBLE PRECISION FUNCTION TRSCE
C                       *******************************
C
     *( TIME , I , ITRAC )
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.8    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C FONCTION  : DONNE LA VALEUR DU TRACEUR POUR TOUTES LES SOURCES
C
C             PERMET DE FAIRE VARIER LA VALEUR AUX SOURCES EN FONCTION
C             DU TEMPS.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   TIME         | -->| TIME
C |   I            | -->| NUMBER OF SOURCE
C |   TRASCE       | -->| ARRAY OF PRESCRIBED VALUES OF THE TRACER
C |                |    | (READ IN THE PARAMETER FILE) 
C |                |    | NAME OF TRASCE IN TELEMAC-2D IS TSCE
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : BORD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER         , INTENT(IN) :: I,ITRAC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      TRSCE = TSCE(I,ITRAC)
C
C-----------------------------------------------------------------------
C
      RETURN
      END

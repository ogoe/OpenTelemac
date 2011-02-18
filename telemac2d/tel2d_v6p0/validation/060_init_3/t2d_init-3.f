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
      IF(TIME.GE.1800.D0.AND.TIME.LE.3600.D0) THEN                                                                          
        DEBSCE = DISCE(I)                                                  
      ELSEIF(TIME.GE.4800.D0.AND.TIME.LE.7200.D0) THEN                                                
        DEBSCE = DISCE(I) 
      ELSE
        DEBSCE = 0.D0
      ENDIF
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
C  TELEMAC 2D VERSION 5.8   30/08/07   J-M HERVOUET (LNH) 01 30 87 80 18
C
C***********************************************************************
C
C FONCTION  : DONNE LA VALEUR DU TRACEUR POUR TOUTES LES SOURCES
C
C             PERMET DE FAIRE VARIER LA VALEUR AUX SOURCES EN FONCTION
C             DU TEMPS.
C
C-----------------------------------------------------------------------
C
C FUNCTION  : GIVES THE PRESCRIBED VALUE OF TRACERS AT THE SOURCES
C
C             THIS VALUE MAY VARY IN TIME.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   TIME         | -->| TIME
C |   I            | -->| SOURCE RANK
C |   TSCE         | -->| ARRAY OF PRESCRIBED VALUES OF THE TRACER
C |                |    | (READ IN THE PARAMETER FILE) 
C |   ITRAC        | -->| TRACER RANK
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
      IF(TIME.GE.1800.D0.AND.TIME.LE.3600.D0) THEN                                                                          
        TRSCE = TSCE(I,ITRAC) 
      ELSEIF(TIME.GE.4800.D0.AND.TIME.LE.7200.D0) THEN                                                
        TRSCE = TSCE(I,ITRAC) 
      ELSE
        TRSCE = 0.D0
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END        

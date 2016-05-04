!                       ********************************
                        DOUBLE PRECISION FUNCTION DEBSCE
!                       ********************************
!
!
     &( TIME , I , DISCE )
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.0    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
! FONCTION  : DONNE LA VALEUR DU DEBIT POUR TOUTES LES SOURCES
!
!             PERMET DE PROGRAMMER DES VARIATIONS EN FONCTION DU TEMPS
!             ET DE LA PROFONDEUR.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   TIME         | -->| TIME
! |   I            | -->| NUMBER OF THE SOURCE
! |   DISCE        | -->| ARRAY OF DISCHARGES OF SOURCES.
! |                |    | READ IN THE PARAMETER FILE.
! |                |    | NAME OF DISCE IS DSCE IN TELEMAC-2D.
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : DIFSOU ET PROSOU
!
!***********************************************************************
!
      USE BIEF
!     USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME,DISCE(*)
      INTEGER         , INTENT(IN) :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-2D
!
      DEBSCE = DISCE(I)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *******************************
                        DOUBLE PRECISION FUNCTION TRSCE
!                       *******************************
!
     &( TIME , I , ITRAC )
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.8    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
! FONCTION  : DONNE LA VALEUR DU TRACEUR POUR TOUTES LES SOURCES
!
!             PERMET DE FAIRE VARIER LA VALEUR AUX SOURCES EN FONCTION
!             DU TEMPS.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   TIME         | -->| TIME
! |   I            | -->| NUMBER OF SOURCE
! |   TRASCE       | -->| ARRAY OF PRESCRIBED VALUES OF THE TRACER
! |                |    | (READ IN THE PARAMETER FILE)
! |                |    | NAME OF TRASCE IN TELEMAC-2D IS TSCE
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : BORD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER         , INTENT(IN) :: I,ITRAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TRSCE = TSCE(I,ITRAC)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
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


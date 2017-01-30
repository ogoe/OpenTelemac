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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
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
      IF(TIME.GE.1800.D0.AND.TIME.LE.3600.D0) THEN
        DEBSCE = DISCE(I)
      ELSEIF(TIME.GE.4800.D0.AND.TIME.LE.7200.D0) THEN
        DEBSCE = DISCE(I)
      ELSE
        DEBSCE = 0.D0
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END


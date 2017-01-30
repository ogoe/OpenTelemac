!                       *****************
                        SUBROUTINE EXCORSTR
!                       *****************
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.2    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!      FONCTION: CORRECTION DU COEFFICIENT DE FROTTEMENT SUR LE FOND
!                QUAND IL EST VARIABLE EN TEMPS.
!
!      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
!      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
!
!
!
!
!-----------------------------------------------------------------------
!  EXAMPLE OF POSSIBLE ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    CHESTR      |<-- |  COEFFICIENT DE FROTTEMENT                   |
! |    X,Y         | -->|  COORDONNEE DU MAILLAGE .                    |
! |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
! |    PRIVE       | -->|  TABLEAU DE TRAVAIL DEFINI DANS PRINCI       |
! |    ZF          | -->|  COTE DU FOND                                |
! |    KFROT       | -->|  LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)|
! |    FFON        | -->|  COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI  |
! |    H           | -->|  HAUTEUR D'EAU.
! |    AT          | -->|  TIME.
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : TELMAC
!
!  SOUS-PROGRAMME APPELE :
!
!**********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
!
      INTEGER :: I
!
!-----------------------------------------------------------------------

      IF (LT == 1) THEN
        DO I = 1, NPOIN
           IF( ABS(CHESTR%R(I) - 0.025D0) < 1.D-7 ) THEN
              CHESTR%R(I) = 0.020D0
           ENDIF
        ENDDO
      END IF

!-----------------------------------------------------------------------
!
      RETURN
      END


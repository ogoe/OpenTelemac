!                       ***************
                        SUBROUTINE HREF
!                       ***************
!
!***********************************************************************
! PROGICIEL : BIEF 5.0         01/03/90    J-M HERVOUET
!***********************************************************************
!
!  FONCTION  : CALCUL DE LA HAUTEUR DE REFERENCE POUR LES EQUATIONS
!              DE BOUSSINESQ
!
!              PAR DEFAUT ON PREND LA HAUTEUR INITIALE
!
!              CE SOUS-PROGRAMME PEUT ETRE MODIFIE
!
!              ON PEUT METTRE PAR EXEMPLE LA HAUTEUR DE LINEARISATION
!
!              SI ON VEUT RETOMBER SUR SAINT-VENANT, ON PEUT METTRE
!              H0 = 0
!
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      H0        |<-- | HAUTEUR DE REFERENCE
! |      H         | -->| HAUTEUR INITIALE
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      ZF        | -->| FOND A MODIFIER.
! |      HAULIN    | -->| PROFONDEUR DE LINEARISATION
! |      COTINI    | -->| COTE INITIALE
! |      MESH      | -->| MAILLAGE
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!     CALL OS( 'X=Y     ' , H0 , H , H , C )
!
      CALL OS( 'X=C     ' , H0 , H , H , 2.4D0 )
!
!-----------------------------------------------------------------------
!
      RETURN
      END


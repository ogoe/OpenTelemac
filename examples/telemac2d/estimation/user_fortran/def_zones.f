!                       ********************
                        SUBROUTINE DEF_ZONES
!                       ********************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.2          17/08/01    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE
!
!  FUNCTION  : DEFINITION OF ZONES IN THE MESH
!
!              THE RESULT MUST BE
!
!              NZONE : THE NUMBER OF ZONES
!
!              ZONE : STRUCTURE OF SIZE NPOIN STATING THE ZONE NUMBER
!                     OF EVERY POINT
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      A         |<-- | MATRICE
! |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
! |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      NZONE = 1
!
      DO I=1,NPOIN
        ZONE%I(I) = 1
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'DEF_ZONES : ',NZONE,' ZONES DEFINIES'
      IF(LNG.EQ.2) WRITE(LU,*) 'DEF_ZONES : ',NZONE,' ZONES DEFINED'
!
!-----------------------------------------------------------------------
!
      RETURN
      END


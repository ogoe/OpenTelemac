!                       *****************
                        SUBROUTINE EXACTE
!                       *****************
!
     &(H,U,ZF,X,NPOIN)
!
!***********************************************************************
! PROGICIEL : 'TELEMAC'       12/12/88    J-M HERVOUET
!
!***********************************************************************
!
!      FONCTION:    SOLUTION EXACTE DE L'ECOULEMENT TRANSCRITIQUE
!                   SUR UN BUMP.
!
!                   PAR CONVENTION, ZF=0. AU POINT CRITIQUE
!
!                   ATTENTION, IL NE S'AGIT ICI QUE DE LA SOLUTION
!                   PERMANENTE, QUI EST TOUTEFOIS MISE DANS LE
!                   FICHIER DE RESULTATS A TOUS LES PAS DE TEMPS.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |     HN         |<-- |  HAUTEUR D'EAU.                              |
! |     U          |<-- |  VITESSE U.
! |     ZF         | -->|  COTE DU FOND.
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I,NPOIN
!
      DOUBLE PRECISION H(NPOIN),U(NPOIN),ZF(NPOIN),X(NPOIN)
      DOUBLE PRECISION Q,H0,A(4)
!
      EXTERNAL FC1
      DOUBLE PRECISION FC1
!
      COMMON/FORFC1/A
!
!-----------------------------------------------------------------------
!
! DEBIT ET HAUTEUR AU POINT CRITIQUE
!
      Q = 0.3D0
      H0 = ( Q**2/9.81D0 )**(1.D0/3.D0)
!
! EQUATION A RESOUDRE : A(1)*H**3 + A(2)*H**2 + A(3)*H + A(4)
!
      A(1) = 1.D0
      A(3) = 0.D0
      A(4) = H0**3.D0/2.D0
      DO I=1,NPOIN
!
        A(2) = ZF(I)-3.D0*H0/2.D0
!
        IF(X(I).GT.10.D0) THEN
!         ON PREND LA PLUS PETITE SOLUTION REELLE
          H(I) = H0
          CALL ZBRENT(FC1,1.D-3,0.1D0,H(I),100)
        ELSEIF(X(I).LT.10.D0) THEN
!         ON PREND LA PLUS GRANDE SOLUTION REELLE
          H(I) = 0.5D0
          CALL ZBRENT(FC1,1.D-3,H0,H(I),100)
        ELSE
!         POINT CRITIQUE
          H(I) = H0
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        U(I) = Q / MAX(H(I),1.D-8)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END


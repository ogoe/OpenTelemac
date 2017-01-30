!                       *****************
                        SUBROUTINE CORSUI
!                       *****************
!
!
     &(H,ZF,X,Y,NPOIN)
!
!***********************************************************************
! PROGICIEL : TELEMAC           01/03/90    J-M HERVOUET
!***********************************************************************
!
!  FONCTION  : FONCTION DE CORRECTION DES FONDS RELEVES
!
!              CE SOUS-PROGRAMME UTILITAIRE NE FAIT RIEN DANS LA
!              VERSION STANDARD. IL EST A LA DISPOSITION DES
!              UTILISATEURS, POUR LISSER OU CORRIGER DES FONDS SAISIS
!              PAR EXEMPLE.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT : TELMAC
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER NPOIN,I
!
      DOUBLE PRECISION H(*),X(*),Y(*),ZF(*)
!
      DOUBLE PRECISION DISTAN,X1,X2,Y1,Y2,HD
      EXTERNAL DISTAN
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VARIABLES POUR LE CALCUL DE LA SITUATION DU POINT
!   X1,Y1,X2,Y2 POINT DEFINISANT LA DROITE DE LIMITE DE BARRAGE
!   X3,Y3 POINT DEFINISANT LES COORDONNEES D POINT A DROITE DE LIMITE DE
!
      X1= 4701.183D0
      Y1= 4143.407D0
      X2= 4655.553D0
      Y2= 4392.104D0
!
      DO I=1,NPOIN
        HD=DISTAN(X1,Y1,X2,Y2,X(I),Y(I))
        IF(HD.GT.0.001D0) THEN
          H(I) = 100.D0 - ZF(I)
        ENDIF
!
!  ZONE DERRIERE LE BARRAGE MAIS QUI N'EST PAS DANS
!  LA RETENUE.
!
        IF((X(I)-4500.D0)**2+(Y(I)-5350.D0)**2.LT.200.D0**2) THEN
          H(I)=0.D0
        ENDIF
!
        H(I)=MAX(0.D0,H(I))
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END


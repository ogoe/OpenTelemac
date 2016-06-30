!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC-2D VERSION 5.0         19/08/98  J-M HERVOUET TEL: 30 87 80 18
!
!***********************************************************************
!
!     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |                | -- |
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TEMPS
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VITESSES : VITESSES NULLES
!
      CALL OS( 'X=C     ' , X=U , C=0.D0 )
      CALL OS( 'X=C     ' , X=V , C=0.D0 )
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DE H , LA HAUTEUR D'EAU
!
      IF(CDTINI(1:10).EQ.'COTE NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES') THEN
!  ZONE A MODIFIER
        WRITE(LU,*) 'CONDITIONS PARTICULIERES A PROGRAMMER'
        CALL PLANTE(1)
        STOP
!  FIN DE LA ZONE A MODIFIER
      ELSE
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        CALL PLANTE(1)
        STOP
      ENDIF
!
      CALL CORSUI(H%R,U%R,V%R,ZF%R,X,Y,NPOIN)
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TRACEUR
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS('X=C     ',X=T%ADR(ITRAC)%P,C=TRAC0(ITRAC))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISATION DE LA VISCOSITE
!
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!               ***************************************************
                DOUBLE PRECISION FUNCTION DISTAN(X1,Y1,X2,Y2,X3,Y3)
!               ***************************************************
!
!***********************************************************************
! PROGICIEL : TELEMAC           23/07/91
!
!***********************************************************************
!
!   FONCTION : CETE FONCTION CALCULE LA DISTANCE ENTRE UNE DROITE
! ET UN POINT SUR LE MAILLAGE
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    X1          | -->  ABSCISSE DU PREMIER POINT SUR LA DROITE
! |    Y1          | -->| COORDONNEE DU PREMIER POINT SUR LA DROITE
! |    X2          | -->  ABSCISSE DU DEUXIEME POINT SUR LA DROITE
! |    Y2          | -->| COORDONNEE DU DEUXIEME POINT SUR LA DROITE
! |    X           | -->| ABSCISSE DU POINT POUR LEQUEL ON CHERCHE DIST1
! |    Y           | -->| COORDONNEE DU POINT POUR LEQUEL ON CHERCHE DIS
! |    DISTAN      |<-- |  DISTANCE ENTRE LA DROITE ET LE POINT
! |________________|____|_______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!***********************************************************************
!
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      DOUBLE PRECISION X1,X2,X3,Y1,Y2,Y3
      DOUBLE PRECISION A1,B1,C1,DET
      INTRINSIC SQRT
      A1=Y1-Y2
      B1=-X1+X2
      C1=X1*Y2-X2*Y1
      DET=SQRT((A1**2)+(B1**2))
      DISTAN=((A1*X3)+(B1*Y3)+C1)/DET
      RETURN
      END
!                       *****************
                        SUBROUTINE CORSUI
!                       *****************
!
!
     &(H,U,V,ZF,X,Y,NPOIN)
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
      DOUBLE PRECISION H(*),X(*),Y(*),ZF(*),U(*),V(*)
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
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
!
!  ZONE DERRIERE LE BARRAGE MAIS QUI N'EST PAS DANS
!  LA RETENUE.
!
        IF((X(I)-4500.D0)**2+(Y(I)-5350.D0)**2.LT.200.D0**2) THEN
          H(I)=0.D0
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

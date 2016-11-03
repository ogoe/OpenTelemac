!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC-2D VERSION 5.9         19/08/98  J-M HERVOUET TEL: 30 87 80 18
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
      INTEGER IPOIN,ITRAC
!
      DOUBLE PRECISION EIKON
!
      INTRINSIC EXP
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
      CALL OS( 'X=0     ' , X=U )
      CALL OS( 'X=0     ' , X=V )
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
      ELSEIF(CDTINI(1:10).EQ.'PARTICULAR') THEN
      DO IPOIN=1,NPOIN
        EIKON=( (X(IPOIN)-10.05D0)**2 + (Y(IPOIN)-10.05D0)**2 ) / 4.D0
        H%R(IPOIN) = 2.4D0 * ( 1.D0 + EXP(-EIKON) )
      ENDDO
      ELSE
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TRACEUR 1
!
      IF(NTRAC.GT.0) THEN
        CALL OS( 'X=0     ' , X=T%ADR(1)%P )
        DO IPOIN=1,NPOIN
          IF((X(IPOIN)-10.05D0)**2+(Y(IPOIN)-10.05D0)**2.LT.4.D0**2)
     &    THEN
            T%ADR(1)%P%R(IPOIN) = 1.D0
          ENDIF
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

!                       *************************
                        SUBROUTINE CONDIM_SISYPHE
!                       *************************
!
     &(U      , V   , QU    , QV  , H   , ZF , Z ,
     & ESOMT  ,THETAWR, Q   , HWR , TWR ,
     & X      , Y   , NPOIN , AT  , PMAREE)
!
!***********************************************************************
! SISYPHE VERSION 5.3                             E. PELTIER    11/09/95
!                                                 C. LENORMANT
!                                                 J.-M. HERVOUET
!
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT
!***********************************************************************
!
!     FONCTION  : VALEURS IMPOSEES
!                         - DU DEBIT VECTORIEL    QU, QV
!                         - DE LA HAUTEUR D'EAU   H
!                         - DE LA COTE DU FOND    ZF
!                         - DE LA SURFACE LIBRE   Z
!                         - DE L'EVOLUTION TOTALE ESOMT
!                         - DU DEBIT              Q
!                         - DE LA HAUTEUR DE HOULE HW
!                         - DE LA PERIODE DE HOULE TW
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   U , V        |<-- | COORDONNEES DES VECTEURS VITESSE
! |   QU , QV      |<-- | DEBIT VECTORIEL SUIVANT X ET SUIVANT Y
! |   H            |<-->| HAUTEUR D'EAU
! |   ZF           |<-->| COTE DU FOND
! |   Z            |<-->| COTE DE SURFACE LIBRE
! |   ESOMT        |<-->| EVOLUTION TOTALE DES FONDS
! |   C            |<-->| CELERITE
! |   Q            |<-->| DEBIT
! |   HW           | -->| HAUTEUR DE HOULE
! |   TW           | -->| PERIODE DE HOULE
! |   X,Y          | -->| COORDONNEES DU MAILLAGE
! |   NPOIN        | -->| NOMBRE DE POINTS DU MAILLAGE
! |   AT           | -->| TEMPS
! |   PMAREE       | -->| PERIODE DE LA MAREE
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! PROGRAMME APPELANT : SISYPH
! PROGRAMMES APPELES :
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)::NPOIN
!
      DOUBLE PRECISION, INTENT(IN):: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN):: AT , PMAREE
! SEDIMENT
      DOUBLE PRECISION, INTENT(INOUT) ::  ZF(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::  ESOMT(NPOIN)
! HYDRODYNAMICS
      DOUBLE PRECISION, INTENT(INOUT):: Z(NPOIN) , H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::QU(NPOIN), QV(NPOIN), Q(NPOIN)
! WAVES
      DOUBLE PRECISION, INTENT (INOUT):: HWR(NPOIN) , TWR(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT):: THETAWR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  --------------------------------------------------------------
!  INITIALISATION DES TABLEAUX NON LUS DANS LE FICHIER RESULTATS:
!  --------------------------------------------------------------
!
      DO I=1,NPOIN
        Z(I)=10.D0
        ZF(I)=0.D0
        H(I)=Z(I)-ZF(I)
!
!  MODIF JMH : U VARIABLE : CROIT LINEAIREMENT JUSQU'A 10 POUR X=8
!                           REDESCEND ENSUITE
!
        IF(X(I).LT.8.D0) THEN
!         QU(I)=10.D0*X(I)/8.D0
          U(I)=X(I)/8.D0
        ELSE
!         QU(I)=10.D0*(1.D0-(X(I)-8.D0)/8.D0)
          U(I)=1.D0-(X(I)-8.D0)/8.D0
        ENDIF
!       QV(I)=0.D0
        V(I)=0.D0
!       Q(I)=SQRT(QU(I)**2+QV(I)**2)
!
      ENDDO
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE NOEROD
!                       *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! TSEF VERSION 3.2                                          C. LENORMANT
!
!***********************************************************************
!
!     FONCTION  : IMPOSE LA VALEUR DE LA COTE DU FOND NON ERODABLE  ZR
!
!
!     LES METHODES DE TRAITEMENT DES FONDS NON ERODABLES PEUVENT CONDUIRE
!     A ZF < ZR A CERTAINS PAS DE TEMPS, POUR PALLIER A CELA ON PEUT CHOISIR
!     CHOISIR DE LISSER LA SOLUTION OBTENUE I.E NLISS > 0.
!
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   H            | -->| HAUTEUR D'EAU
! |   ZF           | -->| COTE DU FOND
! |   ZR           |<-- | COTE DU FOND NON ERODABLE
! |   Z            | -->| COTE DE SURFACE LIBRE
! |   X,Y          | -->| COORDONNEES DU MAILLAGE
! |   NPOIN        | -->| NOMBRE DE POINTS DU MAILLAGE
! |   CHOIX        | -->| METHODE CHOISIE POUR LE TRAITEMENT DES FONDS
! |                | -->| NON ERODABLES
! |   NLISS        |<-->| NOMBRE DE LISSAGES
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT)::  ZR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION   PI,XMAX,ZEMAX
!
!-----------------------------------------------------------------------
!
!-----------------------------------
! TRAITEMENT DES FONDS NON ERODABLES
!------------------------------------
!
!       PAR DEFAUT, ZR=ZF-100 !
!    ZEMAX: EPAISSEUR MAX DU LIT
        ZEMAX=0.10D0
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-ZEMAX,NPOIN)
!    XMAX: LONGUEUR DU PLATIER
        XMAX=6.D0
        DO I=1,NPOIN
           IF(X(I).LE.XMAX.OR.X(I).GE.10.5D0) THEN
             ZR(I)=ZF(I)
           ENDIF
        ENDDO
!
!       NLISS CORRESPOND AU NOMBRE DE LISSAGES EFFECTUEES SUR
!       LES VALEURS NEGATIVES DE LA VARIABLE (ZF-ZR)
!       PAR DEFAUT NLISS = 0
!
        NLISS = 0
!
!-----------------------------------------------------------------------
!
      RETURN
      END

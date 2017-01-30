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


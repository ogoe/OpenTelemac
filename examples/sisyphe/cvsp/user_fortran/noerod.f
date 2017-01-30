!                       *****************
                        SUBROUTINE NOEROD
!                       *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! SISYPHE VERSION 5.1                             C. LENORMANT
!
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT
!***********************************************************************
!
!     FONCTION  : IMPOSE LA VALEUR DE LA COTE DU FOND NON ERODABLE  ZR
!
!
!     RQ: LES METHODES DE TRAITEMENT DES FONDS NON ERODABLES PEUVENT CONDUIRE
!     A ZF < ZR A CERTAINS PAS DE TEMPS, POUR PALLIER A CELA ON PEUT CHOISIR
!     CHOISIR DE LISSER LA SOLUTION OBTENUE i.e NLISS > 0.
!
!     FUNCTION  : IMPOSE THE RIGID BED LEVEL  ZR
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   H            | -->| WATER DEPTH
! |   ZF           | -->| BED LEVEL
! |   ZR           |<-- | RIGID BED LEVEL
! |   Z            | -->| FREE SURFACE
! |   X,Y          | -->| 2D COORDINATES
! |   NPOIN        | -->| NUMBER OF 2D POINTS
! |   CHOIX        | -->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
! |   NLISS        |<-->| NUMBER OF SMOOTHINGS
! |________________|____|______________________________________________
! MODE : -->(INPUT), <--(RESULT), <-->(MODIFIED DATA)
!-----------------------------------------------------------------------
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!-----------------------------------------------------------------------
      INTEGER I
!--------------------
! RIGID BEDS POSITION
!---------------------
!
!       DEFAULT VALUE:       ZR=ZF-100
!
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-0.3D0,NPOIN)
!
        DO I=1,NPOIN
        IF (X(I) < 100.0D0) THEN
           ! ZR(I)=ZF(I)
        END IF
        END DO
!
!------------------
! SMOOTHING OPTION
!------------------
!       NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!                DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
        NLISS = 0
!
!
      RETURN
      END SUBROUTINE NOEROD


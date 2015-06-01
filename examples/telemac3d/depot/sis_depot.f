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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPOIN,CHOIX,NLISS,I
!
      DOUBLE PRECISION Z(NPOIN) , ZF(NPOIN) , ZR(NPOIN)
      DOUBLE PRECISION X(NPOIN) , Y(NPOIN)  , H(NPOIN)
!
!-----------------------------------------------------------------------
!
!--------------------
! RIGID BEDS POSITION
!---------------------
!
!     HERE ZR=ZF-0.01D0 : PROVISIONAL, SHOULD BE 0,
!                         WAITING FOR TELEMAC-3D CORRECTION... SO THAT
!                         ZF ALWAYS >= ZR
!
      CALL OV('X=Y+C   ',ZR,ZF,ZF,-0.01D0,NPOIN)
!
!------------------
! SMOOTHING OPTION
!------------------
!     NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!                DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
      NLISS = 0
!
!-----------------------------------------------------------------------
!
      RETURN
      END

C                       *****************
                        SUBROUTINE NOEROD
C                       *****************
C
     * (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
C
C***********************************************************************
C SISYPHE VERSION 5.1                             C. LENORMANT
C                                                
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT   
C***********************************************************************
C
C     FONCTION  : IMPOSE LA VALEUR DE LA COTE DU FOND NON ERODABLE  ZR
C
C
C     RQ: LES METHODES DE TRAITEMENT DES FONDS NON ERODABLES PEUVENT CONDUIRE
C     A ZF < ZR A CERTAINS PAS DE TEMPS, POUR PALLIER A CELA ON PEUT CHOISIR 
C     CHOISIR DE LISSER LA SOLUTION OBTENUE i.e NLISS > 0.  
C
C     FUNCTION  : IMPOSE THE RIGID BED LEVEL  ZR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   H            | -->| WATER DEPTH
C |   ZF           | -->| BED LEVEL
C |   ZR           |<-- | RIGID BED LEVEL
C |   Z            | -->| FREE SURFACE 
C |   X,Y          | -->| 2D COORDINATES
C |   NPOIN        | -->| NUMBER OF 2D POINTS
C |   CHOIX        | -->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
C |   NLISS        |<-->| NUMBER OF SMOOTHINGS
C |________________|____|______________________________________________
C MODE : -->(INPUT), <--(RESULT), <-->(MODIFIED DATA)
C-----------------------------------------------------------------------
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN,CHOIX,NLISS,I
C
      DOUBLE PRECISION Z(NPOIN) , ZF(NPOIN) , ZR(NPOIN)
      DOUBLE PRECISION X(NPOIN) , Y(NPOIN)  , H(NPOIN)
C
C-----------------------------------------------------------------------
C
C--------------------
C RIGID BEDS POSITION
C---------------------
C
C     HERE ZR=ZF-0.01D0 : PROVISIONAL, SHOULD BE 0,
C                         WAITING FOR TELEMAC-3D CORRECTION... SO THAT
C                         ZF ALWAYS >= ZR
C                                                              
      CALL OV('X=Y+C   ',ZR,ZF,ZF,-0.01D0,NPOIN)                                                    
C
C------------------
C SMOOTHING OPTION
C------------------
C     NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
C                DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
C
      NLISS = 0        
C
C-----------------------------------------------------------------------
C
      RETURN
      END 

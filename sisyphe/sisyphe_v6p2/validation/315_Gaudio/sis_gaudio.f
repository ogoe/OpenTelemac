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
      use declarations_sisyphe, only : mesh
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS 
C
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)   
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
      double precision y1,y2,y3,y4,y5,y6,x1,x2,x3,x4,x5,x6
C
C-----------------------------------------------------------------------
      INTEGER I
C--------------------
C RIGID BEDS POSITION
C---------------------
C
C       DEFAULT VALUE:       ZR=ZF-100 
C                                                              
CGL        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-100.D0,NPOIN)                                                    
      
        DO i=1,npoin
            IF(       MESH%X%R(i) .GE.  0.000
     *          .AND. MESH%X%R(i) .LE.  6.000   ) THEN
                  ZR(I) = ZF(I) - 0.15 
            ELSE
                  ZR(I) = ZF(I)
            END IF
      
        END DO
cmw
cmw       print*,'CHESTR%R(i)' 
cmw        
cmw        DO i=1,npoin         
cmw            IF(       CHESTR%R(i) .GT. 0.003) THEN        
cmw                  ZR(I) = ZF(I)
cmw            END IF 
cmw         
cmw        END DO
cmw        
cmwc----------Sohlschwelle 1------------
!RK
         Y1 =  -0.1
         Y2 =  0.61
         X1 =  1.98948
!RK         X2 =  2.01436
         X2 = 2.01451
            
         DO i=1,npoin
            IF(       MESH%Y%R(i) .GE. Y1
     *          .AND. MESH%Y%R(i) .LE. Y2
     *          .AND. MESH%X%R(i) .GE. X1
     *          .AND. MESH%X%R(i) .LE. X2   ) THEN
            
                  ZR(I) = ZF(I) - 0.00000001
C                  ZR(I) = ZF(I)
            
            END IF
         END DO   
                        
c----------Sohlschwelle 2------------
         Y3 =  -0.10
         Y4 =  0.61
         X3 =  4.48948
!RK         X4 =  4.51436
!ER         X4 = 4.51451
	 X4 = 4.5
            
         DO i=1,npoin
            IF(       MESH%Y%R(i) .GE. Y3
     *          .AND. MESH%Y%R(i) .LE. Y4
     *          .AND. MESH%X%R(i) .GE. X3
     *          .AND. MESH%X%R(i) .LE. X4   ) THEN
            
                  ZR(I) = ZF(I) - 0.00000001
C                  ZR(I) = ZF(I)
            
            END IF
         END DO   
                        
c----------Sohlschwelle 3------------
!RK         Y5 =  0.0
!         Y6 =  0.6
!         X5 =  5.48948
!         X6 =  5.570
!            
         DO i=1,npoin
             IF(MESH%X%R(i).GT.4.5) THEN
!            IF(       MESH%Y%R(i) .GE. Y5
!     *          .AND. MESH%Y%R(i) .LE. Y6
!     *          .AND. MESH%X%R(i) .GE. X5
!     *          .AND. MESH%X%R(i) .LE. X6   ) THEN
!            
                  ZR(I) = ZF(I) - 0.00000001
C                  ZR(I) = ZF(I)
            
            END IF
         END DO   
                        
C
C------------------
C SMOOTHING OPTION
C------------------
C       NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
C                DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
C
        NLISS = 0        
C
C
      RETURN
      END SUBROUTINE NOEROD
























































































































































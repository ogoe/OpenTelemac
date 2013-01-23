C     
C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.2          01/03/90    J-M HERVOUET
C***********************************************************************
C USER SUBROUTINE CORFON
C
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      ZF        |<-->| FOND A MODIFIER.
C |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C |      A         |<-- | MATRICE
C |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
C |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
C |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
C |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
C |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
	USE BIEF
        USE DECLARATIONS_TELEMAC2D
C
        IMPLICIT NONE
        INTEGER LNG,LU
        COMMON/INFO/LNG,LU
C
        DOUBLE PRECISION PI
        INTEGER I
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  Bosse à t=0
C
  	PI=3.141592653D0 
        DO I=1,NPOIN                                                            
        ZF%R(I) = 0.D0
          IF (X(I) .GE. 400.D0 .AND. X(I) .LE. 600.D0
     *	     .AND. Y(I) .GE. -100.D0 .AND. Y(I) .LE. 100.D0) THEN                        
           ZF%R(I)=(SIN(PI*(X(I)-400.D0)/200.D0))**2
     *      *(SIN(PI*(Y(I)+100.D0)/200.D0))**2                                                                    
          ENDIF                                                                                                                        
        END DO
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
      RETURN
      END                  
 

C                       *****************
                        SUBROUTINE CONDIN
C                       *****************
C
C***********************************************************************
C TELEMAC-2D VERSION 5.0         19/08/98  J-M HERVOUET TEL: 30 87 80 18
C
C***********************************************************************
C
C     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |                | -- |  
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      INTEGER ITRAC 
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TEMPS
C
      AT = 0.D0
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DES VITESSES : VITESSES NULLES
C
      CALL OS( 'X=C     ' , X=U , C=0.D0 )
      CALL OS( 'X=C     ' , X=V , C=0.D0 )
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DE H , LA HAUTEUR D'EAU
C
      IF(CDTINI(1:10).EQ.'COTE NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES') THEN
C  ZONE A MODIFIER                                                      
       STOP 'CONDITIONS PARTICULIERES A PROGRAMMER'                 
C  FIN DE LA ZONE A MODIFIER      
      ELSE
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        STOP
      ENDIF
C
      CALL CORSUI(H%R,U%R,V%R,ZF%R,X,Y,NPOIN)   
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TRACEUR
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS('X=C     ',X=T%ADR(ITRAC)%P,C=TRAC0(ITRAC))
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
C INITIALISATION DE LA VISCOSITE
C
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C               ***************************************************    
                DOUBLE PRECISION FUNCTION DISTAN(X1,Y1,X2,Y2,X3,Y3)      
C               ***************************************************      
C                                                                        
C*********************************************************************** 
C PROGICIEL : TELEMAC           23/07/91                                 
C                                                                       
C*********************************************************************** 
C                                                                       
C   FONCTION : CETE FONCTION CALCULE LA DISTANCE ENTRE UNE DROITE        
C ET UN POINT SUR LE MAILLAGE                                           
C----------------------------------------------------------------------- 
C                             ARGUMENTS                                  
C .________________.____.______________________________________________. 
C |      NOM       |MODE|                   ROLE                       | 
C |________________|____|______________________________________________|
C |    X1          | -->  ABSCISSE DU PREMIER POINT SUR LA DROITE       
C |    Y1          | -->| COORDONNEE DU PREMIER POINT SUR LA DROITE    
C |    X2          | -->  ABSCISSE DU DEUXIEME POINT SUR LA DROITE     
C |    Y2          | -->| COORDONNEE DU DEUXIEME POINT SUR LA DROITE    
C |    X           | -->| ABSCISSE DU POINT POUR LEQUEL ON CHERCHE DIST1 
C |    Y           | -->| COORDONNEE DU POINT POUR LEQUEL ON CHERCHE DIS 
C |    DISTAN      |<-- |  DISTANCE ENTRE LA DROITE ET LE POINT         
C |________________|____|_______________________________________________ 
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)  
C*********************************************************************** 
C                                                                        
C                                                                        
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
C                       *****************                                
                        SUBROUTINE CORSUI                                 
C                       *****************                                
C                                                                         
C                                                                        
     *(H,U,V,ZF,X,Y,NPOIN)                                              
C                                                                        
C***********************************************************************  
C PROGICIEL : TELEMAC           01/03/90    J-M HERVOUET                  
C*********************************************************************** 
C                                                                      
C  FONCTION  : FONCTION DE CORRECTION DES FONDS RELEVES                
C                                                                      
C              CE SOUS-PROGRAMME UTILITAIRE NE FAIT RIEN DANS LA       
C              VERSION STANDARD. IL EST A LA DISPOSITION DES              
C              UTILISATEURS, POUR LISSER OU CORRIGER DES FONDS SAISIS    
C              PAR EXEMPLE.                                               
C                                                                        
C-----------------------------------------------------------------------  
C                             ARGUMENTS                                   
C .________________.____.______________________________________________   
C |      NOM       |MODE|                   ROLE                          
C |________________|____|_______________________________________________  
C |      ZF        |<-->| FOND A MODIFIER.                                
C |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).  
C |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.                   
C |________________|____|______________________________________________   
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
C-----------------------------------------------------------------------  
C                                                                         
C PROGRAMME APPELANT : TELMAC                                             
C PROGRAMMES APPELES : RIEN EN STANDARD                                   
C                                                                         
C***********************************************************************  
C                                                                         
      IMPLICIT NONE                                                       
C                                                                         
      INTEGER NPOIN,I                                                
C                                                                         
      DOUBLE PRECISION H(*),X(*),Y(*),ZF(*),U(*),V(*)                     
C                                                                         
      DOUBLE PRECISION DISTAN,X1,X2,Y1,Y2,HD                              
      EXTERNAL DISTAN                                                     
C                                                                         
C-----------------------------------------------------------------------  
C                                                                         
C   INITIALISATION DES VARIABLES POUR LE CALCUL DE LA SITUATION DU POINT 
C   X1,Y1,X2,Y2 POINT DEFINISANT LA DROITE DE LIMITE DE BARRAGE          
C   X3,Y3 POINT DEFINISANT LES COORDONNEES D POINT A DROITE DE LIMITE DE 
C                                                                        
       X1= 4701.183D0                                                     
       Y1= 4143.407D0                                                    
       X2= 4655.553D0                                               
       Y2= 4392.104D0                                                    
C                                                                       
       DO 99 I=1,NPOIN                                                  
         HD=DISTAN(X1,Y1,X2,Y2,X(I),Y(I))                              
         IF(HD.GT.0.001D0) THEN                                                                                                                                                      
           H(I) = 100.D0 - ZF(I)                                        
           U(I) = 0.D0                                                   
           V(I) = 0.D0                                                  
         ENDIF                                                           
C                                                                         
C  ZONE DERRIERE LE BARRAGE MAIS QUI N'EST PAS DANS                      
C  LA RETENUE.                                                            
C                                                                        
         IF((X(I)-4500.D0)**2+(Y(I)-5350.D0)**2.LT.200.D0**2) THEN       
           H(I)=0.D0                                                      
         ENDIF                                                            
C                                                                         
99     CONTINUE                                                           
C                                                                         
C-----------------------------------------------------------------------  
C                                                                         
      RETURN                                                              
      END

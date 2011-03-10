!                       *****************
                        SUBROUTINE CONDIM 
!                       *****************
! 
!
!***********************************************************************
! TELEMAC 3D VERSION 5.1    11/12/00      J-M HERVOUET(LNH) 30 87 80 18
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!      FONCTION:
!      =========
!
!    INITIALISATION DES TABLEAUX DES GRANDEURS PHYSIQUES
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : TELEMAC-3D
! SOUS-PROGRAMMES APPELES : OV , (CALCOT)
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I,J
!
!***********************************************************************
! TIME ORIGIN
!
      AT  = 0.D0
!
! INITIALISATION DE H , LA HAUTEUR D'EAU.
!
      IF(.NOT.SUIT2) THEN
!
!     INITIALISATION OF H , THE DEPTH
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     *   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' , H   , H , H , 0.D0 )
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     *       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H , H , COTINI )
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     *       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     *       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     *       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     *       CDTINI(1:07).EQ.'SPECIAL') THEN
!     ZONE A MODIFIER
!     FOR SPECIAL INITIAL CONDITIONS ON DEPTH, PROGRAM HERE                                                     
        IF(LNG.EQ.1) WRITE(LU,10)                                       
        IF(LNG.EQ.2) WRITE(LU,11)                                       
10      FORMAT(1X,'CONDIM : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     *      ,/,1X,'         VOUS DEVEZ MODIFIER CONDIM')                
11      FORMAT(1X,'CONDIM : WITH SPECIAL INITIAL CONDITIONS'            
     *      ,/,1X,'         YOU HAVE TO MODIFY CONDIM')                 
        CALL PLANTE(1)                                                  
        STOP
!     END OF SPECIAL INITIAL CONDITIONS                                                            
!     FIN DE LA ZONE A MODIFIER      
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIM : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIM: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        STOP
      ENDIF 
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR LUE DANS LE FICHIER BINAIRE 1'
        IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS READ IN THE BINARY FILE 1'
      ENDIF
!
!
!
!
!     SPECIFIQUE MALPASSET !!!!!!!!!!!!!!!!!
!
      CALL CORSUI(H%R,Z,X,Y,NPOIN2) 
!
!     SPECIFIQUE MALPASSET !!!!!!!!!!!!!!!!!
!
!
!
!
!
!  CLIPPING OF H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),HMIN)
      ENDDO
!
      CALL OS ('X=Y     ', HN, H, H, 0.D0)

!-----------------------------------------------------------------------
!
!     INITIALISATION DE LA COTE DU PLAN INTERMEDIAIRE DE REFERENCE.
!     PAR DEFAUT, CE PLAN EST PLACE ENTRE FOND ET SURFACE AU PRORATA
!     DU PARAMETRE NPLINT.
!
      IF (NPLINT.GE.2) THEN
        CALL OV( 'X=C     ' , Z((NPLINT-1)*NPOIN2+1 : NPLINT*NPOIN2),
     *                Z, Z, COTINT , NPOIN2)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DE ZSTAR, LE RAPPORT ENTRE LA HAUTEUR D'EAU SOUS
!     UN PLAN QUASI HORIZONTAL ET LA HAUTEUR D'EAU TOTALE
!
! CAS SANS PLAN INTERMEDIAIRE DE REFERENCE
! ----------------------------------------
!
!         ON DOIT AVOIR :
!            * ZSTAR%R(1)     = 0.D0 ( PLAN DU FOND )
!            * ZSTAR%R(NPLAN) = 1.D0 ( PLAN DE LA SURFACE LIBRE )
!         ET POUR TOUT I COMPRIS ENTRE 1 ET NPLAN-1
!            * ZSTAR%R(I) < ZSTAR%R(I+1)
!
! CAS AVEC PLAN INTERMEDIAIRE DE REFERENCE
! ----------------------------------------
!
!         ON DOIT AVOIR :
!            * ZSTAR%R(1)      = -1.D0 ( PLAN DU FOND )
!            * ZSTAR%R(NPLINT) =  0.D0 ( PLAN INTERMEDIAIRE DE REFERENCE
!            * ZSTAR%R(NPLAN)  =  1.D0 ( PLAN DE LA SURFACE LIBRE )
!         ET POUR TOUT I COMPRIS ENTRE 1 ET NPLAN-1
!            * ZSTAR%R(I) < ZSTAR%R(I+1)
!
!     PAR DEFAUT, LES PLANS QUASI HORIZONTAUX SONT REGULIEREMENT ESPACES
!
!***********************************************************************
!     POUR DONNER VOTRE PROPRE REPARTITION DES PLANS, MODIFIEZ LES
!     DEUX BOUCLES SUIVANTES
!     REMARQUE : NPLINT=1 QUAND IL N'Y A PAS DE PLAN INTERMEDIAIRE
!     ATTENTION : EN CAS DE TRANSFORMATION SIGMA GENERALISEE,
!     ---------   ZSTAR(2) A ZSTAR(NPLAN-1) DOIVENT ETRE MODIFIEES
!                 ET CONTENIR LA COTE DE POSITIONNEMENT DES DIFFERENTS
!                 PLANS DU MAILLAGE (IL VA DE SOIT QUE CELLES-CI DOIVENT
!                 ETRE DONNEES DANS UN ORDRE STRICTEMENT CROISSANT).
!***********************************************************************
!
      IF (NPLINT.GE.2) THEN
        DO IPLAN = 1,NPLINT-1
          ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLINT-1)
        END DO
      ENDIF
!
      DO IPLAN = NPLINT,NPLAN
        ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLAN-NPLINT)
      END DO
!
!***********************************************************************
!
!     COMPUTATION OF ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISATION OF VELOCITIES
!
      IF(SUIT2) THEN       
        DO I=1,NPLAN
         DO J=1,NPOIN2
         U%R((I-1)*NPOIN2+J)=U2D%R(J)
         V%R((I-1)*NPOIN2+J)=V2D%R(J)
         ENDDO
        ENDDO
      ELSE
        CALL OS( 'X=C     ' , U , U , U , 0.0D0 )
        CALL OS( 'X=C     ' , V , V , V , 0.0D0 )
      ENDIF
!
      CALL OS( 'X=C     ' , W , W , W , 0.0D0 )
!
!-----------------------------------------------------------------------
!
!     TRACERS INITIALIZATION
!
      IF (NTRAC.NE.0) THEN
        CALL OS( 'X=C     ', TA, TA, TA, 0.D0)
      ENDIF
!
!
!-----------------------------------------------------------------------
!   INITIALISATION DU MODELE K-EPSILON (FACULTATIF)
!   SI VOUS LE FAITES, INDIQUEZ AKEP = .FALSE.
!
      AKEP=.TRUE.
!
!     IF(ITURBV.EQ.3) THEN
!
!       HERE INITIALISE K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALIZE THE PRESSURE FIELDS TO 0.0
! 
!
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
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
     *(H,ZF,X,Y,NPOIN)                                              
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
      DOUBLE PRECISION H(*),X(*),Y(*),ZF(*)                     
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
         ENDIF                                                           
C                                                                         
C  ZONE DERRIERE LE BARRAGE MAIS QUI N'EST PAS DANS                      
C  LA RETENUE.                                                            
C                                                                        
         IF((X(I)-4500.D0)**2+(Y(I)-5350.D0)**2.LT.200.D0**2) THEN       
           H(I)=0.D0                                                      
         ENDIF   
C
         H(I)=MAX(0.D0,H(I))
C                                                                        
99     CONTINUE                                                           
C                                                                         
C-----------------------------------------------------------------------  
C                                                                         
      RETURN                                                              
      END

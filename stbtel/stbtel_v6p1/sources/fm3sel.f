C                       *****************                               
                        SUBROUTINE FM3SEL                               
C                       *****************                               
C                                                                       
     *(X,Y,NPOIN,NBOR,NFIC,STD,NVAR,TEXTE,TEXTLU,VARCLA,NVARCL,   
     * TITRE,SORLEO,NSOR,W,IKLE,                                 
     * IKLES,ITRAV,NELEM,NPTFR,NDP,MXPTVS,MXELVS,DATE,TIME,             
     * DEBU,SUIT,ECRI,LISTIN,IPARAM,IPOBO)                                 
C                                                                       
C***********************************************************************
C PROGICIEL STBTEL V5.2       02/01/96    J-M HERVOUET (LNH) 30 71 80 18 
C                                                                       
C***********************************************************************
C                                                                       
C     COMME FMTSEL, MAIS LA DIMENSION DE SORLEO  EST           
C     PARAMETREE.                                                       
C                                                                       
C     FONCTIONS :  LECTURE DU FICHIER GEOMETRIQUE AU STANDARD SELAFIN   
C                  ECRITURE DU FICHIER GEOMETRIQUE AU STANDARD SELAFIN  
C                                                                       
C     LES FONCTIONS DE CE SOUS-PROGRAMME PEUVENT ETRE PILOTEES AVEC     
C     LES ARGUMENTS DEBU, SUIT, ET ECRI                                 
C                                                                       
C     ATTENTION : 1) SI DEBU, SUIT ET ECRIT SONT A .FALSE.              
C                    FM3SEL LIT LA GEOMETRIE.                           
C                                                                       
C                 2) SI DEBU ITRAV DOIT ETRE LE TABLEAU IA DES ENTIERS  
C                    ET ON NE DOIT PAS SE SERVIR DE IKLE ET IKLES       
C                    CAR LE SOUS-PROGRAMME DE POINTEURS N'A PAS ENCORE  
C                    ETE APPELE.                                        
C-----------------------------------------------------------------------
C                             ARGUMENTS                                 
C .________________.____.______________________________________________ 
C |      NOM       |MODE|                   ROLE                        
C |________________|____|______________________________________________ 
C |   X,Y          |<-->| COORDONNEES DU MAILLAGE.                      
C |   NPOIN        |<-->| NOMBRE DE POINTS DU MAILLAGE.                 
C |   NBOR         | -->| NUMEROTAION GLOBALE DES POINTS DE BORD.       
C |   NFIC         | -->| NUMERO DE CANAL DU FICHIER A LIRE OU ECRIRE.  
C |   STAND        | -->| NON UTILISE                                   
C |   STD          | -->| BINAIRE DU FICHIER (STD, IBM, I3E)            
C |   NVAR         |<-->| NOMBRE DE VARIABLES DANS LE FICHIER           
C |   TEXTE        |<-->| NOMS ET UNITES DES VARIABLES.                 
C |   TEXTLU       |<-->| NOMS ET UNITES DES VARIABLES QU'ON VA LIRE.   
C |   VARCLA       | -->| TABLEAU CONTENANT LES VARIABLES CLANDESTI-NES.
C |   NVARCL       | -->| NOMBRE DE VARIABLES CLANDESTI-NES.            
C |   TITRE        |<-->| TITRE DU FICHIER.                             
C |   SORLEO       | -->| VARIABLES QUE L'ON SOUHAITE ECRIRE DANS LE    
C |                |    | FICHIER (TABLEAU DE 26 LOGIQUES)                                             
C |   NSOR         | -->| DIMENSION DE SOLRLEO                
C |   W            | -->| TABLEAU DE TRAVAIL CONSIDERE ICI COMME REEL   
C |                |    | DE TAILLE NPOIN.                              
C |   IKLE         |<-->| TABLE DE CONNECTIVITE (I.E. PASSAGE DE LA     
C |                |    | NUMEROTATION LOCALE DES POINTS D'UN ELEMENT   
C |                |    | A LA NUMEROTATION GLOBALE                     
C |   IKLES        | -->| TABLEAU DE TRAVAIL SERVANT A MODIFIER IKLE    
C |                |    | DIMENSION NELEM * NDP                         
C |   ITRAV        | -->| TABLEAU DE TRAVAIL ENTIER DE DIMENSION NPOIN  
C |   NELEM        |<-->| NOMBRE D'ELEMENTS DU MAILLAGE.                
C |   NPTFR        |<-->| NOMBRE DE POINTS FRONTIERE DU DOMAINE.        
C |   NDP          |<-->| NOMBRE DE SOMMETS PAR ELEMENT.                                                
C |   DEBU         | -->| ON LIT UNIQUEMENT LE DEBUT DU FICHIER POUR    
C |                |    | CONNAITRE LES NOMBRES DE POINTS AVEC LESQUELS 
C |                |    | ON POURRA CONSTRUIRE LES POINTEURS.           
C |   SUIT         | -->| ON LIT TOUTE LA PARTIE GEOMETRIE DU FICHIER   
C |                |    | POUR SE PLACER SUR LES ENREGISTREMENTS DES    
C |                |    | RESULTATS.                                    
C |   ECRI         | -->| ON ECRIT LA PARTIE GEOMETRIE DU FICHIER       
C |   LISTIN       | -->| ECRITURE D'INFORMATIONS SUR LISTING (OU NON)  
C |________________|____|______________________________________________ 
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
C-----------------------------------------------------------------------
C                                                                       
C PROGRAMMES APPELES : LIT , ECRIT                                      
C                                                                       
C***********************************************************************
C                                                                       
C    LISTE DES ENREGISTREMENTS DU FICHIER GEOMETRIQUE:                  
C                                                                       
C      1    : TITRE DE L'ETUDE                                          
C      2    : NOMBRE DE FONCTIONS LUES SUR LA GRILLE 1 ET LA GRILLE 2.  
C      3    : NOM ET UNITE DES VARIABLES                                
C      4    : 1,0,0,0,0,0,0,0,0,0                                       
C      5    : NELEM,NPOIN,NDP,1                                         
C      6    : IKLE                                                      
C      7    : IPOBO TABLEAU DE DIMENSION NPOIN, 0 POUR LES POINTS       
C             INTERIEURS, UN NUMERO SINON.                              
C      8    : X                                                         
C      9    : Y                                                         
C                                                                       
C    CE QUI SUIT N'EST PAS FAIT DANS FM3SEL.                            
C                                                                       
C     10    : TEMPS                                                     
C     11    : VARIABLES DECLAREES EN 3 (DANS L'ORDRE DES DECLARATIONS)  
C                                                                       
C***********************************************************************
C                                                                       
      IMPLICIT NONE                                                     
      INTEGER LNG,LU                                                    
      COMMON/INFO/LNG,LU                                                
C                                                                       
      DOUBLE PRECISION X(*),Y(*),XBID(2)                                
      REAL W(*)                                                         
C                                                                       
C                     IKLE(NELEM,NDP) IKLES(NDP,NELEM)                  
      INTEGER NBOR(*),IKLE(*)        ,IKLES(*)        ,IB(10),ITRAV(*)  
      INTEGER NPOIN,ISTAT,NVAR,I,IBID(1),MXPTVS,MXELVS,TIME(3),DATE(3)  
      INTEGER NFIC,NVARCL,NSOR                                    
      INTEGER IELEM,NELEM,NPTFR,NDP,IPARAM(10),IPOBO(*)                           
C                                                                       
      LOGICAL DEBU,SUIT,ECRI,LISTIN,SORLEO(*)                 
C                                                                       
      CHARACTER*1 CBID                                                  
      CHARACTER*3 STD                                                   
      CHARACTER*72 TITRE                                                
      CHARACTER*80 TITSEL                                               
      CHARACTER*32 TEXTE(*),TEXTLU(*),VARCLA(NVARCL)                    
C                        NSOR      NSOR+NVARCL                          
C-----------------------------------------------------------------------
C                                                                       
C   ON SE PLACE AU DEBUT DU FICHIER                                     
C                                                                       
      REWIND NFIC                                                       
C                                                                       
C   LEC/ECR 1   : NOM DU FICHIER GEOMETRIQUE.                           
C                                                                       
      IF(ECRI) THEN                                                     
        TITSEL = TITRE // 'SERAPHIN'                                    
        CALL ECRI2(XBID,IBID,TITSEL,80,'CH',NFIC,STD,ISTAT)
        IF(LNG.EQ.1) WRITE(LU,*) 'TITRE :',TITSEL
        IF(LNG.EQ.2) WRITE(LU,*) 'TITLE :',TITSEL             
      ELSE                                                              
        CALL LIT(XBID,W,IBID,TITRE,72,'CH',NFIC,STD,ISTAT)              
      ENDIF                                                             
C                                                                       
C   LEC/ECR 2   : NOMBRE DE FONCTIONS DE DISCRETISATION 1 ET 2          
C                                                                       
      IF(ECRI) THEN                                                     
        IB(1)=0                                                         
        IB(2)=0                                                         
        DO 91 I=1,NSOR                                                  
          IF(SORLEO(I)) IB(1) = IB(1) + 1                               
91      CONTINUE                                                        
        IB(1) = IB(1) + NVARCL                                          
        CALL ECRI2(XBID,IB,CBID,2,'I ',NFIC,STD,ISTAT)                  
      ELSE                                                              
        CALL LIT(XBID,W,IB,CBID,2,'I ',NFIC,STD,ISTAT)                  
      ENDIF                                                             
      NVAR =  IB(1)  +  IB(2)  -  NVARCL                                
C                                                                       
C   LEC/ECR 3 : NOMS ET UNITES DES VARIABLES                            
C                                                                       
      IF(NVAR.GE.1) THEN                                                
        IF(ECRI) THEN                                                   
          DO 19 I=1,NSOR                                                
            IF(SORLEO(I)) THEN                                          
             CALL ECRI2(XBID,IBID,TEXTE(I)(1:32),32,'CH',NFIC,STD,ISTAT)
            ENDIF                                                       
19        CONTINUE                                                      
          IF(NVARCL.NE.0) THEN                                          
            DO 18 I=1,NVARCL                                            
            CALL ECRI2(XBID,IBID,VARCLA(I)(1:32),32,'CH',NFIC,STD,ISTAT)
18          CONTINUE                                                    
          ENDIF                                                         
        ELSE                                                            
          DO 10 I=1,NVAR+NVARCL                                         
            CALL LIT(XBID,W,IBID,TEXTLU(I),32,'CH',NFIC,STD,ISTAT)      
10        CONTINUE                                                      
        ENDIF                                                           
      ENDIF                                                             
C                                                                       
C   LEC/ECR 4   : LISTE DE 10 PARAMETRES ENTIERS                        
C                                                                       
      IF(ECRI) THEN                                                     
        IB(1) = 1                                                       
        DO 29 I = 2,10                                                  
         IB(I) = 0                                                      
29      CONTINUE                                                        
C   Y-A-T-IL PASSAGE DE LA DATE ?                                       
        IF(DATE(1)+DATE(2)+DATE(3)+TIME(1)+TIME(2)+TIME(3).NE.0) THEN   
         IB(10) = 1                                                     
        ENDIF                                                           
C   ECRITURE DU TABLEAU DE 10 PARAMETRES
        IF(IPARAM(8).EQ.0.AND.IPARAM(9).EQ.0) THEN                                
          CALL ECRI2(XBID,IB,CBID,10,'I ',NFIC,STD,ISTAT)
        ELSE
C         ON RECRIT IPARAM QUI CONTIENT DES INFORMATIONS SUR LE PARALLELISME
          CALL ECRI2(XBID,IPARAM,CBID,10,'I ',NFIC,STD,ISTAT)
        ENDIF                         
C   PASSAGE DE LA DATE                                                  
        IF(IB(10).EQ.1) THEN                                            
          IB(1)=DATE(1)                                                 
          IB(2)=DATE(2)                                                 
          IB(3)=DATE(3)                                                 
          IB(4)=TIME(1)                                                 
          IB(5)=TIME(2)                                                 
          IB(6)=TIME(3)                                                 
          CALL ECRI2(XBID,IB,CBID,6,'I ',NFIC,STD,ISTAT)                
        ENDIF                                                           
      ELSE                                                              
        CALL LIT(XBID,W,IB,CBID,10,'I ',NFIC,STD,ISTAT)                 
        IF(IB(10).EQ.1) THEN                                            
          CALL LIT(XBID,W,IB,CBID,6,'I ',NFIC,STD,ISTAT)                
          DATE(1)=IB(1)                                                 
          DATE(2)=IB(2)                                                 
          DATE(3)=IB(3)                                                 
          TIME(1)=IB(4)                                                 
          TIME(2)=IB(5)                                                 
          TIME(3)=IB(6)                                                 
        ENDIF                                                           
      ENDIF                                                             
C                                                                       
C   LEC/ECR 5 : 4 ENTIERS                                               
C                                                                       
      IF(ECRI) THEN                                                     
        IB(1) = NELEM                                                   
        IB(2) = NPOIN                                                   
        IB(3) = NDP                                                     
        IB(4) = 1                                                       
        CALL ECRI2(XBID,IB,CBID,4,'I ',NFIC,STD,ISTAT)              
      ELSE                                                              
        CALL LIT(XBID,W,IB,CBID,4,'I ',NFIC,STD,ISTAT)                  
        NELEM = IB(1)                                                   
        NPOIN = IB(2)                                                   
        NDP   = IB(3)                                                   
      ENDIF                                                             
C                                                                       
C   LEC/ECR 6 : IKLE                                                    
C                                                                       
      IF(DEBU) THEN                                                     
C       MODIFICATION POUR LE CALCUL DE MXPTVS ET MXELVS                 
C       ON LIT MAINTENANT VRAIMENT IKLES ET ON LE RANGE DANS ITRAV      
C       A L'ADRESSE 1+NPOIN                                             
        CALL LIT(XBID,W,ITRAV(1+NPOIN),                                 
     *           CBID,NELEM*NDP,'I ',NFIC,STD,ISTAT)                    
      ELSEIF(SUIT) THEN                                                 
        CALL LIT(XBID,W,IB   ,CBID,    2    ,'I ',NFIC,STD,ISTAT)       
      ELSEIF(ECRI) THEN                                                 
C       INVERSION DE IKLE  EN IKLES POUR SELAFIN                        
        DO 27 I      = 1,NDP                                            
        DO 28 IELEM  = 1,NELEM                                          
          IKLES((IELEM-1)*NDP+I) = IKLE((I-1)*NELEM+IELEM)              
28      CONTINUE                                                        
27      CONTINUE                                                        
        CALL ECRI2(XBID   ,IKLES,CBID,NELEM*NDP,'I ',NFIC,STD,ISTAT)  
      ELSE                                                              
        CALL LIT(XBID,W,IKLES,CBID,NELEM*NDP,'I ',NFIC,STD,ISTAT)       
C       INVERSION DE IKLES EN IKLE.                                     
        DO 12 I      = 1,NDP                                            
        DO 11 IELEM  = 1,NELEM                                          
          IKLE((I-1)*NELEM+IELEM) = IKLES((IELEM-1)*NDP+I)              
11      CONTINUE                                                        
12      CONTINUE                                                        
      ENDIF                                                             
C                                                                       
C   LEC/ECR 7 : IPOBO                                                   
C                                                                       
      IF(DEBU) THEN                                                     
        CALL LIT(XBID,W,ITRAV,CBID,NPOIN,'I ',NFIC,STD,ISTAT)           
        NPTFR = 0                                                       
        IF(NPOIN.GE.1) THEN                                             
          DO 22 I = 1 , NPOIN                                           
            IF(ITRAV(I).NE.0) NPTFR = NPTFR + 1                         
22        CONTINUE                                                      
        ENDIF                                                           
C       ITRAV(1) : IPOBO  ITRAV(1+NPOIN) : IKLES                        
C       ITRAV(1+NPOIN+NDP*NELEM) : TABLEAU DE TRAVAIL.                  
        CALL MXPTEL(MXPTVS,MXELVS,ITRAV(1+NPOIN),                       
     *              ITRAV(1+NPOIN+NDP*NELEM),                           
     *              NPOIN,NELEM,NDP,ITRAV,LISTIN)                       
C       IPOBO EST MODIFIE PAR MXPTEL                                    
      ELSEIF(ECRI) THEN
        IF(IPARAM(8).EQ.0.AND.IPARAM(9).EQ.0) THEN                                                
          DO 40 I=1,NPOIN                                                 
           ITRAV(I) = 0                                                   
40        CONTINUE                                                        
          DO 41 I =1,NPTFR                                                
           ITRAV(NBOR(I)) = I                                             
41        CONTINUE                                                        
          CALL ECRI2(XBID   ,ITRAV,CBID,NPOIN,'I ',NFIC,STD,ISTAT)
        ELSE
C       PARALLELISME
          CALL ECRI2(XBID   ,IPOBO,CBID,NPOIN,'I ',NFIC,STD,ISTAT)
        ENDIF        
      ELSE                                                              
        CALL LIT(XBID,W,IB,CBID,2,'I ',NFIC,STD,ISTAT)                  
      ENDIF                                                             
C                                                                       
C   LEC/ECR  8 ET 9 : X ET Y  COORDONNEES DES POINTS DU MAILLAGE        
C                                                                       
      IF(DEBU.OR.SUIT) THEN                                             
        CALL LIT(XBID,W,IBID,CBID,2    ,'R4',NFIC,STD,ISTAT)            
        CALL LIT(XBID,W,IBID,CBID,2    ,'R4',NFIC,STD,ISTAT)            
      ELSEIF(ECRI) THEN                                                 
        CALL ECRI2(X   ,IBID,CBID,NPOIN,'R4',NFIC,STD,ISTAT)            
        CALL ECRI2(Y   ,IBID,CBID,NPOIN,'R4',NFIC,STD,ISTAT) 
        IF(LNG.EQ.1) WRITE(LU,*) 'ECRITURE DE X ET Y' 
        IF(LNG.EQ.2) WRITE(LU,*) 'WRITING X AND Y'           
      ELSE                                                              
        CALL LIT(X   ,W,IBID,CBID,NPOIN,'R4',NFIC,STD,ISTAT)            
        CALL LIT(Y   ,W,IBID,CBID,NPOIN,'R4',NFIC,STD,ISTAT)            
      ENDIF                                                             
C                                                                       
      IF(DEBU.AND.LISTIN) THEN                                          
        IF(LNG.EQ.1) WRITE(LU,300) TITRE                                
        IF(LNG.EQ.1) WRITE(LU,500) NPTFR,NELEM,NPOIN                    
        IF(LNG.EQ.2) WRITE(LU,301) TITRE                                
        IF(LNG.EQ.2) WRITE(LU,501) NPTFR,NELEM,NPOIN                    
        IF(NPOIN.LT.3.OR.NPTFR.LT.3.OR.NPTFR.GE.NPOIN) THEN             
          IF(LNG.EQ.1) WRITE(LU,23) NPOIN,NPTFR                         
          IF(LNG.EQ.2) WRITE(LU,24) NPOIN,NPTFR                         
          STOP                                                          
        ENDIF                                                           
      ENDIF                                                             
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  FORMATS D'IMPRESSION :                                               
C                                                                       
23    FORMAT(1X,'FM3SEL : NOMBRE DE POINTS DU MAILLAGE : ',1I9,/,1X,    
     *          '         NOMBRE DE POINTS DE FRONTIERE: ',1I9,/,1X,    
     *          '         DONNEES ERRONEES, ARRET DU PROGRAMME')        
24    FORMAT(1X,'FM3SEL : NUMBER OF POINTS IN THE MESH: ',1I9,/,1X,     
     *          '         NUMBER OF BOUNDARY POINTS: ',1I9,/,1X,        
     *          '         WRONG DATA, PROGRAMME STOPPED')               
300   FORMAT(1X,//,1X,'TITRE: ',A72,/)                                  
301   FORMAT(1X,//,1X,'TITLE: ',A72,/)                                  
500   FORMAT(1X,'NOMBRE DE POINTS FRONTIERE: ',1I9,/,1X,                
     *'NOMBRE D''ELEMENTS:',1I9,/,1X,'NOMBRE REEL DE POINTS:',1I9)      
501   FORMAT(1X,'NUMBER OF BOUNDARY POINTS: ',1I9,/,1X,                 
     *'NUMBER OF ELEMENTS:',1I9,/,1X,'NUMBER OF POINTS:',1I9)           
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
      RETURN                                                            
      END

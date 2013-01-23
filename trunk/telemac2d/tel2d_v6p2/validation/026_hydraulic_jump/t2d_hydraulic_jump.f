C                       ***************************
                        SUBROUTINE PRERES_TELEMAC2D
C                       ***************************
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.1    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
C                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |      LT        | -->| NUMERO D'ITERATION
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : TELMAC
C
C  SOUS-PROGRAMME APPELE : OV
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
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C     
      LOGICAL IMP,LEO
C
      INTEGER LTT,N,IMAX
C
      DOUBLE PRECISION HHH,XMAX
C
      INTRINSIC MAX,SQRT
C
C-----------------------------------------------------------------------
C
C LOGIQUES POUR DECIDER DES SORTIES
C
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.PTINIL) IMP=.TRUE.
      LTT=(LT/LEOPRD)*LEOPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.PTINIG) LEO=.TRUE.
C
C     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
C
C
C=======================================================================
C CALCUL DE LA CELERITE (MISE DANS FU, VOIR LE BLOC VARSOR)
C=======================================================================
C
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        DO 5 N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
5       CONTINUE
      ENDIF
C
C=======================================================================
C CALCUL DE LA SURFACE LIBRE (= H + ZF, MISE DANS FV)
C=======================================================================
C
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL OS( 'X=Y+Z   ' , FV , H , ZF , 0.D0 )
      ENDIF
C
C=======================================================================
C CALCUL DU NOMBRE DE FROUDE
C=======================================================================
C
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        DO 10 N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
10      CONTINUE
      ENDIF
C
C=======================================================================
C CALCUL DU DEBIT SCALAIRE
C=======================================================================
C
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        DO 30 N=1,NPOIN
         T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
30      CONTINUE
      ENDIF
C
C=======================================================================
C CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT X
C=======================================================================
C
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL OS( 'X=YZ    ' , T4 , H , U , 0.D0 )
      ENDIF
C
C=======================================================================
C CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT Y
C=======================================================================
C
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL OS( 'X=YZ    ' , T5 , H , V , HHH )
      ENDIF
C
C=======================================================================
C CALCUL DE LA VITESSE SCALAIRE
C=======================================================================
C
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , T6 , U , V , HHH )
      ENDIF
C
C=======================================================================
C CALCUL DU NOMBRE DE COURANT
C=======================================================================
C
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
C                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF (LNG.EQ.1) WRITE(LU,78) XMAX
        IF (LNG.EQ.2) WRITE(LU,79) XMAX
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
C 
C=======================================================================  
C CALCUL DE LA VITESSE ET DE LA HAUTEUR EXACTE : FORME CONSERVATIVE                          
C=======================================================================  
C                                                                         
      IF(((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))).AND.              
     *   ((LEO.AND.SORLEO(24)).OR.(IMP.AND.SORIMP(24)))) THEN 
C                   HAUTEUR          VITESSE            
        CALL EXACTE(PRIVE%ADR(1)%P%R,PRIVE%ADR(2)%P%R,ZF%R,X,NPOIN,1)                                   
      ENDIF
C                                                                         
C=======================================================================  
C CALCUL DE LA SURFACE LIBRE EXACTE                                       
C=======================================================================  
C                                                                         
      IF((LEO.AND.SORLEO(25)).OR.(IMP.AND.SORIMP(25))) THEN
C                            Z               
        CALL OV( 'X=Y+Z   ' ,PRIVE%ADR(3)%P%R,
     *                       PRIVE%ADR(1)%P%R,ZF%R,0.D0,NPOIN)                
      ENDIF
C
C=======================================================================  
C CALCUL DE LA SURFACE LIBRE "NON-CONSERVATIVE"                                        
C=======================================================================  
C                                                                         
      IF((LEO.AND.SORLEO(26)).OR.(IMP.AND.SORIMP(26))) THEN               
        DO 40 N=1,NPOIN                                                   
         HHH = MAX(PRIVE%ADR(1)%P%R(N),1.D-8)                                           
         PRIVE%ADR(4)%P%R(N)=SQRT(PRIVE%ADR(2)%P%R(N)**2/(HHH*GRAV))                        
40      CONTINUE                                                          
      ENDIF
C
C=======================================================================
C
1000  CONTINUE
      RETURN
      END
C                       *****************                                 
                        SUBROUTINE EXACTE                                 
C                       *****************                                 
C                                                                         
     *(HN,U,ZF,X,NPOIN,ICONS)                                                   
C                                                                         
C***********************************************************************  
C PROGICIEL : 'TELEMAC'       12/12/88    F. LEPEINTRE                    
C                             10/02/92    J-M HERVOUET (REMPLACEMENT      
C                             DE ZRPOLY PAR ZBRENT, IMPLICIT NONE ET      
C                             DOUBLE PRECISION)                           
C                             02/03/92    F. LEPEINTRE                    
C***********************************************************************  
C                                                                         
C      FONCTION:    SOLUTION EXACTE DE L'ECOULEMENT TRANSCRITIQUE         
C                   SUR UN BUMP. AVEC UN RESSAUT                          
C                                                                         
C                   PAR CONVENTION, ZF=0. AU POINT CRITIQUE               
C                                                                                                                                                  
C      ATTENTION : ON UTILISE ICI LE FAIT QUE LE MAILLAGE                 
C                  EST RECTANGULAIRE.                                     
C 
C
C      ICONS = 1 : HAUTEURS CONJUGUEES CLASSIQUES
C      ICONS = 2 : HAUTEURS CONJUGUEES "NON CONSERVATIVES"
C                                                                        
C-----------------------------------------------------------------------  
C                             ARGUMENTS                                   
C .________________.____.______________________________________________.  
C |      NOM       |MODE|                   ROLE                       |  
C |________________|____|______________________________________________|  
C |     HN         |<-- |  HAUTEUR D'EAU.                              |  
C |     U          |<-- |  VITESSE U.                                     
C |     ZF         | -->|  COTE DU FOND.                                  
C |     X          | -->|  ABCISSES DES POINTS DU MAILLAGE                
C |     NPOIN      | -->|  NOMBRE DE POINTS DU MAILLAGE                   
C |________________|____|______________________________________________|  
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
C**********************************************************************   
C                                                                         
      IMPLICIT NONE                                                       
      INTEGER LNG,LU                                                      
      COMMON/INFO/LNG,LU                                                  
C                                                                         
      DOUBLE PRECISION U(132,11),ZF(132,11),X(132,11),HN(132,11)          
      DOUBLE PRECISION YVF(300),YV(300),PRAD(300),YAVAL                  
      DOUBLE PRECISION QFIXG,G,ST,PRK,XRK,YRK,XRKPR,YRKPR,YCRIT           
      DOUBLE PRECISION XRKCO,YRKCO,YFLU,CSTE,RES,Y(1),Z(1)                      
C                                                                         
      INTEGER IM,JM,I,J,ICRIT,INOEUD,NOEUD,NOEUDF,ND,NG,NRK,N,IC,NPOIN
      INTEGER ICONS  
C                                                                         
      DOUBLE PRECISION A(4)                                               
      COMMON/FORFC1/A
      COMMON/HCRIT/YCRIT                                                     
C                                                                         
      EXTERNAL F                                                   
      DOUBLE PRECISION F                                           
C                                                                         
      INTRINSIC REAL                                                      
C                                                                         
C-----------------------------------------------------------------------  
C 
C     LARGEUR DU CANAL EGALE A 1.
C 
C     MAILLAGE DE CARRES DECOUPES EN TRIANGLES, L'ORDRE DES POINTS EST TEL
C     QU'ON PEUT FAIRE COMME SUR UN MAILLAGE REGULIER.                                                                       
      IM = 132                                                            
      JM = 11 
C     POINT DE PASSAGE EN CRITIQUE (DIFFERENT DU SEUIL)
C     LE FOND A ETE CALCULE POUR AVOIR LE POINT CRITIQUE A
C     LA PREMIERE MAILLE APRES LE SEUIL.
C                                                            
      ICRIT = 63 
C     COTE AVAL                                                                                                            
      YAVAL  = 0.6D0 
C     DEBIT LINEIQUE (PAR M2 DE LARGEUR)                                                     
      QFIXG  = 1.D0                                                       
      G      = 9.81D0 
C     COEFFICIENT DE STRICKLER                                                    
      ST     = 40.D0                                                   
C                                                                                                                              
      DO 10 NOEUD=1,IM-1                                                  
        ND = NOEUD + 1                                                    
        NG = NOEUD 
C       PENTE RADIER ? (AVEC UN SIGNE -)                                                      
        PRAD(NOEUD) = -(ZF(ND,5)-ZF(NG,5)) / (X(ND,5)-X(NG,5))            
10    CONTINUE                                                            
C                                                                         
C     PAR RUNGE-KUTTA (METHODE DE LA TANGENTE AMELIOREE)                  
C     NOMBRE DE SOUS PAS                                                  
      NRK = 10000                                                         
C     PAS DE RUNGE-KUTTA                                                  
      PRK = (X(IM,5)-X(1,5))/FLOAT(NRK-1)                                
C                                                                         
C     ON COMMENCE PAR CALCULER LA LIGNE D'EAU FLUVIALE                    
C     DEPUIS L'AVAL TANT QUE Y SUPERIEUR A YCRITIQUE                      
C                                                                         
      YCRIT=(QFIXG**2/G)**(1.D0/3.D0)                                     
      YVF(IM) = YAVAL                                                   
      XRK = X(IM,5)                                                     
      YRK = YAVAL                                                         
      IC  = IM-1                                                          
C                                                                         
      DO 20 N=1,NRK                                                       
C                                                                         
C        PREDICTION                                                       
         XRKPR = XRK - PRK                                                
         YRKPR = YRK - PRK*F(YRK,IC,QFIXG,PRAD,ST)                        
         IF(YRKPR.LT.YCRIT) THEN                                         
C           CHARGE INSUFFISANTE POUR PASSER LE SEUIL EN FLUVIAL           
            NOEUDF = IC+1                                                 
            GOTO 30                                                       
         ENDIF                                                            
C        CORRECTION                                                       
         XRKCO = XRKPR                                                    
         YRKCO = YRK - PRK*(F(YRK  ,IC,QFIXG,PRAD,ST) +                     
     *                      F(YRKPR,IC,QFIXG,PRAD,ST))*0.5D0                          
         IF(YRKCO.LT.YCRIT) THEN                                         
C           CHARGE INSUFFISANTE POUR PASSER LE SEUIL EN FLUVIAL           
            NOEUDF = IC+1                                                 
            GOTO 30                                                       
         ENDIF                                                            
C                                                                         
C        EST-ON SORTI DE LA MAILLE COURANTE ?                             
         IF(XRKCO.LE.X(IC,5)) THEN                                       
C           CALCUL DE Y AU NOEUD PAR INTERPOLATION LINEAIRE               
            YVF(IC) = (YRK-YRKCO)/PRK*(X(IC,5)-XRK)+ YRK                  
C           CHANGEMENT DE MAILLE COURANTE                                 
            IC = IC-1                                                     
            IF (IC.EQ.0) GOTO 40                                          
         ENDIF                                                            
C        ACTUALISATION                                                    
         XRK = XRKCO                                                      
         YRK = YRKCO                                                      
C                                                                         
20       CONTINUE                                                         
40       DO 50 NOEUD=1,IM                                               
            YV(NOEUD) = YVF(NOEUD)                                        
50       CONTINUE                                                         
         GOTO 60                                                          
C                                                                         
30       CONTINUE   
C
C
C        CALCUL DE LA LIGNE D'EAU A PARTIR DU POINT CRITIQUE
C                                                                        
C        PARTIE FLUVIALE
C                                                  
C        PAR RUNGE-KUTTA (METHODE DE LA TANGENTE AMELIOREE)               
C        NOMBRE DE SOUS PAS                                               
         NRK = 10000                                                     
C        PAS DE RUNGE-KUTTA                                               
         PRK = (X(ICRIT,5)-X(1,5))/REAL(NRK-1)                           
C                                                                        
         YV(ICRIT) = YCRIT                                             
         XRK = X(ICRIT,5)                                                
         YRK = YV(ICRIT)                                            
         IC  = ICRIT-1                                                   
C                                                                         
         DO 70  N=1,NRK                                                   
C                                                                         
C        PREDICTION                                                       
           XRKPR = XRK - PRK                                              
           YRKPR = YRK - PRK*F(YRK,IC,QFIXG,PRAD,ST)                      
C        CORRECTION                                                       
           XRKCO = XRKPR                                                  
           YRKCO = YRK - PRK*(F(YRK  ,IC,QFIXG,PRAD,ST)+                    
     *                        F(YRKPR,IC,QFIXG,PRAD,ST))/2.D0                                
C                                                                         
C        EST-ON SORTI DE LA MAILLE COURANTE ?                             
           IF (XRKCO.LE.X(IC,5)) THEN                                     
C            CALCUL DE Y AU NOEUD PAR INTERPOLATION LINEAIRE              
             YV(IC) = (YRK-YRKCO)/PRK*(X(IC,5)-XRK)+ YRK                  
C            CHANGEMENT DE MAILLE COURANTE                                
             IC = IC-1                                                    
             IF (IC.EQ.0) GOTO 80                                         
           ENDIF                                                          
C          ACTUALISATION                                                  
           XRK = XRKCO                                                    
           YRK = YRKCO                                                    
C                                                                         
70         CONTINUE 
C                                                      
C          PARTIE TORRENTIELLE
C                                            
80         CONTINUE
C                                                                                              
C          PAR RUNGE-KUTTA (METHODE DE LA TANGENTE AMELIOREE)             
C          NOMBRE DE SOUS PAS                                             
           NRK = 10000                                                   
C          PAS DE RUNGE-KUTTA                                             
           PRK = (X(IM,5)-X(ICRIT,5))/REAL(NRK-1)                      
C                                                                         
           XRK = X(ICRIT,5)
C          0.9999 POUR PARTIR SUR LA BONNE BRANCHE DE SOLUTION                                                         
           YRK = 0.9999D0*YCRIT                                         
           IC  = ICRIT + 1                                               
C                                                                         
      DO 90 N=1,NRK                                                       
C                                                                         
C        PREDICTION                                                       
         XRKPR = XRK + PRK                                                
         YRKPR = YRK + PRK*F(YRK,IC-1,QFIXG,PRAD,ST)                      
C        CORRECTION                                                       
         XRKCO = XRKPR                                                    
         YRKCO = YRK + PRK*(F(YRK,IC-1,QFIXG,PRAD,ST)+                    
     *                      F(YRKPR,IC-1,QFIXG,PRAD,ST))/2.D0                        
C                                                                         
C        EST-ON SORTI DE LA MAILLE COURANTE                               
         IF (XRKCO.GE.X(IC,5)) THEN                                       
C           CALCUL DE Y AU NOEUD PAR INTERPOLATION LINEAIRE               
            YV(IC) = (YRKCO-YRK)/PRK*(X(IC,5)-XRK)+ YRK                   
C           CHANGEMENT DE MAILLE COURANTE                                 
            IC = IC+1                                                     
         ENDIF                                                            
C                                                                         
C        ACTUALISATION 
C                                                      
         XRK = XRKCO                                                         
         YRK = YRKCO
C                                                         
90    CONTINUE                                                                                                                                                                      
C                                                                         
C     RECHERCHE D'UN RESSAUT 
C                                             
      IF (NOEUDF.EQ.IM) GOTO 120                                        
      DO 100 NOEUD=NOEUDF,IM                                            
C        HAUTEUR CONJUGUEE DU FLUVIAL H1*H2(H1+H2)=2HC**3                                    
         YFLU = YVF(NOEUD)
         IF(ICONS.EQ.1) THEN                                                                                                 
           RES = (-YFLU**2+SQRT(YFLU**4+8*YFLU*YCRIT**3))/(2*YFLU)
         ELSEIF(ICONS.EQ.2) THEN
           RES = (YCRIT**3/2.D0+SQRT(YCRIT**6/4.D0
     *                    +2.D0*YFLU**3*YCRIT**3))/(2*YFLU**2)
         ELSE
           WRITE(LU,*) 'ICONS = 1 OU 2, PAS ',ICONS
           STOP
         ENDIF                            
         IF (RES.LE.YV(NOEUD)) THEN                                       
           DO 110 INOEUD=NOEUD+1,IM                                     
           YV(INOEUD)=YVF(INOEUD)                                         
110        CONTINUE                                                       
           GOTO 60                                                        
         ENDIF                                                            
100      CONTINUE                                                         
C        PAS DE RESSAUT                                                   
120      CONTINUE                                                         
C                                                                         
60    CONTINUE                                                            
C                                                                         
C-----------------------------------------------------------------------  
C                                                                                                                                                 
      DO 130 I=1,IM                                                     
      DO 140 J=1,JM                                                     
        HN(I,J) = YV(I)                                                   
         U(I,J) = QFIXG/HN(I,J)                                           
140   CONTINUE                                                            
130   CONTINUE                                                            
C                                                                         
C-----------------------------------------------------------------------  
C                                                                         
      RETURN                                                              
      END                                                                 
C                       ***************************                       
                        DOUBLE PRECISION FUNCTION F                       
C                       ***************************                       
C                                                                         
     *(Z,MAILLE,QFIXG,PRAD,ST)                                            
C                                                                         
C***********************************************************************  
C PROGICIEL : TELEMAC        07/12/88    J-M HERVOUET (LNH) 30 71 80 18   
C***********************************************************************  
C                                                                         
C  FONCTION  :                                                            
C                                                                         
C-----------------------------------------------------------------------  
C                             ARGUMENTS                                   
C .________________.____.______________________________________________   
C |      NOM       |MODE|                   ROLE                          
C |________________|____|______________________________________________   
C |   Z            | -->|                                                 
C |   MAILLE       | -->|                                                 
C |   QFIXG        | -->|                                                 
C |   PRAD         | -->|                                                 
C |   ST           | -->|                                                 
C |________________|____|______________________________________________   
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
C***********************************************************************  
C                                                                         
      IMPLICIT NONE                                                       
      INTEGER LNG,LU                                                      
      COMMON/INFO/LNG,LU                                                    
C                                                                        
      DOUBLE PRECISION PRAD(*),G,ANUM,DENO,QFIXG,ST,Z,YCRIT,Y                     
C                                                                         
      INTEGER MAILLE                                                      
C                                                                         
      INTRINSIC ABS,SIGN                                                  
C 
      COMMON/HCRIT/YCRIT
C                                                                        
C-----------------------------------------------------------------------  
C 
C     SI ON EST A LA PROFONDEUR CRITIQUE (DENO=0), ON PREND LA VALEUR
C     DE LA PENTE SUR UN POINT PROCHE
C
      IF(ABS(Z-YCRIT).LT.1.D-5) THEN
        Y=Z+0.001D0
      ELSE
        Y=Z
      ENDIF
C                                                                        
      G = 9.81D0                                                          
      ANUM = PRAD(MAILLE) - (QFIXG**2/(ST**2*Y**(10.D0/3.D0)))            
      DENO = 1.D0 - (QFIXG**2/(G*Y**3))                                   
C     IF (ABS(DENO).LT.1.D-6) DENO = SIGN(1.D-6,DENO)                     
C                                                                         
      F = ANUM/DENO                                                       
C                                                                         
C-----------------------------------------------------------------------  
C                                                                         
      RETURN                                                              
      END                                                                 
C                       ***************************
                        SUBROUTINE NOMVAR_TELEMAC2D
C                       ***************************
C
     *(TEXTE,TEXTPR,MNEMO)
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.1    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C FONCTION  :  FIXE LES NOMS DES VARIABLES DU CODE POUR LES FICHIERS
C              DE RESULTAT ET DE GEOMETRIE (TEXTE) ET POUR LE FICHIER
C              DE RESULTATS DU CALCUL PRECEDENT (TEXTPR)
C
C              EN GENERAL TEXTE ET TEXTPR SONT EGAUX SAUF SI ON FAIT
C              UNE SUITE A PARTIR D'UN AUTRE LOGICIEL.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE |
C |________________|____|______________________________________________|
C |   TEXTE        |<-- | NOM DES VARIABLES
C |   TEXTPR       |<-- | NOM DES VARIABLES DU CALCUL PRECEDENT
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : PREDON
C
C SOUS-PROGAMME APPELE : NEANT
C
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      CHARACTER*32 TEXTE(100),TEXTPR(100)
      CHARACTER*8  MNEMO(100)
C
C-----------------------------------------------------------------------
C
C  ENGLISH
C
      IF(LNG.EQ.2) THEN
C
      TEXTE (1 ) = 'VELOCITY U      M/S             '
      TEXTE (2 ) = 'VELOCITY V      M/S             '
      TEXTE (3 ) = 'CELERITY        M/S             '
      TEXTE (4 ) = 'WATER DEPTH     M               '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'FROUDE NUMBER                   '
      TEXTE (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTE (9 ) = 'TRACER                          '
      TEXTE (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITY       M2/S            '
      TEXTE (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTE (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTE (15) = 'SCALAR VELOCITY M/S             '
      TEXTE (16) = 'WIND ALONG X    M/S             '
      TEXTE (17) = 'WIND ALONG Y    M/S             '
      TEXTE (18) = 'AIR PRESSURE    PASCAL          '
      TEXTE (19) = 'BOTTOM FRICTION                 '
      TEXTE (20) = 'DRIFT ALONG X   M               '
      TEXTE (21) = 'DRIFT ALONG Y   M               '
      TEXTE (22) = 'COURANT NUMBER                  '
      TEXTE (23) = 'EXACT DEPTH     M               '                     
      TEXTE (24) = 'EXACT VELOCITY  M/S             '                     
      TEXTE (25) = 'EXACT ELEVATION M               '       
      TEXTE (26) = 'NONCONS   ELEV  M               '
C
C TEXTPR IS USED FOR READING PREVIOUS COMPUTATION FILES.
C IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
C FROM ANOTHER CODE WITH DIFFERENT NAMES THAT YOU HAVE TO
C WRITE HERE.
C
      TEXTPR (1 ) = 'VELOCITY U      M/S             '
      TEXTPR (2 ) = 'VELOCITY V      M/S             '
      TEXTPR (3 ) = 'CELERITY        M/S             '
      TEXTPR (4 ) = 'WATER DEPTH     M               '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'FROUDE NUMBER                   '
      TEXTPR (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTPR (9 ) = 'TRACER                          '
      TEXTPR (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITY       M2/S            '
      TEXTPR (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTPR (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTPR (15) = 'SCALAR VELOCITY M/S             '
      TEXTPR (16) = 'WIND ALONG X    M/S             '
      TEXTPR (17) = 'WIND ALONG Y    M/S             '
      TEXTPR (18) = 'AIR PRESSURE    PASCAL          '
      TEXTPR (19) = 'BOTTOM FRICTION                 '
      TEXTPR (20) = 'DRIFT ALONG X   M               '
      TEXTPR (21) = 'DRIFT ALONG Y   M               '
      TEXTPR (22) = 'COURANT NUMBER                  '
      TEXTPR (23) = 'VARIABLE 23     UNIT   ??       '
      TEXTPR (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTPR (25) = 'VARIABLE 25     UNIT   ??       '
      TEXTPR (26) = 'VARIABLE 26     UNIT   ??       '
C
C-----------------------------------------------------------------------
C
C  FRANCAIS OU AUTRE
C
      ELSE
C
      TEXTE (1 ) = 'VITESSE U       M/S             '
      TEXTE (2 ) = 'VITESSE V       M/S             '
      TEXTE (3 ) = 'CELERITE        M/S             '
      TEXTE (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'FROUDE                          '
      TEXTE (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTE (9 ) = 'TRACEUR                         '
      TEXTE (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITE TURB. M2/S            '
      TEXTE (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTE (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTE (15) = 'VITESSE SCALAIREM/S             '
      TEXTE (16) = 'VENT X          M/S             '
      TEXTE (17) = 'VENT Y          M/S             '
      TEXTE (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTE (19) = 'FROTTEMENT                      '
      TEXTE (20) = 'DERIVE EN X     M               '
      TEXTE (21) = 'DERIVE EN Y     M               '
      TEXTE (22) = 'NBRE DE COURANT                 '
      TEXTE (23) = 'HAUTEUR EXACTE  M               '                     
      TEXTE (24) = 'VITESSE EXACTE  M/S             '                     
      TEXTE (25) = 'SURFACE EXACTE  M               '       
      TEXTE (26) = 'NONCONS   ELEV  M               '
C
C TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
C A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
C DE CALCUL A PARTIR D'UN AUTRE CODE.
C
      TEXTPR (1 ) = 'VITESSE U       M/S             '
      TEXTPR (2 ) = 'VITESSE V       M/S             '
      TEXTPR (3 ) = 'CELERITE        M/S             '
      TEXTPR (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'FROUDE                          '
      TEXTPR (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTPR (9 ) = 'TRACEUR                         '
      TEXTPR (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITE TURB. M2/S            '
      TEXTPR (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTPR (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTPR (15) = 'VITESSE SCALAIREM/S             '
      TEXTPR (16) = 'VENT X          M/S             '
      TEXTPR (17) = 'VENT Y          M/S             '
      TEXTPR (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTPR (19) = 'FROTTEMENT                      '
      TEXTPR (20) = 'DERIVE EN X     M               '
      TEXTPR (21) = 'DERIVE EN Y     M               '
      TEXTPR (22) = 'NBRE DE COURANT                 '
      TEXTPR (23) = 'VARIABLE 23     UNITES ??       '
      TEXTPR (24) = 'VARIABLE 24     UNITES ??       '
      TEXTPR (25) = 'VARIABLE 25     UNITES ??       '
      TEXTPR (26) = 'NONCONS   ELEV  M               '
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C   ALIAS DES NOMS DE VARIABLES POUR LE FICHIER DES PARAMETRES
C
C     UVCHSBFQTKEDIJMXYPWAGLNORZ
C     VITESSE U
      MNEMO(1)   = 'U       '
C     VITESSE V
      MNEMO(2)   = 'V       '
C     CELERITE
      MNEMO(3)   = 'C       '
C     HAUTEUR D'EAU
      MNEMO(4)   = 'H       '
C     SURFACE LIBRE
      MNEMO(5)   = 'S       '
C     FOND
      MNEMO(6)   = 'B       '
C     FROUDE
      MNEMO(7)   = 'F       '
C     DEBIT SCALAIRE
      MNEMO(8)   = 'Q       '
C     TRACEUR
      MNEMO(9)   = 'T       '
C     ENERGIE TURBUL.
      MNEMO(10)   = 'K       '
C     DISSIPATION
      MNEMO(11)   = 'E       '
C     VISCOSITE TURB.
      MNEMO(12)   = 'D       '
C     DEBIT SUIVANT X
      MNEMO(13)   = 'I       '
C     DEBIT SUIVANT Y
      MNEMO(14)   = 'J       '
C     VITESSE SCALAIRE
      MNEMO(15)   = 'M       '
C     VENT X
      MNEMO(16)   = 'X       '
C     VENT Y
      MNEMO(17)   = 'Y       '
C     PRESSION ATMOS.
      MNEMO(18)   = 'P       '
C     FROTTEMENT
      MNEMO(19)   = 'W       '
C     DERIVE EN X
      MNEMO(20)   = 'A       '
C     DERIVE EN Y
      MNEMO(21)   = 'G       '
C     NBRE DE COURANT
      MNEMO(22)   = 'L       '
C     VARIABLE 23
      MNEMO(23)   = 'N       '
C     VARIABLE 24
      MNEMO(24)   = 'O       '
C     VARIABLE 25
      MNEMO(25)   = 'R       '
C     VARIABLE 26
      MNEMO(26)   = 'Z       '
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.1          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORFON
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
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
      INTEGER I
      DOUBLE PRECISION N
C
C-----------------------------------------------------------------------
C
C  LISSAGES EVENTUELS DU FOND
C
      IF(LISFON.GT.0) THEN
C
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      DO I=1,NPOIN
        ZF%R(I) = MAX(-0.2D0,-0.0246875D0*(X(I)-10.D0)**2)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END













c                       ***************
                        SUBROUTINE BORH
C                       ***************
C
C***********************************************************************
C
C  ARTEMIS    VERSION 5.1 22/08/00   D. AELBRECHT (LNH) 01 30 87 74 12 
C
C  LINKED TO BIEF VERS. 5.0          J-M HERVOUET (LNH) 01 30 87 80 18
C
C***********************************************************************
C
C      FUNCTION :  SPECIFIES THE CONDITIONS AT EACH NODE OF THE BOUNDARY
C
C      IN VERSION 5.1, COUPLING WITH COWADIS IS POSSIBLE. 
C      INFORMATION FROM A COWADIS RESULT FILE CAN BE INPUT TO DEFINE
C      INCIDENT WAVE BOUNDARY CONDITIONS WITH NON UNIFORM DIRECTION
C
C      THIS SUBROUTINE CAN BE EXTENDED BY THE USER
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   RP           |<-- |  COEFFICIENTS DE REFLEXION DES PAROIS        |
C |   TETAP        |<-- |  ANGLE D'ATTAQUE DE LA HOULE SUR LES LIMITES |
C |                |    |  PAS SEULEMENT LES PAROIS, MAIS AUSSI LES    |
C |                |    |  LES FRONTIERES LIQUIDES                     |
C |                |    |  (COMPTE PAR RAPPORT A LA NORMALE EXTERIEURE |
C |                |    |   DANS LE SENS DIRECT)                       |
C |   ALFAP        |<-- |  DEPHASAGE INDUIT PAR LA PAROI ENTRE L'ONDE  |
C |                |    |  REFLECHIE ET L'ONDE INCIDENTE (SI ALFAP EST |
C |                |    |  POSITIF, L'ONDE REFLECHIE EST EN RETARD)    |
C |   HB           |<-- |  HAUTEUR DE LA HOULE AUX FRONTIERES OUVERTES |
C |   TETAB        |<-- |  ANGLE D'ATTAQUE DE LA HOULE (FRONT. OUV.)   |
C |                |    |  (COMPTE PAR RAPPORT A L'AXE DES X DANS LE   |
C |                |    |   SENS DIRECT)                               |
C |    H           | -->|  HAUTEUR D'EAU                               |
C |    K           | -->|  NOMBRE D'ONDE                               |
C |    C,CG        | -->|  VITESSES DE PHASE ET DE GROUPE              |
C |    C           | -->|  CELERITE AU TEMPS N                         |
C |    ZF          | -->|  FOND                                        |
C |    X,Y         | -->|  COORDONNEES DES POINTS DU MAILLAGE          |
C |  TRA01,...,3   |<-->|  TABLEAUX DE TRAVAIL                         |
C | XSGBOR,YSGBOR  | -->|  NORMALES EXTERIEURES AUX SEGMENTS DE BORD   |
C |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
C |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
C |   KP1BOR       | -->|  NUMERO DU POINT FRONTIERE SUIVANT           |
C |   OMEGA        | -->|  PULSATION DE LA HOULE                       |
C |   PER          | -->|  PERIODE DE LA HOULE                         |
C |   TETAH        | -->|  ANGLE DE PROPAGATION DE LA HOULE            |
C |   GRAV         | -->|  GRAVITE                                     |
C |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
C |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
C |   KENT,KLOG    | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
C |   KSORT,KINC   |    |  LIMITES                                     |
C |                |    |  KENT  : ENTREE (VALEUR IMPOSEE)             |
C |                |    |  KLOG  : PAROI                               |
C |                |    |  KSORT : SORTIE                              |
C |                |    |  KINC  : ONDE INCIDENTE                      |
C |   PRIVE        | -->|  TABLEAU DE TRAVAIL (DIMENSION DANS PRINCI)  |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : ARTEMIS
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I
C
CCP
      INTEGER IG0  , IG            
      DOUBLE PRECISION PHASOI,AUXIC,AUXIS,DEGRAD,X0,Y0,KK
CCP

      DOUBLE PRECISION PI
      PARAMETER( PI = 3.1415926535897932384626433D0)
C
      INTRINSIC COS,SIN
C
C ---------------------------------------------------------------------
C
C DEBUT : DECLARATIONS SUPPLEMENTAIRES POUR LECTURE FICHIER COWADIS
C
C ATTENTION, POUR L'INSTANT BEAUCOUP DE CHOSES EN DUR !!
C
      LOGICAL COUPLA
C
      INTEGER NCOW,NPTH,ISTAT,IB(4),IPARAM(100),IDATE(6)
      INTEGER NVAR,NP,ID(2),JB
C
      DOUBLE PRECISION Z(1),ATT(1),RADDEG
C
C     IL FAUT DIMENSIONNER LES TABLEAUX EN DUR
C
      DOUBLE PRECISION XCOWA(15000) ,YCOWA(15000)
      DOUBLE PRECISION HSCOWA(15000),DMCOWA(15000)
      DOUBLE PRECISION HSARTE(12000),DMARTE(12000)
C
      REAL TAB1(15000)
C
      CHARACTER*80 TTITRE    , BID
      CHARACTER*32 TTEXTE(40)
      CHARACTER*3  BINCOW
C
C
C FIN : DECLARATIONS SUPPLEMENTAIRES POUR LECTURE FICHIER COWADIS
C
C-----------------------------------------------------------------------
C
C DEBUT : LECTURE FICHIER COWADIS
C
C     RECUPERATION DES RESULTATS ISSUS DE COWADIS 
C     POUR DEFINIR LES CONDITIONS AUX LIMITES ARTEMIS
C
C     ATTENTION ENCORE BEAUCOUP DE CHOSES EN DUR
C
C     **********************************************************
C
C     SPECIFIER LE NUMERO DU PAS DE TEMPS DE CALCUL DANS COWADIS
C     (NPTH POURRA DEVENIR 1 MOT-CLE DANS UNE PROCHAINE VERSION)
C
      NPTH = 2
C
C     **********************************************************
C
      COUPLA = .TRUE.
C
C   (COUPLA POURRA DEVENIR 1 MOT-CLE DANS UNE PROCHAINE VERSION)
C
      NCOW   = 24
      BINCOW = 'STD'
      RADDEG = 180.D0/PI
C
      IF (COUPLA) THEN
C
C        LECTURE DU TITRE DU FICHIER COWADIS
C
         CALL LIT(Z,TAB1,IB,TTITRE,72,'CH',NCOW,BINCOW,ISTAT)
C
C        LECTURE DU NOMBRE DE VARIABLES ET DE LEURS NOMS
C
         CALL LIT(Z,TAB1,IB,BID,2,'I ',NCOW,BINCOW,ISTAT)
         NVAR=IB(1)
C
         DO 85 I=1,NVAR
C
            CALL LIT(Z,TAB1,IB,TTEXTE(I),32,'CH',NCOW,BINCOW,ISTAT)
C
            IF (TTEXTE(I).EQ.'HAUTEUR_HM0     M               ')
     *          ID(1)=I
            IF (TTEXTE(I).EQ.'TETA_MOYEN      DEG             ')
     *          ID(2)=I
C
 85      CONTINUE
C
C        VARIABLES IPARAM ET IDATE
C
         CALL LIT(Z,TAB1,IPARAM,BID,10,'I ',NCOW,BINCOW,ISTAT)
         IF (IPARAM(10).EQ.1) THEN
            CALL LIT(Z,TAB1,IDATE,BID,6,'I ',NCOW,BINCOW,ISTAT)
         ENDIF
C
C        NELEM, NPOIN
C
         CALL LIT(Z,TAB1,IB,BID,2,'I ',NCOW,BINCOW,ISTAT)
         NP=IB(2)
C
         WRITE(LU,*) '------------------------------------------'
         IF (LNG.EQ.1) WRITE(LU,230) 
         IF (LNG.EQ.2) WRITE(LU,231)
 230     FORMAT(/,1X,'BORH : LECTURE DU FICHIER COWADIS')
 231     FORMAT(/,1X,'BORH : READING COWADIS FILE')
         IF (LNG.EQ.1) WRITE(LU,240) NP 
         IF (LNG.EQ.2) WRITE(LU,241) NP
 240     FORMAT(/,1X,'NOMBRE DE POINTS DU MAILLAGE COWADIS :',1X,I7)
 241     FORMAT(/,1X,'NUMBER OF NODES OF COWADIS MESH :',1X,I7)
         IF (LNG.EQ.1) WRITE(LU,250) 
         IF (LNG.EQ.2) WRITE(LU,251)
 250     FORMAT(/,1X,'MAILLAGES ARTEMIS ET COWADIS DIFFERENTS :',1X,
     *          'ON INTERPOLE')
 251     FORMAT(/,1X,'COWADIS AND ARTEMIS MESHES ARE DIFFERENT :',1X,
     *          'INTERPOLATION')
C
C        IKLE ET IPOBO...
C
         READ(NCOW)
         READ(NCOW)
C
C        XCOWA ET YCOWA
C
         CALL LIT(XCOWA,TAB1,IB,BID,NP,'R4',NCOW,BINCOW,ISTAT)
         CALL LIT(YCOWA,TAB1,IB,BID,NP,'R4',NCOW,BINCOW,ISTAT)
C
C        PAS DE TEMPS ET VARIABLES
C
         IF(NPTH.GT.1) THEN
           DO 111 I=1,(NPTH-1)*(NVAR+1)
             READ(NCOW)
 111       CONTINUE
         ENDIF
C
         CALL LIT(ATT,TAB1,IB,BID,1,'R4',NCOW,BINCOW,ISTAT)
C
C        VARIABLES D'INDICE NPTH
C
         DO 95 I=1,NVAR
            IF (I.EQ.ID(1)) THEN
               CALL LIT(HSCOWA,TAB1,IB,BID,NP,'R4',NCOW,BINCOW,ISTAT)
            ELSEIF (I.EQ.ID(2)) THEN
               CALL LIT(DMCOWA,TAB1,IB,BID,NP,'R4',NCOW,BINCOW,ISTAT)
            ELSE
               READ(NCOW)
            ENDIF
 95      CONTINUE
C
C        IMPRESSIONS SUR LE LISTING
C
         IF (LNG.EQ.1) WRITE(LU,260) ATT
         IF (LNG.EQ.2) WRITE(LU,261) ATT
 260     FORMAT(/,1X,'TEMPS DU CALCUL COWADIS RETENU :',1X,F10.2,' s')
 261     FORMAT(/,1X,'TIME READ IN COWADIS FILE :',1X,F10.2,' s')
         IF (LNG.EQ.1) WRITE(LU,270) 
         IF (LNG.EQ.2) WRITE(LU,271)
 270     FORMAT(/,1X,'VARIABLES DE COWADIS RETENUES :')
 271     FORMAT(/,1X,'VARIABLES READ IN COWADIS FILE :')
         WRITE(LU,280) TTEXTE(ID(1)), TTEXTE(ID(2)) 
 280     FORMAT(/,5X,'=> ',A32,/,5X,'=> ',A32)
C
C        MODIFICATION DE LA VARIABLE DMARTE POUR ARTEMIS :
C        CHANGEMENT DE REPERE ET D'UNITE
C
         DO 99 I=1,NP
            DMCOWA(I) = 90.D0 - DMCOWA(I)
 99      CONTINUE
C
         IF (LNG.EQ.1) WRITE(LU,290) 
         IF (LNG.EQ.2) WRITE(LU,291)
 290     FORMAT(/,1X,'BORH : FIN DE LECTURE DU FICHIER COWADIS')
 291     FORMAT(/,1X,'BORH : END OF READING COWADIS FILE')
         WRITE(LU,*) ' '
         WRITE(LU,*) '------------------------------------------'
         
         REWIND(NCOW)
C
C        INTERPOLATION
c         IF (NCSIZE .LE. 1) THEN

c            CALL FASPDA (X,Y,HSARTE,NPOIN,NPTFR,MESH%NBOR%I,
c     *           XCOWA,YCOWA,HSCOWA,NP)
c            CALL FASPDA (X,Y,DMARTE,NPOIN,NPTFR,MESH%NBOR%I,
c     *                XCOWA,YCOWA,DMCOWA,NP)
c         ELSE
            
            CALL FASPDA (X,Y,HSARTE,NPOIN,NPTFR,MESH%NBOR%I,
     *           XCOWA,YCOWA,HSCOWA,NP)
            CALL FASPDA (X,Y,DMARTE,NPOIN,NPTFR,MESH%NBOR%I,
     *           XCOWA,YCOWA,DMCOWA,NP)
c         END IF

      ENDIF
C
C
C FIN : FIN LECTURE FICHIER COWADIS
C
C--------------------------------------------------------------------
C
C CONDITIONS AUX LIMITES
C UN SEGMENT EST SOLIDE SI IL EST DE TYPE KLOG.
C UN SEGMENT EST ONDE INCIDENTE SI IL EST DE TYPE KINC.
C UN SEGMENT EST UNE ENTREE SI IL EST DE TYPE KENT.
C UN SEGMENT EST UNE SORTIE SI IL EST DE TYPE KSORT.
C
C TOUS LES ANGLES SONT EN DEGRES
C                         ------
C ---------------------------------------
C INITIALISATION DES VARIABLES PAR DEFAUT
C ---------------------------------------
      TETAB%R(:) = TETAH
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 1.D0
C
C ------------ 
C PAROI SOLIDE
C ------------ 
C

      DO I=1,NPTFR
       JB=BOUNDARY_COLOUR%I(I)
C ------------ 
C PAROI SOLIDE
C ------------ 
C
      IF(JB.GE.1.AND.JB.LE.84)THEN
         LIHBOR%I(I) = KLOG
         RP%R(I) = 0.5D0
         TETAP%R(I) = 0.D0
         ALFAP%R(I) = 0.D0
      ENDIF 
           
      IF(JB.GE.85.AND.JB.LE.142)THEN
         LIHBOR%I(I) = KLOG
         RP%R(I) = 1.D0
         TETAP%R(I) = 0.D0
         ALFAP%R(I) = 0.D0
      ENDIF 
     
      IF(JB.GE.143.AND.JB.LE.260)THEN
         LIHBOR%I(I) = KLOG
         RP%R(I) = 0.5D0
         TETAP%R(I) = 0.D0
         ALFAP%R(I) = 0.D0
      ENDIF 
C
C ------------ 
C FRONTIERE ONDE INCIDENTE
C ------------ 
C
      IF(JB.GE.261.AND.JB.LE.281)THEN
         LIHBOR%I(I) = KINC
         HB%R(I)     = HSARTE(MESH%NBOR%I(I))
         TETAB%R(I)  = DMARTE(MESH%NBOR%I(I))
         TETAP%R(I)  = 0.D0
         ALFAP%R(I)  = 0.D0
      ENDIF 
C
C ------------ 
C PAROI SOLIDE
C ------------ 
C
      IF(JB.GE.282.AND.JB.LE.302)THEN
         LIHBOR%I(I) = KLOG
         RP%R(I) = 0.5D0
         TETAP%R(I) = 0.D0
         ALFAP%R(I) = 0.D0
      ENDIF 
      
      ENDDO
C
C-----------------------------------------------------------------------
C                                                                       
C -----------------------------
C
      RETURN                                                            
      END                                                               
C                       *****************                                 
                        SUBROUTINE FASPDA                              
C                       *****************                                 
C                                                                       
     *(X,Y,VARINT,NPOIN,NPTFR,NBOR,XRELV,YRELV,VRELV,NP)                 
C                                                                       
C***********************************************************************
C
C BIEF VERSION 4.0              17/08/94  J-C GALLAND   01 30 87 78 13     
C                                         J-M HERVOUET  01 30 87 80 18     
C MODIFIE POUR ARTEMIS 5.0      22/08/00  D AELBRECHT   01 30 87 74 12
C
C***********************************************************************
C                                                                       
C   FONCTION : INTERPOLATION D'UNE VARIABLE SUR LES POINTS DU MAILLAGE A     
C              PARTIR DE POINTS RELEVES D'UN AUTRE MAILLAGE    
C                                                                       
C-----------------------------------------------------------------------
C                             ARGUMENTS                                 
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    X,Y         | -->|  COORDONNEES DU MAILLAGE                      
C |    VARINT      | <--|  VARIABLE INTERPOLEE EN X,Y                        
C |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE.                
C |    XRELV       | -->|  ABCISSES DES POINTS RELEVES                  
C |    YRELV       | -->|  ORDONNEES DES POINTS RELEVES                 
C |    VRELV       | -->|  VALEURS DE LA VARIABLE AUX POINTS RELEVES 
C |    NP          | -->|  NOMBRE DE POINTS RELEVES                     
C |    NBOR        | -->|  NUMEROTATION GLOBALE DES POINTS DE BORD      
C |    NPTFR       | -->|  NOMBRE DE POINTS DE BORD.                    
C |    DM          | -->|  DISTANCE MINIMUM A LA COTE TOLEREE POUR      
C |                |    |  ACCEPTER UN POINT RELEVE.                    
C |________________|____|______________________________________________ 
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
C---------------------------------------------------------------------- 
C                                                                       
C SOUS-PROGRAMME APPELE: CROSFR                                         
C                                                                       
C***********************************************************************
C                                                                       
      IMPLICIT NONE                                                     
C                                                                       
      INTEGER NP,N,NPOIN,NPTFR,INUM,I,IFR                                   
      INTEGER, INTENT(IN) ::  NBOR(NPTFR)
C                                                                       
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),XRELV(NP),YRELV(NP),VRELV(NP)  
      DOUBLE PRECISION DIST1,DIST2,DIST3,DIST4                          
      DOUBLE PRECISION ZCADR1,ZCADR2,ZCADR3,ZCADR4                      
      DOUBLE PRECISION DIFX,DIFY,DIST,X1,Y1,X2,Y2,X3,Y3,X4,Y4           
      DOUBLE PRECISION ZNUM,ZDEN,VARINT(NPOIN)
C                                                                       
      LOGICAL OK1,OK2,OK3,OK4                                           
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  BOUCLE SUR LES POINTS DU MAILLAGE :                                  
C                                                                       
      DO 100 IFR = 1 , NPTFR                                              
C
      I = NBOR(IFR)
C                                                                       
C     FOND INTERPOLE A PARTIR DE 4 QUADRANTS                            
C                                                                       
C ---->  INITIALISATIONS:                                               
C                                                                       
      DIST1=1.D12                                                       
      DIST2=1.D12                                                       
      DIST3=1.D12                                                       
      DIST4=1.D12                                                       
C                                                                       
      OK1 = .FALSE.                                                     
      OK2 = .FALSE.                                                     
      OK3 = .FALSE.                                                     
      OK4 = .FALSE.                                                     
C                                                                       
      ZCADR1=0.D0                                                       
      ZCADR2=0.D0                                                       
      ZCADR3=0.D0                                                       
      ZCADR4=0.D0                                                       
C                                                                       
C --------->  BOUCLE SUR LES POINTS RELEVES (IL Y EN A NP):             
      DO 31 N=1,NP                                                      
           DIFX = XRELV(N)-X(I)                                         
           DIFY = YRELV(N)-Y(I)                                         
           DIST = DIFX*DIFX + DIFY*DIFY                                 
C                                                                       
             IF ( DIST.LT.1.D-6 ) DIST=1.D-6                            
C ->QUADRANT 1 :                                                        
               IF( DIFX.LE.0.D0.AND.DIFY.LE.0.D0) THEN                  
                 IF(DIST.LE.DIST1)THEN                                  
                      X1=XRELV(N)                                       
                      Y1=YRELV(N)                                       
                      DIST1=DIST                                        
                      ZCADR1=VRELV(N)                                   
                      OK1 = .TRUE.                                      
                 ENDIF                                                  
C ->QUADRANT 2 :                                                        
              ELSE IF( DIFX.GE.0.D0.AND.DIFY.LE.0.D0) THEN              
                 IF(DIST.LE.DIST2)THEN                                  
                      X2=XRELV(N)                                       
                      Y2=YRELV(N)                                       
                      DIST2=DIST                                        
                      ZCADR2=VRELV(N)                                   
                      OK2 = .TRUE.                                      
                 ENDIF                                                  
C ->QUADRANT 3 :                                                        
              ELSE IF( DIFX.GE.0.D0.AND.DIFY.GE.0.D0) THEN              
                 IF(DIST.LE.DIST3)THEN                                  
                      X3=XRELV(N)                                       
                      Y3=YRELV(N)                                       
                      DIST3=DIST                                        
                      ZCADR3=VRELV(N)                                   
                      OK3 = .TRUE.                                      
                 ENDIF                                                  
C ->QUADRANT 4 :                                                        
              ELSE IF( DIFX.LE.0.D0.AND.DIFY.GE.0.D0) THEN              
                 IF(DIST.LE.DIST4)THEN                                  
                      X4=XRELV(N)                                       
                      Y4=YRELV(N)                                       
                      DIST4=DIST                                        
                      ZCADR4=VRELV(N)                                   
                      OK4 = .TRUE.                                      
                 ENDIF                                                  
              ENDIF                                                     
 31        CONTINUE                                                     
C                                                                       
C --------->  FIN DE LA BOUCLE SUR LES POINTS RELEVES.                  
C                                                                       
         ZNUM = 0.D0                                                    
         ZDEN = 0.D0                                                    
         INUM = 0                                                       
         IF(OK1) THEN                                                   
          ZNUM = ZNUM + ZCADR1/DIST1                                    
          ZDEN = ZDEN + 1.D0/DIST1                                      
          INUM = INUM + 1                                               
         ENDIF                                                          
         IF(OK2) THEN                                                   
          ZNUM = ZNUM + ZCADR2/DIST2                                    
          ZDEN = ZDEN + 1.D0/DIST2                                      
          INUM = INUM + 1                                               
         ENDIF                                                          
         IF(OK3) THEN                                                   
          ZNUM = ZNUM + ZCADR3/DIST3                                    
          ZDEN = ZDEN + 1.D0/DIST3                                      
          INUM = INUM + 1                                               
         ENDIF                                                          
         IF(OK4) THEN                                                   
          ZNUM = ZNUM + ZCADR4/DIST4                                    
          ZDEN = ZDEN + 1.D0/DIST4                                      
          INUM = INUM + 1                                               
         ENDIF                                                          
C                                                                       
         IF(INUM.NE.0) THEN                                             
C           VARINT : VARIABLE AU POINT                                      
            VARINT(I)=ZNUM/ZDEN                                               
         ELSE
            WRITE(*,*) 'INUM = ', INUM
            WRITE(*,*) 'PAS DE POINT TROUVE POUR INTERPOLER '
            WRITE(*,*) 'IGLB = ', I
            VARINT(I) = 0.D0                                               
         ENDIF                                                          
C                                                                       
100   CONTINUE                                                          
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
C PROGICIEL : TELEMAC 5.0          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORFON
C
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C
C-----------------------------------------------------------------------
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
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  LISSAGES EVENTUELS DU FOND
C
      IF(LISFON.GT.0) THEN
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
      ENDIF
C
C-----------------------------------------------------------------------
      DO I = 1,NPOIN
         ZF%R(I) = MIN(ZF%R(I),12.D0)
      ENDDO
C
      RETURN
      END                  

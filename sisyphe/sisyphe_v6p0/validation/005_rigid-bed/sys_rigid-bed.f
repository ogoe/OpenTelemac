C                       *************************
                        SUBROUTINE CONDIM_SISYPHE
C                       *************************
C
     * (U      , V   , QU    , QV  , H   , ZF , Z ,
     *  ESOMT  ,THETAW , Q   , HW  , TW  , 
     *  X      , Y   , NPOIN , AT  , PMAREE)
C
C***********************************************************************
C SISYPHE VERSION 5.3                             E. PELTIER    11/09/95
C                                                 C. LENORMANT
C                                                 J.-M. HERVOUET
C                                                
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT      
C***********************************************************************
C
C     FONCTION  : VALEURS IMPOSEES
C                         - DU DEBIT VECTORIEL    QU, QV
C                         - DE LA HAUTEUR D'EAU   H
C                         - DE LA COTE DU FOND    ZF
C                         - DE LA SURFACE LIBRE   Z
C                         - DE L'EVOLUTION TOTALE ESOMT
C                         - DU DEBIT              Q
C                         - DE LA HAUTEUR DE HOULE HW
C                         - DE LA PERIODE DE HOULE TW
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   U , V        |<-- | COORDONNEES DES VECTEURS VITESSE
C |   QU , QV      |<-- | DEBIT VECTORIEL SUIVANT X ET SUIVANT Y
C |   H            |<-->| HAUTEUR D'EAU
C |   ZF           |<-->| COTE DU FOND
C |   Z            |<-->| COTE DE SURFACE LIBRE
C |   ESOMT        |<-->| EVOLUTION TOTALE DES FONDS
C |   C            |<-->| CELERITE
C |   Q            |<-->| DEBIT
C |   HW           | -->| HAUTEUR DE HOULE
C |   TW           | -->| PERIODE DE HOULE
C |   X,Y          | -->| COORDONNEES DU MAILLAGE
C |   NPOIN        | -->| NOMBRE DE POINTS DU MAILLAGE
C |   AT           | -->| TEMPS
C |   PMAREE       | -->| PERIODE DE LA MAREE
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C PROGRAMME APPELANT : SISYPH
C PROGRAMMES APPELES : 
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN , I
C
      DOUBLE PRECISION   U(NPOIN) , V(NPOIN)     , H(NPOIN)
      DOUBLE PRECISION   QU(NPOIN), QV(NPOIN)    , Q(NPOIN)
      DOUBLE PRECISION   ESOMT(NPOIN)
      DOUBLE PRECISION   HW(NPOIN), TW(NPOIN) , THETAW(NPOIN)
      DOUBLE PRECISION   Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION   X(NPOIN) , Y(NPOIN)
      DOUBLE PRECISION   AT       , PMAREE 
C
C-----------------------------------------------------------------------
C
C  --------------------------------------------------------------
C  INITIALISATION DES TABLEAUX NON LUS DANS LE FICHIER RESULTATS:
C  --------------------------------------------------------------
C
      DO 1 I=1,NPOIN                                                            
         Z(I)=10.D0                                                
         ZF(I)=0.D0                                                             
         H(I)=Z(I)-ZF(I)
C
C  MODIF JMH : U VARIABLE : CROIT LINEAIREMENT JUSQU'A 10 POUR X=8
C                           REDESCEND ENSUITE
C
         IF(X(I).LT.8.D0) THEN                                                        
C          QU(I)=10.D0*X(I)/8.D0
           U(I)=X(I)/8.D0
         ELSE
C          QU(I)=10.D0*(1.D0-(X(I)-8.D0)/8.D0)
           U(I)=1.D0-(X(I)-8.D0)/8.D0
         ENDIF                                                             
C        QV(I)=0.D0 
         V(I)=0.D0                                                            
C        Q(I)=SQRT(QU(I)**2+QV(I)**2)
C                                                     
1     CONTINUE  
C-----------------------------------------------------------------------
C
      RETURN
      END   
C                       *****************
                        SUBROUTINE NOEROD
C                       *****************
C
     * (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
C
C***********************************************************************
C TSEF VERSION 3.2                                          C. LENORMANT
C
C***********************************************************************
C
C     FONCTION  : IMPOSE LA VALEUR DE LA COTE DU FOND NON ERODABLE  ZR
C
C
C     LES METHODES DE TRAITEMENT DES FONDS NON ERODABLES PEUVENT CONDUIRE
C     A ZF < ZR A CERTAINS PAS DE TEMPS, POUR PALLIER A CELA ON PEUT CHOISIR 
C     CHOISIR DE LISSER LA SOLUTION OBTENUE I.E NLISS > 0.  
C
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   H            | -->| HAUTEUR D'EAU
C |   ZF           | -->| COTE DU FOND
C |   ZR           |<-- | COTE DU FOND NON ERODABLE
C |   Z            | -->| COTE DE SURFACE LIBRE
C |   X,Y          | -->| COORDONNEES DU MAILLAGE
C |   NPOIN        | -->| NOMBRE DE POINTS DU MAILLAGE
C |   CHOIX        | -->| METHODE CHOISIE POUR LE TRAITEMENT DES FONDS
C |                | -->| NON ERODABLES
C |   NLISS        |<-->| NOMBRE DE LISSAGES
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN , CHOIX , NLISS , I
C
      DOUBLE PRECISION   Z(NPOIN) , ZF(NPOIN)    , ZR(NPOIN)
      DOUBLE PRECISION   X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION   PI,XMAX,ZEMAX
C
C-----------------------------------------------------------------------
C
C-----------------------------------
C TRAITEMENT DES FONDS NON ERODABLES
C------------------------------------
C
C       PAR DEFAUT, ZR=ZF-100 !                                                              
C    ZEMAX: EPAISSEUR MAX DU LIT
        ZEMAX=0.10D0
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-ZEMAX,NPOIN) 
C    XMAX: LONGUEUR DU PLATIER
        XMAX=6.D0         
        DO 100 I=1,NPOIN
           IF(X(I).LE.XMAX.OR.X(I).GE.10.5D0) THEN
               ZR(I)=ZF(I)
           ENDIF
 100    CONTINUE                                                                                 
C
C       NLISS CORRESPOND AU NOMBRE DE LISSAGES EFFECTUEES SUR
C       LES VALEURS NEGATIVES DE LA VARIABLE (ZF-ZR)
C       PAR DEFAUT NLISS = 0 
C
        NLISS = 0       
C
C-----------------------------------------------------------------------
C
      RETURN
      END

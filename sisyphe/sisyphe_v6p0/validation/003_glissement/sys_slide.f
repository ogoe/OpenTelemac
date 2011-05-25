C                       *************************
                        SUBROUTINE CONDIM_SISYPHE
C                       *************************
C
     * (U      , V   , QU    , QV  , H   , ZF , Z ,
     *  ESOMT  , THETAW   , Q     , HW  , TW  ,
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
      DOUBLE PRECISION   HW(NPOIN), TW(NPOIN), THETAW(NPOIN)
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
      DO I=1,NPOIN                                                            
         QU(I)=0.D0                                                             
         Q(I) =0.D0                                                                                                                      
         QV(I)=0.D0                                                             
         Z(I) =17.D0                                                             
         ZF(I)=16.D0-X(I)
         U(I) =0.D0
         V(I) =0.D0
         H(I) =Z(I)-ZF(I)                                                             
      ENDDO                                                                  
C                                                          
C-----------------------------------------------------------------------
C
      RETURN
      END

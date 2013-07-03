C                       ***************
                        SUBROUTINE BORH
C                       ***************
C
C***********************************************************************
C
C  ARTEMIS    VERSION 3.2 02/06/99   D. AELBRECHT (LNH) 01 30 87 74 12 
C
C  LINKED TO BIEF VERS. 5.0          J-M HERVOUET (LNH) 01 30 87 80 18
C
C***********************************************************************
C
C      FONCTION:    PREND EN COMPTE LES CONDITIONS AUX LIMITES
C                   DE L'UTILISATEUR
C                   ELLES SONT DONNEES PAR SEGMENT.
C
C      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
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
C APPELE PAR : ARTEMI
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
      INTEGER I,JB
C
      DOUBLE PRECISION PI,BID
      PARAMETER( PI = 3.1415926535897932384626433D0)
C
      INTRINSIC COS,SIN
C
C-----------------------------------------------------------------------
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

      DO I=1,NPTFR
       JB=BOUNDARY_COLOUR%I(I)
C -----------------------------
C EXEMPLE DE CONDITIONS LIMITES  :
C
C                                                                       
C PAROIS SOLIDES                                                        
      IF(JB.GE.2.AND.JB.LE.139)THEN
         LIHBOR%I(I) = KLOG
         RP%R(I) = 1.D0
         TETAP%R(I) = 90.D0
         ALFAP%R(I) = 0.D0
      ENDIF      
      IF(JB.GE.146.AND.JB.LE.283)THEN
         LIHBOR%I(I) = KLOG
         RP%R(I) = 1.D0
         TETAP%R(I) = 90.D0
         ALFAP%R(I) = 0.D0
      ENDIF      
C
C   
C PAROIS AUX FRONTIERES OUVERTES                                        
C
      IF(JB.GE.140.AND.JB.LE.145)THEN
         LIHBOR%I(I) = KSORT
         TETAP%R(I) = 0.D0
      ENDIF      
C
C
C FRONTIERE ONDE INCIDENTE
C
      IF(JB.GE.284.AND.JB.LE.288)THEN
       LIHBOR%I(I) = KINC
       HB%R(I) = 0.202D0
       TETAB%R(I) = 0.D0
       TETAP%R(I) = 0.D0
       ALFAP%R(I) = 0.D0
      ENDIF      
      
      IF(JB.EQ.1)THEN
       LIHBOR%I(I) = KINC
       HB%R(I) = 0.202D0
       TETAB%R(I) = 0.D0
       TETAP%R(I) = 0.D0
       ALFAP%R(I) = 0.D0
      ENDIF  
      
      ENDDO    
C                                                        
C POUR UN CALCUL EN HOULE ALEATOIRE MULTIDIRECTIONNELLE, LE CODE        
C CALCULE LES DIRECTIONS DE PROPAGATION A PARTIR DES DONNEES DU FICHIER 
C DES MOTS CLES.                                                        
C                                                                       
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
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
C
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      DO 20 I = 1,NPOIN
         IF (X(I).LT.-5.D0) THEN
            ZF%R(I) = -0.616D0
         ENDIF
         IF (X(I).GE.-5.D0.AND.X(I).LT.5.D0) THEN
            ZF%R(I) = -0.616D0 + 0.05*(X(I) + 5.D0)
         ENDIF
         IF (X(I).GE.5.D0.AND.X(I).LT.9.4) THEN
            ZF%R(I) = -0.116D0 -0.025*(X(I) - 5.D0)
         ENDIF
         IF (X(I).GE.9.4) THEN
            ZF%R(I) = -0.226D0 + 0.05*(X(I) - 9.4)
         ENDIF
         IF (ZF%R(I).GT.-0.04D0) THEN
            ZF%R(I) = -0.04D0
         ENDIF
 20   CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END                  

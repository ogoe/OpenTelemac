
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
C
      DOUBLE PRECISION PI,BID
C
C     ---------------------------------------- 
C     VOS NOUVELLES DECLARATIONS DE VARIABLES :
C     ---------------------------------------- 
C                                                                       
C JCB :                                                                       
      INTEGER I   , IG,IG0 , IB,JB,IDEB,ISUIV
      INTEGER I1,I32,I33,I42,I43,I58,I59,I82,I83
      INTEGER I103,I104,I135,I136,I165
C JCB
C
      DOUBLE PRECISION X0,Y0,KK,DEGRAD,AUXIC,AUXIS,PHASOI
C
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
      HB%R(:)    = 0.04D0      
C


      DO IB=1,NPTFR
       JB=BOUNDARY_COLOUR%I(IB)
                  
      IF(JB.GE.1.AND.JB.LE.32)THEN
         LIHBOR%I(IB)=KLOG
	 RP%R(IB)=0.23D0
	 TETAP%R(IB)=0.D0
      ENDIF
C
      IF(JB.GE.33.AND.JB.LE.42)THEN        
         LIHBOR%I(IB)=KLOG
	 RP%R(IB)=1.D0
	 TETAP%R(IB)=0.D0
      ENDIF
C
       IF(JB.GE.59.AND.JB.LE.82)THEN
         LIHBOR%I(IB)=KLOG
	 RP%R(IB)=1.D0
	 TETAP%R(IB)=0.D0
      ENDIF
C
      IF(JB.GE.83.AND.JB.LE.103)THEN
         LIHBOR%I(IB)=KLOG
	 RP%R(IB)=0.05D0
	 TETAP%R(IB)=45.D0
      ENDIF
C
      IF(JB.GE.104.AND.JB.LE.135)THEN
         LIHBOR%I(IB)=KLOG
	 RP%R(IB)=0.05D0
	 TETAP%R(IB)=0.D0
      ENDIF
C
      IF(JB.GE.136.AND.JB.LE.165)THEN
         LIHBOR%I(IB)=KLOG
	 RP%R(IB)=0.23D0
	 TETAP%R(IB)=0.D0
      ENDIF

      IF(JB.GE.43.AND.JB.LE.58)THEN
         LIHBOR%I(IB)=KINC
         HB%R(IB)    =0.04D0
         TETAP%R(IB) =0.D0
         TETAB%R(IB) =TETAH
         ALFAP%R(IB) = 0.D0
      ENDIF
      
      ENDDO


	
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
C     ---------------------------------------- 
C     VOS NOUVELLES DECLARATIONS DE VARIABLES :
C     ---------------------------------------- 
C                                                                       
C JCB :                                                                       
C
C
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
C EXEMPLE :
C
C      DO 10 I = 1,NPOIN
C        ZF%R(I) = -1.D0 -0.02D0*Y(I)
C        IF (Y(I).GE.700.D0) THEN
C           ZF%R(I) = -15.D0
C        ENDIF
C10    CONTINUE
C
C-----------------------------------------------------------------------
C VOTRE MODIFICATION DES FONDS :
C-----------------------------------------------------------------------
C
C JCB :
C
C
      RETURN
      END                  
 
 

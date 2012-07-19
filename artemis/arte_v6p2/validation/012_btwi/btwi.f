
C                       ***************
                        SUBROUTINE BORH
C                       ***************
C
C***********************************************************************
C
C  ARTEMIS    VERSION 6.2   07/12   D. AELBRECHT (LNH) 01 30 87 74 12 
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
      INTEGER I
C
      DOUBLE PRECISION PI,BID         
C
C     ---------------------------------------- 
C     VOS NOUVELLES DECLARATIONS DE VARIABLES :
C     ---------------------------------------- 
C                                                                       
C JCB :                                                                       
C
      INTEGER IG              
C
CCP
      INTEGER IG0 ,JB             
      DOUBLE PRECISION PHASOI,AUXIC,AUXIS,DEGRAD,X0,Y0,KK
      DOUBLE PRECISION DINF,KINF,DLOC,KLOC,AUX1,AUX2
CCP
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
      HB%R(:)    = 1.0D0 
C --- INCIDENT WAVE PHASE DEFINITION
      DEGRAD=PI/180.D0
      PHASOI=0.D0

C --- WAVE LENGTH FROM INFINITY
C COMPUTE KINF FROM OMEGA AND DINF (explicit method)
      DINF  =5.1D0
      X0    =0.
      Y0    =0.

      AUX1  =DINF*OMEGA**2/GRAV
      AUX2 = 1.D0 +    AUX1 *( 0.6522D0 +
     &                 AUX1 *( 0.4622D0 +
     &                 AUX1 *
     &                 AUX1 *( 0.0864D0 +
     &                 AUX1 *( 0.0675D0 ) )))
      KINF = SQRT( AUX1*(AUX1 + 1.D0/AUX2) )
      KINF = KINF/DINF

      


      DO I=1,NPTFR
       JB=BOUNDARY_COLOUR%I(I)

C PAROIS SOLIDES
C
      IF(JB.GE.245.AND.JB.LE.902)THEN
	 LIHBOR%I(I)=KLOG
	 RP%R(I)=1.D0  
	 TETAP%R(I)=0.D0
	 ALFAP%R(I)=0.D0
      ENDIF
C
C
C PAROIS LIQUIDES -FRONTIERE LIBRE
C 
      IF(JB.GE.903.AND.JB.LE.953)THEN
	 LIHBOR%I(I)=KSORT
	 TETAP%R(I)=24.D0
      ENDIF
C
      IF(JB.GE.954.AND.JB.LE.1042)THEN
	 LIHBOR%I(I)=KSORT
	 TETAP%R(I)=66.D0
      ENDIF
C
C
C
C PAROIS LIQUIDES - FRONTIERE ONDE INCIDENTE
C
C
      IF(JB.GE.1.AND.JB.LE.244)THEN
	 LIHBOR%I(I)=KINC
         TETAB%R(I)=114.D0
	 AUXIC =COS(TETAB%R(I)*DEGRAD)
         AUXIS =SIN(TETAB%R(I)*DEGRAD)
	 HB%R(I)=2.80D0
CCP ---- PHASE (WE CHOOSE K=KINF ON THE INCIDENT WAVE BOUNDARY TO COMPUTE THE PHASE)	 
	 IG   = MESH%NBOR%I(I)
	 PHASOI=KINF*AUXIC*(X(IG)-X0)+KINF*AUXIS*(Y(IG)-Y0)
         ALFAP%R(I) = PHASOI/DEGRAD
      ENDIF
      ENDDO
C
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
      DOUBLE PRECISION PI,BID
C
      PARAMETER( PI = 3.1415926535897932384626433D0)
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
C
      ZF%R(13453)=-2.5
      ZF%R(13454)=-2.5
      ZF%R(13455)=-2.5
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

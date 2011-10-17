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
      INTEGER I    
C JCB
C
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
      TETABT(:) = TETAH
      TETAPT(:) = 0.D0
      ALFAPT(:) = 0.D0
      RPT(:)    = 0.D0
      HBT(:)    = 0.0D0
C onde incidente 
      DO 10 I=4101,4200
         LIHBORT(I) = KINC
         HBT(I) = 0.01D0
         TETABT(I) = 0.D0
         TETAPT(I) = 0.D0
 10   CONTINUE

         LIHBORT(1) = KINC
         HBT(1) = 0.01D0
         TETABT(1) = 0.D0
         TETAPT(1) = 0.D0



C solide en y=0
      DO 20 I=2,2000
         LIHBORT(I) = KLOG
         RPT(I) = 1.D0
         TETAPT(I) = 0.D0
         ALFAPT(I) = 0.D0
 20   CONTINUE
 

C solide libre (ex sortie absorbante) 
      DO 50 I=2001,2101
          LIHBORT(I) = KSORT
	  TETAPT(I)=0.D0
c         LIHBORT(I) = KLOG
c         RPT(I) = 0.D0
c         TETAPT(I) = 0.D0
c         ALFAPT(I) = 0.D0

 50   CONTINUE

C solide en y = 1.6
      DO 60 I=2102,4100
         LIHBORT(I) = KLOG
         RPT(I) = 1.D0
         TETAPT(I) = 0.D0
         ALFAPT(I) = 0.D0
 60   CONTINUE

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
      REAL*8  NRID
      REAL*8  D1,LCP,AA,XCP,XDEBUT,XRCP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  LISSAGES EVENTUELS DU FOND
C
C      IF(LISFON.GT.0) THEN
C        MAS=.TRUE.
C        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
C     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C      ENDIF
C
C-----------------------------------------------------------------------

C Variables 
      D1  = 0.313
      AA  = 0.05
      LCP = 1.
C number of wavelength on the bottom     
      NRID= 10.
C   
      XDEBUT=25.
      
      XCP = XDEBUT+LCP*NRID/2.
C


C SINUSOIDAL TOPOGRAPHY
      DO I = 1,NPOIN
       IF ( abs(X(I)-XCP).LT.(NRID*LCP/2.) ) THEN
        XRCP    = X(I)-XDEBUT
        ZF%R(I) = AA*SIN(2*PI*XRCP/LCP)
       ENDIF
      ENDDO
C
      RETURN
      END                  



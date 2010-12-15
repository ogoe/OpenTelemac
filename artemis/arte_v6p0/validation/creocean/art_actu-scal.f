C********************************************************************
C
C          ETUDE DU PORT DE BORME LES MIMOSAS
C********************************************************************
                        SUBROUTINE BORH
C                       ***************
C
C***********************************************************************
C
C    CE PROGRAMME MARCHE POUR LE MAILLAGE 
C        borme_3_cl (et geo)
C        distance de 3 m entre points de calcul
C    
C    Les fronti≈res houle incidente et sortie libre correspondent
C    au cas de houles venant de l'Est (90∞)
C
C    Les coef de reflexion sont pris ≈gaux »:
C         0.15   (talus d'enrochements)
C         1     (bassins du port - mur verticaux)
C         0.05    (plage et cote basse)
C
C***********************************************************************
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
C |   LIHBOR%R       | -->|  CONDITIONS AUX LIMITES SUR H              |
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
      DOUBLE PRECISION PI
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
      HBT(:)    = 0.D0
C
C**************************************************
C CONDITIONS AUX LIMITES DES FRONTIERES SOLIDES
C**************************************************                                                                    
C   plage au Nord Est
         write(*,*) 'je suis dans bord1'
      DO 10 I = 1,1029
         LIHBORT(I) = KLOG
         RPT(I) = 0.05D0
         TETAPT(I) = 0.D0
         ALFAPT(I) = 0.D0
 10   CONTINUE
C   enrochements perpendiculaires a la plage
      DO 20 I = 704,784
         LIHBORT(I) = KLOG
         RPT(I) = 0.15D0
         TETAPT(I) = 45.D0
         ALFAPT(I) = 0.D0
 20   CONTINUE
C   plage et cote basse
      DO 30 I = 785,947 
         LIHBORT(I) = KLOG 
         RPT(I) = 0.05D0
         TETAPT(I) = 0.D0
         ALFAPT(I) = 0.D0
 30   CONTINUE
C bassins du port de Borme et capitainerie (ile)
      DO 40 I = 948,1029
         LIHBORT(I) = KLOG
         RPT(I) = 1.D0
         TETAPT(I) = 0.D0
         ALFAPT(I) = 0.D0
 40   CONTINUE
      DO 50 I = 1,267
         LIHBORT(I) = KLOG
         RPT(I) = 1.D0
         TETAPT(I) = 0.D0
         ALFAPT(I) = 0.D0
 50   CONTINUE
C   musoir et digue du port en enrochements
      DO 60 I = 268,331
         LIHBORT(I) = KLOG
         RPT(I) = 0.15D0
         TETAPT(I) = 0.D0
         ALFAPT(I) = 0.D0
 60   CONTINUE
C**************************************************
C CONDITIONS AUX LIMITES DES FRONTIERES LQUIDES
C**************************************************    
C
C limite sud: Onde Incidente
       write(*,*) 'je suis dans bord2'
      DO 70 I = 332,406
         LIHBORT(I) = KINC
         HBT(I) = 2.D0
         TETABT(I) = 180.D0
         TETAPT(I) = 63.D0
 70   CONTINUE
C
C limite Est : Onde Incidente
      DO 80 I =  407,497
         LIHBORT(I) = KINC
         HBT(I) = 2.D0
         TETABT(I) = 180.D0
         TETAPT(I) = 0.D0
 80   CONTINUE
C
C limite nord: Onde incidente tant que Prof>5m
      DO 90 I = 498,569
         LIHBORT(I) = KINC
         HBT(I) = 2.D0
         TETABT(I) = 180.D0
         TETAPT(I) = 73.D0
 90   CONTINUE
C
C limite Nord (h<5m): paroi absorbante
      DO 100 I = 570,641
         LIHBORT(I) = KLOG
         RPT(I) = 0.D0
         TETAPT(I) = 0.D0
         ALFAPT(I) = 0.D0
 100   CONTINUE
       write(*,*) 'je suis dans bord3'
      RETURN                                                            
      END

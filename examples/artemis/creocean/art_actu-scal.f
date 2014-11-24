!********************************************************************
!
!          ETUDE DU PORT DE BORME LES MIMOSAS
!********************************************************************
                        SUBROUTINE BORH
!                       ***************
!
!***********************************************************************
!
!    CE PROGRAMME MARCHE POUR LE MAILLAGE 
!        borme_3_cl (et geo)
!        distance de 3 m entre points de calcul
!    
!    Les fronti≈res houle incidente et sortie libre correspondent
!    au cas de houles venant de l'Est (90∞)
!
!    Les coef de reflexion sont pris ≈gaux »:
!         0.15   (talus d'enrochements)
!         1     (bassins du port - mur verticaux)
!         0.05    (plage et cote basse)
!
!***********************************************************************
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   RP           |<-- |  COEFFICIENTS DE REFLEXION DES PAROIS        |
! |   TETAP        |<-- |  ANGLE D'ATTAQUE DE LA HOULE SUR LES LIMITES |
! |                |    |  PAS SEULEMENT LES PAROIS, MAIS AUSSI LES    |
! |                |    |  LES FRONTIERES LIQUIDES                     |
! |                |    |  (COMPTE PAR RAPPORT A LA NORMALE EXTERIEURE |
! |                |    |   DANS LE SENS DIRECT)                       |
! |   ALFAP        |<-- |  DEPHASAGE INDUIT PAR LA PAROI ENTRE L'ONDE  |
! |                |    |  REFLECHIE ET L'ONDE INCIDENTE (SI ALFAP EST |
! |                |    |  POSITIF, L'ONDE REFLECHIE EST EN RETARD)    |
! |   HB           |<-- |  HAUTEUR DE LA HOULE AUX FRONTIERES OUVERTES |
! |   TETAB        |<-- |  ANGLE D'ATTAQUE DE LA HOULE (FRONT. OUV.)   |
! |                |    |  (COMPTE PAR RAPPORT A L'AXE DES X DANS LE   |
! |                |    |   SENS DIRECT)                               |
! |    H           | -->|  HAUTEUR D'EAU                               |
! |    K           | -->|  NOMBRE D'ONDE                               |
! |    C,CG        | -->|  VITESSES DE PHASE ET DE GROUPE              |
! |    C           | -->|  CELERITE AU TEMPS N                         |
! |    ZF          | -->|  FOND                                        |
! |    X,Y         | -->|  COORDONNEES DES POINTS DU MAILLAGE          |
! |  TRA01,...,3   |<-->|  TABLEAUX DE TRAVAIL                         |
! | XSGBOR,YSGBOR  | -->|  NORMALES EXTERIEURES AUX SEGMENTS DE BORD   |
! |   LIHBOR%R       | -->|  CONDITIONS AUX LIMITES SUR H              |
! |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
! |   KP1BOR       | -->|  NUMERO DU POINT FRONTIERE SUIVANT           |
! |   OMEGA        | -->|  PULSATION DE LA HOULE                       |
! |   PER          | -->|  PERIODE DE LA HOULE                         |
! |   TETAH        | -->|  ANGLE DE PROPAGATION DE LA HOULE            |
! |   GRAV         | -->|  GRAVITE                                     |
! |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
! |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
! |   KENT,KLOG    | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
! |   KSORT,KINC   |    |  LIMITES                                     |
! |                |    |  KENT  : ENTREE (VALEUR IMPOSEE)             |
! |                |    |  KLOG  : PAROI                               |
! |                |    |  KSORT : SORTIE                              |
! |                |    |  KINC  : ONDE INCIDENTE                      |
! |   PRIVE        | -->|  TABLEAU DE TRAVAIL (DIMENSION DANS PRINCI)  |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : ARTEMI
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I,IG,IG0,IPER
!
      DOUBLE PRECISION PHASOI,AUXIC,AUXIS,DEGRAD,X0,Y0,KK
      DOUBLE PRECISION PI
      PARAMETER( PI = 3.1415926535897932384626433D0)
!
      INTRINSIC COS,SIN
!
!-----------------------------------------------------------------------
!
! CONDITIONS AUX LIMITES
! UN SEGMENT EST SOLIDE SI IL EST DE TYPE KLOG.
! UN SEGMENT EST ONDE INCIDENTE SI IL EST DE TYPE KINC.
! UN SEGMENT EST UNE ENTREE SI IL EST DE TYPE KENT.
! UN SEGMENT EST UNE SORTIE SI IL EST DE TYPE KSORT.
!
! TOUS LES ANGLES SONT EN DEGRES
!                         ------
! ---------------------------------------
! INITIALISATION DES VARIABLES PAR DEFAUT
! ---------------------------------------
      TETAB%R(:) = TETAH
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 0.D0

      


!
!**************************************************
! CONDITIONS AUX LIMITES DES FRONTIERES SOLIDES
!**************************************************                                                                    
!   plage au Nord Est
      WRITE(*,*) 'JE SUIS DANS BORD1'
      DO I = 1,1029
        LIHBOR%I(I) = KLOG
        RP%R(I)     = 0.05D0
        TETAP%R(I)  = 0.D0
        ALFAP%R(I)  = 0.D0
      ENDDO
!   enrochements perpendiculaires a la plage
      DO I = 704,784
        LIHBOR%I(I) = KLOG
        RP%R(I) = 0.15D0
        TETAP%R(I) = 45.D0
        ALFAP%R(I) = 0.D0
      ENDDO
!   plage et cote basse
      DO I = 785,947 
        LIHBOR%I(I) = KLOG 
        RP%R(I) = 0.05D0
        TETAP%R(I) = 0.D0
        ALFAP%R(I) = 0.D0
      ENDDO
! bassins du port de Borme et capitainerie (ile)
      DO I = 948,1029
        LIHBOR%I(I) = KLOG
        RP%R(I) = 1.D0
        TETAP%R(I) = 0.D0
        ALFAP%R(I) = 0.D0
      ENDDO
      DO I = 1,267
        LIHBOR%I(I) = KLOG
        RP%R(I) = 1.D0
        TETAP%R(I) = 0.D0
        ALFAP%R(I) = 0.D0
      ENDDO
!   musoir et digue du port en enrochements
      DO I = 268,331
        LIHBOR%I(I) = KLOG
        RP%R(I) = 0.15D0
        TETAP%R(I) = 0.D0
        ALFAP%R(I) = 0.D0
      ENDDO
!**************************************************
! CONDITIONS AUX LIMITES DES FRONTIERES LQUIDES
!**************************************************    
!
      DEGRAD=PI/180.D0
      PHASOI=0.D0
      AUXIC =COS(180.D0*DEGRAD)
      AUXIS =SIN(180.D0*DEGRAD)

!  --- REFERENCE POINT FOR THE PHASE
!  -- THIS METHOD DOESN'T WORK IN PARALLEL 
      IG0=MESH%NBOR%I(332)
      X0=X(IG0)
      Y0=Y(IG0)


! limite sud: Onde Incidente
      DO I = 332,406
        LIHBOR%I(I) = KINC
        HB%R(I) = 2.D0
        TETAB%R(I) = 180.D0
        TETAP%R(I) = 63.D0
!    -- PHASE
        IG    = MESH%NBOR%I(I)
        KK    = K%R(IG)
        PHASOI=PHASOI+KK*AUXIC*(X(IG)-X0)+KK*AUXIS*(Y(IG)-Y0)
        ALFAP%R(I) = PHASOI/DEGRAD
!    -- INCREMENT
        X0=X(IG)
        Y0=Y(IG)
      ENDDO
!
! limite Est : Onde Incidente
      DO I =  407,497
        LIHBOR%I(I) = KINC
        HB%R(I) = 2.D0
        TETAB%R(I) = 180.D0
        TETAP%R(I) = 0.D0
!    -- PHASE
        IG    = MESH%NBOR%I(I)
        KK    = K%R(IG)
        PHASOI=PHASOI+KK*AUXIC*(X(IG)-X0)+KK*AUXIS*(Y(IG)-Y0)
        ALFAP%R(I) = PHASOI/DEGRAD
!    -- INCREMENT
        X0=X(IG)
        Y0=Y(IG)
      ENDDO
!
! limite nord: Onde incidente tant que Prof>5m
      DO I = 498,569
        LIHBOR%I(I) = KINC
        HB%R(I) = 2.D0
        TETAB%R(I) = 180.D0
        TETAP%R(I) = 73.D0
!    -- PHASE
        IG    = MESH%NBOR%I(I)
        KK    = K%R(IG)
        PHASOI=PHASOI+KK*AUXIC*(X(IG)-X0)+KK*AUXIS*(Y(IG)-Y0)
        ALFAP%R(I) = PHASOI/DEGRAD
!    -- INCREMENT
        X0=X(IG)
        Y0=Y(IG)
      ENDDO
!
! limite Nord (h<5m): paroi absorbante
      DO I = 570,641
        LIHBOR%I(I) = KLOG
        RP%R(I) = 0.0D0
        TETAP%R(I) = 0.D0
        ALFAP%R(I) = 0.D0
      ENDDO
       
       
      RETURN                                                            
      END

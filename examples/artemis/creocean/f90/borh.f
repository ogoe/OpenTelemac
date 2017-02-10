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
!    Les frontieres houle incidente et sortie libre correspondent
!    au cas de houles venant de l'Est (90degree)
!
!    Les coef de reflexion sont pris egaux a:
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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I,JB
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
      HB%R(:)    = 0.D0
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
!
      DO I=1,NPTFR
        JB=BOUNDARY_COLOUR%I(I)
!
!**************************************************
! CONDITIONS AUX LIMITES DES FRONTIERES SOLIDES
!**************************************************
! limite Nord du modele (h<5m): paroi absorbante
        IF(JB.GE.570.AND.JB.LE.641)THEN
          RP%R(I) = 0.0D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
!   plage au Nord Ouest
        IF(JB.GE.642.AND.JB.LE.703)THEN
          RP%R(I)     = 0.05D0
          TETAP%R(I)  = 0.D0
          ALFAP%R(I)  = 0.D0
        ENDIF
!
!   enrochements perpendiculaires a la plage
        IF(JB.GE.704.AND.JB.LE.784)THEN
          RP%R(I) = 0.15D0
          TETAP%R(I) = 45.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
!   plage et cote basse
        IF(JB.GE.785.AND.JB.LE.947)THEN
          RP%R(I) = 0.05D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
! bassins du port de Borme et capitainerie (ile)
        IF(JB.GE.948.AND.JB.LE.1029)THEN
          RP%R(I) = 1.D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
        IF(JB.GE.1.AND.JB.LE.267)THEN
          RP%R(I) = 1.D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
!   musoir et digue du port en enrochements
        IF(JB.GE.268.AND.JB.LE.331)THEN
          RP%R(I) = 0.15D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
!**************************************************
! CONDITIONS AUX LIMITES DES FRONTIERES LQUIDES
!**************************************************
! limite sud: Onde Incidente
        IF(JB.GE.332.AND.JB.LE.406)THEN
          HB%R(I) = 2.D0
          TETAB%R(I) = 180.D0
          TETAP%R(I) = 63.D0
        ENDIF
!
! limite Est : Onde Incidente
        IF(JB.GE.407.AND.JB.LE.497)THEN
          HB%R(I) = 2.D0
          TETAB%R(I) = 180.D0
          TETAP%R(I) = 0.D0
        ENDIF
!
! limite nord: Onde incidente tant que Prof>5m
        IF(JB.GE.498.AND.JB.LE.569)THEN
          HB%R(I) = 2.D0
          TETAB%R(I) = 180.D0
          TETAP%R(I) = 73.D0
        ENDIF

      ENDDO !I=1,NPTFR

      RETURN
      END

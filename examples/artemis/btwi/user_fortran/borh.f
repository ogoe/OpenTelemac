!                       ***************
                        SUBROUTINE BORH
!                       ***************
!
!***********************************************************************
!
!  ARTEMIS    VERSION 6.2   07/12   D. AELBRECHT (LNH) 01 30 87 74 12
!
!  LINKED TO BIEF VERS. 5.0          J-M HERVOUET (LNH) 01 30 87 80 18
!
!***********************************************************************
!
!      FONCTION:    PREND EN COMPTE LES CONDITIONS AUX LIMITES
!                   DE L'UTILISATEUR
!                   ELLES SONT DONNEES PAR SEGMENT.
!
!      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
!
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
! |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
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
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
      INTEGER I
!
      DOUBLE PRECISION PI,BID
!
!     ----------------------------------------
!     VOS NOUVELLES DECLARATIONS DE VARIABLES :
!     ----------------------------------------
!
! JCB :
!
      INTEGER IG
!
!CP
      INTEGER IG0 ,JB
      DOUBLE PRECISION PHASOI,AUXIC,AUXIS,DEGRAD,X0,Y0,KK
      DOUBLE PRECISION DINF,KINF,DLOC,KLOC,AUX1,AUX2
!CP
!
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
      HB%R(:)    = 1.0D0
! --- INCIDENT WAVE PHASE DEFINITION
      DEGRAD=PI/180.D0
      PHASOI=0.D0

! --- WAVE LENGTH FROM INFINITY
! COMPUTE KINF FROM OMEGA AND DINF (explicit method)
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

!       PAROIS SOLIDES
!
        IF(JB.GE.245.AND.JB.LE.902)THEN
          LIHBOR%I(I)=KLOG
          RP%R(I)=1.D0
          TETAP%R(I)=0.D0
          ALFAP%R(I)=0.D0
        ENDIF
!
!
!       PAROIS LIQUIDES -FRONTIERE LIBRE
!
        IF(JB.GE.903.AND.JB.LE.953)THEN
          LIHBOR%I(I)=KSORT
          TETAP%R(I)=24.D0
        ENDIF
!
        IF(JB.GE.954.AND.JB.LE.1042)THEN
          LIHBOR%I(I)=KSORT
          TETAP%R(I)=66.D0
        ENDIF
!
!
!
!       PAROIS LIQUIDES - FRONTIERE ONDE INCIDENTE
!
!
        IF(JB.GE.1.AND.JB.LE.244)THEN
          LIHBOR%I(I)=KINC
          TETAB%R(I)=114.D0
          AUXIC =COS(TETAB%R(I)*DEGRAD)
          AUXIS =SIN(TETAB%R(I)*DEGRAD)
          HB%R(I)=2.80D0
!CP     --- PHASE (WE CHOOSE K=KINF ON THE INCIDENT WAVE BOUNDARY TO COMPUTE THE PHASE)
          IG   = MESH%NBOR%I(I)
          PHASOI=KINF*AUXIC*(X(IG)-X0)+KINF*AUXIS*(Y(IG)-Y0)
          ALFAP%R(I) = PHASOI/DEGRAD
        ENDIF
      ENDDO
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END


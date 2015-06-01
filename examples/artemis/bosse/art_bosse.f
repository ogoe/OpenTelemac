!                       ***************
                        SUBROUTINE BORH
!                       ***************
!
!***********************************************************************
!
!  ARTEMIS    VERSION 3.2 02/06/99   D. AELBRECHT (LNH) 01 30 87 74 12
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
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!
      DOUBLE PRECISION PI,BID
!
!     ----------------------------------------
!     VOS NOUVELLES DECLARATIONS DE VARIABLES :
!     ----------------------------------------
!
! JCB :
      INTEGER I    ,JB
! JCB
!
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
      HB%R(:)    = 0.0D0


      DO I=1,NPTFR
        JB=BOUNDARY_COLOUR%I(I)

!       onde incidente
        IF(JB.GE.1101.AND.JB.LE.1600)THEN
          LIHBOR%I(I) = KINC
          HB%R(I)     = 0.01D0
          TETAB%R(I)  = 0.D0
          TETAP%R(I)  = 0.D0
          ALFAP%R(I)  = 0.D0
        ENDIF
        IF(JB.EQ.1)THEN
          LIHBOR%I(I) = KINC
          HB%R(I)     = 0.01D0
          TETAB%R(I)  = 0.D0
          TETAP%R(I)  = 0.D0
          ALFAP%R(I)  = 0.D0
        ENDIF



!       solide en y=0
        IF(JB.GE.2.AND.JB.LE.300)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 1.D0
          TETAP%R(I) = 90.D0
          ALFAP%R(I) = 0.D0
        ENDIF


!       solide libre
        IF(JB.GE.301.AND.JB.LE.801)THEN
          LIHBOR%I(I) = KSORT
          TETAP%R(I)=0.D0
        ENDIF

!       solide en y = 1.6
        IF(JB.GE.802.AND.JB.LE.1100)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 1.D0
          TETAP%R(I) = 90.D0
          ALFAP%R(I) = 0.D0
        ENDIF

      ENDDO
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE ART_CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : TELEMAC 5.0          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE ART_CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      A         |<-- | MATRICE
! |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
! |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I
!
      DOUBLE PRECISION PI,BID
!
      PARAMETER( PI = 3.1415926535897932384626433D0)
!
      DOUBLE PRECISION :: RCP,BCP,HCP,XCP,DCP,YCP

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  LISSAGES EVENTUELS DU FOND
!
!      IF(LISFON.GT.0) THEN
!        MAS=.TRUE.
!        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
!     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!      ENDIF
!
!-----------------------------------------------------------------------

! Variables
      RCP  = 1.
      BCP  = 0.4*RCP
!      HCP  = BCP/0.3

! Circle center
      XCP = 6.*RCP
      YCP = 10.*RCP


! Elliptical obstacle
      DO I = 1,NPOIN
        DCP=( (X(I)-XCP)**2. + (Y(I)-YCP)**2. )**0.5

        IF ( DCP.LT.RCP ) THEN
          ZF%R(I) = BCP*( 1. - (DCP/RCP)**2. )
          ELSE
          ZF%R(I) = 0D0
        ENDIF
      ENDDO
!
      RETURN
      END


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
      INTEGER I    , IG, IB,JB,IG0,ISUIV,IDEB
! JCB
!
!
      PARAMETER( PI = 3.1415926535897932384626433D0)
!
      INTRINSIC COS,SIN
!
      DOUBLE PRECISION HINC,THB,AUXIC,AUXIS,DEGRAD,PHASOI
      DOUBLE PRECISION X0,Y0,KK,AUX1,AUX2,KINF,DINF


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
!      WRITE(6,*) 'ON ENTRE DANS BORH'
      TETAB%R(:) = TETAH
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 0.0D0
 
      
      DO IB=1,NPTFR
       JB=BOUNDARY_COLOUR%I(IB)
! --------------------------------
! Incident wave
      IF(JB.GE.313.AND.JB.LE.513)THEN
         LIHBOR%I(IB)= KINC
         HB%R(IB)    = 1D0
         TETAP%R(IB) = 45.D0
         TETAB%R(IB) = -45D0
      ENDIF
!      
      IF(JB.GE.1.AND.JB.LE.112)THEN
         LIHBOR%I(IB)= KINC
         HB%R(IB)    = 1D0
         TETAP%R(IB) = 90.D0
         TETAB%R(IB) = -45D0
      ENDIF
! --------------------------------

! --------------------------------
! Free exit
      IF(JB.GE.113.AND.JB.LE.312)THEN
         LIHBOR%I(IB) = KSORT
         TETAP%R(IB) = 45.D0
      ENDIF 
! --------------------------------
      ENDDO



!-----------------------------------------------------------------------
!	FOR REFERENCE (INPUT KINC WHEN NO AUTOMATIC PHASE CALCULATED)

! -----Incident wave-------------- 
!      DEGRAD     =  PI/180.0D0
!      HINC       = 1.0D0
!      THB        = -45.D0
!      AUXIC      = COS(THB*DEGRAD)
!      AUXIS      = SIN(THB*DEGRAD)
!      PHASOI = 0.
!
! --- WAVE LENGTH FROM INFINITY
! COMPUTE KINF FROM OMEGA AND DINF (explicit method)
!      DINF  =100D0
!      X0    =0.
!      Y0    =0.

!      AUX1  =DINF*OMEGA**2/GRAV
!      AUX2 = 1.D0 +    AUX1 *( 0.6522D0 +
!     &                 AUX1 *( 0.4622D0 +
!     &                 AUX1 *
!     &                 AUX1 *( 0.0864D0 +
!     &                 AUX1 *( 0.0675D0 ) )))
!      KINF = SQRT( AUX1*(AUX1 + 1.D0/AUX2) )
!      KINF = KINF/DINF

! --------------------------------
! Incident wave
!      IF(JB.GE.313.AND.JB.LE.513)THEN
!         LIHBOR%I(IB) = KINC
!         HB%R(IB)    = HINC
!         TETAP%R(IB) = 45.D0
!         TETAB%R(IB) = THB
!CP ---- PHASE (WE CHOOSE K=KINF ON THE INCIDENT WAVE BOUNDARY TO COMPUTE THE PHASE)
!          IG   = MESH%NBOR%I(IB)
!          PHASOI=KINF*AUXIC*(X(IG)-X0)+KINF*AUXIS*(Y(IG)-Y0)
!          ALFAP%R(IB) = PHASOI/DEGRAD
!      ENDIF
      

!      IF(JB.GE.1.AND.JB.LE.112)THEN
!         LIHBOR%I(IB) = KINC
!         HB%R(IB)    = HINC
!         TETAP%R(IB) = 90.D0
!         TETAB%R(IB) = THB
!CP ---- PHASE (WE CHOOSE K=KINF ON THE INCIDENT WAVE BOUNDARY TO COMPUTE THE PHASE)
!          IG   = MESH%NBOR%I(IB)
!          PHASOI=KINF*AUXIC*(X(IG)-X0)+KINF*AUXIS*(Y(IG)-Y0)
!          ALFAP%R(IB) = PHASOI/DEGRAD
!      ENDIF
! --------------------------------
								 
      RETURN								 
      END								 
!			*****************
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
      REAL*8  NRID
      REAL*8  D1,LCP,AA,XCP,XDEBUT,XRCP
!
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

! bathy 
      DO I = 1,NPOIN
        ZF%R(I) = -100D0
      ENDDO
!
      RETURN
      END                  

















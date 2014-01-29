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
      DOUBLE PRECISION X0,Y0,KK


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
      WRITE(6,*) 'ON ENTRE DANS BORH'
      TETAB%R(:) = TETAH
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 0.0D0
      
      PRB%R(:)   =0.D0
      PIB%R(:)   =0.D0
      DDXPRB%R(:)=0.D0
      DDYPRB%R(:)=0.D0
      DDXPIB%R(:)=0.D0
      DDYPIB%R(:)=0.D0
      
      DEGRAD     =  PI/180.0D0
      HINC       = 0.01D0
      THB        = 0.D0
      AUXIC      = COS(THB*DEGRAD)
      AUXIS      = SIN(THB*DEGRAD)
      PHASOI = 0.
      
      DO IB=1,NPTFR
       JB=BOUNDARY_COLOUR%I(IB)


! solide en y=0
      IF(JB.GE.2.AND.JB.LE.2000)THEN
         LIHBOR%I(IB) = KLOG
         RP%R(IB) = 1.D0
         TETAP%R(IB) = 0.D0
         ALFAP%R(IB) = 0.D0
      ENDIF 

! solide libre
      IF(JB.GE.2001.AND.JB.LE.2101)THEN
          LIHBOR%I(IB) = KSORT
          TETAP%R(IB)=0.D0
      ENDIF
      
! solide en y = 1.6
      IF(JB.GE.2102.AND.JB.LE.4100)THEN
         LIHBOR%I(IB) = KLOG
         RP%R(IB) = 1.D0
         TETAP%R(IB) = 0.D0
         ALFAP%R(IB) = 0.D0
      ENDIF

! Incident wave with PHASE=0.
      IF(JB.GE.4101.AND.JB.LE.4200)THEN
         LIHBOR%I(IB) = KINC
         HB%R(IB)    = 0.01D0
         TETAP%R(IB) = 0.D0
         ALFAP%R(IB) = 0.D0
      ENDIF
      IF(JB.EQ.1)THEN
         LIHBOR%I(IB) = KINC
         HB%R(IB)    = 0.01D0
         TETAP%R(IB) = 0.D0
         ALFAP%R(IB) = 0.D0
      ENDIF
      
      
      
      
      
      

! Example : Incident potential with phase = 0. 
!      PHASOI=0.D0    
!      IF(JB.GE.4101.AND.JB.LE.4200)THEN
!	 LIHBOR%I(IB)= KPOT
!	 IG	     = MESH%NBOR%I(IB)
!	 KK	     = K%R(IG)
!	 PRB%R(IB)   = HINC*GRAV/(2.0D0*OMEGA)*SIN(PHASOI)
!	 PIB%R(IB)   =-HINC*GRAV/(2.0D0*OMEGA)*COS(PHASOI)
!	 DDXPRB%R(IB)= HINC*GRAV/(2.0D0*OMEGA)*KK*AUXIC*COS(PHASOI)
!	 DDYPRB%R(IB)= HINC*GRAV/(2.0D0*OMEGA)*KK*AUXIS*COS(PHASOI)
!	 DDXPIB%R(IB)= HINC*GRAV/(2.0D0*OMEGA)*KK*AUXIC*SIN(PHASOI)
!	 DDYPIB%R(IB)= HINC*GRAV/(2.0D0*OMEGA)*KK*AUXIS*SIN(PHASOI)
!	 TETAP%R(IB) = 0.D0
!	 ALFAP%R(IB) = 0.D0	
!      ENDIF
!      IF(JB.EQ.1)THEN
!	 LIHBOR%I(IB) = KPOT
!	 IG	     = MESH%NBOR%I(IB)
!	 KK	     = K%R(IG)
!	 PRB%R(IB)   = HINC*GRAV/(2.0D0*OMEGA)*SIN(PHASOI)
!	 PIB%R(IB)   =-HINC*GRAV/(2.0D0*OMEGA)*COS(PHASOI)
!	 DDXPRB%R(IB)= HINC*GRAV/(2.0D0*OMEGA)*KK*AUXIC*COS(PHASOI)
!	 DDYPRB%R(IB)= HINC*GRAV/(2.0D0*OMEGA)*KK*AUXIS*COS(PHASOI)
!	 DDXPIB%R(IB)= HINC*GRAV/(2.0D0*OMEGA)*KK*AUXIC*SIN(PHASOI)
!	 DDYPIB%R(IB)= HINC*GRAV/(2.0D0*OMEGA)*KK*AUXIS*SIN(PHASOI)	
!	 TETAP%R(IB) = 0.D0
!	 ALFAP%R(IB) = 0.D0	
!      ENDIF
!
!      
      ENDDO



!-----------------------------------------------------------------------
!									 
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

! Variables 
      D1  = 0.313
      AA  = 0.05
      LCP = 1.
! nombre de longueur d'onde sur la bathymetrie     
      NRID= 10.
!   
      XDEBUT=25.
      
      XCP = XDEBUT+LCP*NRID/2.
!


! bathy sinusoidale
      DO I = 1,NPOIN
       IF ( abs(X(I)-XCP).LT.(NRID*LCP/2.) ) THEN
        XRCP    = X(I)-XDEBUT
        ZF%R(I) = AA*SIN(2*PI*XRCP/LCP)
       ENDIF
      ENDDO
!
      RETURN
      END                  

















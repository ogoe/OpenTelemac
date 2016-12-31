!                    *****************
                     SUBROUTINE CONDIH
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETER ARRAYS.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        02/06/1999
!+        V5P1
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
      INTEGER I
!
      REAL, DIMENSION(NPOIN) :: RR
!
      DOUBLE PRECISION COTE
      DOUBLE PRECISION PI,BID,DHTEST
      PARAMETER( PI = 3.1415926535897932384626433D0 )
!
      INTRINSIC SINH, SQRT
!
      DOUBLE PRECISION UU1

!
!-----------------------------------------------------------------------
!
      CALL MAJUS(CDTINI)
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WATER DEPTH H
!
      IF(INCLUS(CDTINI,'COTE NULLE').OR.
     &   INCLUS(CDTINI,'ZERO ELEVATION') ) THEN
        COTE = 0.D0
        CALL OS( 'X=C     ' , H , SBID , SBID , COTE )
        CALL OS( 'X=X-Y   ' , H , ZF  , SBID , BID  )
      ELSEIF(INCLUS(CDTINI,'COTE CONSTANTE').OR.
     &       INCLUS(CDTINI,'CONSTANT ELEVATION') ) THEN
        COTE = COTINI
        CALL OS( 'X=C     ' , H , SBID , SBID , COTE )
        CALL OS( 'X=X-Y   ' , H , ZF  , SBID , BID  )
      ELSEIF(INCLUS(CDTINI,'HAUTEUR NULLE').OR.
     &       INCLUS(CDTINI,'ZERO DEPTH') ) THEN
        CALL OS( 'X=C     ' , H , SBID , SBID , 0.D0 )
      ELSEIF(INCLUS(CDTINI,'HAUTEUR CONSTANTE').OR.
     &       INCLUS(CDTINI,'CONSTANT DEPTH') ) THEN
        CALL OS( 'X=C     ' , H , SBID , SBID , HAUTIN )
      ELSEIF(INCLUS(CDTINI,'PARTICULIERES').OR.
     &       INCLUS(CDTINI,'SPECIAL')        ) THEN
!  TO BE MODIFIED BY USER
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIH : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &         ,/,'         VOUS DEVEZ MODIFIER CONDIH')
11      FORMAT(1X,'CONDIH : WITH SPECIAL INITIAL CONDITIONS'
     &         ,/,'         YOU HAVE TO MODIFY CONDIH')
        CALL PLANTE(0)
        STOP
!  END OF CODE TO BE MODIFIED BY USER
      ELSE
        IF(LNG.EQ.1) WRITE(LU,20) CDTINI
        IF(LNG.EQ.2) WRITE(LU,21) CDTINI
20      FORMAT(1X,'CONDIH : CONDITION INITIALE INCONNUE :',/,A72)
21      FORMAT(1X,'CONDIH : UNKNOWN INITIAL CONDITION :',/,A72)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CLIPS H (VALUES LOWER THAN 1.D-2 NOT ALLOWED)
!
      CALL CLIP(H,1.D-2,.TRUE.,1.D6,.FALSE.,NPOIN)
!
!-----------------------------------------------------------------------
!
!   COMPUTES THE WAVE NUMBER: K
!   USING AN EXPLICIT FORMULATION (SEE EDF'S EXCELLENT REPORT BY
!   F. DHELLEMMES 'PRECIS SUR LES VAGUES' )
!
!
      OMEGA = 2.D0*PI/PER
      CALL OS('X=CY    ', T1 , H , SBID , OMEGA**2/GRAV )
!
!     INITIALISES DHTEST
!
      DHTEST = 1.D6
!
      DO 100 I=1,NPOIN
        T2%R(I) = 1.D0 + T1%R(I) *( 0.6522D0 +
     &                   T1%R(I) *( 0.4622D0 +
     &                   T1%R(I) *
     &                   T1%R(I) *( 0.0864D0 +
     &                   T1%R(I) *( 0.0675D0 ) )))
        T2%R(I) = SQRT( T1%R(I)*(T1%R(I) + 1.D0/T2%R(I)) )
        K%R(I)  = T2%R(I)/H%R(I)
        DHTEST  = MIN( DHTEST , H%R(I) )
100   CONTINUE
!
!   CHECKS WHETHER H HAS BEEN CLIPPED OR NOT
!
      IF (DHTEST.LE.1.01D-2) THEN
        IF(LNG.EQ.1) WRITE(LU,120)
        IF(LNG.EQ.2) WRITE(LU,121)
120     FORMAT(1X,'CONDIH : ATTENTION !! VOUS AVEZ ATTEINT LE SEUIL '
     &         ,/,'         MINI DE HAUTEUR D''EAU (1 CM).'
     &         ,/,'         VERIFIEZ BATHY OU CONDITIONS INITIALES')
121     FORMAT(1X,'CONDIH : WARNING !! YOU REACHED MINIMUM THRESHOLD'
     &         ,/,'         FOR WATER DEPTH (1 CM). CHECK THE'
     &         ,/,'         BATHYMETRY OR INITIAL CONDITIONS')
      ENDIF
!
!-----------------------------------------------------------------------
!
!   COMPUTES PHASE VELOCITY
!
      CALL OS('X=CY    ', T1    , K     , SBID , 1.D0/OMEGA )
      CALL OS('X=1/Y   ', C     , T1    , SBID , BID        )
!
!-----------------------------------------------------------------------
!
!   COMPUTES GROUP VELOCITY
!
      DO I=1,NPOIN
        CG%R(I) = C%R(I)/2.D0 *
     &            (1.D0 + 2.D0*K%R(I)*H%R(I)/SINH(2.D0*K%R(I)*H%R(I)))
      ENDDO
!
!
!-----------------------------------------------------------------------
!
!     COURANT :
!
!      IF(COURANT) THEN
!        CALL OS( 'X=C     ', X=UC, C=CURRENTX)
!        CALL OS( 'X=C     ', X=VC, C=CURRENTY)
!      ENDIF
      IF(COURANT) THEN
        DO I=1,NPOIN!
        UC%R(I)=0.D0
        VC%R(I)=0.D0
        RR(I)=0.D0
        RR(I)=SQRT((X(I)-2500.D0)**2+(Y(I)-2500.D0)**2)
!
        IF(X(I).GE.2500.D0.AND.Y(I).GE.2500.D0 )THEN
!
        IF(RR(I).LT.900.D0)THEN
        UC%R(I)= (0.9D0*(RR(I)/900.D0)**2)*((Y(I)-2500.D0)/RR(I))
        VC%R(I)= -(0.9D0*(RR(I)/900.D0)**2)*((X(I)-2500.D0)/RR(I))
        ELSEIF(RR(I).GE.900.D0)THEN
        UC%R(I)= (1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &  ((Y(I)-2500.D0)/RR(I))
        VC%R(I)=-(1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &  ((X(I)-2500.D0)/RR(I))
        ENDIF
!
        ELSEIF(X(I).GE.2500.D0.AND.Y(I).LT.2500.D0)THEN
!
        IF(RR(I).LT.900.D0)THEN
        UC%R(I)= (0.9D0*(RR(I)/900.D0)**2)*((Y(I)-2500.D0)/RR(I))
        VC%R(I)= -(0.9D0*(RR(I)/900.D0)**2)*((X(I)-2500.D0)/RR(I))
        ELSEIF(RR(I).GE.900.D0)THEN
        UC%R(I)= (1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &  ((Y(I)-2500.D0)/RR(I))
        VC%R(I)= -(1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &  ((X(I)-2500.D0)/RR(I))
        ENDIF
!
        ELSEIF(X(I).LT.2500.D0.AND.Y(I).GE.2500.D0)THEN
!
        IF(RR(I).LT.900.D0)THEN
        UC%R(I)= (0.9D0*(RR(I)/900.D0)**2)*((Y(I)-2500.D0)/RR(I))
        VC%R(I)= -(0.9D0*(RR(I)/900.D0)**2)*((X(I)-2500.D0)/RR(I))
        ELSEIF(RR(I).GE.900.D0)THEN
        UC%R(I)= (1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &  ((Y(I)-2500.D0)/RR(I))
        VC%R(I)= -(1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &  ((X(I)-2500.D0)/RR(I))
        ENDIF
!
        ELSEIF(X(I).LT.2500.D0.AND.Y(I).LT.2500.D0)THEN
!
        IF(RR(I).LT.900.D0)THEN
        UC%R(I)= (0.9D0*(RR(I)/900.D0)**2)*((Y(I)-2500.D0)/RR(I))
        VC%R(I)= -(0.9D0*(RR(I)/900.D0)**2)*((X(I)-2500.D0)/RR(I))
        ELSEIF(RR(I).GE.900.D0)THEN
        UC%R(I)= (1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &          ((Y(I)-2500.D0)/RR(I))
        VC%R(I)= -(1.D0*EXP(-((1000.D0-RR(I))/300.D0)**2))*
     &          ((X(I)-2500.D0)/RR(I))
        ENDIF
!
        ENDIF
        ENDDO
        ENDIF


!-----------------------------------------------------------------------
!
      RETURN
      END
!                       ***************
                        SUBROUTINE BORH
!                       ***************
!
!***********************************************************************
!
!  ARTEMIS    VERSION 6.1 28/06/11   D. AELBRECHT (LNH) 01 30 87 74 12
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
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
      INTEGER I,JB
!
      DOUBLE PRECISION PI,BID
!
!     ----------------------------------------
!     VOS NOUVELLES DECLARATIONS DE VARIABLES :
!     ----------------------------------------
!
! JCB :
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
!
! PAROIS LIQUIDES - FRONTIERE ONDE INCIDENTE
!
      DO I=1,NPTFR
        JB=BOUNDARY_COLOUR%I(I)
!
        IF(JB.GE.601.AND.JB.LE.800) THEN
          LIHBOR%I(I)=KINC
          HB%R(I)=1.D0
          TETAB%R(I)=0.D0
          TETAP%R(I)=0.D0
        ENDIF
        IF(JB.EQ.1) THEN
          LIHBOR%I(I)=KINC
          HB%R(I)=1.D0
          TETAB%R(I)=0.D0
          TETAP%R(I)=0.D0
        ENDIF
!
!       PAROIS LIQUIDES -FRONTIERE LIBRE
!
        IF(JB.GE.201.AND.JB.LE.401) THEN
          LIHBOR%I(I)=KSORT
          TETAP%R(I)=0.D0
        ENDIF
!
!       PAROIS SOLIDES
        IF(JB.GE.2.AND.JB.LE.200) THEN
          LIHBOR%I(I)=KLOG
          RP%R(I)=1.D0
          TETAP%R(I)=90.D0
        ENDIF
        IF(JB.GE.402.AND.JB.LE.600) THEN
          LIHBOR%I(I)=KLOG
          RP%R(I)=1.D0
          TETAP%R(I)=90.D0
        ENDIF
!
      ENDDO
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE ART_CORFON
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+  EXAMPLE :
!+
!+      DO 10 I = 1,NPOIN
!+        ZF%R(I) = -1.D0 -0.02D0*Y(I)
!+        IF (Y(I).GE.700.D0) THEN
!+           ZF%R(I) = -15.D0
!+        ENDIF
!+10    CONTINUE
!
!history  J-M HERVOUET
!+        01/03/1990
!+        V5P1
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
      DO  I = 1,NPOIN
        ZF%R(I) = -100.D0
      ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE CORRXY
!                    *****************
!
     & (X,Y,NPOIN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE COORDINATES OF THE POINTS IN THE MESH.
!
!warning  USER SUBROUTINE; COMMENTED LINES ARE AN EXAMPLE
!code
!+  EXAMPLE : MULTIPLIES BY A CONSTANT (SCALES THE MESH)
!+            CHANGES THE ORIGIN
!+
!+      DO I = 1 , NPOIN
!+         X(I) = 3.D0 * X(I) + 100.D0
!+         Y(I) = 5.D0 * Y(I) - 50.D0
!+      ENDDO
!warning  DO NOT PERFORM ROTATIONS AS IT WILL CHANGE
!+            THE NUMBERING OF THE LIQUID BOUNDARIES
!
!history  EMILE RAZAFINDRAKOTO (LNHE)
!+        17/10/05
!+        V5P6
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| X              |<->| ABSCISSAE OF POINTS IN THE MESH
!| X,Y            |<->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CORRXY => CORRXY
      USE DECLARATIONS_SPECIAL
!
!
!     OTHER DATA ARE AVAILABLE WITH THE DECLARATIONS OF EACH PROGRAM
!
!     USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
!     THE CALLING PROGRAM AND THE NEEDED MODIFICATION
!     BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
!     ALL THE DATA STRUCTURE OF THIS CODE IS AVAILABLE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  EXAMPLE : MULTIPLIES BY A CONSTANT (SCALES THE MESH)
!            CHANGES THE ORIGIN
!
      DO I=1,NPOIN
        X(I)=X(I)*(5.D0/2.D0)
        Y(I)=Y(I)*(5.D0/2.D0)
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'CORRXY (BIEF) : PAS DE MODIFICATION DES COORDONNEES'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*)'CORRXY (BIEF):NO MODIFICATION OF COORDINATES'
        WRITE(LU,*)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!



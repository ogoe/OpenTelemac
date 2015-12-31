!                       ***************
                        SUBROUTINE BORH
!                       ***************
!
!***********************************************************************
!
!  ARTEMIS    VERSION 6.1 28/06/11  D. AELBRECHT (LNH) 01 30 87 74 12
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
      INTEGER I
!
      DOUBLE PRECISION PI
!
!     ----------------------------------------
!     VOS NOUVELLES DECLARATIONS DE VARIABLES :
!     ----------------------------------------
!
! JCB :
      INTEGER IG
      DOUBLE PRECISION R0,R1
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
!      TETAB%R(:) = TETAH
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 0.0D0
!
! ------------
! VOS CONDITIONS AUX LIMITES :
! ------------
!
! PAROIS LIQUIDES - FRONTIERE ONDE INCIDENTE
!
!      WRITE(6,*) 'TETAH=',TETAH
      DO I=212,292
        LIHBOR%I(I)=KINC
        HB%R(I)=0.117D0
!       TETAB%R(I)=-90.D0
!      WRITE(6,*) 'TETAB=',TETAB%R(I)
      END DO
!
! PAROIS LIQUIDES -FRONTIERE LIBRE (PLage)
!
      DO I=1,99
        LIHBOR%I(I)=KLOG
        RP%R(I)=.3D0
        TETAP%R(I)=0.D0
      END DO
!

! PAROIS SOLIDES - BRISE-LAME ( COEFF D'AMORTISSEMENT DIFF. DE 1)
!
      DO I=331,348
        LIHBOR%I(I)=KLOG
        RP%R(I)=.3D0
        ALFAP%R(I)=0.D0
        TETAP%R(I)=0.D0
      END DO
!
      DO I=349,387
        LIHBOR%I(I)=KLOG
        RP%R(I)=1.D0
        ALFAP%R(I)=0.D0
        TETAP%R(I)=0.D0
      END DO
!
! PAROIS SOLIDES - BORD DROIT
!
      DO I=100,211
        LIHBOR%I(I)=KLOG
        RP%R(I)=1.D0
        ALFAP%R(I)=0.D0
        TETAP%R(I)=90.D0
      END DO
!
! PAROIS SOLIDES - BORDS GAUCHES
!
      DO I=293,330
        LIHBOR%I(I)=KLOG
        RP%R(I)=1.D0
        ALFAP%R(I)=0.D0
        TETAP%R(I)=90.D0
      END DO
      DO I=388,458
        LIHBOR%I(I)=KLOG
        RP%R(I)=1.D0
        ALFAP%R(I)=0.D0
!
!     TETAP N'A PAS D'IMPORTANCE
!
      END DO
!---------
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
!  USER SUBROUTINE CORFON
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
      INTEGER I,II
!
!
!     ----------------------------------------
!     VOS NOUVELLES DECLARATIONS DE VARIABLES :
!     ----------------------------------------
!
! JCB :
      INTEGER KK
      DOUBLE PRECISION ECHEL
! JCB
!
!
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
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
      DO  I = 1,NPOIN
        ZF%R(I) = 0.D0
      ENDDO
!-----------------------------------------------------------------------
!
! EXEMPLE :
!
!      DO 10 I = 1,NPOIN
!        ZF%R(I) = -1.D0 -0.02D0*Y(I)
!        IF (Y(I).GE.700.D0) THEN
!           ZF%R(I) = -15.D0
!        ENDIF
!10    CONTINUE
!
!-----------------------------------------------------------------------
! VOTRE MODIFICATION DES FONDS :
!-----------------------------------------------------------------------
!
! JCB :
!
      ECHEL=1.D0
      DO KK=1,NPOIN
        IF((Y(KK)/ECHEL).GE. 7.12D0)THEN
          ZF%R(KK)=-0.33D0*ECHEL
        ELSEIF ((Y(KK)/ECHEL) .LE. -9.13D0)THEN
          ZF%R(KK)=-0.0044*ECHEL
!       ELSEIF ((Y(KK)/ECHEL) .LE. -8D0)THEN
!        ZF%R(KK)=-0.027*ECHEL
        ELSE
          ZF%R(KK)=-0.02D0*((Y(KK)/ECHEL)+9.35D0)*ECHEL
        ENDIF
      ENDDO
!-------------------------------------------------------
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
!
!
!     OTHER DATA ARE AVAILABLE WITH THE DECLARATIONS OF EACH PROGRAM
!
!     USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
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
        X(I)=X(I)/22.5D0
        Y(I)=Y(I)/22.5D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END



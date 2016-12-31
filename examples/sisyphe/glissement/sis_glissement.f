!                    *****************
                     SUBROUTINE NOEROD
!                    *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    FIXES THE NON-ERODABLE BED ELEVATION ZR.
!
!note     METHODS OF TREATMENT OF NON-ERODABLE BEDS CAN LEAD TO ZF.
!note  CHOOSE TO SMOOTH THE SOLUTION WITH NLISS > 0.
!
!history  C. LENORMANT
!+
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
!| CHOIX          |-->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
!| H              |-->| WATER DEPTH
!| NLISS          |<->| NUMBER OF SMOOTHINGS
!| NPOIN          |-->| NUMBER OF 2D POINTS
!| X,Y            |-->| 2D COORDINATES
!| Z              |-->| FREE SURFACE
!| ZF             |-->| BED LEVEL
!| ZR             |<--| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!--------------------
! RIGID BEDS POSITION
!---------------------
!
!     DEFAULT VALUE:       ZR=ZF-100
!
!     CALL OV( 'X=C       ',ZR,ZF,ZF,-100.D0,NPOIN)
      DO I=1,NPOIN
        IF(X(I).GT.2.5D0) THEN
          ZR(I)=0.D0
        ELSE
          ZR(I)=12.5D0
        ENDIF
      ENDDO
!
!------------------
! SMOOTHING OPTION
!------------------
!
!     NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!             DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
      NLISS = 0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *********************
                     SUBROUTINE INIT_COMPO
!                    *********************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+  EXAMPLE:
!+      NCOUCHES(J) = 10
!+      ES(J,1) = 1.D0
!+      ES(J,2) = 1.D0
!+      ES(J,3) = 1.D0
!+      ES(J,4) = 1.D0
!+      ES(J,5) = 1.D0
!+      ES(J,6) = 1.D0
!+      ES(J,7) = 1.D0
!+      ES(J,8) = 1.D0
!+      ES(J,9) = 1.D0
!+        DO I = 1, NSICLA
!+          DO K = 1, NCOUCHES(J)
!+          AVAIL(J,K,I) = AVA0(I)
!+          ENDDO
!+        ENDDO
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002
!+        V6P0
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
!| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I , J
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
!       BY DEFAULT : UNIFORM BED COMPOSITION
!
        NCOUCHES(J) = 1
        DO I = 1, NSICLA
          AVAIL(J,1,I) = AVA0(I)
        ENDDO

        AVAIL(J,1,1)=       0.2D0*MESH%X%R(J) /16.D0
        AVAIL(J,1,2)=       0.6D0*MESH%X%R(J) /16.D0
        AVAIL(J,1,3)=(16.D0-0.8D0*MESH%X%R(J))/16.D0

!
!  TO BE FILLED BY THE USER
!      NCOUCHES(J) = 10
!      ES(J,1) = 1.D0
!      ES(J,2) = 1.D0
!      ES(J,3) = 1.D0
!      ES(J,4) = 1.D0
!      ES(J,5) = 1.D0
!      ES(J,6) = 1.D0
!      ES(J,7) = 1.D0
!      ES(J,8) = 1.D0
!      ES(J,9) = 1.D0
!        DO I = 1, NSICLA
!          DO K = 1, NCOUCHES(J)
!          AVAIL(J,K,I) = AVA0(I)
!          ENDDO
!        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *************************
                        SUBROUTINE CONDIM_SISYPHE
!                       *************************
!
     & (U      , V   , QU    , QV  , H   , ZF , Z ,
     &  ESOMT  , THETAWR   , Q     , HWR  , TWR  ,
     &  X      , Y   , NPOIN , AT  , PMAREE)
!
!***********************************************************************
! SISYPHE VERSION 5.3                             E. PELTIER    11/09/95
!                                                 C. LENORMANT
!                                                 J.-M. HERVOUET
!
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT
!***********************************************************************
!
!     FONCTION  : VALEURS IMPOSEES
!                         - DU DEBIT VECTORIEL    QU, QV
!                         - DE LA HAUTEUR D'EAU   H
!                         - DE LA COTE DU FOND    ZF
!                         - DE LA SURFACE LIBRE   Z
!                         - DE L'EVOLUTION TOTALE ESOMT
!                         - DU DEBIT              Q
!                         - DE LA HAUTEUR DE HOULE HW
!                         - DE LA PERIODE DE HOULE TW
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   U , V        |<-- | COORDONNEES DES VECTEURS VITESSE
! |   QU , QV      |<-- | DEBIT VECTORIEL SUIVANT X ET SUIVANT Y
! |   H            |<-->| HAUTEUR D'EAU
! |   ZF           |<-->| COTE DU FOND
! |   Z            |<-->| COTE DE SURFACE LIBRE
! |   ESOMT        |<-->| EVOLUTION TOTALE DES FONDS
! |   C            |<-->| CELERITE
! |   Q            |<-->| DEBIT
! |   HW           | -->| HAUTEUR DE HOULE
! |   TW           | -->| PERIODE DE HOULE
! |   X,Y          | -->| COORDONNEES DU MAILLAGE
! |   NPOIN        | -->| NOMBRE DE POINTS DU MAILLAGE
! |   AT           | -->| TEMPS
! |   PMAREE       | -->| PERIODE DE LA MAREE
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! PROGRAMME APPELANT : SISYPH
! PROGRAMMES APPELES :
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)::NPOIN
!
      DOUBLE PRECISION, INTENT(IN):: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN):: AT , PMAREE
! SEDIMENT
      DOUBLE PRECISION, INTENT(INOUT) ::  ZF(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::  ESOMT(NPOIN)
! HYDRODYNAMICS
      DOUBLE PRECISION, INTENT(INOUT):: Z(NPOIN) , H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::QU(NPOIN), QV(NPOIN), Q(NPOIN)
! WAVES
      DOUBLE PRECISION, INTENT (INOUT):: HWR(NPOIN) , TWR(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT):: THETAWR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!     --------------------------------------------------------------
!     INITIALISATION DES TABLEAUX NON LUS DANS LE FICHIER RESULTATS:
!     --------------------------------------------------------------
!
      DO I=1,NPOIN
        QU(I)=0.D0
        Q(I) =0.D0
        QV(I)=0.D0
        Z(I) =17.D0
        ZF(I)=16.D0-X(I)
        U(I) =0.D0
        V(I) =0.D0
        H(I) =Z(I)-ZF(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

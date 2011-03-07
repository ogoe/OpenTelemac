!                    *****************
                     SUBROUTINE FLUSED
!                    *****************
!
     & (ATABOF , BTABOF , ATABOS , BTABOS ,
     &  LITABF , LITABS , TA     , WC     ,
     &  X      , Y      , Z      , HN     ,
     &  GRADZFX, GRADZFY, GRADZSX, GRADZSY,
     &  TOB    , PDEPOT , FLUER  , TOCD   ,
     &  NPOIN3 , NPOIN2 , NPLAN  , KLOG   , SEDCO)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    WRITES THE FLUXES AT THE BOTTOM AND FREE SURFACE
!+                FOR THE SEDIMENT.
!
!note     CHECKS MASS BALANCE AT THE BOTTOM AND FREE SURFACE.
!+         RESULTS IN A BOUNDARY CONDITION ON SEDIMENT FLUXES.
!
!history  C LE NORMANT (LNH)
!+        13/05/92
!+        V5P5
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| ATABOF         |---|
!| ATABOS         |---|
!| BTABOF         |---|
!| BTABOS         |---|
!| FLUER          |-->| FLUX D'EROSION EN CHAQUE POINT 2D
!| GRADZFX        |---|
!| GRADZFY        |---|
!| GRADZSX        |---|
!| GRADZSY        |---|
!| HN             |-->| HAUTEUR D'EAU
!| KLOG           |-->| INDICATEUR DE PAROI SOLIDE
!| LITABF         |---|
!| LITABS         |---|
!| NPLAN          |-->| NOMBRE DE PLANS DISCRETISANT LA VERTICALE
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| NPOIN3         |-->| NOMBRE DE POINTS 3D
!| PDEPOT         |<--| PROBABILITE DE DEPOT EN CHAQUE POINT 2D
!| SEDCO          |-->| LOGIQUE POUR SEDIMENT COHESIF
!| TA             |-->| CONCENTRATION EN SEDIMENTS
!| TOB            |-->| CONTRAINTE DE FROTTEMENT AU FOND
!| TOCD           |-->| CONTRAINTE CRITIQUE DE DEPOT
!| WC             |-->| VITESSE DE CHUTE DU SEDIMENT
!| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3, NPOIN2, NPLAN, KLOG
      LOGICAL, INTENT(IN) :: SEDCO
!
!     BOTTOM
!     ****
!
!     BY POINTS
!     ----------
!
      INTEGER, INTENT(IN) :: LITABF(NPOIN2)
!
!     BY FACES
!     ---------
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOF(NPOIN2), BTABOF(NPOIN2)
!
!     FREE SURFACE
!     *******
!
!     BY POINTS
!     ----------
!
      INTEGER, INTENT(INOUT) :: LITABS(NPOIN2)
!
!     BY FACES
!     ---------
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOS(NPOIN2), BTABOS(NPOIN2)
!
!     OTHER ARRAYS
!
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: TA(NPOIN3), WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: GRADZFX(NPOIN2), GRADZFY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZSX(NPOIN2), GRADZSY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TOB(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: PDEPOT(NPOIN2), FLUER(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: TOCD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION NZ
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE DEPOSITION PROBABILITY
!
      IF(SEDCO) THEN
!
!       COHESIVE SEDIMENT
!
        DO I=1,NPOIN2
          IF(LITABF(I).EQ.KLOG) THEN
            PDEPOT(I)=MAX(1.D0-(TOB(I)/MAX(TOCD,1.D-6)),0.D0)
          ELSE
            PDEPOT(I)=0.D0
          ENDIF
        ENDDO
!
      ELSE
!
!       NON COHESIVE SEDIMENT : PDEPOT = 1.
!
        CALL OV('X=C     ',PDEPOT,PDEPOT,PDEPOT,1.D0,NPOIN2)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMMON COMPUTATION OF THE TRACER FLUX ON THE BOTTOM
!
      DO I=1,NPOIN2
!
!       ALREADY DONE IN LIMI3D (AND ALL BOTTOM POINTS ARE KLOG IF NOT
!                               MODIFIED BY USER IN BORD3D)
!       ATABOF(I)=0.D0
!       BTABOF(I)=0.D0
!
        IF(LITABF(I).EQ.KLOG) THEN
!
!         COMPONENT ALONG Z OF THE OUTGOING NORMAL VECTOR
!
          NZ = 1.D0+GRADZFX(I)**2+GRADZFY(I)**2
          NZ = -1.D0/SQRT(NZ)
!         WC
          ATABOF(I) = - WC(I) * PDEPOT(I) * NZ
          BTABOF(I) = - FLUER(I) * NZ
!
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITION AT THE FREE SURFACE
!
!     FLUX  = 0 (SETTLING VELOCITY FLUX + DIFFUSIVE FLUX)
!
!     ALREADY DONE IN LIMI3D !!
!
!     DO I=1,NPOIN2
!       ATABOS(I)=0.D0
!       BTABOS(I)=0.D0
!     ENDDO
!
!-----------------------------------------------------------------------
!
        RETURN
        END
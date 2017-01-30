!                    *****************
                     SUBROUTINE FLUSED
!                    *****************
!
     &(ATABOF , BTABOF , ATABOS , BTABOS  ,
     & LITABF , LITABS , TA     , WC      ,
     & X      , Y      , Z      , HN      ,
     & GRADZFX, GRADZFY, GRADZSX, GRADZSY ,
     & TOB    , FLUDPT , FLUER  , TOCD    ,
     & NPOIN3 , NPOIN2 , NPLAN  , KLOG    ,
     & HMIN   , SEDCO  , SETDEP , SEDNCO  ,
     & WCS    , MIXTE  , FLUDPTC, FLUDPTNC)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   03/06/2014
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
!history  C. VILLARET (HR-WALLINGFORD) & J-M HERVOUET (EDF LAB, LNHE)
!+        20/01/2014
!+        V7P0
!+   Erosion and deposition fluxes cancelled on tidal flats.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        03/06/2014
!+        V7P0
!+   Crushed planes treated with IPBOT.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM)
!| ATABOS         |<->| FOR BOUNDARY CONDITION (SURFACE) NOT USED
!| BTABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM)
!| BTABOS         |<->| FOR BOUNDARY CONDITION (SURFACE) NOT USED
!| FLUDPT         |<->| IMPLICIT DEPOSITION FLUX
!| FLUDPTC        |<->| IMPLICIT DEPOSITION FLUX FOR COHESIVE SEDIMENT
!| FLUDPTNC       |<->| IMPLICIT DEPOSITION FLUX FOR NON-COHESIVE SEDIMENT
!| FLUER          |<->| EROSION  FLUX FOR EACH 2D POINT
!| GRADZFX        |-->| NOT USED
!| GRADZFY        |-->| NOT USED
!| GRADZSX        |-->| NOT USED
!| GRADZSY        |-->| NOT USED
!| HMIN           |-->| MINIMUM WATER DEPTH TO PREVENT EROSION ON TIDAL FLATS
!| HN             |-->| WATER DEPTH AT TIME N
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LITABF         |-->| FOR BOUNDARY CONDITION BOTTOM
!| LITABS         |<->| FOR BOUNDARY CONDITION SURFACE (NOT USED)
!| MIXTE          |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| SEDCO          |-->| LOGICAL FOR COHESIVE SEDIMENT
!| SEDNCO         |-->| LOGICAL, SEDIMENT NON-COHESIVE OR NOT
!| SETDEP         |-->| CHOICE OF CONVECTION SCHEME FOR VERTICAL SETTLING
!| TA             |-->| CONCENTRATION OF SEDIMENTS
!| TOB            |<->| BOTTOM FRICTION
!| TOCD           |-->| CRITICAL SHEAR STRESS FOR SEDIMENT DEPOSITION
!| WC             |-->| SETTLING VELOCITY OF MUD
!| WCS            |-->| SETTLING VELOCITY OF SAND
!| X              |-->| COORDINATE
!| Y              |-->| COORDINATE
!| Z              |-->| COORDINATE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: IPBOT,SIGMAG,OPTBAN
      USE INTERFACE_TELEMAC3D, EX_FLUSED => FLUSED
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,NPOIN2,NPLAN,KLOG,SETDEP
      LOGICAL, INTENT(IN) :: SEDCO, SEDNCO, MIXTE
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
      DOUBLE PRECISION, INTENT(IN) :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: GRADZFX(NPOIN2), GRADZFY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZSX(NPOIN2), GRADZSY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TOB(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUDPT(NPOIN2), FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUDPTC(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUDPTNC(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: TOCD
      DOUBLE PRECISION, INTENT(IN) :: WCS(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: HMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I3D
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE DEPOSITION PROBABILITY
!
      IF(SEDCO) THEN
!
!       COHESIVE SEDIMENT (Here FLUDPT >0)
!
        IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
          DO I=1,NPOIN2
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
!             DEPOSITION ON THE FIRST FREE PLANE WITH LOCAL VELOCITY
              I3D=I+IPBOT%I(I)*NPOIN2
              FLUDPT(I) = WC(I3D)*MAX(1.D0-TOB(I)/MAX(TOCD,1.D-6),0.D0)
            ELSE
!             TIDAL FLAT
              FLUDPT(I) = 0.D0
            ENDIF
          ENDDO
        ELSE
          DO I=1,NPOIN2
            FLUDPT(I) = WC(I)*MAX(1.D0-(TOB(I)/MAX(TOCD,1.D-6)),0.D0)
          ENDDO
        ENDIF
!
      ENDIF

      IF(SEDNCO) THEN
!
!       NON COHESIVE SEDIMENT
!
        IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
          DO I=1,NPOIN2
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
!             DEPOSITION ON THE FIRST FREE PLANE WITH LOCAL VELOCITY
!             BEGINNING OF SPECIFIC TO THIS CASE
!             FLUDPT(I) = WC(I)
              FLUDPT(I) = 0.D0
!             END OF SPECIFIC TO THIS CASE
            ELSE
!             TIDAL FLAT
              FLUDPT(I) = 0.D0
            ENDIF
          ENDDO
        ELSE
          DO I=1,NPOIN2
!           BEGINNING OF SPECIFIC TO THIS CASE
!           FLUDPT(I) = WC(I)
            FLUDPT(I) = 0.D0
!           END OF SPECIFIC TO THIS CASE
          ENDDO
        ENDIF
!
      ENDIF

      IF(MIXTE) THEN

        IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
          DO I=1,NPOIN2
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
!             DEPOSITION ON THE FIRST FREE PLANE WITH LOCAL VELOCITY
              I3D = I+IPBOT%I(I)*NPOIN2
              FLUDPTC(I) = WC(I3D)*MAX(1.D0-TOB(I)/MAX(TOCD,1.D-6),0.D0)
              FLUDPTNC(I)= WCS(I)
              FLUDPT(I)  = FLUDPTC(I)+FLUDPTNC(I)
            ELSE
!             TIDAL FLAT
              FLUDPT(I)   = 0.D0
              FLUDPTC(I)  = 0.D0
              FLUDPTNC(I) = 0.D0
            ENDIF
          ENDDO
        ELSE
          DO I=1,NPOIN2
            FLUDPTC(I)  = WC(I)*MAX(1.D0-(TOB(I)/MAX(TOCD,1.D-6)),0.D0)
            FLUDPTNC(I) = WCS(I)
            FLUDPT(I)   = FLUDPTC(I)+FLUDPTNC(I)
          ENDDO
        ENDIF
!
      ENDIF
!

!-----------------------------------------------------------------------
!
!     COMPUTATION OF THE TRACER FLUX ON THE BOTTOM
!
      IF(SETDEP.EQ.1) THEN
!
!       USING HMIN TO CLIP EROSION (DIFFERENT FROM USING IPBOT)
        DO I=1,NPOIN2
          IF(HN(I).LE.HMIN) THEN
            FLUER(I) = 0.D0
          ENDIF
        ENDDO
!
        DO I=1,NPOIN2
          IF(LITABF(I).EQ.KLOG) THEN
!           TOM : erosion and deposition are treated with advection
            ATABOF(I) = 0.D0
            BTABOF(I) = 0.D0
          ENDIF
        ENDDO
!
      ELSEIF(SIGMAG.OR.OPTBAN.EQ.1) THEN
!
        DO I=1,NPOIN2
          ATABOF(I) = 0.D0
          BTABOF(I) = 0.D0
          IF(LITABF(I).EQ.KLOG) THEN
!           NO EROSION AND DEPOSITION ON TIDAL FLATS
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
              ATABOF(I) = -FLUDPT(I)
              BTABOF(I) =  FLUER(I)
            ENDIF
          ENDIF
        ENDDO
!
      ELSE
!
        DO I=1,NPOIN2
          IF(LITABF(I).EQ.KLOG) THEN
!           NZ = 1.D0+GRADZFX(I)**2+GRADZFY(I)**2
!           NZ = -1.D0/SQRT(NZ)
!           WC
!           ATABOF(I) = - WC(I) * PDEPOT(I) * NZ
!           BTABOF(I) = - FLUER(I) * NZ
!           JMH: BEWARE, IN DIFF3D NZ IS CONSIDERED AS -1.
!                HENCE WRONG FORMULA BELOW IS ACTUALLY CORRECT
            ATABOF(I) = -FLUDPT(I)
            BTABOF(I) =  FLUER(I)
          ENDIF
        ENDDO
!
      ENDIF
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


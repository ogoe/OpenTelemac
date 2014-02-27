!                    *****************
                     SUBROUTINE FLUSED
!                    *****************
!
     &(ATABOF , BTABOF , ATABOS , BTABOS ,
     & LITABF , LITABS , TA     , WC     ,
     & X      , Y      , Z      , HN     ,
     & GRADZFX, GRADZFY, GRADZSX, GRADZSY,
     & TOB    , FLUDPT , FLUER  , TOCD   ,
     & NPOIN3 , NPOIN2 , NPLAN  , KLOG   , 
     & HMIN, SEDCO, SETDEP)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM) 
!| ATABOS         |<->| FOR BOUNDARY CONDITION (SURFACE) NOT USED
!| BTABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM) 
!| BTABOS         |<->| FOR BOUNDARY CONDITION (SURFACE) NOT USED
!| EXPSETDEP      |-->| EXPLICIT DEPOSITION SCHEME 
!| FLUDPT         |-->| IMPLICIT DEPOSITION FLUX
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
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| PDEPOT         |<->| PROBABILITY OF DEPOSIT FOR EACH 2D POINT
!| SEDCO          |-->| LOGICAL FOR COHESIVE SEDIMENT
!| SETDEP         |-->| CHOICE OF CONVECTION SCHEME FOR VERTICAL SETTLING
!| TA             |-->| CONCENTRATION OF SEDIMENTS
!| TOB            |<->| BOTTOM FRICTION
!| TOCD           |-->| CRITICAL SHEAR STRESS FOR SEDIMENT DEPOSITION 
!| WC             |-->| SETTLING VELOCITY
!| X              |-->| COORDINATE
!| Y              |-->| COORDINATE
!| Z              |-->| COORDINATE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: NPTFR2,NBOR2,LIHBOR,
     &                                  IPBOT,SIGMAG,OPTBAN
      USE DECLARATIONS_TELEMAC, ONLY: KENT, KSORT
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3, NPOIN2, NPLAN, KLOG
      LOGICAL, INTENT(IN) :: SEDCO
      INTEGER, INTENT(IN) :: SETDEP
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
      DOUBLE PRECISION, INTENT(INOUT) :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: GRADZFX(NPOIN2), GRADZFY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZSX(NPOIN2), GRADZSY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TOB(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUDPT(NPOIN2), FLUER(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: TOCD
      DOUBLE PRECISION, INTENT(IN) :: HMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IPTFR,I3D,IPLAN
!     DOUBLE PRECISION NZ
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
        DO I=1,NPOIN2
          FLUDPT(I)= WC(I)*MAX(1.D0-(TOB(I)/MAX(TOCD,1.D-6)),0.D0)
        ENDDO
!
      ELSE
!
!       NON COHESIVE SEDIMENT : PDEPOT = 1.
!
        DO I=1,NPOIN2
          FLUDPT(I)= WC(I)
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CORRECTION OF EROSION FLUXES ON TIDAL FLATS
!     NOTE JMH: HMIN NOT DONE FOR THAT... 
!
      DO I=1,NPOIN2
        IF(HN(I).LE.HMIN) THEN
          FLUER(I)= 0.D0
        ENDIF            
      ENDDO
!
!-----------------------------------------------------------------------
!
!     PREVENTING EROSION AND DEPOSITION ON THE EXIT BOUNDARY
!     (Fixed water depth)
!
      DO IPTFR = 1,NPTFR2
        IF(LIHBOR%I(IPTFR).EQ.KENT) THEN
          I = NBOR2%I(IPTFR)
          FLUDPT(I)=0.D0
          FLUER(I)=0.D0
          DO IPLAN=1, NPLAN
            I3D= I + (IPLAN-1)*NPOIN2
            WC(I3D)=0.D0
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     COMMON COMPUTATION OF THE TRACER FLUX ON THE BOTTOM
!
      IF(SETDEP.EQ.1) THEN
!
        DO I=1,NPOIN2
          IF(LITABF(I).EQ.KLOG) THEN
!           TOM : erosion and deposition are treated in MURD3D_POS  
            ATABOF(I) = 0.D0
            BTABOF(I) = 0.D0     
          ENDIF    
        ENDDO
!
      ELSEIF(SIGMAG.OR.OPTBAN.EQ.1) THEN
!
        DO I=1,NPOIN2
          ATABOF(I)=0.D0
          BTABOF(I)=0.D0
          IF(LITABF(I).EQ.KLOG) THEN
!           NO EROSION AND DEPOSITION ON TIDAL FLATS
            IF(IPBOT%I(I).NE.NPLAN-1) THEN
              ATABOF(I) = - FLUDPT(I)  
              BTABOF(I) = FLUER(I) 
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
            ATABOF(I) = - FLUDPT(I) 
            BTABOF(I) = FLUER(I)         
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

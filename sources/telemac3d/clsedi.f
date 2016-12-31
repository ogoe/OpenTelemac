!                    *****************
                     SUBROUTINE CLSEDI
!                    *****************
!
     &( ATABOF , BTABOF  , ATABOS , BTABOS  , TA     ,
     &  WC     , GRADZFX , GRADZFY, GRADZSX , GRADZSY,
     &  X      , Y       , Z      , HN      , DELTAR ,
     &  TOB    , DENSI   , TRA03  , EPAI    , CFDEP  ,
     &  CONC   , HDEP    , FLUER  , FLUDPT  , LITABF ,
     &  LITABS , KLOG    , NPOIN3 , NPOIN2  , NPLAN  ,
     &  NCOUCH , ITURBV  , DT     , RHO0    ,  RHOS  ,
     &  TOCD   , MPART   , TOCE   , UETCAR  , GRAV   ,
     &  SEDCO  , DMOY    , CREF   , ZREF    , CF     ,
     &  AC     , KSPRATIO, ICR    , ICQ     , RUGOF  ,
     &  SETDEP , HMIN    , WCS    , EPAICO  , EPAINCO,
     &  MIXTE  , SEDNCO  , FLUDPTC, FLUDPTNC, FLUERC ,
     &  FLUERNC, NTRAC   , ITRAC)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    EXPRESSES THE BOUNDARY CONDITIONS FOR THE SEDIMENT,
!+                AT THE BOTTOM AND SURFACE (FOR COHESIVE SEDIMENT OR NOT).
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  CAMILLE LEQUETTE
!+        **/06/03
!+
!+
!
!history  C LE NORMANT (LNH)
!+        12/09/07
!+        V5P0
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |-->| CRITICAL SHIELDS PARAMETER
!| ATABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM)
!| ATABOS         |<->| FOR BOUNDARY CONDITION (SURFACE)
!| BTABOF         |<->| FOR BOUNDARY CONDITION (BOTTOM)
!| BTABOS         |<->| FOR BOUNDARY CONDITION (SURFACE)
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| CFDEP          |-->| MUD DEPOSITION CONCENTRATION (G/L)
!| CONC           |-->| MUD CONCENTRATION FOR EACH LAYER
!| CREF           |<->| EQUILIBRIUM NEAR-BED CONCENTRATION
!| DELTAR         |-->| DELTA RHO / RHO0 = (RHO-RHO0)/RHO0
!| DENSI          |<->| WATER DENSITY
!| DMOY           |-->| DIAMETRE MOYEN DES GRAINS
!| DT             |-->| HYDRODYNAMICS TIME STEP
!| EPAI           |<->| THICKNESS OF BOTTOM LAYERS IN
!|                |   | MATERIAL COORDINATES (EPAI=DZ/(1+IVIDE))
!| EPAICO         |-->| THICKNESS OF COHESIVE SUB-LAYER
!| EPAINCO        |-->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| SETDEP         |-->| SETTLING SCHEME (0 or 1)
!| FLUDPT         |<->| IMPLICIT DEPOSITION FLUX
!| FLUDPTC        |<->| IMPLICIT DEPOSITION FLUX FOR COHESIVE SEDIMENT
!| FLUDPTNC       |<->| IMPLICIT DEPOSITION FLUX FOR NON-COHESIVE SEDIMENT
!| FLUER          |<->| EROSION FLUX FOR POINTS IN 2D
!| FLUERC         |<->| EROSION FLUX FOR COHESIVE SEDIMENT IN 2D
!| FLUERNC        |<->| EROSION FLUX FOR NON-COHESIVE SEDIMENT IN 2D
!| GRADZFX        |-->| GRADIENT-X OF BOTTOM
!| GRADZFY        |-->| GRADIENT-Y OF BOTTOM
!| GRADZSX        |-->| GRADIENT-X OF SURFACE
!| GRADZSY        |-->| GRADIENT-Y OF SURFACE
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HDEP           |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| HMIN           |-->| THRESHOLD FOR EROSION FLUXES ON TIDAL FLATS
!| HN             |-->| WATER DEPTH AT TIME N
!| ICR            |-->| FLAG FOR THE SKIN FRICTION OPTION
!| ICQ            |-->| FLAG FOR THE REFERENCE CONCENTRATION FORMULA
!| ITRAC          |-->| INDEX OF THE ACTIVE TRACER
!| ITURBV         |-->| VERTICAL TURBULENCE MODEL
!| KLOG           |-->| CONVENTION FOR LOGARITHMIC WALL
!| KSPRATIO       |-->| RELATION BETWEEN SKIN BED ROUGHNESS AND SEDIMENT DIAMETER
!| LITABF         |<->| FOR BOUNDARY CONDITION BOTTOM
!| LITABS         |<->| FOR BOUNDARY CONDITION SURFACE
!| MIXTE          |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| MPART          |-->| EROSION COEFFICIENT (PARTHENIADES'S LAW)
!| NCOUCH         |-->| NUMBER OF LAYERS FOR THE COHESIVE MULTILAYER MODEL
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| NTRAC          |-->| NUMBER OF TRACERS
!| RHO0           |-->| WATER DENSITY (REFERENCE)
!| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
!| RUGOF          |-->| FRICTION COEFFICIENT
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| SEDNCO         |-->| LOGICAL, SEDIMENT NON-COHESIVE OR NOT
!| TA             |-->| SEDIMENT CONCENTRATION
!| TOB            |-->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOCD           |-->| CRITICAL DEPOSITION SHEAR STRESS
!| TOCE           |-->| CRITICAL EROSION SHEAR STRESS
!| TRA03          |<->| WORK STRUCTURE FOR USER
!| UETCAR         |-->| SQUARE OF THE FRICTION VELOCITY
!| WC             |-->| SETTLING VELOCITY OF MUD
!| WCS            |-->| SETTLING VELOCITY OF SAND
!| X              |-->| FIRST NODE COORDINATE
!| Y              |-->| SECOND NODE COORDINATE
!| Z              |-->| THIRD NODE COORDINATE
!| ZREF           |-->| VERTICAL COORDINATE OF THE HARD BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_CLSEDI => CLSEDI
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2,NPOIN3,KLOG,ICQ, ITRAC, NTRAC
      INTEGER, INTENT(IN) :: NCOUCH,ITURBV,NPLAN,ICR
!
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOF(NPOIN2), BTABOF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOS(NPOIN2), BTABOS(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: TA(NPOIN3),CFDEP
      DOUBLE PRECISION, INTENT(IN)  :: WC(NPOIN3), DELTAR(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: WCS(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TOB,CREF,ZREF,RUGOF
      TYPE(BIEF_OBJ), INTENT(IN)    :: DMOY,HN,CF
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: EPAICO(NPOIN2), EPAINCO(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NPOIN2,NCOUCH)
!
      DOUBLE PRECISION, INTENT(INOUT) :: DENSI(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA03(NPOIN2),UETCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2),FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUERC(NPOIN2), FLUERNC(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUDPT(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUDPTC(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUDPTNC(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: GRADZFX(NPOIN2),GRADZFY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZSX(NPOIN2),GRADZSY(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: DT, RHO0, RHOS, HMIN
      DOUBLE PRECISION, INTENT(IN) :: TOCD, GRAV
      DOUBLE PRECISION, INTENT(IN) :: MPART, TOCE(NPOIN2,NCOUCH)
!
      INTEGER, INTENT(INOUT)       :: LITABF(NPOIN2), LITABS(NPOIN2)
      LOGICAL, INTENT(IN)          :: SEDCO, MIXTE, SEDNCO
      INTEGER, INTENT(IN)          :: SETDEP
      DOUBLE PRECISION, INTENT(IN) :: AC, KSPRATIO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION KSP,A,ZERO,HCLIP,MU
      INTEGER IPOIN
!
!-----------------------------------------------------------------------
!
      ZERO = 1.D-6
!
      IF (MIXTE) THEN
        DO IPOIN=1,NPOIN2
          FLUERC(IPOIN) = 0.D0
          FLUERNC(IPOIN)= 0.D0
        ENDDO
      ENDIF
!
      DO IPOIN=1,NPOIN2
!       COMPUTES THE FLUID DENSITY
        DENSI(IPOIN) = (DELTAR(IPOIN)+1.D0)*RHO0
!       COMPUTES THE STRESS AT THE BOTTOM
        TOB%R(IPOIN) = DENSI(IPOIN)*UETCAR(IPOIN)
      ENDDO
!
      IF(ICR.EQ.1) THEN
!
        DO IPOIN=1,NPOIN2
!         CORRECTION FOR SKIN FRICTION (SEE TOB_SISYPHE)
          KSP=KSPRATIO *DMOY%R(IPOIN)
          IF(CF%R(IPOIN).GT.ZERO.AND.HN%R(IPOIN).GT.KSP) THEN
            HCLIP=MAX(HN%R(IPOIN),KSP)
            A = 2.5D0*LOG(12.D0*HCLIP/KSP)
            MU =2.D0/(A**2*CF%R(IPOIN))
          ELSE
            MU=0.D0
          ENDIF
          TOB%R(IPOIN) = MU* TOB%R(IPOIN)
        ENDDO
!
      ENDIF
!
!      -----COMPUTES THE EXPLICIT EROSION FLUX-----
!
      IF(SEDCO) THEN
!
        CALL ERODC(CONC,EPAI,FLUER,TOB%R,DENSI,
     &             MPART,DT,NPOIN2,NCOUCH,TOCE,
     &             HN%R,HMIN,MIXTE,EPAICO)
!
      ELSEIF(SEDNCO) THEN
!
        CALL ERODNC(CFDEP,WC,HDEP,FLUER,TOB,DT,
     &              NPOIN2,NPOIN3,KSPRATIO,AC,RHOS,RHO0,HN,
     &              GRAV,DMOY,CREF,ZREF,CF,ICQ,RUGOF,Z,UETCAR,
     &              SETDEP,EPAINCO,MIXTE)
!
      ELSEIF(MIXTE) THEN
!
        CALL ERODC(CONC,EPAI,FLUERC,TOB%R,DENSI,
     &             MPART,DT,NPOIN2,NCOUCH,TOCE,
     &             HN%R,HMIN,MIXTE,EPAICO)
!
        CALL ERODNC(CFDEP,WCS,HDEP,FLUERNC,TOB,DT,
     &              NPOIN2,NPOIN3,KSPRATIO,AC,RHOS,RHO0,HN,
     &              GRAV,DMOY,CREF,ZREF,CF,ICQ,RUGOF,Z,UETCAR,
     &              SETDEP,EPAINCO,MIXTE)
!
        DO IPOIN=1,NPOIN2
          FLUER(IPOIN) = FLUERNC(IPOIN) + FLUERC(IPOIN)
        ENDDO
!
      ENDIF

!      -----WRITES THE BOUNDARY CONDITIONS AT THE BOTTOM / SURFACE-----
!      -----                FOR THE SEDIMENT                      -----
!
      CALL FLUSED(ATABOF , BTABOF , ATABOS , BTABOS  ,
     &            LITABF , LITABS , TA     , WC      ,
     &            X      , Y      , Z      , HN%R    ,
     &            GRADZFX, GRADZFY, GRADZSX, GRADZSY ,
     &            TOB%R  , FLUDPT , FLUER  , TOCD    ,
     &            NPOIN3 , NPOIN2 , NPLAN  , KLOG    ,
     &            HMIN   , SEDCO  , SETDEP , SEDNCO  ,
     &            WCS    , MIXTE  , FLUDPTC, FLUDPTNC)
!
!-----------------------------------------------------------------------
!


      IF(MIXTE.AND.ITRAC.EQ.NTRAC)THEN
        DO IPOIN=1,NPOIN2
          ATABOF(IPOIN) = -FLUDPTC(IPOIN)
          BTABOF(IPOIN) =  FLUERC(IPOIN)
        ENDDO
      ELSEIF(MIXTE.AND.(ITRAC.EQ.NTRAC-1))THEN
        DO IPOIN=1,NPOIN2
          ATABOF(IPOIN) = -FLUDPTNC(IPOIN)
          BTABOF(IPOIN) =  FLUERNC(IPOIN)
        ENDDO
      ENDIF


      RETURN
      END

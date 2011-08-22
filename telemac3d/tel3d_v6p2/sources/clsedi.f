!                    *****************
                     SUBROUTINE CLSEDI
!                    *****************
!
     &( ATABOF , BTABOF , ATABOS , BTABOS , TA     ,
     &  WC     , GRADZFX, GRADZFY, GRADZSX, GRADZSY,
     &  X      , Y      , Z      , HN     , DELTAR ,
     &  TOB    , DENSI  , TRA03  ,  IVIDE  , EPAI   ,
     &  CONC   , HDEP   , FLUER  , PDEPOT , LITABF ,
     &  LITABS , KLOG   , NPOIN3 , NPOIN2 , NPLAN  ,
     &  NPFMAX , NCOUCH , NPF    , ITURBV , DT     , RHO0   ,
     &  RHOS   , CFDEP  , TOCD   , MPART  , TOCE   , TASSE  ,
     &  GIBSON , PRIVE  , UETCAR ,
     &  GRAV   , SEDCO  , DMOY   , CREF   ,ZREF, CF,
     &  AC     , KSPRATIO,ICR,ICQ,RUGOF)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
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
!| DENSI          |-->| WATER DENSITY
!| DMOY           |-->| DIAMETRE MOYEN DES GRAINS
!| DT             |-->| HYDRODYNAMICS TIME STEP 
!| EPAI           |<->| THICKNESS OF BOTTOM LAYERS IN 
!|                |   | MATERIAL COORDINATES (EPAI=DZ/(1+IVIDE))
!| FLUER          |<--| EROSION FLUX FOR POINTS IN 2D
!| GIBSON         |-->| LOGICAL, FOR GIBSON'S MODEL
!| GRADZFX        |-->| GRADIENT-X OF BOTTOM 
!| GRADZFY        |-->| GRADIENT-Y OF BOTTOM 
!| GRADZSX        |-->| GRADIENT-X OF SURFACE  
!| GRADZSY        |-->| GRADIENT-Y OF SURFACE   
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HDEP           |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER) 
!| HN             |-->| WATER DEPTH AT TIME N
!| ICQ            |-->| FLAG FOR THE REFERENCE CONCENTRATION FORMULA
!| ITURBV         |-->| VERTICAL TURBULENCE MODEL
!| IVIDE          |<->| VOID RATIO
!| KLOG           |-->| CONVENTION FOR LOGARITHMIC WALL
!| KSPRATIO       |-->| RELATION BETWEEN SKIN BED ROUGHNESS AND SEDIMENT DIAMETER
!| LITABF         |<->| FOR BOUNDARY CONDITION BOTTOM 
!| LITABS         |<->| FOR BOUNDARY CONDITION SURFACE
!| MPART          |-->| EROSION COEFFICIENT (PARTHENIADES'S LAW)
!| NCOUCH         |-->| NUMBER OF LAYERS FOR THE COHESIVE MULTILAYER MODEL 
!| NPF            |-->| NUMBER OF POINTS WITHIN THE BED
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES 
!|                |   | FOR THE GIBSON'S MODEL
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS 
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| PDEPOT         |<--| PROBABILITY OF DEPOSITION FOR EVERY POINT 2D
!| PRIVE          |-->| BLOCK OF PRIVATE ARRAYS FOR USER 
!| RHO0           |-->| WATER DENSITY (REFERENCE)
!| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
!| RUGOF          |-->| FRICTION COEFFICIENT
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT 
!| TA             |-->| SEDIMENT CONCENTRATION
!| TASSE          |-->| LOGICAL, CONSOLIDATION TAKEN INTO ACCOUNT OR NOT
!| TOB            |-->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOCD           |-->| CRITICAL DEPOSITION SHEAR STRESS 
!| TOCE           |-->| CRITICAL EROSION SHEAR STRESS 
!| TRA03          |<->| WORK STRUCTURE FOR USER
!| UETCAR         |-->| SQUARE OF THE FRICTION VELOCITY
!| WC             |-->| SETTLING VELOCITY OF SEDIMENT
!| X,Y,Z          |-->| NODE COORDINATES 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_CLSEDI => CLSEDI
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN2,NPOIN3,KLOG,NPFMAX,ICQ
      INTEGER, INTENT(IN) :: NCOUCH,ITURBV,NPLAN,ICR
!
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOF(NPOIN2), BTABOF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOS(NPOIN2), BTABOS(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: WC(NPOIN3), DELTAR(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: PRIVE,TOB,CREF,ZREF,RUGOF
      TYPE(BIEF_OBJ), INTENT(IN)    :: DMOY,HN,CF
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NCOUCH)
!
      DOUBLE PRECISION, INTENT(INOUT) :: DENSI(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA03(NPOIN2),UETCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2),FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: PDEPOT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZFX(NPOIN2),GRADZFY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZSX(NPOIN2),GRADZSY(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: DT    , RHO0 , RHOS
      DOUBLE PRECISION, INTENT(IN) :: CFDEP , TOCD , GRAV
      DOUBLE PRECISION, INTENT(IN) :: MPART , TOCE
!
      INTEGER, INTENT(INOUT) :: LITABF(NPOIN2), LITABS(NPOIN2)
      INTEGER, INTENT(INOUT) :: NPF(NPOIN2)
!
      LOGICAL, INTENT(IN)          :: TASSE , GIBSON , SEDCO
      DOUBLE PRECISION, INTENT(IN) :: AC, KSPRATIO
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      DOUBLE PRECISION KSP,A,C,ZERO,HCLIP,MU
      INTEGER IPOIN,I
!
!-----------------------------------------------------------------------
!
      ZERO = 1.D-6
!
      DO IPOIN=1,NPOIN2
C       COMPUTES THE FLUID DENSITY
        DENSI(IPOIN) = (DELTAR(IPOIN)+1.D0)*RHO0
C       COMPUTES THE STRESS AT THE BOTTOM
        TOB%R(IPOIN) = DENSI(IPOIN)*UETCAR(IPOIN)
      ENDDO
!
      IF(ICR.EQ.1) THEN
!
        DO IPOIN=1,NPOIN2
C         CORRECTION FOR SKIN FRICTION (SEE TOB_SISYPHE)
          KSP=KSPRATIO *DMOY%R(IPOIN)
          IF(CF%R(IPOIN) > ZERO.AND.HN%R(IPOIN).GT.KSP) THEN
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
C      -----COMPUTES THE EXPLICIT EROSION FLUX-----
!
      IF(SEDCO) THEN
!
        IF(TASSE) THEN
!
          CALL ERODC(CONC,EPAI,FLUER,TOB%R,DENSI,
     &               MPART,DT,NPOIN2,NCOUCH)
!
        ELSE
!
          CALL ERODE(IVIDE,EPAI,HDEP,FLUER,TOB%R,DENSI,
     &               NPOIN2,NPFMAX,NPF,MPART,TOCE,
     &               CFDEP,RHOS,DT,GIBSON)
!
        ENDIF

      ELSE
!
          CALL ERODNC(CFDEP,WC,HDEP,FLUER,TOB,DT,
     &                NPOIN2,NPOIN3,KSPRATIO,AC,RHOS,RHO0,HN,
     &                GRAV,DMOY,CREF,ZREF,CF,ICQ,RUGOF)
!
      ENDIF
!
C      -----WRITES THE BOUNDARY CONDITIONS AT THE BOTTOM / SURFACE-----
C      -----                FOR THE SEDIMENT                      -----
!
      CALL FLUSED(ATABOF , BTABOF , ATABOS , BTABOS ,
     &            LITABF , LITABS , TA     , WC     ,
     &            X      , Y      , Z      , HN%R   ,
     &            GRADZFX, GRADZFY, GRADZSX, GRADZSY,
     &            TOB%R  , PDEPOT , FLUER  , TOCD   ,
     &            NPOIN3 , NPOIN2 , NPLAN  , KLOG   , SEDCO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

!                    *****************
                     SUBROUTINE ERODNC
!                    *****************
!
     &(CFDEP  , WCS    , HDEP     , FLUER  , TOB   , DT    ,
     & NPOIN2 , NPOIN3 , KSPRATIO , AC     , RHOS  , RHO0  , HN ,
     & GRAV   , DMOY   , CREF     , ZREF   , CF    , ICQ   ,RUGOF,
     & Z      , UETCAR , SETDEP   , EPAINCO, MIXTE)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    MODELS EROSION FOR NON-COHESIVE SEDIMENTS.
!
!history  CAMILLE LEQUETTE
!+        06/06/2003
!+        V5P3
!+   First version.
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        05/05/2014
!+        V7P0
!+   New extrapolation of Rouse profile removed. It spoils the test case
!+   depot, which is so far the only official test case of Telemac-3D
!+   with erosion and deposition. This must be clarified.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!+   WC changed into WCS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |-->| CRITICAL SHIELDS PARAMETER
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT (NOT USED)
!| CFDEP          |-->| CONCENTRATION OF FRESH DEPOSIT DES DEPOTS FRAIS
!| CREF           |<->| EQUILIBRIUM CONCENTRATION
!| DMOY           |-->| MEAN DIAMETER OF GRAINS
!| DT             |-->| TIME STEP
!| EPAINCO        |-->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| FLUER          |<->| EROSION  FLUX
!| GRAV           |-->| GRAVITY ACCELERATION
!| HDEP           |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| HN             |-->| WATER DEPTH AT TIME N
!| ICQ            |-->| FLAG FOR REFERENCE CONCENTRATION FORMULA
!| KSPRATIO       |-->| NOT USED
!| MIXTE          |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| RHO0           |-->| WATER DENSITY AT REFERENCE CONCENTRATION
!| RHOS           |-->| SEDIMENT DENSITY
!| RUGOF          |<->| FRICTION COEFFICIENT ON THE BOTTOM
!| SETDEP         |-->| OPTION FOR THE TREATMENT OF SETTLING VELOCITY
!| TOB            |-->| BOTTOM FRICTION
!| UETCAR         |-->| SQUARE OF THE FRICTION VELOCITY
!| WCS            |-->| SETTLING VELOCITY FOR SAND
!| Z              |-->| NODE COORDINATES
!| ZREF           |<->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY: KARMAN,PRANDTL,FICT
!
      USE INTERFACE_TELEMAC3D, EX_ERODNC => ERODNC
!     TRIGGERS A PGI COMPILER ERROR
      USE INTERFACE_SISYPHE,ONLY:SUSPENSION_FREDSOE,SUSPENSION_VANRIJN
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,ICQ,SETDEP
!
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2),FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: EPAINCO(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT,CFDEP,GRAV,RHOS,RHO0
      DOUBLE PRECISION, INTENT(IN)    :: KSPRATIO,AC
!
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: DMOY,TOB,CF,HN
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: CREF,ZREF,RUGOF
!
      DOUBLE PRECISION, INTENT(IN)    :: WCS(NPOIN3)

      LOGICAL, INTENT(IN)             :: MIXTE

      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN3), UETCAR(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,I
      DOUBLE PRECISION USTAR, ROUSE, ROUSE_Z, DELTAZ, QS
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
!  ---------------------------------------------------------------------
!  ------- COMPUTES THE REFERENCE CONCENTRATION CREF (IN G/L) ----------
!  ---------------------------------------------------------------------
!
!     CV depth to extrapolate the near bed concentrations (see also KEPLC)
!     For FV scheme use delta= Dz1/4
!     FE scheme          = Dz1/2
!
!     ZYSERMAN & FREDSOE (1994) (BY DEFAULT)
!
!     SO FAR DMOY IS A CONSTANT
!
      IF(ICQ.EQ.1) THEN
        CALL OS('X=CY    ', X=ZREF, Y=DMOY, C=2.D0)
        CALL SUSPENSION_FREDSOE(DMOY%R(1),TOB, NPOIN2,
     &                        GRAV,RHO0,RHOS,1.D-6,AC,CREF)
        CALL OS('X=CY    ', X=ZREF, Y=DMOY, C=2.D0)
      ELSEIF(ICQ.EQ.3) THEN
        CALL OS('X=CY    ', X=ZREF, Y=RUGOF, C=0.5D0)
        CALL SUSPENSION_VANRIJN(DMOY%R(1),TOB,NPOIN2,
     &                 GRAV,RHO0,RHOS,1.D-06,1.D-06,AC,CREF,ZREF)
      ENDIF
!
!     UNITS FOR CREF G/L, NOT LIKE IN SISYPHE
!
      CALL OS('X=CX    ',X=CREF,C=RHOS)
!
!     JMH ON 05/05/2014
!     Following lines put under condition of SETDEP=1, they change a
!     lot the test case depot (76 cm of deposition at the
!     entrance, while the bed should not evolve too much, as
!     we simulate a Rouse profile. Lines below are probably compatible
!     with FLUDPT done when SETDEP=1.
!
!     CV: Extrapolation of Rouse profile from ZREF to 1/2 or 1/4 of first grid mesh
!
      IF(SETDEP.EQ.1) THEN
!
        DO IPOIN =1,NPOIN2
          USTAR=MAX(SQRT(UETCAR(IPOIN)),1.D-6)
          ROUSE=PRANDTL*WCS(IPOIN)/KARMAN/USTAR
!         rouse profile extrapolation up to 1/4 of the first layer
          DELTAZ=(Z(IPOIN +NPOIN2)-Z(IPOIN))/FICT
          ROUSE_Z=ZREF%R(IPOIN)/(HN%R(IPOIN)-ZREF%R(IPOIN))
     &           *(HN%R(IPOIN)-DELTAZ)/DELTAZ
          CREF%R(IPOIN)=CREF%R(IPOIN)*ROUSE_Z**ROUSE
        ENDDO
!
      ENDIF
!
!  ------------------------------------------------------------
!  -----------------     EROSION STEP    ----------------------
!  ------------------------------------------------------------
!
      IF(MIXTE) THEN

        DO I=1,NPOIN2
!
        FLUER(I)= WCS(I)*CREF%R(I)
        QS=CFDEP*EPAINCO(I)
        FLUER(I)=MIN(FLUER(I),QS/DT)
!
        ENDDO

      ELSE

        DO I=1,NPOIN2
!
!       COMPUTES THE EROSION FLUX
!
        FLUER(I)= WCS(I)*CREF%R(I)
!
!       QUANTITY OF SOLID IN THE LAYER BEFORE EROSION
!
!       CFDEP IN KG/M3 ( ~ 0.65 RHOS )
        QS=CFDEP*HDEP(I)
!
!       LAYER THICKNESS AFTER EROSION
!
!CV     HDEP(I)=MAX(0.D0,HDEP(I)-(FLUER(I)*DT/CFDEP))
!
!       LIMITS THE EROSION FLUX
!
!       BEGINNING OF SPECIFIC TO THIS CASE
!       FLUER(I)=MIN(FLUER(I),QS/DT)
        FLUER(I)=0.D0
!       END OF SPECIFIC TO THIS CASE
!
      ENDDO
!
      ENDIF
!-----------------------------------------------------------------------
!
      RETURN
      END


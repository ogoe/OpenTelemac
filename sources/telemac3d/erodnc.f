
!                    *****************
                     SUBROUTINE ERODNC
!                    *****************
!
     &(CFDEP  , WC     , HDEP     , FLUER , TOB   , DT    ,
     & NPOIN2 , NPOIN3 , KSPRATIO , AC    , RHOS  , RHO0  , HN ,
     & GRAV   , DMOY   , CREF     , ZREF  , CF    , ICQ   ,RUGOF,
     & Z, UETCAR)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODELS EROSION
!+                FOR NON-COHESIVE SEDIMENTS.
!
!history  CAMILLE LEQUETTE
!+        **/06/2003
!+
!+
!
!history  J.-M. HERVOUET
!+        12/09/2007
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
!| AC             |-->| CRITICAL SHIELDS PARAMETER
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT (NOT USED)
!| CFDEP          |-->| CONCENTRATION OF FRESH DEPOSIT DES DEPOTS FRAIS
!| CREF           |<->| EQUILIBRIUM CONCENTRATION
!| DMOY           |-->| MEAN DIAMETER OF GRAINS
!| DT             |-->| TIME STEP
!| SETDEP         |-->| EXPLICIT SETTLING (INTEGER)
!| FLUER          |<->| EROSION  FLUX
!| GRAV           |-->| GRAVITY ACCELERATION
!| HDEP           |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| HN             |-->| WATER DEPTH AT TIME N
!| ICQ            |-->| FLAG FOR REFERENCE CONCENTRATION FORMULA
!| KSPRATIO       |-->| NOT USED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| RHO0           |-->| WATER DENSITY AT REFERENCE CONCENTRATION
!| RHOS           |-->| SEDIMENT DENSITY
!| RUGOF          |<->| FRICTION COEFFICIENT ON THE BOTTOM
!| TOB            |-->| BOTTOM FRICTION
!| UETCAR         |-->| SQUARE OF THE FRICTION VELOCITY
!| WC             |-->| SETTLING VELOCITY
!|     Z          |-->| NODE COORDINATES 
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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3, ICQ
!
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2),FLUER(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: WC(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT,CFDEP,GRAV,RHOS,RHO0
      DOUBLE PRECISION, INTENT(IN)    :: KSPRATIO,AC
!
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: DMOY,TOB,CF,HN
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: CREF,ZREF,RUGOF
!      
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN3), UETCAR(NPOIN2)
C 
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN
      DOUBLE PRECISION USTAR, ROUSE, ROUSE_Z, DELTAZ
C
      INTEGER I
      DOUBLE PRECISION QS
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
!  ---------------------------------------------------------------------
!  ------- COMPUTES THE REFERENCE CONCENTRATION CREF (IN G/L) ----------
!  ---------------------------------------------------------------------
! CV depth to extrapolate the near bed concentrations (see also KEPLC)
! For FV scheme use delta= Dz1/4
!     FE scheme          = Dz1/2
!
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
! CV: Extrapolation of Rouse profile from ZREF to 1/2 or 1/4 of first grid mesh
!
        do IPOIN =1,NPOIN2
           USTAR=MAX(SQRT(UETCAR(IPOIN)),1.D-6) 
!
           ROUSE=PRANDTL*WC(IPOIN)/KARMAN/USTAR
          ! rouse profile extrapolation up to 1/4 of the first layer
!          DELTAZ=(MESH3D%Z%R(IPOIN +NPOIN2)-MESH3D%Z%R(IPOIN))/4.D0
         DELTAZ=(Z(IPOIN +NPOIN2)-Z(IPOIN))/FICT
!
           ROUSE_Z=ZREF%R(IPOIN)/(HN%R(IPOIN)-ZREF%R(IPOIN))
     &       *(HN%R(IPOIN)-DELTAZ)/DELTAZ
            CREF%R(IPOIN)=CREF%R(IPOIN)*ROUSE_Z**ROUSE
        enddo            
!
!  ------------------------------------------------------------
!  -----------------     EROSION STEP    ----------------------
!  ------------------------------------------------------------
!
      DO I=1,NPOIN2
!
!       COMPUTES THE EROSION FLUX
!
!CV        FLUER(I)=-WC(I)*CREF%R(I)
        FLUER(I)= WC(I)*CREF%R(I)
!
!       QUANTITY OF SOLID IN THE LAYER BEFORE EROSION
!
!       CFDEP IN KG/M3 ( ~ 0.65 RHOS )
        QS=CFDEP*HDEP(I)
!
!       LAYER THICKNESS AFTER EROSION
!
!CV        HDEP(I)=MAX(0.D0,HDEP(I)-(FLUER(I)*DT/CFDEP))
!
!       LIMITS THE EROSION FLUX
!
        FLUER(I)=MIN(FLUER(I),QS/DT)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

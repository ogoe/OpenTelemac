!                    *****************
                     SUBROUTINE ERODNC
!                    *****************
!
     &(CFDEP  , WC     , HDEP     , FLUER , TOB   , DT    ,
     & NPOIN2 , NPOIN3 , KSPRATIO , AC    , RHOS  , RHO0  , HN ,
     & GRAV   , DMOY   , CREF     , CF )
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
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
!| AC             |---|
!| CF             |---|
!| CFDEP          |-->| CONCENTRATION DES DEPOTS FRAIS
!| CREF           |<->| CONCENTRATION DEQUILIBRE
!| DMOY           |---|
!| DT             |-->| PAS DE TEMPS HYDRAULIQUE
!| FLUER          |<--| VALEUR DU FLUX D'EROSION
!| GRAV           |-->| CONSTANTE GRAVITATIONNELLE
!| HDEP           |<->| EPAISSEUR DE LA COUCHE DES DEPOTS FRAIS
!| HN             |-->| HAUTEUR D'EAU A L'INSTANT N
!| KSPRATIO       |---|
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE2D
!| NPOIN3         |---|
!| RHO0           |---|
!| RHOS           |-->| DENSITE DU SEDIMENT
!| TOB            |-->| CONTRAINTE DE FROTTEMENT AU FOND
!| WC             |-->| VITESSE DE CHUTE DU SEDIMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_ERODNC => ERODNC
!     TRIGGERS A PGI COMPILER ERROR
!     USE INTERFACE_SISYPHE, ONLY : SUSPENSION_FREDSOE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3
!
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2),FLUER(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: WC(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT,CFDEP,GRAV,RHOS,RHO0
      DOUBLE PRECISION, INTENT(IN)    :: KSPRATIO,AC
!
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: DMOY,TOB,CF,HN
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: CREF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
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
!
!
!     ZYSERMAN & FREDSOE (1994) (BY DEFAULT)
!
!     SUBROUTINE FROM SISYPHE LIBRARY
! MODIF V6P0
! CORRECTION FOR SKIN FRICTION (CF)
! HERE TAKES TAUP = TOB
!      CALL SUSPENSION_FREDSOE(DMOY,CF, TOB,HN,NPOIN2,KSPRATIO,
!     &                        GRAV,RHO0,RHOS,1.D-6,AC,CREF,1.D-3)
!                                            ZERO          HMIN
!                                  TAUP
      CALL SUSPENSION_FREDSOE(DMOY,TOB, NPOIN2,
     &                        GRAV,RHO0,RHOS,1.D-6,AC,CREF)
!                                            ZERO
!
!     UNITS FOR CREF G/L, NOT LIKE IN SISYPHE
!
      CALL OS('X=CX    ',X=CREF,C=RHOS)
!
!  ------------------------------------------------------------
!  -----------------     EROSION STEP    ----------------------
!  ------------------------------------------------------------
!
      DO I=1,NPOIN2
!
!       COMPUTES THE EROSION FLUX
!
        FLUER(I)=-WC(I)*CREF%R(I)
!
!       QUANTITY OF SOLID IN THE LAYER BEFORE EROSION
!
!       CFDEP IN KG/M3 ( ~ 0.65 RHOS )
        QS=CFDEP*HDEP(I)
!
!       LAYER THICKNESS AFTER EROSION
!
        HDEP(I)=MAX(0.D0,HDEP(I)-(FLUER(I)*DT/CFDEP))
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
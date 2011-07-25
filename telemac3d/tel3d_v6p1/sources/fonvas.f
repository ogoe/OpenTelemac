!                    *****************
                     SUBROUTINE FONVAS
!                    *****************
!
     &(IVIDE  , EPAI   , CONC  , TREST  , TEMP   , HDEP  ,
     & PDEPOT , ZR     , ZF    , TA     , WC     , TRA01 ,
     & TRA02  , TRA03  , NPOIN2, NPOIN3 , NPFMAX , NCOUCH,
     & NPF    , LT     , DT     , DTC   , GRAV   , RHOS  ,
     & CFMAX  , CFDEP  , EPAI0 , TASSE  , GIBSON )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODELS THE MUD BED EVOLUTION.
!
!history  C LE NORMANT (LNH)
!+        13/05/92
!+        V5P1
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
!| CFDEP          |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| CFMAX          |<->| CONCENTRATION OF CONSOLIDATED MUD (G/L)
!| CONC           |-->| MUD BED LAYER CONCENTRATION
!|                |   | (MULTILAYER MODEL)
!| DT             |-->| HYDRAULIC TIME STEP
!| DTC            |-->| TIME STEP FOR CONSOLIDATION PHENOMENON
!| EPAI           |<->| THICKNESS OF SOLID FRACTION OF THE BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS
!| EPAI0          |<->| REFERENCE THICKNESS TP CREATE NEW ELEMENTS
!| GIBSON         |-->| LOGICAL FOR GIBSON SETTLING MODEL
!| GRAV           |-->| GRAVITY ACCELERATION
!| HDEP           |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| IVIDE          |<->| VOID RATIO
!|                |   | (GIBSON MODEL ONLY)
!| LT             |-->| CURRENT TIME STEP NUMBER
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MULTILAYER SETTLING MODEL)
!| NPF            |-->| NUMBER OF POINTS OF THE BOTTOM ON ONE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES
!|                |   | DISCRETIZATION OF MUD BED (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF POINTS  (2D MESH)
!| NPOIN3         |-->| NUMBER OF POINTS  (3D MESH)
!| PDEPOT         |<->| PROBABILITY OF DEPOSIT
!| RHOS           |-->| SEDIMENT DENSITY
!| TA             |-->| ACTIVE TRACOR
!| TASSE          |-->| MULTILAYER SETTLING MODEL LOGICAL
!| TEMP           |<->| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL)
!| TRA01          |<->| WORK ARRAY
!| TRA02          |<->| WORK ARRAY
!| TRA03          |<->| WORK ARRAY
!| TREST          |<->| CONSOLIDATION TIME SCALE
!|                |   | (ONLY FOR MULTILAYER MODEL)
!| WC             |-->| SETTLING VELOCITY
!| ZF             |<->| BOTTOM ELEVATION
!| ZR             |-->| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_FONVAS => FONVAS
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) ::  LT, NPOIN2, NPOIN3
      INTEGER, INTENT(IN) ::  NPFMAX, NCOUCH
!
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TREST(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2), PDEPOT(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPFMAX-1,NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TA(NPOIN3), WC(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPFMAX,6)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPFMAX),TRA03(NPFMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: DT, RHOS, GRAV, DTC
      DOUBLE PRECISION, INTENT(INOUT) ::  CFMAX , CFDEP, EPAI0
!
      INTEGER, INTENT(INOUT) ::  NPF(NPOIN2)
!
      LOGICAL, INTENT(IN) :: TASSE, GIBSON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION C
      INTEGER IPOIN, IC
      INTRINSIC MOD
!
!=======================================================================
!
!     COMPUTES THE DEPOSITED QUANTITY (IN MATERIAL COORDINATES)
!
      IF(TASSE) THEN
        DO IPOIN=1,NPOIN2
          EPAI(NCOUCH,IPOIN)=EPAI(NCOUCH,IPOIN)-
     &         WC(IPOIN)*PDEPOT(IPOIN)*TA(IPOIN)*DT/CONC(NCOUCH)
        ENDDO
      ELSE
        DO IPOIN=1,NPOIN2
          HDEP(IPOIN)=HDEP(IPOIN)-
     &                WC(IPOIN)*PDEPOT(IPOIN)*TA(IPOIN)*DT/CFDEP
        ENDDO
      ENDIF
!
!     STARTS THE COMPUTATION OF BOTTOM ELEVATION
!
      CALL OV( 'X=Y     ', ZF, ZR, ZR, C, NPOIN2)
!
      IF (GIBSON) THEN
!
!     -----CHECKS THE TIMESTEP TO TAKE CONSOLIDATION-----
!     -----              INTO ACCOUNT               -----
!
         IF (MOD(LT*DT,DTC).LT.1.D-8) THEN
!
!     -----MANAGES THE DEPOSITED QUANTITY : CREATES-----
!     -----     NEW LAYERS NEAR THE MUDDY BED      -----
!
           CALL GESTDP( IVIDE  , EPAI   , HDEP    ,
     &                  NPOIN2 , NPFMAX , NPF     ,
     &                  EPAI0  , CFDEP  , RHOS    )
!
!     -----CONSOLIDATES THE MUDDY BED-----
!     -----    (GIBSON EQUATION)     -----
!
           CALL TASSEM( IVIDE , EPAI ,
     &                  NPOIN2, NPFMAX, NPF  ,
     &                  GRAV  , RHOS  , DTC  , CFMAX ,
     &                  TRA01 , TRA02 ,TRA03 )
!
         ENDIF
!
!     -----UPDATES THE BOTTOM ELEVATION-----
!
         CALL ACTUZF(IVIDE,EPAI,ZF,NPOIN2,NPFMAX,NPF)
!
      ELSEIF(TASSE) THEN
!
!     -----MODELS CONSOLIDATION (SIMPLE)-----
!
         IF (MOD(LT*DT,DTC).LT.1.D-8)
     &    CALL TASSEC( CONC   , EPAI , TREST , TEMP , DTC ,
     &                 NPOIN2 , NCOUCH )
!
!     -----UPDATES THE BOTTOM ELEVATION-----
!
          DO IPOIN = 1 , NPOIN2
            DO IC = 1 , NCOUCH
              ZF(IPOIN) = ZF(IPOIN) + EPAI(IC,IPOIN)
            ENDDO
            HDEP(IPOIN)=0.D0
          ENDDO
!
      ENDIF
!
      CALL OV( 'X=Y+Z   ' , ZF, ZF, HDEP, C, NPOIN2)
!
!=======================================================================
!
      RETURN
      END SUBROUTINE FONVAS

!                    *****************
                     SUBROUTINE IMPSED
!                    *****************
!
     &(IVIDE , EPAI  , CONC  , TEMP  , HDEP  , PDEPOT,
     & FLUER , ZR    , ZF    , TA    , WC    , X     ,
     & Y     , NPOIN2, NPOIN3, NPFMAX, NCOUCH, NPF   ,
     & LT    , RHOS  , CFMAX , CFDEP , EPAI0 ,
     & TASSE , GIBSON, PRIVE , LISPRD)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GENERATES A RESULT FILE THAT REPRESENTS GRAPHICALLY
!+                THE MUD BED EVOLUTION.
!
!history  C LE NORMANT (LNH)
!+        06/05/93
!+        V5P4
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
!| CFDEP          |-->| CONCENTRATION (G/L) OF FRESH DEPOSIT
!| CFMAX          |-->| CONCENTRATION (G/L) OF CONSOLIDATED MUD
!| CONC           |-->| CONCENTRATION OF MUD BED LAYER
!|                |   | (ONLY FOR MULTILAYER MODEL)
!| EPAI           |-->| THICKNESS OF SOLID BED LAYER
!| EPAI0          |-->| REFERENCE THICKNESS
!|                |   | FOR NEW GRID POINTS GENERATION
!| FLUER          |-->| EROSION FLUX
!| GIBSON         |-->| LOGICAL FOR GIBSON MODEL
!| HDEP           |-->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| IVIDE          |-->| VOID RATIO
!|                |   | (GIBSON MODEL ONLY)
!| LISPRD         |-->| TIME STEP FOR GRAPHICAL PRINTOUTS
!| LT             |-->| NUMBER OF TIME STEP
!| NCOUCH         |-->| NUMBER OF BED LAYERS
!|                |   | (MULTILAYER CONSOLIDATION MODEL)
!| NPF            |-->| NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES
!|                |   | WITHIN THE MUD BED (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| PDEPOT         |-->| PROBABILITY OF DEPOSITION
!| PRIVE          |<->| PRIVATE ARRAYS FOR USERS
!| RHOS           |-->| SEDIMENT DENSITY
!| TA             |-->| SUSPENDED SEDIMENT CONCENTRATION (G/L)
!| TASSE          |-->| LOGICAL FOR MULTILAYER CONSOLIDATION MODEL
!| TEMP           |-->| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL ONLY)
!| WC             |-->| SETTLING VELOCITY (M/S)
!| X              |-->| COORDINATES OF 2D MESH
!| Y              |-->| COORDINATES OF 2D MESH
!| ZF             |-->| BOTTOM LEVEL
!| ZR             |-->| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2,NPOIN3,NPFMAX,NCOUCH,LT,LISPRD
!
      DOUBLE PRECISION, INTENT(IN)  :: EPAI(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: HDEP(NPOIN2) , PDEPOT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)  :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: ZR(NPOIN2), ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN3), Y(NPOIN3)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: PRIVE
!
      DOUBLE PRECISION, INTENT(IN)  :: RHOS,CFMAX,CFDEP,EPAI0
!
      INTEGER, INTENT(IN) :: NPF(NPOIN2)
!
      LOGICAL, INTENT(IN) :: TASSE , GIBSON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC MOD
!
!=======================================================================
!
      IF (MOD(LT,LISPRD).NE.0) RETURN
!
!      PRINTOUTS SPECIFIED BY USER
!
      RETURN
      END SUBROUTINE IMPSED

!                    *****************
                     SUBROUTINE FSGRAD
!                    *****************
!
     &(GRADZS,ZFLATS,Z,ZF,IELM2H,MESH2D,MSK,MASKEL,UNSV2D,T2_01,
     & NPOIN2,OPTBAN,S)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FREE SURFACE GRADIENT, TAKING INTO
!+                ACCOUNT THE TREATMENT OF TIDAL FLATS.
!
!history  J-M HERVOUET (LNHE)
!+        28/07/2009
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
!| GRADZS         |<->| FREE SURFACE GRADIENT (BLOCK OF 2 COMPONENTS)
!| IELM2H         |-->| TYPE OF ELEMENT
!| MASKEL         |-->| ARRAY OF MASKS, PER ELEMENT
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE IS MASKING, MASKEL IS TO BE USED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| S              |-->| EMPTY BIEF_OBJ STRUCTURE
!| T2_01          |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASES
!| Z              |-->| Z COORDINATES OF THE 3D MESH
!| ZF             |-->| BOTTOM ELEVATION
!| ZFLATS         |<->| PIECE-WISE LINEAR FREE SURFACE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)                  :: IELM2H,NPOIN2,OPTBAN
      DOUBLE PRECISION, TARGET, INTENT(IN) :: Z(NPOIN2)
      LOGICAL, INTENT(IN)                  :: MSK
      TYPE(BIEF_OBJ), INTENT(INOUT)        :: GRADZS,ZFLATS,T2_01
      TYPE(BIEF_OBJ), INTENT(IN)           :: ZF,UNSV2D,S,MASKEL
      TYPE(BIEF_MESH), INTENT(INOUT)       :: MESH2D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVE_T2_01,ZZF
!
!-----------------------------------------------------------------------
!
      IF(OPTBAN.EQ.1) THEN
!
!       COMPUTES THE FREE SURFACE GRADIENT AS IN TELEMAC-2D
!
        CALL CRSL11(ZFLATS%R,Z,
     &              ZF%R,MESH2D%IKLE%I,MESH2D%NELEM,MESH2D%NELMAX)
        CALL VECTOR(GRADZS%ADR(1)%P,'=','GRADF          X',IELM2H,
     &              1.D0,ZFLATS,S,S,S,S,S,MESH2D,MSK,MASKEL)
        CALL VECTOR(GRADZS%ADR(2)%P,'=','GRADF          Y',IELM2H,
     &              1.D0,ZFLATS,S,S,S,S,S,MESH2D,MSK,MASKEL)
!
      ELSE
!
        SAVE_T2_01=>T2_01%R
        T2_01%R   =>Z
!
        CALL CPSTVC(ZF,T2_01)
!       THIS COPY IS REPLACED WITH T2_01%R POINTING TO Z
!       CALL OV('X=Y     ',T2_01%R,Z,T2_01%R,0.D0,NPOIN2)
        CALL VECTOR(GRADZS%ADR(1)%P,'=','GRADF          X',IELM2H,
     &              1.D0,T2_01,S,S,S,S,S,MESH2D,MSK,MASKEL)
        CALL VECTOR(GRADZS%ADR(2)%P,'=','GRADF          Y',IELM2H,
     &              1.D0,T2_01,S,S,S,S,S,MESH2D,MSK,MASKEL)
!
        T2_01%R=>SAVE_T2_01
!
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(GRADZS%ADR(1)%P,2,MESH2D)
        CALL PARCOM(GRADZS%ADR(2)%P,2,MESH2D)
      ENDIF
!
!     DIVISION BY INTEGRAL OF 2D BASES
!
      CALL OS('X=XY    ',X=GRADZS%ADR(1)%P,Y=UNSV2D)
      CALL OS('X=XY    ',X=GRADZS%ADR(2)%P,Y=UNSV2D)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

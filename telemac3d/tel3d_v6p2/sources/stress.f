!                    *****************
                     SUBROUTINE STRESS
!                    *****************
!
     & (SEM3D,OP,T2,T3,BFBORL,BFBORF,BFBORS,NPOIN2,NPOIN3,MESH2D,
     &  MESH3D,IELM3,IELM2H,IELM2V,SV,MSK,MASKBR,MASKEL)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ADDS TO OR REMOVES FROM SEM3D THE EXPLICIT STRESS
!+               (DEPENDING ON OP).
!
!history  J.M. HERVOUET (LNHE)
!+        20/07/05
!+        V5P6
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
!| BFBORF         |---| LOGARITHMIC LAW FOR COMPONENT ON THE BOTTOM:
!|                |---|  NU*DF/DN = AFBORF*U + BFBORF
!| BFBORL         |---| LOGARITHMIC LAW FOR COMPONENT ON THE
!|                |---| LATERAL BOUNDARIES:
!|                |---| NU*DF/DN = AFBORL*U + BFBORL
!| BFBORS         |---| LOGARITHMIC LAW FOR COMPONENT AT THE SURFACE:
!|                |---| NU*DF/DN = AFBORS*U + BFBORS
!| IELM2H         |-->| DISCRETISATION TYPE FOR 2D HORIZONTAL MESH
!| IELM2V         |-->| DISCRETISATION TYPE FOR 2D VERTICAL MESH
!| IELM3          |-->| TYPE OF ELEMENT IN 3D
!| MASKBR         |<->| 3D MASK ON LATERAL BOUNDARIES
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH2D         |-->| 2D MESH
!| MESH3D         |-->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| OP             |-->| 8 CHARACTERS STRING : 'X=X+Y   ' OR 'X=X-Y   '
!| SEM3D          |<->| RIGHT HAND SIDE OF EQUATION
!| SV             |-->| VOID STRUCTURE
!| T2             |-->| 2D WORK BIEF_OBJ STRUCTURES
!| T3             |-->| 3D WORK BIEF_OBJ STRUCTURES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: BFBORL,BFBORF,BFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKBR,MASKEL,SV
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3,T2
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D,MESH3D
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3
      INTEGER, INTENT(IN)             :: IELM2H,IELM2V,IELM3
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SEM3D
      LOGICAL, INTENT(IN)             :: MSK
      CHARACTER(LEN=8), INTENT(IN)    :: OP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LATERAL BOUNDARIES
!
      IF(BFBORL%TYPR.NE.'0') THEN
        CALL VECTOR(T3,'=','MASVEC          ',IELM2V,1.D0,BFBORL,
     &              SV,SV,SV,SV,SV,MESH3D,MSK,MASKBR)
        CALL OSDB(OP,SEM3D,T3,T3,1.D0,MESH3D)
      ENDIF
!
!     BOTTOM (IPBOT NOT TREATED HERE)
!
      IF(BFBORF%TYPR.NE.'0') THEN
        CALL VECTOR(T2,'=','MASVEC          ',IELM2H,1.D0,BFBORF,
     &              SV,SV,SV,SV,SV,MESH2D,MSK,MASKEL)
        CALL OV(OP,SEM3D%R(1:NPOIN2),T2%R,T2%R,0.D0,NPOIN2)
      ENDIF
!
!     FREE SURFACE
!
      IF(BFBORS%TYPR.NE.'0') THEN
        CALL VECTOR(T2,'=','MASVEC          ',IELM2H,1.D0,BFBORS,
     &              SV,SV,SV,SV,SV,MESH2D,MSK,MASKEL)
        CALL OV(OP,SEM3D%R(NPOIN3-NPOIN2+1:NPOIN3),
     &          T2%R,T2%R,0.D0,NPOIN2)
      ENDIF
!
!=======================================================================
!
      RETURN
      END

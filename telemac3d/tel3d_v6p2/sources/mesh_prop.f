!                    ********************
                     SUBROUTINE MESH_PROP
!                    ********************
!
     &(HPROP,HN,H,PROLIN,HAULIN,TETA,NSOUSI,ZPROP,
     & IPBOT,NPOIN2,NPLAN,OPTBAN,SIGMAG,OPT_HNEG,
     & MDIFF,MESH3D,VOLU3D,VOLU3DPAR,UNSV3D,MSK,MASKEL,IELM3)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE MESH FOR THE PROPAGATION STEP.
!
!note     THE BOTTOM OF ZPROP MUST BE DONE BEFORE.
!
!history  J-M HERVOUET (LNHE)
!+        20/05/2010
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
!| H              |-->| WATER DEPTH
!| HAULIN         |-->| MEAN DEPTH FOR LINEARISATION
!| HN             |-->| WATER DEPTH AT TIME N
!| HPROP          |<->| PROPAGATION DEPTH (DONE IN CVDFTR)
!| IELM3          |-->| TYPE OF ELEMENT
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MDIFF          |<->| MASS MATRIX
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NSOUSI         |-->| SUB-ITERATIONS NUMBER
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| OPT_HNEG       |-->| OPTION FOR NEGATIVE DEPTHS
!| PROLIN         |-->| CORRESPOND TO KEYWORD "LINEARIZED PROPAGATON"
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| TETA           |-->| SEMI-IMPLICITATION FOR H
!| UNSV3D         |<->| INVERSE OF VOLUME OF BASIS FUNCTIONS 
!| VOLU3D         |<->| INTEGRAL OF TEST FUNCTIONS IN 3D
!| VOLU3DPAR      |<->| VOLU3D FOR EACH DOMAIN IN PARALLEL MODE
!| ZPROP          |<->| Z DURING PROPAGATION
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
      INTEGER, INTENT(IN)            :: NSOUSI,NPOIN2,NPLAN,OPT_HNEG
      INTEGER, INTENT(IN)            :: OPTBAN,IELM3
      LOGICAL, INTENT(IN)            :: PROLIN,SIGMAG,MSK
      DOUBLE PRECISION, INTENT(IN)   :: TETA,HAULIN
      TYPE(BIEF_OBJ),   INTENT(IN)   :: HN,H,IPBOT,MASKEL
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: HPROP,ZPROP,MDIFF,UNSV3D,VOLU3D
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: VOLU3DPAR
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!-----------------------------------------------------------------------
!
      IF(PROLIN) THEN
        CALL OS( 'X=C     ' , X=HPROP , C=HAULIN    )
      ELSEIF(NSOUSI.EQ.1) THEN
        CALL OS( 'X=Y     ' , X=HPROP , Y=HN )
      ELSE
        CALL OS( 'X=CY    ' , X=HPROP , Y=HN , C=1.D0-TETA )
        CALL OS( 'X=X+CY  ' , X=HPROP , Y=H  , C= TETA )
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CLIPS HPROP
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.NE.2) THEN
        CALL OS('X=+(Y,C)',X=HPROP,Y=HPROP,C=0.D0)
      ENDIF
!
!     COMPUTES THE NEW MESH
!
      CALL CALCOT(ZPROP%R,HPROP%R)
!
!     COMPUTES THE REAL BOTTOM PLANE
!
      CALL PLANE_BOTTOM(IPBOT%I,ZPROP%R,NPOIN2,NPLAN,SIGMAG,OPTBAN,
     &                  HPROP%R)
!
!     COMPUTES THE INVERSE OF VOLUME OF BASIS FUNCTIONS 3D IN UNSV3D
!
!     ZPROP IS TEMPORARILY PUT IN MESH3D%Z
!
      SAVEZ     =>MESH3D%Z%R
      MESH3D%Z%R=>ZPROP%R
!
      IF(OPTBAN.EQ.1.OR.SIGMAG) THEN
!       OBTAINED BY LUMPING MASS-MATRIX (MASS-MATRIX WITH A TREATMENT
!                                        OF TIDAL FLATS)
        CALL MATRIX(MDIFF,'M=N     ','MATMAS          ',
     &              IELM3,IELM3,1.D0,
     &              ZPROP,ZPROP,ZPROP,
     &              ZPROP,ZPROP,ZPROP,MESH3D,MSK,MASKEL)
        CALL LUMP(VOLU3D,MDIFF,MESH3D,1.D0)
      ELSE
!       SIMPLEST WAY
        CALL VECTOR(VOLU3D,'=','MASBAS          ',IELM3,1.D0,ZPROP,
     &              ZPROP,ZPROP,ZPROP,ZPROP,ZPROP,MESH3D,MSK,MASKEL)
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        CALL OS('X=Y     ',X=VOLU3DPAR,Y=VOLU3D)
        CALL PARCOM(VOLU3DPAR,2,MESH3D)
        CALL OS('X=1/Y   ',X=UNSV3D,Y=VOLU3DPAR,
     &          IOPT=2,INFINI=0.D0,ZERO=1.D-6)
      ELSE
        CALL OS('X=1/Y   ',X=UNSV3D,Y=VOLU3D,
     &          IOPT=2,INFINI=0.D0,ZERO=1.D-6)
      ENDIF
!
!     RESTORES Z
!
      MESH3D%Z%R=>SAVEZ
!
!-----------------------------------------------------------------------
!
      RETURN
      END

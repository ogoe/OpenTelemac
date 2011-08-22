!                    *****************
                     SUBROUTINE VELRES
!                    *****************
!
     &(U,V,W,DP,PX,PY,PZ,MSK,MASKEL,MESH3D,S,IELM3,NPLAN,OPTBAN,
     & UNSV3D,NPOIN3,NPOIN2,SIGMAG,IPBOT)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FINAL, SOLENOIDAL VELOCITY FIELD (UE, VE, WE)
!+                GIVEN THE DYNAMIC PRESSURE P AND THE INTERMEDIATE
!+                VELOCITY FIELD (UP, VP, WP).
!code
!+                   DPART DP
!+   UE_I = UP_I +  -----------
!+                   DPART X_I
!+
!+   U,V,W ARE UP AT THE BEGINNING
!+   U,V,W ARE UE AT THE END
!+
!+
!+   I.E. REALISES THE FINAL VELOCITY PROJECTION STEP
!+   NOTE: PHYSICAL DYNAMIC PRESSURE IS DP  MULTIPLIED BY RHO0/DT
!+        (NEEDED FOR OUTPUTS)
!
!history  JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!+        12/98 - 04/99
!+
!+   NON-HYDROSTATIC VERSION
!
!history  JM HERVOUET (LNHE)
!+        21/05/2010
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
!| DP             |-->| HYDRODYNAMIC PRESSURE, TIMESTEP N+1
!| IELM3          |-->| TYPE OF ELEMENT
!| IPBOT          |-->| PLANE NUMBER OF LAST CRUSHED PLANE (0 IF NONE)
!| MASKEL         |<->| ELEMENT MASKING
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| MASKING LOGICAL FLAG
!|                |   | IF YES, THERE IS MASKED ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| PX             |<->| H-DYN. PRESSURE PARTIAL DERIVATIVES
!| PY             |<->| H-DYN. PRESSURE PARTIAL DERIVATIVES
!| PZ             |<->| H-DYN. PRESSURE PARTIAL DERIVATIVES
!| S              |-->|
!| SIGMAG         |-->| LOGICAL FOR GENERALISED SIGMA TRANSFORMATION
!| U              |<->| COMPONENT OF VELOCITY
!| UNSV3D         |-->| INVERSE OF VOLUME OF BASIS FUNCTIONS 
!| V              |<->| COMPONENT OF VELOCITY
!| W              |<->| COMPONENT OF VELOCITY
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
      INTEGER, INTENT(IN)            :: IELM3,NPLAN,OPTBAN,NPOIN3,NPOIN2
      TYPE(BIEF_OBJ),  INTENT(IN)    :: S,DP,UNSV3D
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: PX,PY,PZ
      DOUBLE PRECISION, INTENT(INOUT):: U(NPOIN3),V(NPOIN3),W(NPOIN3)
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH3D
      INTEGER, INTENT(IN)            :: IPBOT(NPOIN2)
      LOGICAL, INTENT(IN)            :: MSK,SIGMAG
!
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: MASKEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,I2D,I3D,I3DP
      CHARACTER(LEN=15) FORMUL
!
!-----------------------------------------------------------------------
! DYNAMIC PRESSURE DERIVATIVES AT NODES
!-----------------------------------------------------------------------
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
!       HERE GRADIENTS WITH FILTERING OF CRUSHED ELEMENTS
        FORMUL='GRADF 2        '
      ELSE
!       STANDARD GRADIENTS
        FORMUL='GRADF          '
      ENDIF
!
      CALL VECTOR(PX,'=',FORMUL//'X',IELM3,1.D0,DP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(PY,'=',FORMUL//'Y',IELM3,1.D0,DP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(PZ,'=',FORMUL//'Z',IELM3,1.D0,DP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM (PX, 2, MESH3D)
        CALL PARCOM (PY, 2, MESH3D)
        CALL PARCOM (PZ, 2, MESH3D)
      ENDIF
!
!-----------------------------------------------------------------------
! FINAL VELOCITY ( PROJECTION )
!-----------------------------------------------------------------------
!
      DO I3D=1,NPOIN3
        U(I3D)=U(I3D)-PX%R(I3D)*UNSV3D%R(I3D)
        V(I3D)=V(I3D)-PY%R(I3D)*UNSV3D%R(I3D)
        W(I3D)=W(I3D)-PZ%R(I3D)*UNSV3D%R(I3D)
      ENDDO
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        DO I2D=1,NPOIN2
          IF(IPBOT(I2D).GT.0) THEN
            I3DP=I2D+IPBOT(I2D)*NPOIN2
!           VALUE OF THE FIRST FREE POINT IS COPIED BELOW
            DO IP=0,IPBOT(I2D)-1
              I3D=I2D+IP*NPOIN2
              U(I3D)=U(I3DP)
              V(I3D)=V(I3DP)
              W(I3D)=W(I3DP)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

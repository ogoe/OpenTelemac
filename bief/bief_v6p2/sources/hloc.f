!                    ***************
                     SUBROUTINE HLOC
!                    ***************
!
     &(NPOIN,NSEG,NPTFR,NUBO,NBOR,VNOCL,XNEBOR,YNEBOR,AIRS,DTHAUT)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE LOCAL SPACE STEP [ |CI|/SUM(LIJ) ].
!
!history  INRIA
!+
!+        V5P4
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
!| AIRS           |-->| AREAS OF CELLS IN THE MESH.
!| DTHAUT         |<--| SPACE STEP
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| NUBO           |-->| FIRST AND SECOND POINT OF SEGMENTS
!| VNOCL          |-->| NORMAL VECTOR TO INTERFACE
!|                |   | (2 FIRST COMPONENTS) AND
!|                |   | SEGMENT LENGTH (3RD COMPONENT)
!| XNEBOR         |-->| X-COMPONENT OF NORMAL VECTOR AT BOUNDARY POINT
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL VECTOR AT BOUNDARY POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NSEG,NPOIN,NPTFR,NUBO(2,*)
      INTEGER, INTENT(IN)           :: NBOR(*)
      DOUBLE PRECISION, INTENT(IN)  :: VNOCL(3,*),XNEBOR(*),YNEBOR(*)
      DOUBLE PRECISION, INTENT(IN)  :: AIRS(NPOIN)
      DOUBLE PRECISION, INTENT(OUT) :: DTHAUT(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,NSG,NUBO1,NUBO2
      DOUBLE PRECISION VNX,VNY,VNL
!
!-----------------------------------------------------------------------
!
!     INITIALISES
!
      DO I=1,NPOIN
        DTHAUT(I) = 0.D0
      ENDDO
!
      DO NSG=1,NSEG
!
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
!
         DTHAUT(NUBO1) = DTHAUT(NUBO1) + VNOCL(3,NSG)
         DTHAUT(NUBO2) = DTHAUT(NUBO2) + VNOCL(3,NSG)
!
      ENDDO
!
      DO K=1,NPTFR
       I=NBOR(K)
       VNX=XNEBOR(K+NPTFR)
       VNY=YNEBOR(K+NPTFR)
       VNL=SQRT(VNX**2+VNY**2)
       DTHAUT(I) = DTHAUT(I) + VNL
!
      ENDDO
!
      DO I=1,NPOIN
         DTHAUT(I) = AIRS(I)/ DTHAUT(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

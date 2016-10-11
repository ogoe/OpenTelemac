
!                    *******************
                     SUBROUTINE WALLDIST
!                    *******************
     &(MESH, WDIST,LIUBOR,KADH,KLOG,NPTFR)

!***********************************************************************
! TELEMAC2D   V7P0                                  31/08/2015
!***********************************************************************
!
!brief    COMPUTES THE DISTANCE TO THE CLOSEST WALL FOR
!                      SPALART ALLMARAS MODEL.
!
!history  A BOURGOIN (LNHE)
!+        31/08/2015
!+        V7p0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MESH           |-->| MESH STRUCTURE
!| NPTFR          |-->| NUMBER OF POINTS IN THE MESH
!| WDIST          |<--| DISTANCE FROM THE CLOSEST WALL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!***********************************************************************
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D, EX_WALLDIST => WALLDIST
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER        , INTENT(IN)    :: KADH,KLOG,NPTFR
      INTEGER        , INTENT(IN)    :: LIUBOR(*)
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: WDIST
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I,J,K
      DOUBLE PRECISION :: DIST,DISTNOTNIL
      DOUBLE PRECISION, PARAMETER :: EPSS=1.D-6
!
!-----------------------------------------------------------------------
!
!
      INTRINSIC SQRT
!
      CALL OS('X=C     ',X=WDIST,C=1.D10)
!
      DO I=1, MESH%NPOIN
        DISTNOTNIL = 1.D10
        IF(NPTFR.GT.0)THEN
          DO J=1,NPTFR
            IF(LIUBOR(J).EQ.KLOG.OR.LIUBOR(J).EQ.KADH) THEN
              K=MESH%NBOR%I(J)
              DIST=SQRT((MESH%X%R(I)-MESH%X%R(K))**2+
     &                  (MESH%Y%R(I)-MESH%Y%R(K))**2)
              IF(DIST.LT.DISTNOTNIL) THEN
                DISTNOTNIL=DIST
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        DISTNOTNIL=MAX(DISTNOTNIL,EPSS)
      ENDDO
!
!-----------------------------------------------------------------------
!
!
      END SUBROUTINE

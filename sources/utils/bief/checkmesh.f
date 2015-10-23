!                    ********************
                     SUBROUTINE CHECKMESH
!                    ********************
!
     &(MESH,NPOIN)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    Checks the mesh.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        15/05/2015
!+        V7P1
!+   First version. Checking the coordinates.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MESH           |-->| MESH STRUCTURE.
!| NPOIN          |-->| NUMBER OF POINTS (NPOIN2 IN 3D)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: NPOIN
      TYPE(BIEF_MESH)  , INTENT(IN) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,IDISTMIN,JDISTMIN
      DOUBLE PRECISION, POINTER :: X(:),Y(:)
      DOUBLE PRECISION DIST2,DIST2MIN
      INTRINSIC SQRT,MIN
!
!-----------------------------------------------------------------------
!
      X=>MESH%X%R
      Y=>MESH%Y%R
!
!-----------------------------------------------------------------------
!
!     CHECKING THE COORDINATES
!
      DIST2MIN=1.D20
      DO I=1,NPOIN-1
        DO J=I+1,NPOIN
           DIST2=(X(I)-X(J))**2+(Y(I)-Y(J))**2
           IF(DIST2.LT.DIST2MIN) THEN
             DIST2MIN=DIST2
             IDISTMIN=I
             JDISTMIN=J
           ENDIF
        ENDDO
      ENDDO
      IF(NCSIZE.GT.1) THEN
        IF(P_DMIN(DIST2MIN).EQ.DIST2MIN) THEN
          IDISTMIN=MESH%KNOLG%I(IDISTMIN)
          JDISTMIN=MESH%KNOLG%I(JDISTMIN)
        ELSE
          IDISTMIN=0
          JDISTMIN=0
          DIST2MIN=0.D0
        ENDIF
        DIST2MIN=P_DMAX(DIST2MIN)
        IDISTMIN=P_IMAX(IDISTMIN)
        JDISTMIN=P_IMAX(JDISTMIN)
      ENDIF
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)
        WRITE(LU,*) 'VERIFICATION DU MAILLAGE'
        WRITE(LU,*)
        WRITE(LU,*) 'PLUS PETITE DISTANCE ENTRE DEUX POINTS : ',
     &              SQRT(DIST2MIN)
        WRITE(LU,*) 'ENTRE LES POINTS : ',IDISTMIN,' ET ',JDISTMIN
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*)
        WRITE(LU,*) 'CHECKING THE MESH'
        WRITE(LU,*)
        WRITE(LU,*) 'SMALLEST DISTANCE BETWEEN TWO POINTS:',
     &              SQRT(DIST2MIN)
        WRITE(LU,*) 'BETWEEN POINTS: ',IDISTMIN,' AND ',JDISTMIN
      ENDIF
      IF(DIST2MIN.LT.1.D-8) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'VALEUR TROP PETITE'
        IF(LNG.EQ.2) WRITE(LU,*) 'VALUE TOO SMALL'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END


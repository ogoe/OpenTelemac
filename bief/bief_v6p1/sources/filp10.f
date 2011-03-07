!                    *****************
                     SUBROUTINE FILP10
!                    *****************
!
     &( F , C , XSOM , YSOM , NSOM , X , Y , NELEM , NELMAX , IKLE )
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES A FUNCTION TO A CONSTANT VALUE
!+                INSIDE OF A POLYGON.
!
!history  C MOULIN (LNH)
!+        06/12/94
!+        V5P1
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
!| C              |---| 
!| F              |---| 
!| IKLE           |---| 
!| NELEM          |---| 
!| NELMAX         |---| 
!| NSOM           |---| 
!| X              |---| 
!| XSOM           |---| 
!| Y              |---| 
!| YSOM           |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FILP10 => FILP10
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSOM , NELEM , NELMAX
      DOUBLE PRECISION, INTENT(INOUT) :: F(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*) , Y(*)
      DOUBLE PRECISION, INTENT(IN) :: XSOM(NSOM) , YSOM(NSOM) , C
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM , I1 , I2 , I3
!
!-----------------------------------------------------------------------
!
      DO 10 IELEM = 1 , NELEM
!
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        IF( INPOLY(X(I1),Y(I1),XSOM,YSOM,NSOM) .AND.
     &      INPOLY(X(I2),Y(I2),XSOM,YSOM,NSOM) .AND.
     &      INPOLY(X(I3),Y(I3),XSOM,YSOM,NSOM) ) F(IELEM) = C
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
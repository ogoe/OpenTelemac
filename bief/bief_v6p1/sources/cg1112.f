!                    *****************
                     SUBROUTINE CG1112
!                    *****************
!
     &(X,DIM1,DIM2,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CHANGES THE DISCRETISATION OF A VECTOR
!+                FROM 11 TO 12 HERE.
!
!history  J-M HERVOUET (LNH)
!+        09/12/94
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
!| DIM1           |---| 
!| DIM2           |---| 
!| IKLE           |-->| TABLE DE CONNECTIVITE.
!| NELEM          |-->| NOMBRE D'ELEMENTS.
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS.
!| X              |<--| VECTEUR A MODIFIER.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,DIM1,DIM2
      DOUBLE PRECISION, INTENT(INOUT) :: X(DIM1,DIM2)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,4)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IDIM
!
      DOUBLE PRECISION TIERS
!
!-----------------------------------------------------------------------
!
      TIERS = 1.D0/3.D0
!
!-----------------------------------------------------------------------
!
!VOCL LOOP,NOVREC
!DIR$ IVDEP
      DO 20 IDIM  = 1 , DIM2
      DO 10 IELEM = 1 , NELEM
!
        X(IKLE(IELEM,4),IDIM) = TIERS * ( X(IKLE(IELEM,1),IDIM)
     &                                  + X(IKLE(IELEM,2),IDIM)
     &                                  + X(IKLE(IELEM,3),IDIM) )
!
10    CONTINUE
20    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE ASMVE1
!                    *****************
!
     &(X, IKLE,W, NPOIN,NELEM,NELMAX,LV)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MULTIPLICATIVE ASSEMBLY LOOP FOR A VECTOR.
!+
!+            THIS FORM OF ASSEMBLY IS USED WITH PRECONDITIONINGS
!+                OF THE TYPE ELEMENTARY 'CROUT'.
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        17/08/94
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
!| IKLE           |-->| CORRESPONDANCE NUMEROTATION LOCALE-GLOBALE
!| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
!| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| NPOIN          |-->| DIMENSION DU TABLEAU X
!| W              |-->| TABLEAUX DE TRAVAIL CONTENANT LE VECTEUR SOUS
!|                |   | FORME NON ASSEMBLEE
!| X              |<->| VECTEUR ASSEMBLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,NPOIN,LV
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IB
!
      INTRINSIC MIN
!
!-----------------------------------------------------------------------
! LOOP IN SCALAR MODE (LV=1) OR WITH FORCED VECTORISATION
!-----------------------------------------------------------------------
!
      IF(LV.EQ.1) THEN
!
!  SCALAR MODE
!
      DO 10 IELEM = 1 , NELEM
        X(IKLE(IELEM)) = X(IKLE(IELEM)) * W(IELEM)
10    CONTINUE
!
      ELSE
!
!  VECTOR MODE
!
      DO 20 IB = 1,(NELEM+LV-1)/LV
!VOCL LOOP,NOVREC
!DIR$ IVDEP
      DO 30 IELEM = 1+(IB-1)*LV , MIN(NELEM,IB*LV)
        X(IKLE(IELEM)) = X(IKLE(IELEM)) * W(IELEM)
30    CONTINUE
20    CONTINUE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
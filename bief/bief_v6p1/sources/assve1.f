!                    *****************
                     SUBROUTINE ASSVE1
!                    *****************
!
     &(X, IKLE,W, NELEM,NELMAX,LV,MSK,MASKEL)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLY LOOP FOR A VECTOR.
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        18/08/94
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
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
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
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,LV
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX),MASKEL(NELMAX)
      LOGICAL         , INTENT(IN)    :: MSK
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
!  WITH MASKING
!
      IF(MSK) THEN
!
      IF(LV.EQ.1) THEN
!
!  SCALAR MODE
!
      DO 10 IELEM = 1 , NELEM
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM) * MASKEL(IELEM)
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
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM) * MASKEL(IELEM)
30    CONTINUE
20    CONTINUE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  WITHOUT MASKING
!
      ELSE
!
      IF(LV.EQ.1) THEN
!
!  SCALAR MODE
!
      DO 40 IELEM = 1 , NELEM
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM)
40    CONTINUE
!
      ELSE
!
!  VECTOR MODE
!
      DO 60 IB = 1,(NELEM+LV-1)/LV
!VOCL LOOP,NOVREC
!DIR$ IVDEP
      DO 50 IELEM = 1+(IB-1)*LV , MIN(NELEM,IB*LV)
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM)
50    CONTINUE
60    CONTINUE
!
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
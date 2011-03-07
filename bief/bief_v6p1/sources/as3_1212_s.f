!                    *********************
                     SUBROUTINE AS3_1212_S
!                    *********************
!
     &(XM,NSEG11,NSEG12,XMT,NELMAX,NELEM,ELTSEG1,ELTSEG2,ELTSEG3,
     &                                   ELTSEG4,ELTSEG5,ELTSEG6)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE.
!+
!+            CASE OF QUASIBUBBLE-QUASIBUBBLE ELEMENT
!+                AND SYMMETRICAL MATRIX.
!
!history  J-M HERVOUET (LNH)
!+        30/06/99
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
!| ELTSEG1        |---|
!| ELTSEG2        |---|
!| ELTSEG3        |---|
!| ELTSEG4        |---|
!| ELTSEG5        |---|
!| ELTSEG6        |---|
!| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| NSEG11         |---|
!| NSEG12         |---|
!| XM             |---|
!| XMT            |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG11,NSEG12
      INTEGER         , INTENT(IN)    :: ELTSEG1(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG3(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG4(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG5(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG6(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: XMT(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG12)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IELEM
!
!-----------------------------------------------------------------------
!
!  INITIALISES
!
      DO ISEG = 1 , NSEG11
        XM(ISEG) = 0.D0
      ENDDO
!
!  ASSEMBLES PART P1
!
      DO IELEM = 1,NELEM
!       TERM 12
        XM(ELTSEG1(IELEM)) = XM(ELTSEG1(IELEM)) + XMT(IELEM,1)
!       TERM 23
        XM(ELTSEG2(IELEM)) = XM(ELTSEG2(IELEM)) + XMT(IELEM,4)
!       TERM 31
        XM(ELTSEG3(IELEM)) = XM(ELTSEG3(IELEM)) + XMT(IELEM,2)
      ENDDO
!
!  ASSEMBLES QUASIBUBBLE PART
!
      DO IELEM = 1,NELEM
!       TERM 14
        XM(ELTSEG4(IELEM)) = XMT(IELEM,3)
!       TERM 24
        XM(ELTSEG5(IELEM)) = XMT(IELEM,5)
!       TERM 34
        XM(ELTSEG6(IELEM)) = XMT(IELEM,6)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
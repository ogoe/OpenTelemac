!                    *********************
                     SUBROUTINE AS3_1111_Q
!                    *********************
!
     &(XM,NSEG1,XMT,NELMAX,NELEM,ELTSEG1,ELTSEG2,ELTSEG3,
     &                           ORISEG1,ORISEG2,ORISEG3)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE.
!+
!+            CASE OF LINEAR-LINEAR ELEMENT
!+                AND NON SYMMETRICAL MATRIX.
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
!| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| NSEG1          |---|
!| ORISEG1        |---|
!| ORISEG2        |---|
!| ORISEG3        |---|
!| XM             |<--| TERMES EXTRA-DIAGONAUX ASSEMBLES XA12,23,31
!| XMT            |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG1
      INTEGER         , INTENT(IN)    :: ELTSEG1(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG3(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG1(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: XMT(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG1,2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IELEM
!
!-----------------------------------------------------------------------
!
!  INITIALISES
!
      DO ISEG = 1 , NSEG1
        XM(ISEG,1) = 0.D0
        XM(ISEG,2) = 0.D0
      ENDDO
!
!  ASSEMBLES
!
      DO IELEM = 1,NELEM
!         TERM 12
          XM(ELTSEG1(IELEM),  ORISEG1(IELEM))
     &  = XM(ELTSEG1(IELEM),  ORISEG1(IELEM)) + XMT(IELEM,1)
!         TERM 23
          XM(ELTSEG2(IELEM),  ORISEG2(IELEM))
     &  = XM(ELTSEG2(IELEM),  ORISEG2(IELEM)) + XMT(IELEM,3)
!         TERM 31
          XM(ELTSEG3(IELEM),  ORISEG3(IELEM))
     &  = XM(ELTSEG3(IELEM),  ORISEG3(IELEM)) + XMT(IELEM,5)
!         TERM 21
          XM(ELTSEG1(IELEM),3-ORISEG1(IELEM))
     &  = XM(ELTSEG1(IELEM),3-ORISEG1(IELEM)) + XMT(IELEM,4)
!         TERM 32
          XM(ELTSEG2(IELEM),3-ORISEG2(IELEM))
     &  = XM(ELTSEG2(IELEM),3-ORISEG2(IELEM)) + XMT(IELEM,6)
!         TERM 13
          XM(ELTSEG3(IELEM),3-ORISEG3(IELEM))
     &  = XM(ELTSEG3(IELEM),3-ORISEG3(IELEM)) + XMT(IELEM,2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *********************
                     SUBROUTINE AS3_1111_Q
!                    *********************
!
     &(XM,NSEG1,XMT,NELMAX,NELEM,ELTSEG1,ELTSEG2,ELTSEG3,
     &                           ORISEG1,ORISEG2,ORISEG3)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
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
!| ELTSEG1        |-->| FIRST SEGMENT OF A TRIANGLE
!| ELTSEG2        |-->| SECOND SEGMENT OF A TRIANGLE
!| ELTSEG3        |-->| THIRD SEGMENT OF A TRIANGLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
!| NSEG1          |-->| NUMBER OF SEGMENTS (HERE JOINING LINEAR POINTS)
!| ORISEG1        |-->| ORIENTATION OF SEGMENT 1 OF TRIANGLE
!| ORISEG2        |-->| ORIENTATION OF SEGMENT 2 OF TRIANGLE
!| ORISEG3        |-->| ORIENTATION OF SEGMENT 3 OF TRIANGLE
!| XM             |<--| ASSEMBLED OFF-DIAGONAL TERMS XA12,23,31
!| XMT            |-->| ELEMENT BY ELEMENT STORAGE OF MATRIX
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

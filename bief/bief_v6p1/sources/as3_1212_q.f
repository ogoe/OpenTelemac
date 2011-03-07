!                    *********************
                     SUBROUTINE AS3_1212_Q
!                    *********************
!
     &(XM,NSEG11,NSEG12,XMT,NELMAX,NELEM,ELTSEG1,ELTSEG2,ELTSEG3,
     &                                   ELTSEG4,ELTSEG5,ELTSEG6,
     &                                   ORISEG1,ORISEG2,ORISEG3)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE.
!+
!+            CASE OF QUASIBUBBLE-QUASIBUBBLE ELEMENT.
!
!history  J-M HERVOUET (LNH)
!+        29/12/05
!+        V5P6
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
!| ORISEG1        |---|
!| ORISEG2        |---|
!| ORISEG3        |---|
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
      INTEGER         , INTENT(IN)    :: ORISEG1(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMT(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG12*2)
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
        XM(ISEG       ) = 0.D0
        XM(ISEG+NSEG12) = 0.D0
      ENDDO
!
!  ASSEMBLES PART P1
!
      DO IELEM = 1,NELEM
!         TERM 12
          XM(ELTSEG1(IELEM)+NSEG12*(ORISEG1(IELEM)-1))
     &  = XM(ELTSEG1(IELEM)+NSEG12*(ORISEG1(IELEM)-1)) + XMT(IELEM,01)
!         TERM 23
          XM(ELTSEG2(IELEM)+NSEG12*(ORISEG2(IELEM)-1))
     &  = XM(ELTSEG2(IELEM)+NSEG12*(ORISEG2(IELEM)-1)) + XMT(IELEM,04)
!         TERM 31
          XM(ELTSEG3(IELEM)+NSEG12*(ORISEG3(IELEM)-1))
     &  = XM(ELTSEG3(IELEM)+NSEG12*(ORISEG3(IELEM)-1)) + XMT(IELEM,08)
!         TERM 21
          XM(ELTSEG1(IELEM)+NSEG12*(2-ORISEG1(IELEM)))
     &  = XM(ELTSEG1(IELEM)+NSEG12*(2-ORISEG1(IELEM))) + XMT(IELEM,07)
!         TERM 32
          XM(ELTSEG2(IELEM)+NSEG12*(2-ORISEG2(IELEM)))
     &  = XM(ELTSEG2(IELEM)+NSEG12*(2-ORISEG2(IELEM))) + XMT(IELEM,10)
!         TERM 13
          XM(ELTSEG3(IELEM)+NSEG12*(2-ORISEG3(IELEM)))
     &  = XM(ELTSEG3(IELEM)+NSEG12*(2-ORISEG3(IELEM))) + XMT(IELEM,02)
      ENDDO
!
!  ASSEMBLES QUASIBUBBLE PART
!
      DO IELEM = 1,NELEM
!       TERM 14
        XM(ELTSEG4(IELEM)) = XMT(IELEM,03)
!       TERM 24
        XM(ELTSEG5(IELEM)) = XMT(IELEM,05)
!       TERM 34
        XM(ELTSEG6(IELEM)) = XMT(IELEM,06)
!       TERM 41
        XM(ELTSEG4(IELEM)+NSEG12) = XMT(IELEM,09)
!       TERM 42
        XM(ELTSEG5(IELEM)+NSEG12) = XMT(IELEM,11)
!       TERM 43
        XM(ELTSEG6(IELEM)+NSEG12) = XMT(IELEM,12)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
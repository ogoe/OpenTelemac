!                    *****************************
                     SUBROUTINE MULT_INTERFACE_SEG
!                    *****************************
!
     &(FSEG,NH_COM_SEG,DIM1NHCOM,NB_NEIGHB_SEG,
     & NB_NEIGHB_PT_SEG,XMUL,NSEG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MULTIPLIES BY A CONSTANT THE INTERFACE VALUES OF A
!+                FUNCTION DEFINED ON SEGMENTS.
!
!history  J-M HERVOUET (LNHE)
!+        27/02/2009
!+        V5P9
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
!| DIM1NHCOM       |-->| FIRST DIMENSION OF NH_COM_SEG
!| FSEG            |<->| THE FUNCTION DEFINED ON SEGMENTS
!| NB_NEIGHB_PT_SEG|-->| NUMBER OF SEGMENTS SHARED WITH A NEIGHBOUR  
!|                 |   | PROCESSOR 
!| NB_NEIGHB_SEG   |-->| NUMBER OF NEIGHBOUR PROCESSORS (FOR SEGMENTS)
!| NH_COM_SEG      |-->| ADDRESSES OF INTERFACE SEGMENTS
!| NSEG            |-->| NUMBER OF SEGMENTS
!| XMUL            |-->| THE CONSTANT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: DIM1NHCOM,NB_NEIGHB_SEG,NSEG
      INTEGER, INTENT(INOUT) :: NH_COM_SEG(DIM1NHCOM,NB_NEIGHB_SEG)
      INTEGER, INTENT(IN)    :: NB_NEIGHB_PT_SEG(NB_NEIGHB_SEG)
      DOUBLE PRECISION, INTENT(INOUT) :: FSEG(NSEG),XMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IPROC,IKA,IADSEG
!
!-----------------------------------------------------------------------
!
!     DONE ONLY IF THERE IS AT LEAST ONE OTHER SUB-DOMAIN SHARING
!     A SEGMENT WITH THIS ONE
!
      IF(NB_NEIGHB_SEG.GT.0) THEN
!
!     LOOP ON ALL NEIGHBOURING SUB-DOMAINS
!
      DO IPROC=1,NB_NEIGHB_SEG
!
        IKA = NB_NEIGHB_PT_SEG(IPROC)
!
!       LOOP ON ALL SEGMENTS SHARED WITH THIS SUB-DOMAIN
!       WHICH CANNOT BE SHARED WITH ANOTHER SUB-DOMAIN (UNLIKE POINTS)
!
        DO ISEG=1,IKA
!         ADDRESS IN SEGMENT NUMBERING
          IADSEG=NH_COM_SEG(ISEG,IPROC)
          FSEG(IADSEG)=FSEG(IADSEG)*XMUL
        ENDDO
!
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

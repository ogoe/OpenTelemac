!                    ********************
                     SUBROUTINE SD_FABSG4
!                    ********************
!
     & (NPOIN,NSEG,DAB1,DAB2,DAB3,DAB4,XAB1,XAB2,XAB3,XAB4,
     &  NPBLK,NSEGBLK,DA,XA)
!
!***********************************************************************
! BIEF   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    TRANSFORMS A 4-MATRIX SYSTEM INTO A SINGLE BLOCK.
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        20/11/06
!+        V5P7
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
!| DA             |<--| RESULTING MATRIX DIAGONAL
!| DAB1           |-->| MATRIX DIAGONAL IN THE ORIGINAL SYSTEM
!| DAB2           |-->| MATRIX DIAGONAL IN THE ORIGINAL SYSTEM
!| DAB3           |-->| MATRIX DIAGONAL IN THE ORIGINAL SYSTEM
!| DAB4           |-->| MATRIX DIAGONAL IN THE ORIGINAL SYSTEM
!| NPBLK          |-->| RANK OF FINAL BLOCK MATRIX
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| NSEGBLK        |-->| NUMBER OF SEGMENTS IN FINAL BLOCK
!| XA             |<--| RESULTING OFF-DIAGONAL TERMS OF MATRIX
!| XAB1           |-->| OFF-DIAGONAL TERMS IN ORIGINAL SYSTEM
!| XAB2           |-->| OFF-DIAGONAL TERMS IN ORIGINAL SYSTEM
!| XAB3           |-->| OFF-DIAGONAL TERMS IN ORIGINAL SYSTEM
!| XAB4           |-->| OFF-DIAGONAL TERMS IN ORIGINAL SYSTEM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_FABSG4 => SD_FABSG4
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEGBLK,NPBLK,NSEG,NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XAB1(NSEG),XAB2(NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: XAB3(NSEG),XAB4(NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: DAB1(NPOIN),DAB2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DAB3(NPOIN),DAB4(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XA(2*NSEGBLK),DA(NPBLK)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISEG,JSEG
!
!----------------------------------------------
!     INFO :      NPBLK   = NPOIN*NBLOC
!                 NSEGBLK = NSEG*4 + 2*NPOIN
!----------------------------------------------
!
!
!
!-------------------
! 1.  BLOCK DIAGONAL
!-------------------
!
      DO I=1,NPOIN
         DA(I) = DAB1(I)
         DA(I+NPOIN) = DAB4(I)
      ENDDO
!
!---------------------------
! 2.   EXTRADIAGONAL TERMS
!---------------------------
!
!
!     BLOCK 1
!     ------
!
      JSEG=0
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         XA(JSEG)        =XAB1(ISEG)
         XA(JSEG+NSEGBLK)=XAB1(ISEG)
      ENDDO
!
!     BLOCKS 2 AND 3 (EXTRA-DIAG)
!     ------------------------
      DO I=1,NPOIN
         JSEG=JSEG+1
         XA(JSEG)        =DAB2(I)
         XA(JSEG+NSEGBLK)=DAB3(I)
      ENDDO
!
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         XA(JSEG)        =XAB2(ISEG)
         XA(JSEG+NSEGBLK)=XAB3(ISEG)
         JSEG=JSEG+1
         XA(JSEG)        =XAB2(ISEG)
         XA(JSEG+NSEGBLK)=XAB3(ISEG)
      ENDDO
!
!     BLOCK 4 (EXTRA)
!     --------------
      DO ISEG=1,NSEG
         JSEG=JSEG+1
         XA(JSEG)        =XAB4(ISEG)
         XA(JSEG+NSEGBLK)=XAB4(ISEG)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

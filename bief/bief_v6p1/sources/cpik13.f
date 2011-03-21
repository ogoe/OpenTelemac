!                    *****************
                     SUBROUTINE CPIK13
!                    *****************
!
     &(IKLE,IKLBOR,ELTSEG,NBOR,NELEM,NELMAX,NPOIN,NPTFR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    EXTENDS THE CONNECTIVITY TABLE.
!+                CASE OF AN EXTENSION TO QUADRATIC ELEMENTS.
!
!history  J-M HERVOUET (LNH)
!+        20/03/08
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
!| ELTSEG         |-->| SEGMENTS NUMBERS IN EVERY ELEMENT
!| IKLBOR         |-->| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!| IKLE           |<->| CONNECTIVITY TABLE
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM,NELMAX,NPOIN,NPTFR
      INTEGER, INTENT(IN)    :: ELTSEG(NELMAX,3)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,6),IKLBOR(NPTFR,*),NBOR(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,K
!
!-----------------------------------------------------------------------
!
!     CONNECTIVITY TABLE OF QUADRATIC GLOBAL POINTS
!
      DO IELEM = 1 , NELEM
!
!       NUMBER=NPOIN+NUMBER OF THE SEGMENT CONTAINING THE POINT
!
        IKLE(IELEM,4) = NPOIN + ELTSEG(IELEM,1)
        IKLE(IELEM,5) = NPOIN + ELTSEG(IELEM,2)
        IKLE(IELEM,6) = NPOIN + ELTSEG(IELEM,3)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CONNECTIVITY TABLE OF QUADRATIC BOUNDARY POINTS
!     GLOBAL NUMBERS OF BOUNDARY QUADRATIC POINTS
!
      DO K=1,NPTFR
        IKLBOR(K,3)=K+NPTFR
!       SEGMENTS 1 TO NPTFR ARE THE BOUNDARY SEGMENTS
        NBOR(IKLBOR(K,3))=NPOIN+K
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

!                    *******************
                     SUBROUTINE COMP_FAC
!                    *******************
!
     &(ELTSEG,IFABOR,NELEM,NPOIN,FAC)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPLETES THE ARRAY FAC FOR QUADRATIC POINTS
!+                AT THE INTERFACE BETWEEN 2 SUBDOMAINS.
!+
!+            FAC%R(1:NPOIN) IS FILLED IN PARINI.
!
!history  J-M HERVOUET (LNHE)
!+        24/10/08
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
!| ELTSEG         |-->| GIVES THE SEGMENT NUMBER OF EDGES OF ELEMENTS
!| FAC            |<->| COEFFICIENT FOR COMPUTING DOT PRODUCTS IN //
!| IFABOR         |-->| -2 MEANS INTERFACE WITH ANOTHER SUB-DOMAIN
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_COMP_FAC => COMP_FAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM,NPOIN
      INTEGER, INTENT(IN)    :: IFABOR(NELEM,3),ELTSEG(NELEM,3)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM
!
        IF(IFABOR(IELEM,1).EQ.-2) FAC%R(NPOIN+ELTSEG(IELEM,1))=0.5D0
        IF(IFABOR(IELEM,2).EQ.-2) FAC%R(NPOIN+ELTSEG(IELEM,2))=0.5D0
        IF(IFABOR(IELEM,3).EQ.-2) FAC%R(NPOIN+ELTSEG(IELEM,3))=0.5D0
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

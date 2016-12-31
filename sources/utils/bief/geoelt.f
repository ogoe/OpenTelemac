!                    *****************
                     SUBROUTINE GEOELT
!                    *****************
!
     &(SURDET,SURFAC,XEL,YEL,NELEM,NELMAX,IELM)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES DETERMINANTS AND SOME OTHER VALUES FOR
!+                ISOPARAMETRIC COORDINATES.
!
!history  J-M HERVOUET (LNH)
!+        10/01/95
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
!| IELM           |-->| TYPE OF ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURDET         |<--| 1.D0/DETERMINANT(TRANSFORMATION)
!| SURFAC         |<--| AREA OF ELEMENTS
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_GEOELT => GEOELT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: IELM,NELEM,NELMAX
      DOUBLE PRECISION, INTENT(OUT) :: SURDET(NELEM),SURFAC(NELEM)
      DOUBLE PRECISION, INTENT(IN)  :: XEL(NELMAX,*),YEL(NELMAX,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
      DOUBLE PRECISION XSOM(4,2)
!
      DOUBLE PRECISION T12,T13,T22,T23,DET,Z(1)
!
!-----------------------------------------------------------------------
!
      CALL SURVOL(SURFAC, XEL,YEL,Z,NELEM,NELMAX,IELM)
!
      IF(IELM.EQ.11) THEN
!
        DO IELEM = 1 , NELEM
!
        XSOM(1,1) = XEL(IELEM,1)
        XSOM(2,1) = XEL(IELEM,2)
        XSOM(3,1) = XEL(IELEM,3)
        XSOM(1,2) = YEL(IELEM,1)
        XSOM(2,2) = YEL(IELEM,2)
        XSOM(3,2) = YEL(IELEM,3)
!
        T12 = - XSOM(1,1) + XSOM(2,1)
        T13 = - XSOM(1,1) + XSOM(3,1)
        T22 = - XSOM(1,2) + XSOM(2,2)
        T23 = - XSOM(1,2) + XSOM(3,2)
!
        DET = T12*T23 - T22*T13
!
        IF(DET.LT.1.D-20) THEN
          IF(LNG.EQ.1) WRITE(LU,98) IELEM
          IF(LNG.EQ.2) WRITE(LU,99) IELEM
98        FORMAT(1X,'GEOELT: ELEMENT ',1I6,' : DETERMINANT NEGATIF')
99        FORMAT(1X,'GEOELT: ELEMENT ',1I6,' : NEGATIVE DETERMINANT')
          CALL PLANTE(1)
          STOP
        ENDIF
!
        SURDET(IELEM) = 1.D0/DET
!
        ENDDO
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,10) IELM
        IF(LNG.EQ.2) WRITE(LU,11) IELM
10      FORMAT(1X,'GEOELT: TYPE D''ELEMENT INCONNU :',1I6)
11      FORMAT(1X,'GEOELT: UNKNOWN TYPE OF ELEMENT :',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

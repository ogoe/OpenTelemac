!                    *****************
                     SUBROUTINE NORMAB
!                    *****************
!
     &(XNEBOR,YNEBOR,XSGBOR,YSGBOR,DISBOR,SURFAC,NELMAX,
     & KP1BOR,NELBOR,NULONE,LGSEG,NPTFR,MESH,T1,XEL,YEL)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    1) COMPUTES THE COMPONENTS OF THE OUTGOING NORMAL VECTOR
!+
!+               - FOR THE BOUNDARY POINTS      (XNEBOR,YNEBOR)
!+
!+               - FOR THE BOUNDARY SEGMENTS    (XSGBOR,YSGBOR)
!+
!+            2) DISTANCE TO THE BOUNDARY OF THE FIRST ELEMENT POINTS
!+
!+            3) LENGTH OF THE BOUNDARY SEGMENTS
!+
!+            4) DISTANCE TO THE BOUNDARY OF THE FIRST INTERNAL POINTS
!code
!+  BEWARE:  XSGBOR AND YSGBOR DIMENSION IS (NPTFR,4):
!+
!+           (K,1) : NORMALISED    , SEGMENT FOLLOWING K
!+           (K,2) : NORMALISED    , SEGMENT PRECEDING K
!+           (K,3) : NOT NORMALISED, SEGMENT FOLLOWING K
!+           (K,4) : NOT NORMALISED, SEGMENT PRECEDING K
!+
!+           XSGBOR(K,1) AND YSGBOR(K,1) ARE THE COMPONENTS
!+           FOR THE SEGMENT FOLLOWING POINT K.
!+
!+           XSGBOR(K,2) AND YSGBOR(K,2) ARE THE COMPONENTS
!+           FOR THE SEGMENT PRECEDING POINT K.
!
!history  J-M HERVOUET (LNHE)
!+        26/06/2008
!+        V5P9
!+   Modifications for parallelism.
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        10/01/2013
!+        V6P3
!+   LGSEG now computed with coordinates per element (XEL and YEL)
!+   This is important in spherical coordinates.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DISBOR         |<--| DISTANCE FROM BOUNDARY POINT TO CLOSER 
!|                |   | INNER POINT
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| LGSEG          |<--| LENGTH OF BOUNDARY SEGMENTS
!| MESH           |-->| MESH STRUCTURE
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NULONE         |-->| NUMBER OF FIRST POINT OF SEGMENT IN ADJACENT ELEMENT
!| NELMAX         |-->| NUMBER OF ELEMENTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| SURFAC         |-->| AREA OF TRIANGLES
!| T1             |<->| BIEF_OBJ STRUCTURE FOR WORK ARRAY
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XNEBOR         |<--| COMPONANT ALONG X OF VECTOR NORMAL TO POINT
!| XSGBOR         |<--| COMPONANT ALONG X OF VECTOR NORMAL TO SEGMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XNEBOR         |<--| COMPONANT ALONG Y OF VECTOR NORMAL TO POINT
!| YSGBOR         |<--| COMPONANT ALONG Y OF VECTOR NORMAL TO SEGMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_NORMAB => NORMAB
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR,NELMAX
      INTEGER, INTENT(IN) :: KP1BOR(NPTFR),NELBOR(NPTFR),NULONE(NPTFR)
!
      DOUBLE PRECISION, INTENT(INOUT) :: XNEBOR(NPTFR,2),YNEBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(INOUT) :: XSGBOR(NPTFR,4),YSGBOR(NPTFR,4)
      DOUBLE PRECISION, INTENT(INOUT) :: DISBOR(NPTFR),LGSEG(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: T1
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K1,K2,N1,N2,IELEM,I1,I2
      DOUBLE PRECISION X12,Y12,XNORM,X1,X2,Y1,Y2,Z(1)
!
      INTEGER NEXT(3)
      DATA NEXT / 2,3,1 /
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE NORMAL VECTORS AND SEGMENT LENGTHS
!
!     0) INITIALISES LGSEG, XSGBOR AND YSGBOR TO 0
!
      IF(NPTFR.GT.0) THEN
!
        DO K1=1,NPTFR
          LGSEG(K1)    = 0.D0
          XSGBOR(K1,1) = 0.D0
          YSGBOR(K1,1) = 0.D0
          XSGBOR(K1,2) = 0.D0
          YSGBOR(K1,2) = 0.D0
        ENDDO
!
!       1) NORMALS BY SEGMENT AND LENGTH OF THE BOUNDARY SEGMENT
!          COMMON VERSION FOR SCALAR/PARALLEL MODES
!
        DO K1=1,NPTFR
!
          K2=KP1BOR(K1)
!         IF SEGMENT IN THE DOMAIN (IN PARALLEL IT MAY NOT BE)
          IF(K2.NE.K1) THEN
            IELEM=NELBOR(K1)
            I1=NULONE(K1)
            I2=NEXT(I1)
            X1=XEL(IELEM,I1)
            Y1=YEL(IELEM,I1)
            X2=XEL(IELEM,I2)
            Y2=YEL(IELEM,I2)
            X12 = X2 - X1
            Y12 = Y2 - Y1
!           LENGTH OF THE BOUNDARY SEGMENT
            LGSEG(K1) = SQRT( X12**2 + Y12**2 )
!           NORMAL TO THE SEGMENT FOLLOWING K1:
            XSGBOR(K1,1) =  Y12
            YSGBOR(K1,1) = -X12
!           NORMAL TO THE SEGMENT PRECEDING THE ONE FOLLOWING K1:
            XSGBOR(K2,2) =  Y12
            YSGBOR(K2,2) = -X12
          ENDIF
!
        ENDDO
!
      ENDIF
!
!     2) COMPLEMENT IN PARALLEL MODE, WITH PARCOM OPTION 1
!        (VALUE OF GREATER ABSOLUTE VALUE)
!
      IF(NCSIZE.GT.1) THEN
        IF(NPTFR.GT.0) THEN
          CALL PARCOM_BORD(LGSEG            ,1,MESH)
          CALL PARCOM_BORD(XSGBOR(1:NPTFR,1),1,MESH)
          CALL PARCOM_BORD(XSGBOR(1:NPTFR,2),1,MESH)
          CALL PARCOM_BORD(YSGBOR(1:NPTFR,1),1,MESH)
          CALL PARCOM_BORD(YSGBOR(1:NPTFR,2),1,MESH)
        ELSE
!         THIS DOES NOTHING FOR THE SUB-DOMAIN, BUT IS
!         NECESSARY TO THE PARALLEL COMMUNICATION
!         IN CASE OF CALL PARCOM_BORD, ALL PROCESSORS MUST CALL IT
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
        ENDIF
      ENDIF
!
!     3) NORMALS BY NODES, APPROXIMATE DISTANCE FROM THE BOUNDARY
!        THE VECTORS ARE THEN NORMALISED
!
      IF(NPTFR.GT.0) THEN
!
      DO K1=1,NPTFR
!
!       NORMAL AT THE POINT: AVERAGE OF 2 NOT NORMALISED NORMALS
!       ASSOCIATED WITH THE 2 ADJACENT SEGMENTS
!
!       NOT NORMALISED VERSION XNEBOR(*,2) AND YNEBOR(*,2)
        XNEBOR(K1,2)=(XSGBOR(K1,1)+XSGBOR(K1,2))*0.5D0
        YNEBOR(K1,2)=(YSGBOR(K1,1)+YSGBOR(K1,2))*0.5D0
!
!       NOT NORMALISED VERSION XSGBOR(*,3) AND XSGBOR(*,4)
!                              YSGBOR(*,3) AND YSGBOR(*,4)
        XSGBOR(K1,3)=XSGBOR(K1,1)
        XSGBOR(K1,4)=XSGBOR(K1,2)
        YSGBOR(K1,3)=YSGBOR(K1,1)
        YSGBOR(K1,4)=YSGBOR(K1,2)
!
!       NORMALISED VERSION XNEBOR(*,1) AND YNEBOR(*,1)
        XNORM=SQRT(XNEBOR(K1,2)**2+YNEBOR(K1,2)**2)
        XNEBOR(K1,1)=XNEBOR(K1,2)/XNORM
        YNEBOR(K1,1)=YNEBOR(K1,2)/XNORM
!
!       NORMALISED VERSION OF XSGBOR AND YSGBOR FOR FOLLOWING SEGMENT
        XNORM=SQRT(XSGBOR(K1,1)**2+YSGBOR(K1,1)**2)
        XSGBOR(K1,1)=XSGBOR(K1,1)/XNORM
        YSGBOR(K1,1)=YSGBOR(K1,1)/XNORM
!
!       NORMALISED VERSION OF XSGBOR AND YSGBOR FOR PRECEDING SEGMENT
        XNORM=SQRT(XSGBOR(K1,2)**2+YSGBOR(K1,2)**2)
        XSGBOR(K1,2)=XSGBOR(K1,2)/XNORM
        YSGBOR(K1,2)=YSGBOR(K1,2)/XNORM
!
!       THIS CAN BE APPROXIMATION OF THE MESH SIZE AT THE BOUNDARY
!       AND IS USED FOR LOG LAW AT THE BOUNDARIES
        IELEM=NELBOR(K1)
        IF(IELEM.GT.0) THEN
          DISBOR(K1) = 2.D0*SURFAC(NELBOR(K1))/LGSEG(K1)
        ELSE
          DISBOR(K1) = 0.D0
        ENDIF
!
      ENDDO
!
      ENDIF
!
!     DISBOR IS POSITIVE, CAN TAKE THE MAX
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM_BORD(DISBOR,3,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

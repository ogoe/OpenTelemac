!                    *****************
                     SUBROUTINE GTSH41
!                    *****************
!
     &(WS,SHP,SHZ,ELT,ETA,IKLE2,NPOIN2,NELEM2,NPLAN,MSK,MASKEL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FIXES THE BARYCENTRIC COORDINATES OF ALL THE MESH
!+                NODES IN THE ELEMENT TOWARDS WHICH POINTS THE
!+                CHARACTERISTIC CURVE, FOR THE TELEMAC-3D PRISMS AND
!+                BEFORE TRACING BACK IN TIME THE CHARACTERISTIC
!+                CURVES.
!
!history  J-M JANIN (LNH)
!+        21/08/2008
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
!| ELT            |<--| ELEMENT CHOSEN FOR EVERY POINT
!| ETA            |---|
!| IKLE2          |-->| 2D CONNECTIVITY TABLE
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| SHP            |<--| HORIZONTAL BARYCENTRIC COORDINATES OF NODES IN 
!|                |   | THEIR ASSOCIATED ELEMENT "ELT".
!| SHZ            |-->| VERTICAL BARYCENTRIC COORDINATES
!| WS             |-->| VERTICAL VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPOIN2,NELEM2,NPLAN
      INTEGER, INTENT(IN)    :: IKLE2(NELEM2,3)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN2,NPLAN),ETA(NPOIN2,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN) :: WS(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: MASKEL(NELEM2)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZ(NPOIN2,NPLAN)
!
      LOGICAL, INTENT(IN) :: MSK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,IPOIN,IELEM,I1,I2,I3
!
!***********************************************************************
!
!     LOOP ON ALL POINTS
!
      DO IPLAN = 1,NPLAN
        DO IELEM=1,NELEM2
          I1=IKLE2(IELEM,1)
          I2=IKLE2(IELEM,2)
          I3=IKLE2(IELEM,3)
          ELT(I1,IPLAN)=IELEM
          ELT(I2,IPLAN)=IELEM
          ELT(I3,IPLAN)=IELEM
          SHP(1,I1,IPLAN)=1.D0
          SHP(2,I1,IPLAN)=0.D0
          SHP(3,I1,IPLAN)=0.D0
          SHP(1,I2,IPLAN)=0.D0
          SHP(2,I2,IPLAN)=1.D0
          SHP(3,I2,IPLAN)=0.D0
          SHP(1,I3,IPLAN)=0.D0
          SHP(2,I3,IPLAN)=0.D0
          SHP(3,I3,IPLAN)=1.D0
        ENDDO
      ENDDO
!
!     ON THE VERTICAL, IT IS DONE DEPENDING ON THE VERTICAL VELOCITY
!
      DO IPLAN = 1,NPLAN
        DO IPOIN=1,NPOIN2
          IF((WS(IPOIN,IPLAN).GT.0.D0.AND.IPLAN.NE.1).OR.
     &                                              IPLAN.EQ.NPLAN) THEN
            ETA(IPOIN,IPLAN) = IPLAN-1
            SHZ(IPOIN,IPLAN) = 1.D0
          ELSE
            ETA(IPOIN,IPLAN) = IPLAN
            SHZ(IPOIN,IPLAN) = 0.D0
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

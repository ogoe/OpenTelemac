!                    *****************
                     SUBROUTINE MATVCT
!                    *****************
!
     &(OP, X , DA,TYPDIA,XA,TYPEXT, Y ,
     & C,IKLE,NPT,NELEM,NELMAX,W,LEGO,IELM1,IELM2,IELMX,LV,
     & S,P,IKLEM1,DIMIKM,LIMVOI,MXPTVS,NPMAX,NPOIN,NPTFR,
     & GLOSEG,SIZGLO,SIZXA,NDP,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MATRIX VECTOR OPERATIONS.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND MATRIX M.
!+
!+   THE RESULT IS VECTOR X (A NON-ASSEMBLED PART OF WHICH CAN BE IN
!+   ARRAY W IF LEGO = .FALSE.)
!+
!+   THESE OPERATIONS ARE DIFFERENTS DEPENDING ON THE DIAGONAL TYPE
!+   AND THE OFF-DIAGONAL TERMS TYPE.
!+
!+   IMPLEMENTED OPERATIONS :
!+
!+      OP = 'X=AY    '  : X = AY
!+      OP = 'X=X+AY  '  : X = X + AY
!+      OP = 'X=X-AY  '  : X = X - AY
!+      OP = 'X=X+CAY '  : X = X + C AY
!+      OP = 'X=TAY   '  : X = TA Y (TA: TRANSPOSE OF A)
!+      OP = 'X=X+TAY '  : X = X + TA Y
!+      OP = 'X=X-TAY '  : X = X - TA Y
!+      OP = 'X=X+CTAY'  : X = X + C TA Y
!
!history  ALGIANE FROEHLY (MATMECA)
!+        31/03/2008
!+
!+   QUADRATIC TRIANGLE
!
!history  J-M HERVOUET (LNHE)
!+        05/02/2010
!+        V6P0
!+   NSEG1 AND NSEG2 MODIFIED BEFORE CALLING MVSEG
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
!| C              |-->| A GIVEN CONSTANT
!| DA             |-->| MATRIX DIAGONAL
!| DIMIKM         |-->| FIRST DIMENSION OF IKLEM1.
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!| IELM1          |-->| TYPE OF ELEMENT FOR LINES
!| IELM2          |-->| TYPE OF ELEMENT FOR COLUMNS
!| IELMX          |-->| TYPE OF ELEMENT OF RESULT
!|                |   | CAN BE IELM1 OR IELM2 DEPENDING ON OP
!| IKLE           |-->| CONNECTIVITY TABLE.
!| IKLEM1         |-->| CONNECTIVITY TABLE USED FOR MATRIX-VECTOR 2
!| LEGO           |-->| = .TRUE. W1,2,... ARE ASSEMBLED ON X
!|                |   | =.FALSE. W1,2,... ARE NOT ASSEMBLED
!| LIMVOI         |-->| ARRAY USED FOR MATRIX-VECTOR 2
!| LV             |-->| VECTOR LENGTH OF THE MACHINE
!| MXPTVS         |-->| MAXIMUM NUMBER OF NEIGHBOURS OF A POINT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS IN THE MESH
!| NPOIN          |-->| NUMBER OF POINTS
!| NPT            |-->| DIMENSION OF DIAGONAL
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OP             |-->| OPERATION TO BE DONE
!| P              |-->| TYPE OF MATRIX X VECTOR PRODUCT.
!| S              |-->| TYPE OF STORAGE.
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!| SIZXA          |-->| FIRST DIMENSION OF ARRAY XA
!| TYPDIA         |-->| TYPE OF DIAGONAL:
!|                |   | TYPDIA = 'Q' : ANY VALUE
!|                |   | TYPDIA = 'I' : IDENTITY
!|                |   | TYPDIA = '0' : ZERO
!| TYPEXT         |-->| TYPE OF OFF-DIAGONAL TERMS
!|                |   | TYPEXT = 'Q' : ANY VALUE
!|                |   | TYPEXT = 'S' : SYMMETRIC
!|                |   | TYPEXT = '0' : ZERO
!| W              |<--| WORK ARRAY WITH NON ASSEMBLED RESULT 
!| X              |<--| RESULTING VECTOR
!| XA             |-->| OFF-DIAGONAL TERMS IN THE MATRIX A
!| Y              |-->| A GIVEN VECTOR USED IN OPERATION OP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MATVCT => MATVCT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: IELM1,IELM2,IELMX,NPOIN,NPMAX,S,P,SIZXA
      INTEGER, INTENT(IN)    :: NDP
      INTEGER, INTENT(INOUT) :: NPT
      INTEGER, INTENT(IN) :: NELEM,NELMAX,LV,DIMIKM,MXPTVS,NPTFR,SIZGLO
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),IKLEM1(*),LIMVOI(*)
      INTEGER, INTENT(IN) :: GLOSEG(SIZGLO,2)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      CHARACTER(LEN=1),INTENT(IN)     :: TYPDIA,TYPEXT
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN)    :: Y(*),DA(*),XA(SIZXA,*),C
      DOUBLE PRECISION, INTENT(INOUT) :: W(NELMAX,*)
      LOGICAL, INTENT(IN)             :: LEGO
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSEG1,NSEG2,SYM,NPT2
!
      INTEGER AAQ(3,3,2),ABQ(3,4,2),BAQ(4,3,2)
      INTEGER ACQ(3,6,2),BBQ(4,4,2),CAQ(6,3,2),PPQ(6,6,2)
      INTEGER AAS(3,3,2),BBS(4,4,2),PPS(6,6,2)
      INTEGER OOS(2,2,2)
!
      DOUBLE PRECISION Z(1)
!
      INTRINSIC MIN
!
!     THESE DATA ALSO APPEAR IN MATVCT
!
!     DATA OOS/  0 ,  1 ,
!    *           1 ,  0 ,
! S=2 NOT IMPLEMENTED
!    *           0 ,  0 ,
!    *           0 ,  0 /
!
!     SYMMETRICAL P1-P1 EBE (S=1)
      DATA AAS/  0 ,  1 ,  2 ,
     &           1 ,  0 ,  3 ,
     &           2 ,  3 ,  0 ,
!     SYMMETRICAL P1-P1 PRE-ASSEMBLED EBE (S=2)
     &           0 ,  1 ,  3 ,
     &           1 ,  0 ,  2 ,
     &           3 ,  2 ,  0 /
!
!     NONSYMMETRICAL P1-P1 EBE (S=1)
      DATA AAQ/  0 ,  4 ,  5 ,
     &           1 ,  0 ,  6 ,
     &           2 ,  3 ,  0 ,
!     NONSYMMETRICAL P1-P1 PRE-ASSEMBLED EBE (S=2)
     &           0 ,  4 ,  3 ,
     &           1 ,  0 ,  5 ,
     &           6 ,  2 ,  0 /
!
!     SYMMETRICAL QUASI-BUBBLE QUASI-BUBBLE EBE (S=1)
      DATA BBS/  0 ,  1 ,  2 ,  3 ,
     &           1 ,  0 ,  4 ,  5 ,
     &           2 ,  4 ,  0 ,  6 ,
     &           3 ,  5 ,  6 ,  0 ,
!     SYMMETRICAL QUASI-BUBBLE QUASI-BUBBLE PRE-ASSEMBLED EBE (S=2)
     &           0 ,  4 ,  6 ,  1 ,
     &           4 ,  0 ,  5 ,  2 ,
     &           6 ,  5 ,  0 ,  3 ,
     &           1 ,  2 ,  3 ,  0 /
!
!     NONSYMMETRICAL QUASI-BUBBLE QUASI-BUBBLE EBE (S=1)
      DATA BBQ/  0 ,  7 ,  8 ,  9 ,
     &           1 ,  0 , 10 , 11 ,
     &           2 ,  4 ,  0 , 12 ,
     &           3 ,  5 ,  6 ,  0 ,
!     NONSYMMETRICAL QUASI-BUBBLE QUASI-BUBBLE PRE-ASSEMBLED EBE (S=2)
     &           0 , 10 ,  6 ,  7 ,
     &           4 ,  0 , 11 ,  8 ,
     &          12 ,  5 ,  0 ,  9 ,
     &           1 ,  2 ,  3 ,  0 /
!
!     NONSYMMETRICAL P1 QUASI-BUBBLE EBE (S=1)
      DATA ABQ/  0 ,  4 ,  7 ,
     &           1 ,  0 ,  8 ,
     &           2 ,  5 ,  0 ,
     &           3 ,  6 ,  9 ,
!     NONSYMMETRICAL P1 QUASI-BUBBLE PRE-ASSEMBLED EBE (S=2)
     &           0 ,  7 ,  3 ,
     &           1 ,  0 ,  8 ,
     &           9 ,  2 ,  0 ,
     &           4 ,  5 ,  6 /
!     NONSYMMETRICAL P1 P2 EBE (S=1)
      DATA ACQ/   0 ,  6 ,  11,
     &            1 ,  0 ,  12,
     &            2 ,  7 ,  0 ,
     &            3 ,  8 ,  13,
     &         	  4 ,  9 ,  14,
     &		  5 ,  10,  15,
! S=2 NOT IMPLEMENTED
     &            0 ,  0 ,  0 ,
     &            0 ,  0 ,  0 ,
     &            0 ,  0 ,  0 ,
     &            0 ,  0 ,  0 ,
     &         	  0 ,  0 ,  0 ,
     &		  0 ,  0 ,  0 /
!     NONSYMMETRICAL QUASI-BUBBLE P1 EBE (S=1)
      DATA BAQ/  0 ,  3 ,  5 ,  7 ,
     &           1 ,  0 ,  6 ,  8 ,
     &           2 ,  4 ,  0 ,  9 ,
!     NONSYMMETRICAL QUASI-BUBBLE P1 PRE-ASSEMBLED EBE (S=2)
     &           0 ,  7 ,  3 ,  4 ,
     &           1 ,  0 ,  8 ,  5 ,
     &           9 ,  2 ,  0 ,  6 /
!     NONSYMMETRICAL P2 P1 EBE (S=1)
      DATA CAQ/  0 ,  3 ,  5 ,  7 , 10 , 13 ,
     &           1 ,  0 ,  6 ,  8 , 11 , 14 ,
     &           2 ,  4 ,  0 ,  9 , 12 , 15 ,
!     NONSYMMETRICAL P2 P1 PRE-ASSEMBLED EBE (S=2)
!     - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
!
!     SYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES EBE (S=1)
      DATA PPS/  0 ,  1 ,  2 ,  3 ,  4 ,  5 ,
     &           1 ,  0 ,  6 ,  7 ,  8 ,  9 ,
     &           2 ,  6 ,  0 , 10 , 11 , 12 ,
     &           3 ,  7 , 10 ,  0 , 13 , 14 ,
     &           4 ,  8 , 11 , 13 ,  0 , 15 ,
     &           5 ,  9 , 12 , 14 , 15 ,  0 ,
!     SYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES PRE-ASSEMBLED EBE (S=2)
!     - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
!
!     NONSYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES EBE (S=1)
      DATA PPQ/  0 , 16 , 17 , 18 , 19 , 20 ,
     &           1 ,  0 , 21 , 22 , 23 , 24 ,
     &           2 ,  6 ,  0 , 25 , 26 , 27 ,
     &           3 ,  7 , 10 ,  0 , 28 , 29 ,
     &           4 ,  8 , 11 , 13 ,  0 , 30 ,
     &           5 ,  9 , 12 , 14 , 15 ,  0 ,
!    NONSYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES PRE-ASSEMBLED EBE (S=2)
!     - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
!
!-----------------------------------------------------------------------
!
      IF(S.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!     TRADITIONAL EBE STORAGE AND TRADITIONAL EBE MATRIX X VECTOR PRODUCT
!
!-----------------------------------------------------------------------
!
      IF(IELM1.EQ.1) THEN
!
        IF(IELM2.EQ.1) THEN
          CALL MV0202(OP, X , DA,TYPDIA,
     &                XA(1,1),XA(1,2),TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),
     &                NPT,NELEM,W(1,1),W(1,2))
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(IELM1.EQ.11) THEN
!
        IF(IELM2.EQ.11) THEN
          IF(TYPEXT(1:1).EQ.'S') THEN
            CALL MV0303(OP, X , DA,TYPDIA,
     &                  XA(1,AAS(1,2,S)),
     &                  XA(1,AAS(1,3,S)),
     &                  XA(1,AAS(2,1,S)),
     &                  XA(1,AAS(2,3,S)),
     &                  XA(1,AAS(3,1,S)),
     &                  XA(1,AAS(3,2,S)),
     &                  TYPEXT,Y,C,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                  NPT,NELEM,
     &                  W(1,1),W(1,2),W(1,3))
          ELSE
            CALL MV0303(OP, X , DA,TYPDIA,
     &                  XA(1,AAQ(1,2,S)),
     &                  XA(1,AAQ(1,3,S)),
     &                  XA(1,AAQ(2,1,S)),
     &                  XA(1,AAQ(2,3,S)),
     &                  XA(1,AAQ(3,1,S)),
     &                  XA(1,AAQ(3,2,S)),
     &                  TYPEXT,Y,C,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                  NPT,NELEM,
     &                  W(1,1),W(1,2),W(1,3))
          ENDIF
!
        ELSEIF(IELM2.EQ.12) THEN
!
          CALL MV0304(OP, X , DA,TYPDIA,
     &                XA(1,ABQ(1,2,S)),
     &                XA(1,ABQ(1,3,S)),
     &                XA(1,ABQ(1,4,S)),
     &                XA(1,ABQ(2,1,S)),
     &                XA(1,ABQ(2,3,S)),
     &                XA(1,ABQ(2,4,S)),
     &                XA(1,ABQ(3,1,S)),
     &                XA(1,ABQ(3,2,S)),
     &                XA(1,ABQ(3,4,S)),
     &                TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NPT,NELEM,
     &                W(1,1),W(1,2),W(1,3),W(1,4))
!
         ELSEIF(IELM2.EQ.13) THEN
!
          NPT2=BIEF_NBPTS(IELM2,MESH)
          CALL MV0306(OP, X , DA,TYPDIA,
     &                XA(1,ACQ(1,2,S)), XA(1,ACQ(1,3,S)),
     &                XA(1,ACQ(1,4,S)), XA(1,ACQ(1,5,S)),
     &                XA(1,ACQ(1,6,S)), XA(1,ACQ(2,1,S)),
     &                XA(1,ACQ(2,3,S)), XA(1,ACQ(2,4,S)),
     &                XA(1,ACQ(2,5,S)), XA(1,ACQ(2,6,S)),
     &                XA(1,ACQ(3,1,S)), XA(1,ACQ(3,2,S)),
     &                XA(1,ACQ(3,4,S)), XA(1,ACQ(3,5,S)),
     &                XA(1,ACQ(3,6,S)),
     &                TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                NPT,NPT2,NELEM,
     &                W(1,1),W(1,2),W(1,3),
     &                W(1,4),W(1,5),W(1,6))
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(IELM1.EQ.12.OR.IELM2.EQ.31.OR.IELM2.EQ.51) THEN
!
        IF(IELM2.EQ.12.OR.IELM2.EQ.31.OR.IELM2.EQ.51) THEN
          IF(TYPEXT(1:1).EQ.'S') THEN
            CALL MV0404(OP, X , DA,TYPDIA,
     &                  XA(1,BBS(1,2,S)),
     &                  XA(1,BBS(1,3,S)),
     &                  XA(1,BBS(1,4,S)),
     &                  XA(1,BBS(2,1,S)),
     &                  XA(1,BBS(2,3,S)),
     &                  XA(1,BBS(2,4,S)),
     &                  XA(1,BBS(3,1,S)),
     &                  XA(1,BBS(3,2,S)),
     &                  XA(1,BBS(3,4,S)),
     &                  XA(1,BBS(4,1,S)),
     &                  XA(1,BBS(4,2,S)),
     &                  XA(1,BBS(4,3,S)),
     &                  TYPEXT, Y,C,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                  NPT,NELEM,
     &                  W(1,1),W(1,2),W(1,3),W(1,4))
          ELSE
            CALL MV0404(OP, X , DA,TYPDIA,
     &                  XA(1,BBQ(1,2,S)),
     &                  XA(1,BBQ(1,3,S)),
     &                  XA(1,BBQ(1,4,S)),
     &                  XA(1,BBQ(2,1,S)),
     &                  XA(1,BBQ(2,3,S)),
     &                  XA(1,BBQ(2,4,S)),
     &                  XA(1,BBQ(3,1,S)),
     &                  XA(1,BBQ(3,2,S)),
     &                  XA(1,BBQ(3,4,S)),
     &                  XA(1,BBQ(4,1,S)),
     &                  XA(1,BBQ(4,2,S)),
     &                  XA(1,BBQ(4,3,S)),
     &                  TYPEXT, Y,C,
     &                  IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                  NPT,NELEM,
     &                  W(1,1),W(1,2),W(1,3),W(1,4))
          ENDIF
        ELSEIF(IELM2.EQ.11) THEN
          CALL MV0403(OP, X , DA,TYPDIA,
     &                XA(1,BAQ(1,2,S)),
     &                XA(1,BAQ(1,3,S)),
     &                XA(1,BAQ(2,1,S)),
     &                XA(1,BAQ(2,3,S)),
     &                XA(1,BAQ(3,1,S)),
     &                XA(1,BAQ(3,2,S)),
     &                XA(1,BAQ(4,1,S)),
     &                XA(1,BAQ(4,2,S)),
     &                XA(1,BAQ(4,3,S)),
     &                TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                NPT,NELEM,
     &                W(1,1),W(1,2),W(1,3),W(1,4))
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(IELM1.EQ.41.OR.IELM1.EQ.13) THEN
!
        IF(IELM2.EQ.41.OR.IELM2.EQ.13) THEN
!
          CALL MV0606(OP, X , DA,TYPDIA,XA,TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                NPT,NELEM,NELMAX,
     &                W(1,1),W(1,2),W(1,3),W(1,4),W(1,5),W(1,6))
!
        ELSEIF(IELM2.EQ.11) THEN
!
!         HERE IELM1=13
          NPT2=BIEF_NBPTS(IELM1,MESH)
          CALL MV0603(OP, X , DA,TYPDIA,
     &                XA(1,CAQ(1,2,S)),XA(1,CAQ(1,3,S)),
     &                XA(1,CAQ(2,1,S)),XA(1,CAQ(2,3,S)),
     &                XA(1,CAQ(3,1,S)),XA(1,CAQ(3,2,S)),
     &                XA(1,CAQ(4,1,S)),XA(1,CAQ(4,2,S)),
     &                XA(1,CAQ(4,3,S)),XA(1,CAQ(5,1,S)),
     &                XA(1,CAQ(5,2,S)),XA(1,CAQ(5,3,S)),
     &                XA(1,CAQ(6,1,S)),XA(1,CAQ(6,2,S)),
     &                XA(1,CAQ(6,3,S)),
     &                TYPEXT, Y,C,
     &                IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                NPT,NPT2,NELEM,
     &                W(1,1),W(1,2),W(1,3),
     &                W(1,4),W(1,5),W(1,6))
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
!
!  IELM1 NOT IMPLEMENTED : ERROR
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
        IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
100     FORMAT(1X,'MATVCT (BIEF) : ELEMENTS ',1I2,' ET ',1I2,/,1X,
     &            'ET STOCKAGE ',1I2,'   CAS NON PREVU')
101     FORMAT(1X,'MATVCT (BIEF) : ELEMENTS ',1I2,' AND ',1I2,/,1X,
     &            'AND STORAGE ',1I2,'   CASE NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     POSSIBLE FINAL ASSEMBLY OF X
!
!     SINCE INIT = FALSE HERE, MAY NOT NEED NPT
      NPT = BIEF_NBPTS(IELMX,MESH)
      IF(LEGO) CALL ASSVEC(X,IKLE,NPT,NELEM,NELMAX,IELMX,W,
     &                     .FALSE.,LV,.FALSE.,Z,NDP)
!
      ELSEIF(S.EQ.3.AND.P.EQ.2) THEN
!
!-----------------------------------------------------------------------
!
!  SEGMENT STORAGE AND FRONTAL MATRIX X VECTOR PRODUCT
!
!-----------------------------------------------------------------------
!
      IF(IELM1.EQ.1) THEN
!
        IF(IELM2.EQ.1) THEN
!         CALL MW0202(OP, X , DA,TYPDIA,
!    *                XA(1,1),XA(1,2),TYPEXT, Y,C,
!    *                IKLE(1,1),IKLE(1,2),
!    *                NPT,NELEM,
!    *                W(1,1),W(1,2))
!       ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(IELM1.EQ.11) THEN
!
        IF(IELM2.EQ.11) THEN
!
          CALL MW0303(OP, X , DA,TYPDIA,XA,TYPEXT, Y,C,
     &                IKLEM1,DIMIKM,LIMVOI,MXPTVS,NPMAX,NPOIN,W)
!
        ELSEIF(IELM2.EQ.12) THEN
!
!         CALL MW0304(OP, X , DA,TYPDIA,
!    *                XA(1,1),XA(1,2),XA(1,3),
!    *                XA(1,4),XA(1,5),XA(1,6),
!    *                XA(1,7),XA(1,8),XA(1,9),
!    *                TYPEXT, Y,C,
!    *                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
!    *                NPT,NELEM,
!    *                W(1,1),W(1,2),W(1,3),W(1,4))
!       ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(IELM1.EQ.12) THEN
!
        IF(IELM2.EQ.12) THEN
!           CALL MW0404(OP, X , DA,TYPDIA,
!    *                  XA(1,1),XA(1,2),XA(1,3),XA(1,1),
!    *                  XA(1,4),XA(1,5),XA(1,2),XA(1,4),
!    *                  XA(1,6),XA(1,3),XA(1,5),XA(1,6),
!    *                  TYPEXT, Y,C,
!    *                  IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
!    *                  NPT,NELEM,
!    *                  W(1,1),W(1,2),W(1,3),W(1,4))
!       ELSEIF(IELM2.EQ.11) THEN
!         CALL MW0403(OP, X , DA,TYPDIA,
!    *                XA(1,1),XA(1,2),XA(1,3),
!    *                XA(1,4),XA(1,5),XA(1,6),
!    *                XA(1,7),XA(1,8),XA(1,9),
!    *                TYPEXT, Y,C,
!    *                IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
!    *                NPT,NELEM,
!    *                W(1,1),W(1,2),W(1,3),W(1,4))
!       ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(IELM1.EQ.41) THEN
!
        IF(IELM2.EQ.41) THEN
!
!         CALL MW0606(OP, X , DA,TYPDIA,XA,TYPEXT, Y,C,
!    *                IKLE(1,1),IKLE(1,2),IKLE(1,3),
!    *                IKLE(1,4),IKLE(1,5),IKLE(1,6),
!    *                NPT,NELEM,NELMAX,
!    *                W(1,1),W(1,2),W(1,3),W(1,4),W(1,5),W(1,6))
!       ELSE
          IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
          IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
          CALL PLANTE(1)
          STOP
        ENDIF
!
!  IELM1 NOT IMPLEMENTED : ERROR
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,100) IELM1,IELM2,S
        IF (LNG.EQ.2) WRITE(LU,101) IELM1,IELM2,S
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!  STORAGE BY SEGMENTS
!
      ELSEIF(S.EQ.3.AND.P.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!  SEGMENT STORAGE AND TRADITIONAL MATRIX X VECTOR PRODUCT
!
!-----------------------------------------------------------------------
!
      NSEG1 = BIEF_NBSEG(IELM1,MESH)
      NSEG2 = BIEF_NBSEG(IELM2,MESH)
!
!     IN LINEAR-QUADRATIC RECTANGULAR MATRICES, PURELY QUADRATIC
!     SEGMENTS ARE NOT CONSIDERED (NUMBER 13,14 AND 15, SO 3 PER ELEMENT)
!
      IF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
        NSEG2=NSEG2-3*NELEM
      ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
        NSEG1=NSEG1-3*NELEM
      ENDIF
!
      IF(TYPEXT(1:1).EQ.'Q') THEN
        SYM = MIN(NSEG1,NSEG2)
      ELSE
        SYM = 0
      ENDIF
      CALL MVSEG (OP, X , DA,TYPDIA,XA(1,1),XA(SYM+1,1),
     &            TYPEXT,Y,C,NPT,NELEM,NSEG1,NSEG2,
     &            GLOSEG(1,1),GLOSEG(1,2),IELM1,IELM2)
!
!-----------------------------------------------------------------------
!
!  STORAGE NOT IMPLEMENTED
!
!-----------------------------------------------------------------------
!
      ELSE
        IF (LNG.EQ.1) WRITE(LU,102) S,P
        IF (LNG.EQ.2) WRITE(LU,103) S,P
102     FORMAT(1X,'MATVCT (BIEF) : ',1I2,' ET ',1I2,/,1X,
     &            'STOCKAGE ET PRODUIT MATRICE-VECTEUR INCOMPATIBLES')
103     FORMAT(1X,'MATVCT (BIEF) : ',1I2,' AND ',1I2,/,1X,
     &            'STORAGE AND MATRIX-VECTOR PRODUCT INCOMPATIBLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!=======================================================================
!
      RETURN
      END

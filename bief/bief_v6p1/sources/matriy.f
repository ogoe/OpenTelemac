!                    *****************
                     SUBROUTINE MATRIY
!                    *****************
!
     &(FORMUL,XM,TYPDIA,TYPEXT,
     & XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,T,LEGO,
     & XEL,YEL,ZEL,SURFAC,IKLE,NBOR,
     & NELEM,NELMAX,IELM1,IELM2,S,NPLAN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS BETWEEN MATRICES.
!+
!+            THE MATRIX IS IDENTIFIED BY THE FORMULATION IN
!+                CHARACTER STRING FORMUL.
!code
!+-----------------------------------------------------------------------
!+
!+  MEANING OF IELM AND IELM2
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS
!+
!+  A OR 11 : P1 TRIANGLE            3
!+  B OR 12 : QUASI-BUBBLE TRIANGLE  4
!+  C OR 13 : P1-ISO P1 TRIANGLE     6
!+  D OR 14 : P2 TRIANGLE            7
!+  E       : NOTHING FOR NOW
!+
!+  F OR 21 : Q1 QUADRILATERAL       4
!+  G OR 22 : Q2 QUADRILATERAL       8
!+  H OR 24 : Q2 QUADRILATERAL       9
!+
!+  T OR 31 : P1 TETRAHEDRON         4
!+
!+  P OR 41 : TELEMAC-3D PRISMS      6
!+
!+-----------------------------------------------------------------------
!
!history
!+        16/09/2005
!+
!+   MT04PP NOW IN 2 OPTIONS, AND WITHOUT VERTICAL UPWIND
!
!history  JM HERVOUET (LNHE)
!+        21/07/2008
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
!| F              |-->| FUNCTION USED IN THE FORMULA
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| G              |-->| FUNCTION USED IN THE FORMULA
!| H              |-->| FUNCTION USED IN THE FORMULA
!| IELM1          |-->| TYPE OF ELEMENT FOR LINES
!| IELM2          |-->| TYPE OF ELEMENT FOR COLUMNS
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LEGO           |-->| IF YES: ASSEMBLE THE DIAGONAL
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| S              |-->| TYPE OF MATRIX STORAGE
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SG             |-->| STRUCTURE OF FUNCTIONS G
!| SH             |-->| STRUCTURE OF FUNCTIONS H
!| SU             |-->| STRUCTURE OF FUNCTIONS U
!| SV             |-->| STRUCTURE OF FUNCTIONS V
!| SW             |-->| STRUCTURE OF FUNCTIONS W
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| TYPDIA         |<--| TYPE OF DIAGONAL
!| TYPEXT         |<--| TYPE OF OFF-DIAGONAL TERMS
!| U              |-->| FUNCTION USED IN THE FORMULA (COMPONENT OF VECTOR)
!| V              |-->| FUNCTION USED IN THE FORMULA (COMPONENT OF VECTOR)
!| W              |-->| FUNCTION USED IN THE FORMULA (COMPONENT OF VECTOR)
!| XEL            |-->| ABSCISSAE OF POINTS, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS, PER ELEMENT
!| ZEL            |-->| ELEVATIONS OF POINTS, PER ELEMENT
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MATRIY => MATRIY
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELMAX,NELEM,IELM1,IELM2,S
      INTEGER, INTENT(IN)             :: NPLAN
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*),NBOR(*)
      LOGICAL, INTENT(INOUT)          :: LEGO
      TYPE(BIEF_OBJ), INTENT(IN)      :: SF,SG,SH,SU,SV,SW
      DOUBLE PRECISION, INTENT(IN)    :: F(*),G(*),H(*),U(*),V(*),W(*)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(*),YEL(*),ZEL(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NELMAX,*),T(NELMAX,*)
      CHARACTER(LEN=16), INTENT(IN)   :: FORMUL
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIA,TYPEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL SIGMAG,INCHYD,SPECAD
!
      CHARACTER(LEN=1) TDIA,TEXT
!
!-----------------------------------------------------------------------
!
      INTEGER ICOORD
!
      INTEGER AAQ(3,3,2),BBQ(4,4,2),ABQ(3,4,2),BAQ(4,3,2),PPQ(6,6,2)
      INTEGER AAS(3,3,2),BBS(4,4,2),OOS(2,2,2),FFS(4,4,2),PPS(6,6,2)
      INTEGER ACQ(3,6,2),CAQ(6,3,2)
!
!  BEWARE: SHOULD TRANSPOSE THE FOLLOWING MATRICES IN NON-SPECIAL
!  CASES, BECAUSE OF THE FORTRAN NOTATION OF DATA
!
!  BEWARE: OM WAS NOT PARAMETERISED WITH THESE ARRAYS
!          THESE DATA ALSO APPEAR IN MATVCT
!
      DATA OOS/  0 ,  1 ,
     &           1 ,  0 ,
! S=2 NOT IMPLEMENTED
     &           0 ,  0 ,
     &           0 ,  0 /
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
!
!     NONSYMMETRICAL QUASI-BUBBLE P1 EBE (S=1)
      DATA BAQ/  0 ,  3 ,  5 ,  7 ,
     &           1 ,  0 ,  6 ,  8 ,
     &           2 ,  4 ,  0 ,  9 ,
!     NONSYMMETRICAL QUASI-BUBBLE P1 PRE-ASSEMBLED EBE (S=2)
     &           0 ,  7 ,  3 ,  4 ,
     &           1 ,  0 ,  8 ,  5 ,
     &           9 ,  2 ,  0 ,  6 /
!     NONSYMMETRICAL P1 P2 EBE (S=1)
      DATA ACQ/   0 ,  6  , 11 ,
     &            1 ,  0  , 12 ,
     &            2 ,  7  ,  0 ,
     &            3 ,  8  , 13 ,
     &            4 ,  9  , 14 ,
     &            5 ,  10 , 15 ,
! S=2 NOT IMPLEMENTED
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  ,
     &            0 ,  0 ,  0  /
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
!     ADDED BY JMJ BUT NOT USED
!     SYMMETRICAL P1-P1 PRISMS AND P2 TRIANGLES EBE (S=1)
      DATA PPS/  0 ,  1 ,  2 ,  3 ,  4 ,  5 ,
     &           1 ,  0 ,  6 ,  7 ,  8 ,  9 ,
     &           2 ,  6 ,  0 , 10 , 11 , 12 ,
     &           3 ,  7 , 10 ,  0 , 13 , 14 ,
     &           4 ,  8 , 11 , 13 ,  0 , 15 ,
     &           5 ,  9 , 12 , 14 , 15 ,  0 ,
!     SYMMETRICAL P1-P1 PRISMS PRE-ASSEMBLED EBE (S=2) - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
!
!     NONSYMMETRICAL P1-P1 PRISMS EBE (S=1)
      DATA PPQ/  0 , 16 , 17 , 18 , 19 , 20 ,
     &           1 ,  0 , 21 , 22 , 23 , 24 ,
     &           2 ,  6 ,  0 , 25 , 26 , 27 ,
     &           3 ,  7 , 10 ,  0 , 28 , 29 ,
     &           4 ,  8 , 11 , 13 ,  0 , 30 ,
     &           5 ,  9 , 12 , 14 , 15 ,  0 ,
!     NONSYMMETRICAL P1-P1 PRISMS PRE-ASSEMBLED EBE (S=2) - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,  0 ,  0 /
!
!     SYMMETRICAL Q1-Q1 QUADRANGLES EBE (S=1)
      DATA FFS/  0 ,  1 ,  2 ,  3 ,
     &           1 ,  0 ,  4 ,  5 ,
     &           2 ,  4 ,  0 ,  6 ,
     &           3 ,  5 ,  6 ,  0 ,
!     SYMMETRICAL Q1-Q1 QUADRANGLES PRE-ASSEMBLED EBE (S=2) - NOT IMPLEMENTED
     &           0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 ,
     &           0 ,  0 ,  0 ,  0 /
!
!-----------------------------------------------------------------------
!
!  TESTS THE TYPE OF MATRIX
!
!=======================================================================
!     MASS MATRIX
!=======================================================================
!
      IF(FORMUL(1:16).EQ.'MATMAS          ') THEN
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT01AA(   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SURFAC,NELEM,NELMAX)
!
            TYPDIA='Q'
            TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!.......................................................................
!         ERROR ON THE COLUMN ELEMENT
!.......................................................................
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
          CALL MT01BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &      XMUL,SURFAC,NELEM,NELMAX)
!
            TYPDIA='Q'
            TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!.......................................................................
!         ERROR ON THE COLUMN ELEMENT
!.......................................................................
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P2 TRIANGLE
!-----------------------------------------------------------------------
!
        ELSEIF(IELM1.EQ.13) THEN
!
!       TESTS THE COLUMN ELEMENT
!
!.......................................................................
!         P2 TRIANGLE
!.......................................................................
!
          IF(IELM2.EQ.13) THEN
             CALL MT01CC
     & (   T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &     T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &     T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),
     &   XM(1,PPS(3,6,S)),
     &     T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &     T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6),
     &     XMUL,SURFAC,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P1 SEGMENT ROW ELEMENT
        ELSEIF(IELM1.EQ.1) THEN
!.......................................................................
!         P1 SEGMENT COLUMN ELEMENT
          IF(IELM2.EQ.1.AND.S.EQ.1) THEN
             CALL MT01OO(   T(1,1)   ,XM(1,OOS(1,2,S)),
     &                                       T(1,2)   ,
     &                   XMUL,SURFAC,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER COLUMN ELEMENT
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!>>>>
!-----------------------------------------------------------------------
!       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
             CALL MT01PP(T,XM,XMUL,ZEL,SURFAC,IKLE,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       T1 TETRAHEDRON ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT01TT(T,XM,XMUL,XEL,YEL,ZEL,IKLE,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER ROW ELEMENT
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!     DIFFUSION MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'MATDIF') THEN
!
!     CHARACTER 7 INFORMS WHETHER INCHYD OR NOT
!
      INCHYD = .FALSE.
      IF(FORMUL(7:7).EQ.'2') INCHYD = .TRUE.
!
!     TESTS THE ROW ELEMENT
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
!
!     CHARACTER 7 ALSO INFORMS WHETHER THE DIFFUSION TERM IS REQUIRED
!     FOR ESTEL
!
      IF(FORMUL(7:7).NE.'3') THEN
!
             CALL MT02AA(  T(1,1),XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                   T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                      T(1,3) ,
     &                         XMUL,SU,U,SV,V,XEL,YEL,SURFAC,
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,FORMUL)
!
      ELSE
!
             CALL MT02AA_2( T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SU,SV,U,V,XEL,YEL,SURFAC,NELEM,NELMAX)
!
      ENDIF
!
      TYPDIA='Q'
      TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT02BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &          XMUL,SU,U,XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P2 TRIANGLE ROW ELEMENT
!
        ELSEIF(IELM1.EQ.13) THEN
!
!.......................................................................
!         P2 TRIANGLE COLUMN ELEMENT
!         USES MATRIX PPS BECAUSE WILL BE A 6X6 SYMMETRICAL MATRIX
!
          IF(IELM2.EQ.13) THEN
             CALL MT02CC
     & (   T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &     T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &     T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),
     &   XM(1,PPS(3,6,S)),
     &     T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &     T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6),
     &          XMUL,SU,U,XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
!
          IF(FORMUL(7:7).EQ.'*') THEN
!           COMPUTATION BASED ON THE TRANSFORMED MESH
            CALL MT02PP_STAR(T,XM,XMUL,SF,SG,SH,F,G,H,
     &                      XEL,YEL,ZEL,SURFAC,IKLE,NELEM,NELMAX,INCHYD,
     &                      FORMUL,NPLAN)
            IF(FORMUL(10:13).EQ.'1234') THEN
              TYPEXT='S'
            ELSEIF(FORMUL(10:13).EQ.'1 3 ') THEN
              TYPEXT='Q'
            ELSEIF(FORMUL(10:13).EQ.' 2 4') THEN
              TYPEXT='Q'
            ELSE
              WRITE(LU,*) 'ERROR ON FORMULA=',FORMUL
              CALL PLANTE(1)
              STOP
            ENDIF
          ELSE
!           COMPUTATION IN REAL MESH
!           CALL MT02PT(T,XM,XMUL,SF,SG,SH,F,G,H,
!    *                  XEL,YEL,ZEL,IKLE,NELEM,NELMAX,INCHYD)
            CALL MT02PP(T,XM,XMUL,SF,SG,SH,F,G,H,
     &                  XEL,YEL,ZEL,SURFAC,IKLE,NELEM,NELMAX,INCHYD,
     &                  FORMUL,NPLAN)
            TYPEXT='S'
          ENDIF
          TYPDIA='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       TETRAHEDRON ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
!
          CALL MT02TT(T,XM,XMUL,SF,SG,SH,F,G,H,
     &                XEL,YEL,ZEL,IKLE,NELEM,NELMAX,INCHYD)
!
          TYPDIA='Q'
          TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER ROW ELEMENT
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!     CONTRIBUTION OF SUPG TO THE MASS MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'MASUPG          ') THEN
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT03AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT03BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SF,SG,SU,SV,F,G,U,V,
     &          XEL,YEL,IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P2 TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.13) THEN
!
!.......................................................................
!         P2 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.13) THEN
             CALL MT03CC
     & (   T(1,1)   ,XM(1,PPQ(1,2,S)),XM(1,PPQ(1,3,S)),
     &   XM(1,PPQ(1,4,S)),XM(1,PPQ(1,5,S)),XM(1,PPQ(1,6,S)),
     &   XM(1,PPQ(2,1,S)),T(1,2),XM(1,PPQ(2,3,S)),XM(1,PPQ(2,4,S)),
     &   XM(1,PPQ(2,5,S)),XM(1,PPQ(2,6,S)),XM(1,PPQ(3,1,S)),
     &   XM(1,PPQ(3,2,S)),T(1,3),XM(1,PPQ(3,4,S)),XM(1,PPQ(3,5,S)),
     &   XM(1,PPQ(3,6,S)),XM(1,PPQ(4,1,S)),XM(1,PPQ(4,2,S)),
     &   XM(1,PPQ(4,3,S)),T(1,4),XM(1,PPQ(4,5,S)),XM(1,PPQ(4,6,S)),
     &   XM(1,PPQ(5,1,S)),XM(1,PPQ(5,2,S)),XM(1,PPQ(5,3,S)),
     &   XM(1,PPQ(5,4,S)),  T(1,5)   ,XM(1,PPQ(5,6,S)),
     &   XM(1,PPQ(6,1,S)),XM(1,PPQ(6,2,S)) ,XM(1,PPQ(6,3,S)),
     &   XM(1,PPQ(6,4,S)),XM(1,PPQ(6,5,S)),T(1,6),
     &          XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &          NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='Q'
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!     U.GRAD U.GRAD MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'MAUGUG') THEN
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT04AA(   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SU,SV,U,V,XEL,YEL,SURFAC,IKLE,
     &                   NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT04BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &          XMUL,SU,SV,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P2 TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.13) THEN
!
!.......................................................................
!         P2 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.13) THEN
             CALL MT04CC
     & (   T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &     T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &     T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),
     &   XM(1,PPS(3,6,S)),
     &     T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &     T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6),
     &          XMUL,SU,SV,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
             CALL MT04PP(T,XM,XMUL,SU,SV,SW,U,V,W,
     &                   XEL,YEL,ZEL,SURFAC,IKLE,NELEM,NELMAX,FORMUL)
!
             TYPDIA='Q'
             IF(FORMUL(7:7).EQ.'2') THEN
               TYPEXT='S'
             ELSE
               TYPEXT='Q'
             ENDIF
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       TETRAHEDRON ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT04TT(T,XM,XMUL,SU,SV,SW,U,V,W,
     &                   XEL,YEL,ZEL,IKLE,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER ROW ELEMENT
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!     U.GRAD MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'MATVGR') THEN
!
      SIGMAG = .FALSE.
      IF(FORMUL(7:7).EQ.'2') SIGMAG = .TRUE.
      SPECAD = .FALSE.
      IF(FORMUL(8:8).EQ.'2') SPECAD = .TRUE.
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT05AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SU,SV,U,V,XEL,YEL,IKLE,
     &                   NELEM,NELMAX,FORMUL)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT05BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SU,SV,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,FORMUL)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P2 TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.13) THEN
!
!.......................................................................
!         P2 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.13) THEN
             CALL MT05CC
     & (   T(1,1)   ,XM(1,PPQ(1,2,S)),XM(1,PPQ(1,3,S)),
     &   XM(1,PPQ(1,4,S)),XM(1,PPQ(1,5,S)),XM(1,PPQ(1,6,S)),
     &   XM(1,PPQ(2,1,S)),T(1,2),XM(1,PPQ(2,3,S)),XM(1,PPQ(2,4,S)),
     &   XM(1,PPQ(2,5,S)),XM(1,PPQ(2,6,S)),XM(1,PPQ(3,1,S)),
     &   XM(1,PPQ(3,2,S)),T(1,3),XM(1,PPQ(3,4,S)),XM(1,PPQ(3,5,S)),
     &   XM(1,PPQ(3,6,S)),XM(1,PPQ(4,1,S)),XM(1,PPQ(4,2,S)),
     &   XM(1,PPQ(4,3,S)),T(1,4),XM(1,PPQ(4,5,S)),XM(1,PPQ(4,6,S)),
     &   XM(1,PPQ(5,1,S)),XM(1,PPQ(5,2,S)),XM(1,PPQ(5,3,S)),
     &   XM(1,PPQ(5,4,S)),  T(1,5)   ,XM(1,PPQ(5,6,S)),
     &   XM(1,PPQ(6,1,S)),XM(1,PPQ(6,2,S)) ,XM(1,PPQ(6,3,S)),
     &   XM(1,PPQ(6,4,S)),XM(1,PPQ(6,5,S)),T(1,6),
     &          XMUL,SU,SV,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &          NELEM,NELMAX,FORMUL)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
             CALL MT05PP(T,XM,XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,
     &                   XEL,YEL,ZEL,IKLE,NELEM,NELMAX,SURFAC,SIGMAG,
     &                   SPECAD,NPLAN)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       TETRAHEDRON ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT05TT(T,XM,XMUL,SU,SV,SW,U,V,W,
     &                   XEL,YEL,ZEL,IKLE,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!     F PSI PSJ MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'FMATMA') THEN
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
!
             CALL MT06AA(   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SF,F,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX)
!
            TYPDIA='Q'
            TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
!       (LATERAL SIDE OF PRISM SPLIT IN TETRAHEDRONS)
!
        ELSEIF(IELM1.EQ.61.OR.IELM1.EQ.81) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.61.OR.IELM1.EQ.81) THEN
!
!     CHARACTER 7 ALSO INFORMS WHETHER THE ADDITIONAL TERM IS REQUIRED
!     FOR ESTEL-3D
!
      IF(FORMUL(7:7).NE.'2') THEN
!
                CALL MT06FT
     & (   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                      T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                       T(1,3)   ,
     &          XMUL,SF,F,XEL,YEL,ZEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          NBOR,NELEM,NELMAX)
!
      ELSE
                CALL MT06FT2
     & (   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                      T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                       T(1,3)   ,
     &          XMUL,SF,F,SU,U,XEL,YEL,ZEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          NBOR,NELEM,NELMAX)
      ENDIF
            TYPDIA='Q'
            TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
!
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT06BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &          XMUL,SF,F,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUADRATIC TRIANGLE ROW ELEMENT
!
        ELSEIF(IELM1.EQ.13) THEN
!
!.......................................................................
!         QUADRATIC TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.13) THEN
             CALL MT06CC
     & ( T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &   T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &   T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),XM(1,PPS(3,6,S)),
     &   T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &   T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6)          ,
     &          XMUL,SF,F,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUADRILATERAL ROW ELEMENT (LATERAL SIDE OF PRISM)
        ELSEIF(IELM1.EQ.71) THEN
!
!.......................................................................
!
!         BEWARE !!!!!!!!!!
!         QUADRANGLE COLUMN ELEMENT FOR TELEMAC-3D PRISMS
          IF(IELM2.EQ.71) THEN
!
               CALL MT06FF
     & (   T(1,1)   ,XM(1,FFS(1,2,S)),XM(1,FFS(1,3,S)),XM(1,FFS(1,4,S)),
     &                      T(1,2)   ,XM(1,FFS(2,3,S)),XM(1,FFS(2,4,S)),
     &                                       T(1,3)   ,XM(1,FFS(3,4,S)),
     &                                                        T(1,4)   ,
     &         XMUL,SF,F,XEL,YEL,ZEL,
     &         IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &         NBOR,NELEM,NELMAX)
!
               TYPDIA='Q'
               TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!<<<<
!-----------------------------------------------------------------------
!       P1 SEGMENT ROW ELEMENT
        ELSEIF(IELM1.EQ.1) THEN
!.......................................................................
!         P1 SEGMENT COLUMN ELEMENT
          IF(IELM2.EQ.1.AND.S.EQ.1) THEN
             CALL MT06OO(   T(1,1)   ,XM(1,OOS(1,2,S)),
     &                                       T(1,2)   ,
     &                   XMUL,SF,F,SURFAC,IKLE(1,1),IKLE(1,2),
     &                   NBOR,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!<<<<
!-----------------------------------------------------------------------
!       P2 SEGMENT ROW ELEMENT
        ELSEIF(IELM1.EQ.2) THEN
!.......................................................................
!         P2 SEGMENT COLUMN ELEMENT
          IF(IELM2.EQ.2.AND.S.EQ.1) THEN
               CALL MT06OC
     & (   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                      T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                       T(1,3)   ,
     &        XMUL,SF,F,SURFAC,IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NBOR,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P1 PRISM ROW ELEMENT
!
        ELSE IF (IELM1.EQ.41) THEN
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF (IELM2.EQ.41) THEN
             CALL MT06PP(T,XM,
     &                   XMUL,SF,F,ZEL,SURFAC,IKLE,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!             CALL SETDIA(M,'Q')
!             CALL SETEXT(M,'S')
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!
!-----------------------------------------------------------------------
!       TETRAHEDRON ROW ELEMENT
!
        ELSE IF (IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF (IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT06TT(T,XM,
     &                   XMUL,SF,F,
     &                   XEL,YEL,ZEL,IKLE,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!             CALL SETDIA(M,'Q')
!             CALL SETEXT(M,'S')
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!     MASS-LUMPED MASS MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'MSLUMP          ') THEN
!
!     TESTS THE ROW ELEMENT
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE
!-----------------------------------------------------------------------
!
        IF(IELM1.EQ.11) THEN
!
!       TESTS THE COLUMN ELEMENT
!
!.......................................................................
!         P1 TRIANGLE
!.......................................................................
!
          IF(IELM2.EQ.11) THEN
             CALL MT07AA(   T(1,1)   ,XM(1,AAS(1,2,S)),XM(1,AAS(1,3,S)),
     &                                       T(1,2)   ,XM(1,AAS(2,3,S)),
     &                                                        T(1,3)   ,
     &                   XMUL,SF,F,SURFAC,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE
!-----------------------------------------------------------------------
!
        ELSEIF(IELM1.EQ.12) THEN
!
!       TESTS THE COLUMN ELEMENT
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE
!.......................................................................
!
          IF(IELM2.EQ.12) THEN
             CALL MT07BB
     & (   T(1,1)   ,XM(1,BBS(1,2,S)),XM(1,BBS(1,3,S)),XM(1,BBS(1,4,S)),
     &                      T(1,2)   ,XM(1,BBS(2,3,S)),XM(1,BBS(2,4,S)),
     &                                       T(1,3)   ,XM(1,BBS(3,4,S)),
     &                                                        T(1,4)   ,
     &          XMUL,SF,F,SURFAC,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P2 TRIANGLE
!-----------------------------------------------------------------------
!
        ELSEIF(IELM1.EQ.13) THEN
!
!       TESTS THE COLUMN ELEMENT
!
!.......................................................................
!         P2 TRIANGLE
!.......................................................................
!
          IF(IELM2.EQ.13) THEN
             CALL MT07CC
     & (   T(1,1)   ,XM(1,PPS(1,2,S)),XM(1,PPS(1,3,S)),
     &   XM(1,PPS(1,4,S)),XM(1,PPS(1,5,S)),XM(1,PPS(1,6,S)),
     &     T(1,2)   ,XM(1,PPS(2,3,S)),XM(1,PPS(2,4,S)),
     &   XM(1,PPS(2,5,S)),XM(1,PPS(2,6,S)),
     &     T(1,3)   ,XM(1,PPS(3,4,S)),XM(1,PPS(3,5,S)),
     &   XM(1,PPS(3,6,S)),
     &     T(1,4)   ,XM(1,PPS(4,5,S)),XM(1,PPS(4,6,S)),
     &     T(1,5)   ,XM(1,PPS(5,6,S)),T(1,6),
     &          XMUL,SF,F,SURFAC,NELEM,NELMAX)
!
             TYPDIA='Q'
             TYPEXT='S'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!     U GRADIENT MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:15).EQ.'MATFGR         ') THEN
!
!     CHARACTER 16 IS THE SELECTED COORDINATE
!
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
!-----------------------------------------------------------------------
!
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
!.......................................................................
!
          IF(IELM2.EQ.11) THEN
             CALL MT08AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT08AB
     & (   T(1,1)   ,XM(1,ABQ(1,2,S)),XM(1,ABQ(1,3,S)),XM(1,ABQ(1,4,S)),
     &  XM(1,ABQ(2,1,S)),   T(1,2)   ,XM(1,ABQ(2,3,S)),XM(1,ABQ(2,4,S)),
     &  XM(1,ABQ(3,1,S)),XM(1,ABQ(3,2,S)),   T(1,3)   ,XM(1,ABQ(3,4,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
           ELSEIF(IELM2.EQ.13) THEN
             CALL MT08AC(   T(1,1)   ,XM(1,ACQ(1,2,S)),XM(1,ACQ(1,3,S)),
     &                   XM(1,ACQ(1,4,S)),XM(1,ACQ(1,5,S)),
     &                   XM(1,ACQ(1,6,S)),XM(1,ACQ(2,1,S)),
     &                   T(1,2)  ,XM(1,ACQ(2,3,S)),
     &                   XM(1,ACQ(2,4,S)),XM(1,ACQ(2,5,S)),
     &                   XM(1,ACQ(2,6,S)),XM(1,ACQ(3,1,S)),
     &                   XM(1,ACQ(3,2,S)),T(1,3)  ,XM(1,ACQ(3,4,S)),
     &                   XM(1,ACQ(3,5,S)),XM(1,ACQ(3,6,S)),
     &                   XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                   NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT08BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!         LINEAR TRIANGLE COLUMN ELEMENT
          ELSEIF(IELM2.EQ.11) THEN
             CALL MT08BA
     &         (     T(1,1)     ,XM(1,BAQ(1,2,S)),XM(1,BAQ(1,3,S)),
     &          XM(1,BAQ(2,1,S)),     T(1,2)     ,XM(1,BAQ(2,3,S)),
     &          XM(1,BAQ(3,1,S)),XM(1,BAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,BAQ(4,1,S)),XM(1,BAQ(4,2,S)),XM(1,BAQ(4,3,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!
!       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.41) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41.AND.ICOORD.EQ.3) THEN
             CALL MT08PP(T,XM,XMUL,SF,F,SURFAC,IKLE,NELEM,NELMAX)
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P1 PRISM ROW ELEMENT
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.31.OR.IELM2.EQ.51) THEN
             CALL MT08TT(T,XM,XMUL,XEL,YEL,ZEL,SF,F,IKLE,NELEM,NELMAX)
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!     F U GRADIENT MATRIX (NOT IMPLEMENTED)
!=======================================================================
!
!     ELSEIF(FORMUL(1:15).EQ.'MATQGR         ') THEN
!
!     CHARACTER 16 IS THE SELECTED COORDINATE
!
!     IF(FORMUL(16:16).EQ.'X') THEN
!       ICOORD=1
!     ELSEIF(FORMUL(16:16).EQ.'Y') THEN
!       ICOORD=2
!     ELSEIF(FORMUL(16:16).EQ.'Z') THEN
!       ICOORD=3
!     ENDIF
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
!-----------------------------------------------------------------------
!
!       IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
!.......................................................................
!
!         IF(IELM1.EQ.11) THEN
!            CALL MT09AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
!    *                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
!    *                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
!    *                   XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
!    *                   NELEM,NELMAX,ICOORD)
!
!            TYPDIA='Q'
!            TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
!         ELSE
!           IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
!           IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
!           IF (LNG.EQ.1) WRITE(LU,2000) IELM1
!           IF (LNG.EQ.2) WRITE(LU,2001) IELM1
!           IF (LNG.EQ.1) WRITE(LU,3000) IELM2
!           IF (LNG.EQ.2) WRITE(LU,3001) IELM2
!           CALL PLANTE(1)
!           STOP
!         ENDIF
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
!       ELSE
!         IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
!         IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
!         IF (LNG.EQ.1) WRITE(LU,2000) IELM1
!         IF (LNG.EQ.2) WRITE(LU,2001) IELM1
!         CALL PLANTE(1)
!         STOP
!       ENDIF
!
!=======================================================================
!     U.N MATRIX  (NOT IMPLEMENTED)
!=======================================================================
!
!     ELSEIF(FORMUL(1:15).EQ.'??????         ') THEN
!
!     CHARACTER 16 IS THE SELECTED COORDINATE
!
!     IF(FORMUL(16:16).EQ.'X') THEN
!       ICOORD=1
!     ELSEIF(FORMUL(16:16).EQ.'Y') THEN
!       ICOORD=2
!     ELSEIF(FORMUL(16:16).EQ.'Z') THEN
!       ICOORD=3
!     ENDIF
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
!-----------------------------------------------------------------------
!
!       IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
!.......................................................................
!
!         IF(IELM1.EQ.11) THEN
!            CALL MT10AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
!    *                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
!    *                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
!    *                   XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
!    *                   NELEM,NELMAX,ICOORD)
!
!            TYPDIA='Q'
!            TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
!         ELSE
!           IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
!           IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
!           IF (LNG.EQ.1) WRITE(LU,2000) IELM1
!           IF (LNG.EQ.2) WRITE(LU,2001) IELM1
!           IF (LNG.EQ.1) WRITE(LU,3000) IELM2
!           IF (LNG.EQ.2) WRITE(LU,3001) IELM2
!           CALL PLANTE(1)
!           STOP
!         ENDIF
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
!       ELSE
!         IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
!         IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
!         IF (LNG.EQ.1) WRITE(LU,2000) IELM1
!         IF (LNG.EQ.2) WRITE(LU,2001) IELM1
!         CALL PLANTE(1)
!         STOP
!       ENDIF
!
!=======================================================================
!     - PSIJ GRAD(F PSII) MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:15).EQ.'MATGRF         ') THEN
!
!     CHARACTER 16 IS THE SELECTED COORDINATE
!
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
!-----------------------------------------------------------------------
!
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
!.......................................................................
!
          IF(IELM2.EQ.11) THEN
             CALL MT11AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT11AB
     & (   T(1,1)   ,XM(1,ABQ(1,2,S)),XM(1,ABQ(1,3,S)),XM(1,ABQ(1,4,S)),
     &  XM(1,ABQ(2,1,S)),   T(1,2)   ,XM(1,ABQ(2,3,S)),XM(1,ABQ(2,4,S)),
     &  XM(1,ABQ(3,1,S)),XM(1,ABQ(3,2,S)),   T(1,3)   ,XM(1,ABQ(3,4,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!
          ELSEIF(IELM2.EQ.13) THEN
             CALL MT11AC(
     &       T(1,1)   ,XM(1,ACQ(1,2,S)),XM(1,ACQ(1,3,S)),
     &       XM(1,ACQ(1,4,S)),XM(1,ACQ(1,5,S)),
     &       XM(1,ACQ(1,6,S)),XM(1,ACQ(2,1,S)),
     &       T(1,2)  ,XM(1,ACQ(2,3,S)),
     &       XM(1,ACQ(2,4,S)),XM(1,ACQ(2,5,S)),
     &       XM(1,ACQ(2,6,S)),XM(1,ACQ(3,1,S)),
     &       XM(1,ACQ(3,2,S)),T(1,3)  ,XM(1,ACQ(3,4,S)),
     &       XM(1,ACQ(3,5,S)),XM(1,ACQ(3,6,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &          IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT11BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!         LINEAR TRIANGLE COLUMN ELEMENT
          ELSEIF(IELM2.EQ.11) THEN
             CALL MT11BA
     &         (     T(1,1)     ,XM(1,BAQ(1,2,S)),XM(1,BAQ(1,3,S)),
     &          XM(1,BAQ(2,1,S)),     T(1,2)     ,XM(1,BAQ(2,3,S)),
     &          XM(1,BAQ(3,1,S)),XM(1,BAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,BAQ(4,1,S)),XM(1,BAQ(4,2,S)),XM(1,BAQ(4,3,S)),
     &          XMUL,SF,F,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!   PSIJ GRAD(F)   U .GRAD(PSII) MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:15).EQ.'MATUGH         ') THEN
!
!     CHARACTER 16 IS THE SELECTED COORDINATE
!
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
!-----------------------------------------------------------------------
!
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
!.......................................................................
!
          IF(IELM2.EQ.11) THEN
             CALL MT12AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,SU,SV,F,U,V,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT12AB
     & (   T(1,1)   ,XM(1,ABQ(1,2,S)),XM(1,ABQ(1,3,S)),XM(1,ABQ(1,4,S)),
     &  XM(1,ABQ(2,1,S)),   T(1,2)   ,XM(1,ABQ(2,3,S)),XM(1,ABQ(2,4,S)),
     &  XM(1,ABQ(3,1,S)),XM(1,ABQ(3,2,S)),   T(1,3)   ,XM(1,ABQ(3,4,S)),
     &          XMUL,SF,SU,SV,F,U,V,
     &          XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         P2 TRIANGLE COLUMN ELEMENT
!.......................................................................
!
          ELSEIF(IELM2.EQ.13) THEN
             CALL MT12AC(
     &       T(1,1)   ,XM(1,ACQ(1,2,S)),XM(1,ACQ(1,3,S)),
     &       XM(1,ACQ(1,4,S)),XM(1,ACQ(1,5,S)),
     &       XM(1,ACQ(1,6,S)),XM(1,ACQ(2,1,S)),
     &       T(1,2)  ,XM(1,ACQ(2,3,S)),
     &       XM(1,ACQ(2,4,S)),XM(1,ACQ(2,5,S)),
     &       XM(1,ACQ(2,6,S)),XM(1,ACQ(3,1,S)),
     &       XM(1,ACQ(3,2,S)),T(1,3)  ,XM(1,ACQ(3,4,S)),
     &       XM(1,ACQ(3,5,S)),XM(1,ACQ(3,6,S)),
     &          XMUL,SF,SU,SV,F,U,V,
     &          XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          IKLE(1,5),IKLE(1,6),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
             CALL MT12BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
          ELSEIF(IELM2.EQ.11) THEN
             CALL MT12BA
     &         (     T(1,1)     ,XM(1,BAQ(1,2,S)),XM(1,BAQ(1,3,S)),
     &          XM(1,BAQ(2,1,S)),     T(1,2)     ,XM(1,BAQ(2,3,S)),
     &          XM(1,BAQ(3,1,S)),XM(1,BAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,BAQ(4,1,S)),XM(1,BAQ(4,2,S)),XM(1,BAQ(4,3,S)),
     &          XMUL,SF,SU,SV,F,U,V,
     &          XEL,YEL,SURFAC,
     &          IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &          NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!=======================================================================
!   PSIJ GRAD(PSII) MATRIX  (SIGN HAS CHANGED COMPARED TO 3.0)
!   (PSIJ GRAD(PSII) IN THE CASE OF B/A)
!=======================================================================
!
      ELSEIF(FORMUL(1:15).EQ.'MATGRA         ') THEN
!
!     CHARACTER 16 IS THE SELECTED COORDINATE
!
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
!-----------------------------------------------------------------------
!
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
!.......................................................................
!
          IF(IELM2.EQ.11) THEN
             CALL MT13AA(   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT13AB(   T(1,1)   ,XM(1,ABQ(1,2,S)),XM(1,ABQ(1,3,S)),
     &                   XM(1,ABQ(1,4,S)),
     &                   XM(1,ABQ(2,1,S)),   T(1,2)   ,XM(1,ABQ(2,3,S)),
     &                   XM(1,ABQ(2,4,S)),
     &                   XM(1,ABQ(3,1,S)),XM(1,ABQ(3,2,S)),   T(1,3)   ,
     &                   XM(1,ABQ(3,4,S)),
     &                   XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT13BA
     &         (     T(1,1)     ,XM(1,BAQ(1,2,S)),XM(1,BAQ(1,3,S)),
     &          XM(1,BAQ(2,1,S)),     T(1,2)     ,XM(1,BAQ(2,3,S)),
     &          XM(1,BAQ(3,1,S)),XM(1,BAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,BAQ(4,1,S)),XM(1,BAQ(4,2,S)),XM(1,BAQ(4,3,S)),
     &          XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          ELSEIF(IELM2.EQ.12) THEN
             CALL MT13BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &          XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       P2 TRIANGLE ROW ELEMENT
        ELSEIF(IELM1.EQ.13) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
             CALL MT13CA
     &         (     T(1,1)     ,XM(1,CAQ(1,2,S)),XM(1,CAQ(1,3,S)),
     &          XM(1,CAQ(2,1,S)),     T(1,2)     ,XM(1,CAQ(2,3,S)),
     &          XM(1,CAQ(3,1,S)),XM(1,CAQ(3,2,S)),     T(1,3)     ,
     &          XM(1,CAQ(4,1,S)),XM(1,CAQ(4,2,S)),XM(1,CAQ(4,3,S)),
     &          XM(1,CAQ(5,1,S)),XM(1,CAQ(5,2,S)),XM(1,CAQ(5,3,S)),
     &          XM(1,CAQ(6,1,S)),XM(1,CAQ(6,2,S)),XM(1,CAQ(6,3,S)),
     &          XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         P2 TRIANGLE COLUMN ELEMENT
          ELSEIF(IELM2.EQ.13) THEN
             CALL MT13CC
     & (   T(1,1)   ,XM(1,PPQ(1,2,S)),XM(1,PPQ(1,3,S)),
     &   XM(1,PPQ(1,4,S)),XM(1,PPQ(1,5,S)),XM(1,PPQ(1,6,S)),
     &   XM(1,PPQ(2,1,S)),T(1,2),XM(1,PPQ(2,3,S)),XM(1,PPQ(2,4,S)),
     &   XM(1,PPQ(2,5,S)),XM(1,PPQ(2,6,S)),XM(1,PPQ(3,1,S)),
     &   XM(1,PPQ(3,2,S)),T(1,3),XM(1,PPQ(3,4,S)),XM(1,PPQ(3,5,S)),
     &   XM(1,PPQ(3,6,S)),XM(1,PPQ(4,1,S)),XM(1,PPQ(4,2,S)),
     &   XM(1,PPQ(4,3,S)),T(1,4),XM(1,PPQ(4,5,S)),XM(1,PPQ(4,6,S)),
     &   XM(1,PPQ(5,1,S)),XM(1,PPQ(5,2,S)),XM(1,PPQ(5,3,S)),
     &   XM(1,PPQ(5,4,S)),  T(1,5)   ,XM(1,PPQ(5,6,S)),
     &   XM(1,PPQ(6,1,S)),XM(1,PPQ(6,2,S)) ,XM(1,PPQ(6,3,S)),
     &   XM(1,PPQ(6,4,S)),XM(1,PPQ(6,5,S)),T(1,6),
     &          XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
!
             TYPDIA='Q'
             TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!-----------------------------------------------------------------------
!         ERROR ON THE COLUMN ELEMENT
!-----------------------------------------------------------------------
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER ROW ELEMENT
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
!>>>>
!=======================================================================
!     MURD MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'MAMURD') THEN
!
!     CHARACTER 7 INFORMS WHETHER SIGMAG OR NOT
!
      SIGMAG = .FALSE.
      IF(FORMUL(7:7).EQ.'2') SIGMAG = .TRUE.
!
!     CHARACTER 8 GIVES THE DETAILS FOR CALL TO VC04PP
!
      SPECAD = .FALSE.
      IF(FORMUL(8:8).EQ.'2') SPECAD = .TRUE.
!
!     CHARACTERS 14 TO 16 GIVE THE SCHEME OPTION
!
      IF(FORMUL(14:16).EQ.'PSI') LEGO = .FALSE.
!
!-----------------------------------------------------------------------
!       P1 PRISM ROW ELEMENT
        IF(IELM1.EQ.41) THEN
!
!.......................................................................
!         P1 PRISM COLUMN ELEMENT
          IF(IELM2.EQ.41) THEN
             CALL MT14PP(T,XM,PPQ(1,1,S),LEGO,
     &                   XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,
     &                   XEL,YEL,ZEL,SURFAC,IKLE,NELEM,NELMAX,SIGMAG,
     &                   SPECAD)
!
            TYPDIA='Q'
            TYPEXT='Q'
!
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!.......................................................................
!         ERROR ON THE COLUMN ELEMENT
!.......................................................................
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       OTHER ROW ELEMENT
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!<<<<
!
!=======================================================================
!     BOUSSINESQ MATRIX
!=======================================================================
!
      ELSEIF(FORMUL(1:7).EQ.'FFBT   ') THEN
!
!-----------------------------------------------------------------------
!       P1 TRIANGLE ROW ELEMENT
        IF(IELM1.EQ.11) THEN
!
!.......................................................................
!         P1 TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.11) THEN
            CALL MT99AA
     &                  (   T(1,1)   ,XM(1,AAQ(1,2,S)),XM(1,AAQ(1,3,S)),
     &                   XM(1,AAQ(2,1,S)),   T(1,2)   ,XM(1,AAQ(2,3,S)),
     &                   XM(1,AAQ(3,1,S)),XM(1,AAQ(3,2,S)),   T(1,3)   ,
     &                   XMUL,SF,F,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,FORMUL,TDIA,TEXT)
!
            TYPDIA = TDIA
            TYPEXT = TEXT
!.......................................................................
!         OTHER
!.......................................................................
!
!         ELSEIF
!
!.......................................................................
!         ERROR ON THE COLUMN ELEMENT
!.......................................................................
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!-----------------------------------------------------------------------
!       OTHER ROW ELEMENT
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!       QUASI-BUBBLE TRIANGLE ROW ELEMENT
!
        ELSEIF(IELM1.EQ.12) THEN
!
!.......................................................................
!         QUASI-BUBBLE TRIANGLE COLUMN ELEMENT
          IF(IELM2.EQ.12) THEN
            CALL MT99BB
     & (   T(1,1)   ,XM(1,BBQ(1,2,S)),XM(1,BBQ(1,3,S)),XM(1,BBQ(1,4,S)),
     &  XM(1,BBQ(2,1,S)),   T(1,2)   ,XM(1,BBQ(2,3,S)),XM(1,BBQ(2,4,S)),
     &  XM(1,BBQ(3,1,S)),XM(1,BBQ(3,2,S)),   T(1,3)   ,XM(1,BBQ(3,4,S)),
     &  XM(1,BBQ(4,1,S)),XM(1,BBQ(4,2,S)),XM(1,BBQ(4,3,S)),   T(1,4)   ,
     &         XMUL,SF,F,XEL,YEL,SURFAC,
     &         IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &         NELEM,NELMAX,FORMUL,TDIA,TEXT)
!
!
            TYPDIA = TDIA
            TYPEXT = TEXT
!
!.......................................................................
!         OTHER
!.......................................................................
!
!
!.......................................................................
!         ERROR ON THE COLUMN ELEMENT
!.......................................................................
!
          ELSE
            IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
            IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
            IF (LNG.EQ.1) WRITE(LU,2000) IELM1
            IF (LNG.EQ.2) WRITE(LU,2001) IELM1
            IF (LNG.EQ.1) WRITE(LU,3000) IELM2
            IF (LNG.EQ.2) WRITE(LU,3001) IELM2
            CALL PLANTE(1)
            STOP
          ENDIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ROW ELEMENT
!-----------------------------------------------------------------------
!
!
        ELSE
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
!
!=======================================================================
!     ERROR: TYPE OF MATRIX NOT IMPLEMENTED
!=======================================================================
!
        IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
        IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
1000  FORMAT(1X,'MATRIY (BIEF) : MATRICE NON PREVUE : ',A16)
1001  FORMAT(1X,'MATRIY (BIEF) : MATRIX NOT IMPLEMENTED:',A16)
2000  FORMAT(1X,'                POUR IELM1 = ',1I6)
2001  FORMAT(1X,'                FOR IELM1 = ',1I6)
3000  FORMAT(1X,'                ET IELM2 = ',1I6)
3001  FORMAT(1X,'                AND IELM2 = ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

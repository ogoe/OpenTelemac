!                    *****************
                     SUBROUTINE VECTOS
!                    *****************
!
     &(VEC,OP,FORMUL,
     & XMUL,F,G,H,U,V,W,SF,SG,SH,SU,SV,SW,
     & T,LEGO,
     & XEL,YEL,ZEL, SURFAC,IKLE,NBOR,
     & XNOR,YNOR,ZNOR,NPT,NELEM,NELMAX,IELM1,LV,MSK,MASKEL,MESH)
!
!***********************************************************************
! BIEF   V6P1                                  21/08/2010
!***********************************************************************
!
!brief    COMPUTES VECTORS.
!+
!+            THE VECTOR IS IDENTIFIED BY THE FORMULATION IN
!+                THE CHARACTER STRING 'FORMUL'.
!code
!+  MEANING OF IELM1
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS
!+
!+  10 : P0 TRIANGLE            1
!+  11 : P1 TRIANGLE            3
!+  12 : QUASI-BUBBLE TRIANGLE  4
!+  13 : P2 TRIANGLE            6
!+
!+  20 : Q0 QUADRILATERAL       1
!+  21 : Q1 QUADRILATERAL       4
!+
!+  40 : TELEMAC-3D P0 PRISMS   1
!+  41 : TELEMAC-3D P1 PRISMS   6
!
!history  JM HERVOUET (LNHE)
!+        16/07/07
!+        V6P0
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
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA 
!| FORMUL         |-->| STRING WITH THE FORMULA DESCRIBING THE VECTOR
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA 
!| H              |-->| FUNCTION USED IN THE VECTOR FORMULA 
!| IELM1          |-->| TYPE OF ELEMENT
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LEGO           |-->| IF YES : THE VECTOR WILL BE ASSEMBLED
!| LV             |-->| VECTOR LENGTH OF THE MACHINE
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPT            |-->| NOMBRE OF POINTS OF VECTOR.
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SH             |-->| BIEF_OBJ STRUCTURE OF H
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SW             |-->| BIEF_OBJ STRUCTURE OF W
!| SURFAC         |-->| AREA OF TRIANGLES
!| T              |-->| WORK ARRAY WITH THE NON ASSEMBLED VECTOR
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA 
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA 
!| W              |-->| FUNCTION USED IN THE VECTOR FORMULA 
!| VEC            |<->| RESULTING VECTOR
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| ZEL            |-->| ELEVATIONS OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| XNOR           |-->| X-COMPONENT OF NORMAL VECTOR
!| YNOR           |-->| Y-COMPONENT OF NORMAL VECTOR
!| ZNOR           |-->| Z-COMPONENT OF NORMAL VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF !, EX_VECTOS => VECTOS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELMAX,NPT,NELEM,IELM1,LV
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),NBOR(*)
!
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(*),YEL(*),ZEL(*)
      DOUBLE PRECISION, INTENT(IN)    :: XNOR(*),YNOR(*),ZNOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,*),VEC(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL,MASKEL(NELMAX)
!
!     STRUCTURES OF FUNCTIONS F, G, H, U, V, W AND REAL DATA
!
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),H(*),U(*),V(*),W(*)
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SH,SU,SV,SW
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
      LOGICAL, INTENT(IN) :: MSK,LEGO
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
      CHARACTER(LEN=1), INTENT(IN) ::  OP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ICOORD
      LOGICAL INIT,SPECAD
!
!-----------------------------------------------------------------------
!
!     CHECKS THE TYPE OF VECTOR
!
!=======================================================================
!     MASS MATRIX VECTOR X VECTOR F EQUALS 1
!=======================================================================
!
      IF(FORMUL(1:16).EQ.'MASBAS          '.OR.
     &   FORMUL(1:16).EQ.'MASBAS2         '     ) THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC00AA(XMUL,SURFAC,NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
             CALL VC00BB(XMUL,SURFAC,NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),T(1,4))
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P2 TRIANGLE
!
        ELSEIF(IELM1.EQ.13) THEN
!
             CALL VC00CC(XMUL,SURFAC,NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),T(1,4),T(1,5),T(1,6))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!            CALL VC00OO(XMUL,SURFAC,NELEM,NELMAX,T(1,1),T(1,2))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
             IF(FORMUL(7:7).EQ.'2') THEN
!              COMPATIBLE WITH MASS-LUMPING IN 2D
               CALL VC00PP2(XMUL,ZEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6))
             ELSE
!              NORMAL COMPUTATION OF INTEGRAL OF BASIS FUNCTIONS
               CALL VC00PP(XMUL,ZEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6))
             ENDIF
!
!-----------------------------------------------------------------------
!
!       ELEMENT TETRAHEDRON T1
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
             CALL VC00TT(XMUL,XEL,YEL,ZEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE (SIDE FACES OF A MESH OF PRISMS
!                            SPLIT IN TETRAHEDRONS)
!
        ELSEIF(IELM1.EQ.61) THEN
!
             CALL VC00FT(XMUL,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NBOR,
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 QUADRILATERAL
!
        ELSEIF(IELM1.EQ.71) THEN
!
!              FOR VERTICAL RECTANGULAR SIDES OF THE PRISMS
               CALL VC00FF(XMUL,XEL,YEL,ZEL,
     &                     IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NBOR,
     &                     NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     MASS MATRIX VECTOR X VECTOR F
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'MASVEC') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC01AA(XMUL,SF,F,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE (SIDE FACES OF A MESH OF PRISMS
!                            SPLIT IN TETRAHEDRONS)
!
        ELSEIF(IELM1.EQ.61.OR.IELM1.EQ.81) THEN
!
           IF(FORMUL(7:7).NE.'2') THEN
!
              CALL VC01FT(XMUL,SF,F,XEL,YEL,ZEL,
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NBOR,
     &             NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
           ELSE
!
              CALL VC01FT2(XMUL,SF,F,SG,G,XEL,YEL,ZEL,
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NBOR,
     &             NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
           ENDIF
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
             CALL VC01BB(XMUL,SF,F,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
        ELSEIF(IELM1.EQ.1) THEN
!
             CALL VC01OO(XMUL,SF,F,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
     &                   T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 QUADRILATERAL
!
        ELSEIF(IELM1.EQ.71) THEN
!
!              FOR VERTICAL RECTANGULAR SIDES OF THE PRISMS
               CALL VC01FF(XMUL,SF,F,XEL,YEL,ZEL,
     &                     IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NBOR,
     &                     NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
             CALL VC01PP(XMUL,SF,F,ZEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6))
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
             CALL VC01TT(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
!
!-----------------------------------------------------------------------
!
!       ELEMENT T0 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.30) THEN
!
             CALL VC01TT0(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(:,1),IKLE(:,2),IKLE(:,3),IKLE(:,4),
     &                   NELEM,NELMAX,VEC)
!
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!                 -    ---->   --->
!     VECTOR:     F  . GRAD(U).GRAD(PSI)
!
!     F DIFFUSION TENSOR WITH DIAGONAL COMPONENTS F, G, H
!
!     EQUIVALENT OF MATDIF * U
!
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'VECDIF') THEN
!
!       ELEMENT SEGMENT P1
!
        IF(IELM1.EQ.41.AND.FORMUL(8:8).EQ.'*') THEN
!
          CALL VC02PP_STAR(XMUL,SF,SG,SH,SU,F,G,H,U,XEL,YEL,ZEL,SURFAC,
     &                     IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                     IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),
     &                     T(1,2),T(1,3),T(1,4),T(1,5),T(1,6),FORMUL)
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR K GRAD(PSI) U.GRAD(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'SUPG            ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC03AA(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     &                   XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
             CALL VC03BB(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     &                   XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!            CALL VC03OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    *                   T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR U GRAD(PSI)
!=======================================================================
!
      ELSEIF(FORMUL(1:6).EQ.'VGRADP') THEN
!
        SPECAD = .FALSE.
        IF(FORMUL(8:8).EQ.'2') SPECAD = .TRUE.
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC04AA(XMUL,SU,SV,U,V,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),SPECAD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
             CALL VC04PP(XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6),SPECAD,FORMUL,
     &                   MESH%NPOIN/BIEF_NBPTS(11,MESH))
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
             CALL VC04TT(XMUL,SU,SV,SW,U,V,W,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),FORMUL)
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR U.N    (N VECTOR NORMAL TO THE ELEMENT)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'FLUBOR          ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE (BOTTOM OR SURFACE OF A 3D MESH ?)
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC05AA(XMUL,SW,W,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE FOR VERTICAL SIDES OF THE PRISMS SPLIT
!       IN TETRAHEDRONS
!
        ELSEIF(IELM1.EQ.61) THEN
!
             CALL VC05FT(XMUL,SU,SV,U,V,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NBOR,
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
!
!       ELEMENT P1 QUADRILATERAL FOR VERTICAL SIDES OF THE PRISMS
!
        ELSEIF(IELM1.EQ.71) THEN
!
             CALL VC05FF(XMUL,SU,SV,U,V,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NBOR,
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
!
!       ELEMENT LINEAR SEGMENT
!
        ELSEIF(IELM1.EQ.1) THEN
!
             CALL VC05OO(XMUL,SU,SV,U,V,XNOR,YNOR,SURFAC,
     &                   IKLE,NBOR,NELEM,NELMAX,T(1,1),T(1,2))
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR U GRAD(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:13).EQ.'VGRADF       ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC08AA(XMUL,SF,SU,SV,F,U,V,XEL,YEL,IKLE,
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3) , FORMUL )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P2 TRIANGLE
!
        ELSEIF(IELM1.EQ.13) THEN
!
             CALL VC08CC(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &            IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &            IKLE(1,5),IKLE(1,6),NELEM,NELMAX,
     &            T(1,1),T(1,2),T(1,3),T(1,4),T(1,5),T(1,6),FORMUL)
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
             CALL VC08BB(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &            IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &            NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!            CALL VC08OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    *                   T(1,1),T(1,2)  )
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
             CALL VC08PP(XMUL,SF,SU,SV,SW,F,U,V,W,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6))
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
             CALL VC08TT(XMUL,SF,SU,SV,SW,F,U,V,W,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4))
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
! A.D. MODIFICATION 25/11/04
!
!=======================================================================
!     VECTOR U GRAD(F) 2
!=======================================================================
!
      ELSEIF(FORMUL(1:13).EQ.'VGRADF2      ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        IF(IELM1.EQ.41) THEN
!
             CALL VC18PP(XMUL,SF,SU,SV,F,U,V,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!  A.D. END OF MODIFICATION 25/11/04
!=======================================================================
!     VECTOR Q GRAD(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'QGRADF          ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC09AA(XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!            CALL VC09OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    *                   T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR F U.N    (N VECTOR NORMAL TO THE ELEMENT)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'FLUBDF          ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
!       IF(IELM1.EQ.11) THEN
!
!            CALL VC10AA(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
!    *                   T(1,1)   ,T(1,2)   ,T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
        IF(IELM1.EQ.1) THEN
!
             CALL VC10OO(XMUL,SF,SU,SV,F,U,V,XNOR,YNOR,SURFAC,
     &                   IKLE,NBOR,NELEM,NELMAX,T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR G GRADIENT(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:15).EQ.'GGRADF         ') THEN
!
!     CHARACTER 16 GIVES THE SELECTED COORDINATE
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
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
! CHECKS IF G IS DISC P1
!
         IF (SG%DIM1.EQ.NELEM.AND.SG%DIM2.EQ.3.AND.
     &       SG%DIMDISC.EQ.11) THEN
!
            CALL VC11AA2(XMUL,SF,SG,SH,F,G,H,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) , ICOORD )
!
! CLASSICAL CASE: G IS P1
!
         ELSE
!
             CALL VC11AA(XMUL,SF,SG,F,G,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) , ICOORD )
         ENDIF
!
!-----------------------------------------------------------------------
!
        ELSEIF(IELM1.EQ.12) THEN
!
             CALL VC11BB(XMUL,SF,SG,F,G,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),T(1,4) , ICOORD )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!            CALL VC11OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
!    *                   T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
             CALL VC11PP(XMUL,SF,SG,F,G,
     &                   XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6),ICOORD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
             CALL VC11TT(XMUL,SF,SG,F,G,
     &                   XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),ICOORD)
!
        ELSEIF(IELM1.EQ.30) THEN
!
! COMMENT JP RENAUD 21/09/2005
! BEWARE: THIS CALL CREATES A P0 VECTOR. THEREFORE
! THERE IS NO NEED TO "ASSEMBLE" IT AFTERWARDS. NOTE
! THAT VEC ITSELF (AND NOT T) IS GIVEN TO VC11TT0 AND
! THAT LEGO IS ALSO SET TO .FALSE. TO AVOID
! CALLING ASSVEC AT THE END OF THE SUBROUTINE.
!
             CALL VC11TT0(XMUL,SF,SG,F,G,
     &                   XEL,YEL,ZEL,
     &                   IKLE(1:NELEM,1),IKLE(1:NELEM,2),
     &                   IKLE(1:NELEM,3),IKLE(1:NELEM,4),
     &                   NELEM,MESH%NPOIN,
     &                   VEC,ICOORD)
!
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR GRADIENT(F)
!=======================================================================
!
      ELSEIF(FORMUL(1:5).EQ.'GRADF') THEN
!
!     CHARACTER 16 GIVES THE SELECTED COORDINATE
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
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC13AA(XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),ICOORD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
        ELSEIF(IELM1.EQ.12) THEN
!
             CALL VC13BB(XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,
     &                   NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),ICOORD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P2 TRIANGLE
!
        ELSEIF(IELM1.EQ.13) THEN
!
            CALL VC13CC(XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                   NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),
     &                   T(1,4),T(1,5),T(1,6),ICOORD)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!            CALL VC13OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
!    *                   T(1,1),T(1,2)  )
!
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P2 SEGMENT
!
!       ELSEIF(IELM1.EQ.2) THEN
!
!            CALL VC13OC(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
!    *                   T(1,1),T(1,2),T(1,3)  )
!
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 PRISM
!
        ELSEIF(IELM1.EQ.41) THEN
!
             IF(FORMUL(1:15).EQ.'GRADF(X,Y)     ') THEN
!            SIMPLIFIED FORMULATION FOR EFFICIENCY AND ACCURACY
             CALL VC13PP2(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6),ICOORD)
             ELSEIF( FORMUL(8:15).EQ.'        ') THEN
!                    FORMUL(6:7) IS LEFT FOR OPTIONS
             CALL VC13PP(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6),ICOORD,FORMUL)
!
             ELSE
               IF(LNG.EQ.1) WRITE(LU,1000) FORMUL
               IF(LNG.EQ.2) WRITE(LU,1001) FORMUL
               CALL PLANTE(1)
               STOP
             ENDIF
!
!-----------------------------------------------------------------------
!
!       ELEMENT T1 TETRAHEDRON
!
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
!
             IF(FORMUL(1:15).EQ.'GRADF          '.OR.
     &          FORMUL(1:15).EQ.'GRADF2         '  ) THEN
             CALL VC13TT(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),ICOORD,FORMUL)
!
             ELSE
               IF(LNG.EQ.1) WRITE(LU,1000) FORMUL
               IF(LNG.EQ.2) WRITE(LU,1001) FORMUL
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
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     TURBULENT PRODUCTION VECTOR
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'PRODF           ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC14AA(XMUL,SU,SV,U,V,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
!       ELSEIF(IELM1.EQ.12) THEN
!
!            CALL VC14BB(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,
!    *                   NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!            CALL VC14OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
!    *                   T(1,1),T(1,2))
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR DIV(HU)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'DIVQ            ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC15AA(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
!       ELSEIF(IELM1.EQ.12) THEN
!
!            CALL VC15BB(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *            XEL,YEL,ZEL,SURFAC,
!    *            IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
!    *            NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!            CALL VC15OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    *                   T(1,1),T(1,2)  )
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     VECTOR K.GRAD(PSI) DIV(U)
!=======================================================================
!
      ELSEIF(FORMUL(1:16).EQ.'SUPGDIVU        ') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC16AA(XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) )
!
!-----------------------------------------------------------------------
!
!       ELEMENT QUASI-BUBBLE TRIANGLE
!
!       ELSEIF(IELM1.EQ.12) THEN
!
!            CALL VC16BB(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *            XEL,YEL,ZEL,SURFAC,
!    *            IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
!    *            NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL)
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 SEGMENT
!
!       ELSEIF(IELM1.EQ.1) THEN
!
!            CALL VC16OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
!    *                   XEL,YEL,ZEL,SURFAC,
!    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
!    *                   T(1,1),T(1,2)  )
!
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!
!=======================================================================
!     VECTOR H U.GRAD(PSI)
!=======================================================================
!
      ELSEIF(FORMUL(1:7).EQ.'HUGRADP') THEN
!
!-----------------------------------------------------------------------
!
!       ELEMENT P1 TRIANGLE
!
        IF(IELM1.EQ.11) THEN
!
             CALL VC19AA(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     &                   XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),FORMUL)
!
!-----------------------------------------------------------------------
!       OTHER
!-----------------------------------------------------------------------
!
!       ELSEIF
!
!-----------------------------------------------------------------------
!       ERROR ON THE ELEMENT TYPE
!-----------------------------------------------------------------------
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!=======================================================================
!     OTHER VECTOR
!=======================================================================
!
!
!=======================================================================
!     ERROR: TYPE OF VECTOR NOT CODED UP
!=======================================================================
!
      ELSE
        IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
        IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  POSSIBLE ASSEMBLY OF THE VECTOR
!
      IF(LEGO) THEN
!
        IF(OP(1:1).EQ.'=') THEN
          INIT = .TRUE.
        ELSEIF(OP(1:1).EQ.'+') THEN
          INIT = .FALSE.
        ELSE
          IF (LNG.EQ.1) WRITE(LU,3000) OP
          IF (LNG.EQ.2) WRITE(LU,3001) OP
3000      FORMAT(1X,'VECTOS (BIEF) : OP NON PREVU :',A1)
3001      FORMAT(1X,'VECTOS (BIEF) : OP NOT RECOGNISED:',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        CALL ASSVEC(VEC, IKLE, NPT ,NELEM,NELMAX,IELM1,
     &              T,INIT,LV,MSK,MASKEL,BIEF_NBPEL(IELM1,MESH))
!
      ENDIF
!
!-----------------------------------------------------------------------
!
1000  FORMAT(1X,'VECTOS (BIEF) : VECTEUR NON PREVU : ',A16)
1001  FORMAT(1X,'VECTOS (BIEF) : VECTOR NOT IMPLEMENTED:',A16)
2000  FORMAT(1X,'                POUR IELM1 = ',1I6)
2001  FORMAT(1X,'                FOR IELM1 = ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

!                    *****************
                     SUBROUTINE VECTOR
!                    *****************
!
     &(VEC,OP,FORMUL,IELM1,XMUL,F,G,H,U,V,W,MESH,MSK,MASKEL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES VECTORS.
!+
!+            THE VECTOR IS IDENTIFIED BY THE FORMULATION IN
!+                THE CHARACTER STRING 'FORMUL'.
!+
!+            'OP' IS = OR +.
!code
!+  MEANING OF IELM1
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS
!+
!+   0 : P0 SEGMENT             1
!+   1 : P1 SEGMENT             2
!+
!+  10 : P0 TRIANGLE            1
!+  11 : P1 TRIANGLE            3
!+  12 : QUASI-BUBBLE TRIANGLE  4
!+
!+  20 : Q0 QUADRILATERAL       1
!+  21 : Q1 QUADRILATERAL       4
!+
!+  40 : TELEMAC-3D P0 PRISMS   1
!+  41 : TELEMAC-3D P1 PRISMS   6
!
!history  REGINA NEBAUER
!+        22/09/05
!+
!+
!
!history  JM HERVOUET (LNHE)
!+        25/06/2008
!+        V5P9
!+   NO CALL TO VECTOS IF THE NUMBER OF ELEMENTS IS 0
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
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| FORMUL         |-->| STRING WITH THE FORMULA DESCRIBING THE VECTOR
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| H              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| IELM1          |-->| TYPE OF ELEMENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| OP             |-->| '=' : WE DO VEC= THE VECTOR
!|                |   | '+' : WE DO VEC=VEC+ THE VECTOR
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| W              |-->| FUNCTION USED IN THE VECTOR FORMULA (BIEF_OBJ)
!| VEC            |<->| RESULTING VECTOR
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VECTOR => VECTOR
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: VEC
      DOUBLE PRECISION,  INTENT(IN)    :: XMUL
      INTEGER,           INTENT(IN)    :: IELM1
      LOGICAL,           INTENT(IN)    :: MSK
      CHARACTER(LEN=16), INTENT(IN)    :: FORMUL
      CHARACTER(LEN=1),  INTENT(IN)    :: OP
      TYPE(BIEF_OBJ),    INTENT(IN)    :: F,G,H,U,V,W,MASKEL
      TYPE(BIEF_MESH),   INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  :: NPT   ! NUMBER OF POINTS PER ELEMENT
      LOGICAL  :: LEGO  ! ASSEMBLY OR NOT
      INTEGER  :: IELM0 ! P0 DISCRETISATION
!
!-----------------------------------------------------------------------
!  POSSIBLE CHANGE OF DISCRETISATION
!-----------------------------------------------------------------------
! IF A VECTOR HAS BEEN ALLOCATED WITH STATUS 1, I.E ITS DISCRETISATION
! CANNOT CHANGE, IT IS NECESSARY TO TEST THE COHERENCE BETWEEN THE
! DISCRETISATION OF THE VECTOR AND THAT PROPOSED IN ARGUMENT.
! MIGHT HAVE SURPRISES OTHERWISE !!!!
!
      IF(VEC%STATUS.EQ.1.AND.VEC%ELM.NE.IELM1) THEN
        IF(LNG.EQ.1) WRITE(LU,1001) VEC%NAME,VEC%ELM,IELM1
        IF(LNG.EQ.2) WRITE(LU,1002) VEC%NAME,VEC%ELM,IELM1
1001    FORMAT(1X,'VECTOR : CHANGEMENT DE DISCRETISATION IMPOSSIBLE',
     &  ' POUR VECTEUR ',A6,' : ',1I6,' <=> ',1I6)
1002    FORMAT(1X,'VECTOR: CHANGING DISCRETIZATION FORBIDDEN',
     &  ' FOR THE VECTOR ',A6,' : ',1I6,' <=> ',1I6)
        CALL PLANTE(1)
        STOP
      ELSEIF(VEC%STATUS.EQ.2.OR.VEC%STATUS.EQ.1) THEN
        NPT = BIEF_NBPTS(IELM1,MESH)
        VEC%ELM = IELM1
        VEC%DIM1= NPT
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LE VECTEUR ',VEC%NAME,' A UN STATUT EGAL A',
     &                VEC%STATUS,' IL NE PEUT ETRE UTILISE DANS VECTOR'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'VECTOR ',VEC%NAME,' HAS A STATUS ',VEC%STATUS,
     &                ' IT CANNOT BE USED IN SUBROUTINE VECTOR'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
! ASSEMBLY: NOT PERFORMED FOR VECTORS
! RESULT OF P0 DISCRETISATION
! LEGO IS SET TO TRUE IF THE RESULTANT VECTOR DISCRETISATION
! IS P1, FALSE OTHERWISE.
!
      IELM0 = 10*(IELM1/10)
      LEGO  = IELM0 .NE. IELM1
!
! MODIFICATION: IN THE FUTURE VARIABLE LEGO IS PASSED IN ARGUMENT
!
!-----------------------------------------------------------------------
!  CALLS THE SUBROUTINE THAT SHUNTS AND ASSEMBLES
!-----------------------------------------------------------------------
!
      IF(DIMENS(IELM1).EQ.MESH%DIM) THEN
!       NORMAL VECTOR: CALL WITH SURFAC, IKLE, NELEM, NELMAX
!                                XEL, YEL, ZEL
        CALL VECTOS(VEC%R,OP,FORMUL,XMUL,
     &              F%R,G%R,H%R,U%R,V%R,W%R,
     &              F,G,H,U,V,W,MESH%W%R,LEGO,
     &              MESH%XEL%R  , MESH%YEL%R  , MESH%ZEL%R  ,
     &              MESH%SURFAC%R,MESH%IKLE%I,MESH%NBOR%I,
     &              MESH%XSGBOR%R, MESH%YSGBOR%R, MESH%ZSGBOR%R,
     &              NPT,MESH%NELEM,MESH%NELMAX,
     &              IELM1,MESH%LV,MSK,MASKEL%R,MESH)
      ELSE
!       BOUNDARY VECTOR: CALL WITH LGSEG, IKLBOR, NELEB, NELEBX
!                                  X, Y, Z
        IF(MESH%NELEB.GT.0) THEN
          CALL VECTOS(VEC%R,OP,FORMUL,XMUL,
     &                F%R,G%R,H%R,U%R,V%R,W%R,
     &                F,G,H,U,V,W,MESH%W%R,LEGO,
     &                MESH%X%R,MESH%Y%R,MESH%Z%R  ,
     &                MESH%LGSEG%R,MESH%IKLBOR%I,MESH%NBOR%I,
     &                MESH%XSGBOR%R,MESH%YSGBOR%R,MESH%ZSGBOR%R,
     &                NPT,MESH%NELEB,MESH%NELEBX,
     &                IELM1,MESH%LV,MSK,MASKEL%R,MESH)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

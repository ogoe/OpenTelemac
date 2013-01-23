!                    *****************
                     SUBROUTINE CFLPSI
!                    *****************
!
     &(SYGMA,U,V,DT,IELM,MESH,MSK,MASKEL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COURANT NUMBER AT EACH POINT OF THE MESH
!+                AND FOR EACH TIMESTEP.
!
!warning  THE COORDINATES ARE HERE GIVEN BY ELEMENTS
!
!history  C MOULIN   (LNH)
!+
!+
!+
!
!history  JMH
!+        17/08/94
!+        V5P3
!+   MODIFICATIONS
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
!| DT             |-->| TIME STEP.
!| IELM           |-->| TYPE OF ELEMENT.
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| SYGMA          |<--| COURANT NUMBER.
!| U              |-->| VELOCITY ALONG X.
!| V              |-->| VELOCITY ALONG Y.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CFLPSI => CFLPSI
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: SYGMA
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: U,V,MASKEL
      DOUBLE PRECISION, INTENT(IN)    :: DT
      INTEGER         , INTENT(IN)    :: IELM
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      LOGICAL         , INTENT(IN)    :: MSK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! P1 TRIANGLES
!
      IF(IELM.EQ.11) THEN
!
        CALL CFLP11(U%R,V%R,MESH%XEL%R,MESH%YEL%R,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX,MESH%W%R)
!
!-----------------------------------------------------------------------
!
! QUASI-BUBBLE TRIANGLES
!
      ELSEIF(IELM.EQ.12) THEN
!
        CALL CFLP12(U%R,V%R,MESH%XEL%R,MESH%YEL%R,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX,MESH%W%R)
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,100) IELM
        IF (LNG.EQ.2) WRITE(LU,101) IELM
100     FORMAT(1X,'CFLPSI : IELM = ',1I6,'  CAS NON PREVU |')
101     FORMAT(1X,'CFLPSI: IELM = ',1I6,' COMBINATION NOT AVAILABLE |')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
! ASSEMBLES THE LIJ
!
      CALL ASSVEC(SYGMA%R,MESH%IKLE%I,BIEF_NBPTS(IELM,MESH),
     &            MESH%NELEM,MESH%NELMAX,IELM,
     &            MESH%W%R,.TRUE.,MESH%LV,MSK,MASKEL%R,
     &            BIEF_NBPEL(IELM,MESH))
!
!-----------------------------------------------------------------------
!
! FINAL RESULT
!
!     MASS OF THE BASES IN BIEF WORKING ARRAY
!
      CALL VECTOR(MESH%T,'=','MASBAS          ',
     &            IELM,1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
!     CORRECTION JMH 27/01/2003
      IF(NCSIZE.GT.1) CALL PARCOM(MESH%T,2,MESH)
!
! DIVIDES BY THE MASS OF THE BASES
!
      CALL CPSTVC(MESH%T,SYGMA)
      CALL OS( 'X=CY/Z  ' , SYGMA , SYGMA , MESH%T , DT ,2,0.D0,1.D-6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

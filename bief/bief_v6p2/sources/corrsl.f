!                    *****************
                     SUBROUTINE CORRSL
!                    *****************
!
     &(NEWSL,OLDSL,ZF,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CORRECTS THE FREE SURFACE COMPUTATION BY ELEMENTS
!+                TO TAKE ACCOUNT OF THE TIDAL FLATS.
!
!history  J-M JANIN (LNH)    ; J-M HERVOUET (LNH)
!+        27/11/92
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
!| MESH           |-->| MESH STRUCTURE
!| NEWSL          |<->| MODIFIED FREE SURFACE, GIVEN PER ELEMENT
!| OLDSL          |-->| ORIGINAL FREE SURFACE
!| ZF             |-->| BATHYMETRY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CORRSL => CORRSL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: NEWSL
      TYPE(BIEF_OBJ) , INTENT(IN)    :: OLDSL,ZF
      TYPE(BIEF_MESH), INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NELEM,NELMAX,IELM
!
!-----------------------------------------------------------------------
!
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
!
      IELM=OLDSL%ELM
!
!-----------------------------------------------------------------------
!
!     VECTOR NEWSL IS MARKED AS BEING DISCONTINUOUS
      NEWSL%DIMDISC=IELM
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11) THEN
!
        CALL CRSL11(NEWSL%R,OLDSL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
        NEWSL%ELM=10
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELM.EQ.12) THEN
!
        CALL CRSL12(NEWSL%R,OLDSL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
        NEWSL%ELM=10
!
!-----------------------------------------------------------------------
!
      ELSE
!
         IF(LNG.EQ.1) WRITE(LU,10) IELM
         IF(LNG.EQ.2) WRITE(LU,11) IELM
10       FORMAT(1X,'CORRSL : DISCRETISATION INCONNUE :',I6)
11       FORMAT(1X,'CORRSL : UNKNOWN DISCRETIZATION :',I6)
         CALL PLANTE(1)
         STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

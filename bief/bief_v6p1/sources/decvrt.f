!                    *****************
                     SUBROUTINE DECVRT
!                    *****************
!
     &(TETA,SL,ZF,MESH)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    IDENTIFIES TIDAL FLATS.
!+
!+            DRYING ELEMENT : TETA = 0,
!+
!+            NORMAL ELEMENT : TETA = 1.
!+
!+            THE CRITERION FOR DRYING ELEMENTS IS THAT OF
!+                J.-M. JANIN : BOTTOM ELEVATION OF A POINT IN AN
!+                ELEMENT BEING HIGHER THAN THE FREE SURFACE
!+                ELEVATION OF ANOTHER.
!
!history  J-M HERVOUET (LNH)
!+        09/12/94
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
!| MESH           |-->| STRUCTURE DE MAILLAGE
!| SL,ZF          |-->| SURFACE LIBRE ET FOND
!| TETA           |<--| INDICATEUR (PAR ELEMENT)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DECVRT => DECVRT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: TETA
      TYPE(BIEF_OBJ) , INTENT(IN)    :: SL,ZF
      TYPE(BIEF_MESH), INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NELEM,NELMAX,IELMS,IELMZ
!
!-----------------------------------------------------------------------
!
      IELMS=SL%ELM
      IELMZ=ZF%ELM
!
!-----------------------------------------------------------------------
!
!  DEPLOYS THE MESH STRUCTURE
!
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
!
!-----------------------------------------------------------------------
!
      IF(IELMS.EQ.11.AND.IELMZ.EQ.11) THEN
!
        CALL DECV11(TETA%R,SL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
!
      ELSEIF((IELMS.EQ.21.AND.IELMZ.EQ.21).OR.
     &       (IELMS.EQ.12.AND.IELMZ.EQ.12)      ) THEN
!
        CALL DECV21(TETA%R,SL%R,ZF%R,MESH%IKLE%I,NELEM,NELMAX)
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,10) IELMS,IELMZ
        IF(LNG.EQ.2) WRITE(LU,11) IELMS,IELMZ
10      FORMAT(1X,'DECVRT : DISCRETISATION NON PREVUE :'    ,I6,' ',I6)
11      FORMAT(1X,'DECVRT : DISCRETIZATION NOT IMPLEMENTED:',I6,' ',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
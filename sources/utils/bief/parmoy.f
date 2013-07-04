!                    *****************
                     SUBROUTINE PARMOY
!                    *****************
!
     &( X , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    AVERAGE OF A VECTOR AT THE INTERFACES BETWEEN
!+                SUB-DOMAINS.
!+
!+            X CAN BE A BLOCK OF VECTORS. IN THIS CASE, ALL THE
!+                VECTORS IN THE BLOCK ARE TREATED.
!
!warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS
!+            IGNORED FOR THE TIME BEING
!
!history  J-M HERVOUET (LNH)
!+        24/04/97
!+        V5P1
!+   AFTER REINHARD HINKELMANN (HANNOVER UNI.)
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
!| X              |<->| VECTOR OR BLOCK OF VECTORS.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PARMOY => PARMOY
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     STRUCTURES: MESH, VECTORS OR BLOCKS
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: X
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,TYPX
!
!  COMPLEMENTS THE INTERFACES:
!
      CALL PARCOM( X , 2 , MESH )
!
!-----------------------------------------------------------------------
!
      TYPX = X%TYPE
!
!-----------------------------------------------------------------------
!
!  CASE WHERE THE STRUCTURES ARE BLOCKS
!
      IF(TYPX.EQ.4) THEN
!
        DO I=1,X%N
          CALL OS('X=XY    ',X=X%ADR(I)%P,Y=MESH%FAC)
        ENDDO
!
!-----------------------------------------------------------------------
!
!  CASE WHERE THE STRUCTURE IS A VECTOR
!
      ELSEIF(TYPX.EQ.2) THEN
!
        CALL OS('X=XY    ',X=X,Y=MESH%FAC)
!
!-----------------------------------------------------------------------
!
!  ERROR ON THE STRUCTURE
!
      ELSE
!
         IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
         IF (LNG.EQ.1) WRITE(LU,53)
50       FORMAT(1X,'PARMOY (BIEF) : NOM DE X : ',A6,'  TYPE : ',1I6)
53       FORMAT(1X,'                CAS NON PREVU')
         IF (LNG.EQ.2) WRITE(LU,51) X%NAME,X%TYPE
         IF (LNG.EQ.2) WRITE(LU,54)
51       FORMAT(1X,'PARMOY (BIEF) : NAME OF X: ',A6,'  TYPE : ',1I6)
54       FORMAT(1X,'                UNEXPECTED CASE')
         CALL PLANTE(1)
         STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

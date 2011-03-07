!                    ***************
                     SUBROUTINE OSDB
!                    ***************
!
     & ( OP , X , Y , Z , C , MESH )
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON VECTORS.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.
!+
!+   HERE X IS A VECTOR DEFINED IN THE DOMAIN.
!+   Y AND Z ARE VECTORS DEFINED ON THE BOUNDARY.
!+   X, Y AND Z MUST BE STRUCTURES.
!+   Y SHOULD NOT BE A DUMMY STRUCTURE.
!+   Z NOT YET IMPLEMENTED.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   OP = 'X=Y     '     :  COPIES Y IN X
!+   OP = 'X=+Y    '     :  IDEM
!+   OP = 'X=X+Y   '     :  ADDS Y TO X
!+   OP = 'X=X-Y   '     :  SUBTRACTS Y FROM X
!+   OP = 'X=CY    '     :  MULTIPLIES Y BY C
!
!history  J-M HERVOUET (LNH)
!+        06/12/94
!+        V5P5
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
!| C              |-->| CONSTANTE DONNEE
!| MESH           |---| 
!| OP             |-->| CHAINE DE CARACTERES INDIQUANT L'OPERATION
!|                |   | A EFFECTUER.
!| X              |<--| VECTEUR RESULTAT
!| Y              |-->| VECTEUR OPERANDE
!| Z              |-->| VECTEUR OPERANDE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_OSDB => OSDB
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      DOUBLE PRECISION, INTENT(IN)  :: C
      CHARACTER(LEN=8), INTENT(IN)  :: OP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: Y,Z
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER NPTFR,IELMX,IELMY
!
!-----------------------------------------------------------------------
!
      IF(X%TYPE.NE.2.OR.Y%TYPE.NE.2) THEN
        IF (LNG.EQ.1) WRITE(LU,100)
        IF (LNG.EQ.2) WRITE(LU,101)
100     FORMAT(1X,'OSDB (BIEF) : X ET Y NE SONT PAS DES VECTEURS')
101     FORMAT(1X,'OSDB (BIEF) : X AND Y ARE NOT VECTORS')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IELMX = X%ELM
      IELMY = Y%ELM
!
      IF((DIMENS(IELMX).NE.2.OR.DIMENS(IELMY).NE.1).AND.
     &   (DIMENS(IELMX).NE.3.OR.DIMENS(IELMY).NE.2)) THEN
        IF (LNG.EQ.1) WRITE(LU,102)
        IF (LNG.EQ.2) WRITE(LU,103)
102     FORMAT(1X,'OSDB (BIEF) : X ET Y ONT DE MAUVAISES DIMENSIONS')
103     FORMAT(1X,'OSDB (BIEF) : X AND Y HAVE WRONG DIMENSIONS')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      NPTFR = Y%DIM1
!
!C 3D MESH
      IF(IELMX.EQ.11.OR.IELMX.EQ.21.OR.IELMX.EQ.31.OR.IELMX.EQ.61.OR.
     &   IELMX.EQ.12.OR.IELMX.EQ.41.OR.IELMX.EQ.51.OR.IELMX.EQ.81) THEN
!       ARRAY NBOR
        CALL OVDB( OP , X%R , Y%R , Z%R , C , MESH%NBOR%I , NPTFR )
      ELSEIF(IELMX.EQ.10.OR.IELMX.EQ.20.OR.
     &       IELMX.EQ.40.OR.IELMX.EQ.50.OR.IELMX.EQ.80) THEN
!       ARRAY NELBOR
        CALL OVDB( OP , X%R , Y%R , Z%R , C , MESH%NELBOR%I , NPTFR )
      ELSE
        IF (LNG.EQ.1) WRITE(LU,104)
        IF (LNG.EQ.2) WRITE(LU,105)
104     FORMAT(1X,'OSDB (BIEF) : DISCRETISATIONS NON PREVUES')
105     FORMAT(1X,'OSDB (BIEF) : DISCRETIZATIONS NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
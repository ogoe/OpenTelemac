!                    *****************
                     SUBROUTINE MT08AA
!                    *****************
!
     &(  A11 , A12 , A13 ,
     &   A21 , A22 , A23 ,
     &   A31 , A32 , A33 ,
     &   XMUL,SF,F,XEL,YEL,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+  EXAMPLE WITH ICOORD = 1
!+
!+                  /                     D
!+  A(I,J)=-XMUL   /  PSI2(J) *    F    * --( PSI1(I) ) D(OMEGA)
!+                /OMEGA                  DX
!+
!+  BEWARE THE MINUS SIGN !!
!+
!+  PSI1: BASES OF TYPE P1 TRIANGLE
!+  PSI2: BASES OF TYPE IELM2
!+
!+  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
!+        09/12/94
!+        V5P1
!+   THE MATRIX IS ALSO COMPUTED IN QUASI-BUBBLE (IT'S THE SAME) 
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
!| A11,A12        |<--| ELEMENTS DE LA MATRICE
!| A13            |---| 
!| A21            |---| 
!| A22            |---| 
!| A23            |---| 
!| A31            |---| 
!| A32            |---| 
!| A33            |---| 
!| ICOORD         |-->| 1: DERIVEE SUIVANT X, 2:SUIVANT Y
!| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!| IKLE2          |---| 
!| IKLE3          |---| 
!| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| XMUL           |-->| FACTEUR MULTIPLICATIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT08AA => MT08AA
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION SUR24,X2,X3,Y2,Y3,F1,F2,F3,F123
!
!-----------------------------------------------------------------------
!
      SUR24 = XMUL/24.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
!
!  CASE WHERE F IS OF P1 DISCRETISATION
!
      IF((IELMF.EQ.11).OR.(IELMF.EQ.12)) THEN
!
! TH
! SAME MATRIX IF F IS QUASI-BUBBLE
! TH
!
!================================
!  CASE OF DERIVATIVE WRT X =
!================================
!
      IF(ICOORD.EQ.1) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO 1 IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1  =  F(IKLE1(IELEM)) * SUR24
      F2  =  F(IKLE2(IELEM)) * SUR24
      F3  =  F(IKLE3(IELEM)) * SUR24
      F123 = F1 + F2 + F3
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM) = (Y3-Y2) * (  F123 + F2  )
      A13(IELEM) = (Y3-Y2) * (  F123 + F3  )
      A23(IELEM) =  Y3     * ( -F123 - F3  )
      A21(IELEM) =  Y3     * ( -F123 - F1  )
      A31(IELEM) =     Y2  * (  F123 + F1  )
      A32(IELEM) =     Y2  * (  F123 + F2  )
!
!   DIAGONAL TERMS
!
      A11(IELEM) = - A21(IELEM) - A31(IELEM)
      A22(IELEM) = - A12(IELEM) - A32(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
1     CONTINUE
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  CASE OF DERIVATIVE WRT Y =
!================================
!
      DO 2 IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2  =  XEL(IELEM,2)
      X3  =  XEL(IELEM,3)
!
      F1  =  F(IKLE1(IELEM)) * SUR24
      F2  =  F(IKLE2(IELEM)) * SUR24
      F3  =  F(IKLE3(IELEM)) * SUR24
      F123 = F1 + F2 + F3
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM) = (X2-X3) * (  F123 + F2  )
      A13(IELEM) = (X2-X3) * (  F123 + F3  )
      A23(IELEM) =     X3  * (  F123 + F3  )
      A21(IELEM) =     X3  * (  F123 + F1  )
      A31(IELEM) =  X2     * ( -F123 - F1  )
      A32(IELEM) =  X2     * ( -F123 - F2  )
!
!   DIAGONAL TERMS
!
      A11(IELEM) = -A21(IELEM) -A31(IELEM)
      A22(IELEM) = -A12(IELEM) -A32(IELEM)
      A33(IELEM) = -A13(IELEM) -A23(IELEM)
!
2     CONTINUE
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
200       FORMAT(1X,'MT08AA (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'MT08AA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(1)
        ENDIF
!
!     ELSEIF(IELMF.EQ. ) THEN
!     OTHER TYPES OF FUNCTIONS F
!
!-----------------------------------------------------------------------
!
      ELSE
       IF (LNG.EQ.1) WRITE(LU,100) IELMF
       IF (LNG.EQ.2) WRITE(LU,101) IELMF
100    FORMAT(1X,'MT08AA (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,' NON PREVUE')
101    FORMAT(1X,'MT08AA (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
       CALL PLANTE(1)
       STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE CHPCON
!                    *****************
!
     &(UCONV,VCONV,U,V,UN,VN,TETAU)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE ADVECTION VECTOR FIELD UCONV,VCONV.
!
!history  J-M HERVOUET (LNH)
!+        17/08/1994
!+        V5P2
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
!| TETAU          |-->| IMPLICITATION SUR U.
!| U,V            |-->| COMPOSANTES DE LA VITESSE.
!| UCONV,VCONV    |-->| COMPOSANTES DU CHAMP CONVECTEUR.
!| UN,VN          |-->| COMPOSANTES DE LA VITESSE A L'ETAPE N.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: TETAU
      TYPE(BIEF_OBJ), INTENT(IN)    :: U,UN,V,VN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UCONV,VCONV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  CREATES CONVECTION ARRAYS UCONV AND VCONV
!
      CALL OS( 'X=CY    ' , UCONV , UN , U , 1.D0-TETAU )
      CALL OS( 'X=X+CY  ' , UCONV , U  , U ,      TETAU )
      CALL OS( 'X=CY    ' , VCONV , VN , U , 1.D0-TETAU )
      CALL OS( 'X=X+CY  ' , VCONV , V  , U ,      TETAU )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
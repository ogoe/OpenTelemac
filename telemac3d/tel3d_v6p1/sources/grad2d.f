!                    *****************
                     SUBROUTINE GRAD2D
!                    *****************
!
     &(DFDX,DFDY,FU,NPLAN,S,UNSV2D,FU2,FU3,FU4,IELM2,MESH2D,MSK,MASKEL)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE 2D GRADIENT OF FUNCTION F.
!
!history  F LEPEINTRE (LNH)    ; J-M JANIN (LNH)
!+        25/11/97
!+        V5P7
!+   
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+        
!+   FORTRAN95 VERSION 
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
!| DFDX           |<--| DF/DX
!| DFDY           |<--| DF/DY
!| FU             |-->| FONCTION A DERIVER
!| FU2            |---| 
!| FU3            |---| 
!| FU4            |---| 
!| IELM2          |-->| TYPE DE DISCRETISATION 2D
!| MASKEL         |-->| MASQUAGE DES ELEMENTS
!| MESH2D         |-->| BLOC DU MAILLAGE 2D
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
!| S              |---| 
!| UNSV2D         |-->| INVERSE DU VOLUME DES BASES EN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: DFDX, DFDY
      TYPE(BIEF_OBJ), INTENT(IN)    :: FU
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UNSV2D,S
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH2D
      INTEGER, INTENT(IN)           :: NPLAN, IELM2
      LOGICAL, INTENT(IN)           :: MSK
!     FU2,3,4 MUST BE 2D WORK FIELD - NO CHECKING
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FU2,FU3,FU4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOIN,I,IPLAN
      DOUBLE PRECISION, POINTER :: SAVEFU2(:),SAVEFU3(:),SAVEFU4(:)
!
!-----------------------------------------------------------------------
!
      NPOIN = MESH2D%NPOIN
!
      SAVEFU2=> FU2%R
      SAVEFU3=> FU3%R
      SAVEFU4=> FU4%R
!
      DO IPLAN=1,NPLAN
!
!     WORKING ON POINTERS RATHER THAN COPYING
      FU2%R=>  FU%R((IPLAN-1)*NPOIN+1:IPLAN*NPOIN)
      FU3%R=>DFDX%R((IPLAN-1)*NPOIN+1:IPLAN*NPOIN)
      FU4%R=>DFDY%R((IPLAN-1)*NPOIN+1:IPLAN*NPOIN)
!
!     CALCUL DE LA DERIVEE DF/DX
!
      CALL VECTOR(FU3,'=','GRADF          X',IELM2,1.D0,FU2,S,S,
     &            S,S,S,MESH2D,MSK,MASKEL)
      IF (NCSIZE.GT.1) CALL PARCOM (FU3, 2, MESH2D)
      CALL OS('X=XY    ' ,X=FU3,Y=UNSV2D)
!
!     CALCUL DE LA DERIVEE DF/DY
!
      CALL VECTOR(FU4,'=','GRADF          Y',IELM2,1.D0,FU2,S,S,
     &            S,S,S,MESH2D,MSK,MASKEL)
      IF (NCSIZE.GT.1) CALL PARCOM (FU4, 2, MESH2D)
      CALL OS('X=XY    ' ,X=FU4,Y=UNSV2D)
!
      ENDDO
!
      FU2%R=>SAVEFU2
      FU3%R=>SAVEFU3
      FU4%R=>SAVEFU4
!
!-----------------------------------------------------------------------
!
      RETURN
      END
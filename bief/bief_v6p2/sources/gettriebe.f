!                    ********************
                     SUBROUTINE GETTRIEBE
!                    ********************
!
     &(XAUX,AD,AX,TETA,IKLE,NPOIN,NELEM,NELMAX,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GETS THE TRIDIAGONAL PART OF A DIFFUSION MATRIX ON
!+                 PRISMS AND REMOVES IT FROM THE INITIAL MATRIX.
!code
!+            IF MTRI IS THIS TRIDIAGONAL PART, MAUX THE RESULT AND MDIF
!+            THE DIFFUSION MATRIX, THIS SUBROUTINE DOES:
!+
!+            MAUX = TETA * MTRI
!+            MDIF CHANGED INTO (1-TETA) * MDIF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNHE)
!+        13/08/08
!+        V5P9
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
!| AD             |-->| DIAGONAL TERMS OF MATRIX
!| AX             |-->| OFF-DIAGONAL TERMS OF MATRIX
!| IKLE           |-->| CONNECTIVITY TABLE
!| MESH           |-->| MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| TETA           |-->| COEFFICIENT USED IN THE RESULT
!| XAUX           |<--| THE RESULTING MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_GETTRIEBE => GETTRIEBE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(IN)    :: TETA
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*),AX(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: AD(NPOIN)
!
      TYPE(BIEF_MESH) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,NPLAN,IAN,ICOM,NPOIN2
!
!-----------------------------------------------------------------------
!
!     CONSIDERS HERE THAT NPOIN < NELMAX TO USE XAUX AS XAUX(NPOIN,3)
!
!     XAUX(I,1) IS COEFFICIENT OF POINT BELOW I IN EQUATION OF POINT I
!     XAUX(I,2) IS THE DIAGONAL
!     XAUX(I,3) IS COEFFICIENT OF POINT ABOVE I IN EQUATION OF POINT I
!
!-----------------------------------------------------------------------
!     INITIALISES THE DIAGONAL AND OFF-DIAGONAL TERMS
!-----------------------------------------------------------------------
!
      CALL OV('X=C     ',XAUX(1,1),AD,AD,0.D0,NPOIN)
      CALL OV('X=CY    ',XAUX(1,2),AD,AD,TETA,NPOIN)
      CALL OV('X=C     ',XAUX(1,3),AD,AD,0.D0,NPOIN)
!
      CALL OV('X=CX    ',AD,AD,AD,1.D0-TETA,NPOIN)
!
!-----------------------------------------------------------------------
!     ADDS TRIDIAGONAL TERMS
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
        XAUX(I1,3)=XAUX(I1,3)+TETA*AX(IELEM,03) ! TERM 1-4
        XAUX(I2,3)=XAUX(I2,3)+TETA*AX(IELEM,08) ! TERM 2-5
        XAUX(I3,3)=XAUX(I3,3)+TETA*AX(IELEM,12) ! TERM 3-6
        XAUX(I4,1)=XAUX(I4,1)+TETA*AX(IELEM,03) ! TERM 4-1
        XAUX(I5,1)=XAUX(I5,1)+TETA*AX(IELEM,08) ! TERM 5-2
        XAUX(I6,1)=XAUX(I6,1)+TETA*AX(IELEM,12) ! TERM 6-3
!
        AX(IELEM,03)=AX(IELEM,03)*(1.D0-TETA)
        AX(IELEM,08)=AX(IELEM,08)*(1.D0-TETA)
        AX(IELEM,12)=AX(IELEM,12)*(1.D0-TETA)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     PARALLEL MODE
!
      IF(NCSIZE.GT.1) THEN
        IAN    = 3
        ICOM   = 2
        NPOIN2 = BIEF_NBPTS(11,MESH)
        NPLAN=NPOIN/NPOIN2
        CALL PARCOM2(XAUX(1,1),XAUX(1,2),XAUX(1,3),
     &               NPOIN2,NPLAN,ICOM,IAN,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

!                    *********************
                     SUBROUTINE T3D_CORFON
!                    *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!+
!+            STANDARD ACTION: SMOOTHES THE BOTTOM ELEVATION.
!+
!+           (KEYWORD:  'NUMBER OF BOTTOM SMOOTHINGS')
!
!note     EQUIVALENT TO CORFON (BIEF LIBRARY), EXCEPT THAT THIS
!+         SUBROUTINE DISTINGUISHES DATA FROM STRUCTURES.
!
!history  J.M. JANIN  (LNH)
!+        25/11/97
!+        V5P1
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
!history  J-M HERVOUET (LNHE)
!+        29/09/2011
!+        V6P2
!+   Name changed into T3D_CORFON to avoid duplication with Telemac-2D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LISFON         |-->| NUMBER OF SMOOTHINGS REQUIRED
!| MASKEL         |-->| MASK OF ELEMENTS
!| MATR2D         |<->| WORK MATRIX IN 2DH
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS FOR USER
!| S              |-->| VOID STRUCTURE
!| ST1            |<->| STRUCTURE OF T1
!| ST2            |<->| STRUCTURE OF T2
!| SZF            |<->| STRUCTURE OF ZF
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY
!| X              |-->| MESH COORDINATE
!| Y              |-->| MESH COORDINATE
!| ZF             |<->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!     SMOOTHES THE BOTTOM ELEVATION
!
      IF(LISFON.GT.0) THEN
!
        MAS = .TRUE.
!
        CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &              1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)
      ENDIF
!
      DO I=1,NPOIN2
        IF (X(I).LE.160.D0) THEN
          ZF(I) = -10.D0
        ENDIF
        IF (X(I).GE.160.D0.AND.X(I).LE.260.D0) THEN
          ZF(I) = -10.D0-5./100.D0*(160.-X(I))
        ENDIF
        IF (X(I).GE.260.D0) THEN
          ZF(I) = -5.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END


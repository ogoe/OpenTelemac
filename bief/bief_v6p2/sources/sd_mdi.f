!                    *****************
                     SUBROUTINE SD_MDI
!                    *****************
!
     &(N,IA,JA,MAX,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES.
!
!note     IMPORTANT: INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!
!history  E. RAZAFINDRAKOTO (LNHE)
!+        20/11/06
!+        V5P7
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
!| FLAG           |<--| FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR 
!|                |   | MEANINGS ARE : 0      NO ERRORS DETECTED
!|                |   |         9*N + VI  INSUFFICIENT STORAGE IN MDI
!| HEAD           |<--| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| IA, JA         |-->| COMPACT STORAGE STRUCTURE OF MATRIX
!| L              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!| LAST           |---| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE
!|                |   | PERMUTATION OF THE ROWS AND COLUMNS OF M 
!|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;  
!|                |   | DIMENSION = N
!| MARK           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| MAX            |-->| DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS
!|                |   | V AND L; 
!| N              |-->| RANK OF MATRIX
!| NEXT           |<--| INVERSE OF THE PERMUTATION RETURNED IN LAST
!|                |   | DIMENSION = N
!| TAG            |-->| SEE DEFINITION IN INTERNAL PARAMATERS OF SD_MD.f 
!| V              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_MDI => SD_MDI
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: N,MAX,IA(*),JA(*)
      INTEGER, INTENT(INOUT) :: V(*),L(*),HEAD(*),LAST(*)
      INTEGER, INTENT(INOUT) :: NEXT(*),MARK(*),TAG,FLAG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER SFS,VI,DVI,VJ,JMIN,JMAX,J
!
!----INITIALISES DEGREES, ELEMENT LISTS, AND DEGREE LISTS
!
      DO 1 VI=1,N
        MARK(VI) = 1
        L(VI) = 0
        HEAD(VI) = 0
1     CONTINUE
      SFS = N+1
!
!----CREATES NONZERO STRUCTURE
!----FOR EACH NONZERO ENTRY A(VI,VJ) IN STRICT UPPER TRIANGLE
!
      DO 3 VI=1,N
        JMIN = IA(VI)
        JMAX = IA(VI+1) - 1
        IF(JMIN.GT.JMAX)  GO TO 3
        DO 2 J=JMIN,JMAX
          VJ = JA(J)
          IF(VI.GE.VJ) GO TO 2
          IF(SFS.GE.MAX) GO TO 101
!
!------ENTERS VJ IN ELEMENT LIST FOR VI
!
          MARK(VI) = MARK(VI) + 1
          V(SFS) = VJ
          L(SFS) = L(VI)
          L(VI) = SFS
          SFS = SFS+1
!
!------ENTERS VI IN ELEMENT LIST FOR VJ
!
          MARK(VJ) = MARK(VJ) + 1
          V(SFS) = VI
          L(SFS) = L(VJ)
          L(VJ) = SFS
          SFS = SFS+1
2       CONTINUE
3     CONTINUE
!
!----CREATES DEGREE LISTS AND INITIALISES MARK VECTOR
!
      DO 4 VI=1,N
        DVI = MARK(VI)
        NEXT(VI) = HEAD(DVI)
        HEAD(DVI) = VI
        LAST(VI) = -DVI
        IF(NEXT(VI).GT.0)  LAST(NEXT(VI)) = VI
        MARK(VI) = TAG
4     CONTINUE
!
      RETURN
!
! ** ERROR -- INSUFFICIENT STORAGE
!
101   FLAG = 9*N + VI
!
!-----------------------------------------------------------------------
!
      RETURN
      END

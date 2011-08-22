!                    *****************
                     SUBROUTINE PARAGL
!                    *****************
!
     &(KNOGL,DIM1_KNOGL,KNOLG,NBOR,NACHB,NPTFR,NPOIN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS ARRAY KNOGL.
!+                MODIFIES NBOR,NACHB TO GO FROM GLOBAL NODE NUMBERS
!+               (WHOLE MESH) TO GLOBAL NODE NUMBERS (SUB-DOMAIN).
!
!history  J-M HERVOUET (LNH)
!+        19/12/05
!+        V5P6
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
!| DIM1_KNOGL     |-->| FIRST DIMENSION OF KNOGL
!| KNOGL          |-->| GLOBAL NUMBER OF A LOCAL POINT IN PARALLEL
!| KNOLG          |<--| LOCAL NUMBER OF A GLOBAL POINT IN PARALLEL
!| NACHB          |<->| NACHB(1,I) : GLOBAL (INPUT) OR LOCAL (OUTPUT)
!|                |   | NUMBER OF INTERFACE POINT.
!|                |   | NACHB(2 TO 5,I) : NUMBER OF OTHER SUB-DOMAINS
!|                |   | CONTAINING THE POINT I.
!|                |   | I IS A NUMBERING OF INTERFACE POINTS.
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PARAGL => PARAGL
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: DIM1_KNOGL,NPTFR,NPOIN
      INTEGER, INTENT(INOUT) :: KNOGL(DIM1_KNOGL),KNOLG(NPOIN)
      INTEGER, INTENT(INOUT) :: NBOR(NPTFR),NACHB(NBMAXNSHARE,NPTIR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  BUILDS THE REVERSE OF ARRAY KNOLG:
!
!     POINTS OUTSIDE THE DOMAIN : 0
!     SEE IN ALMESH THE SIZE OF KNOGL: SUM OF NPOIN OF ALL SUB-DOMAINS
      DO I=1,DIM1_KNOGL
        KNOGL(I) = 0
      ENDDO
!
      DO I=1,NPOIN
        KNOGL(KNOLG(I))=I
      ENDDO
!
!  FROM IKLE TO SUB-DOMAIN NUMBERING
!  ALREADY DONE BY THE DOMAIN PARTITIONER
!      DO 18 J=1,3
!      DO 17 I=1,NELEM
!        IKLE(I,J)=KNOGL(IKLE(I,J))
!17    CONTINUE
!18    CONTINUE
!
!  FROM NBOR TO SUB-DOMAIN NUMBERING
!  EXCEPT FOR ESTEL3D WHERE NBOR ALREADY HAS LOCAL NUMBERS
!  SEE M_UNV2MESH.F90 /ESTEL3D  AND PARTEL.F /PARALLEL
!
      IF(NNAMECODE(1).NE.'ESTEL3D                 ') THEN
        DO 19 I=1,NPTFR
          NBOR(I)=KNOGL(NBOR(I))
19      CONTINUE
      ENDIF
!
!  FROM NACHB TO SUB-DOMAIN NUMBERING
!
      DO 21 I=1,NPTIR
        NACHB(1,I)=KNOGL(NACHB(1,I))
21    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END

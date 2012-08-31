!                    ******************
                     SUBROUTINE SKIPGEO
!                    ******************
!
     &(NFIC,TITFIC,NPOIN,NVAR,TEXTLU,NPLAN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SKIPS THE GEOMETRY IN A SELAFIN FILE.
!code
!+    LIST OF RECORDS IN THE GEOMETRY FILE:
!+
!+      1    : TITLE
!+      2    : NUMBER OF FUNCTIONS READ ON GRIDS 1 AND 2
!+      3    : VARIABLE NAMES AND UNITS
!+      4    : 1,0,0,0,0,0,0,0,0,N
!+      4.1  : DATE(3 INTEGERS) AND TIME(3 INTEGERS) IF N=1
!+      5    : NELEM,NPOIN,NDP,1
!+      6    : IKLE
!+      7    : ARRAY IPOBO (SIZE NPOIN), 0 FOR INTERNAL POINTS
!+             A NUMBER OTHERWISE
!+      8    : X
!+      9    : Y
!
!history  J-M HERVOUET (LNH)
!+        18/11/04
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
!| NFIC           |-->| LOGICAL UNIT OF FILE TO READ
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN          |<--| NUMBER OF POINTS IN THE MESH
!| NVAR           |<--| NUMBER OF VARIABLES IN THE FILE
!| TEXTLU         |<--| NAMES OF VARIABLES (32 CHARACTERS FOR EACH)
!|                |   | 16 FIRST : NAME  16 LAST : UNIT
!| TITFIC         |<--| TITLE OF FILE (FIRST RECORD)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NFIC
      INTEGER, INTENT(OUT), OPTIONAL :: NPLAN
      INTEGER, INTENT(OUT)           :: NPOIN,NVAR
      CHARACTER(LEN=72), INTENT(OUT) :: TITFIC
      CHARACTER(LEN=32), INTENT(OUT) :: TEXTLU(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XBID(1)
      REAL W(1)
      INTEGER IB(10),ISTAT,I,IBID(1)
      CHARACTER*1 CBID
!
!-----------------------------------------------------------------------
!
!   BEGINNING OF FILE
!
      REWIND NFIC
!
!   LEC/ECR 1   : NAME OF GEOMETRY FILE
!
      CALL LIT(XBID,W,IBID,TITFIC,72,'CH',NFIC,'STD',ISTAT)
!
!   LEC/ECR 2   : NUMBER OF DISCRETISATION FUNCTIONS 1 AND 2
!
      CALL LIT(XBID,W,IB,CBID,2,'I ',NFIC,'STD',ISTAT)
      NVAR = IB(1)+IB(2)
!
!   LEC/ECR 3 : VARIABLE NAMES AND UNITS
!
      IF(NVAR.GE.1) THEN
        DO 10 I=1,NVAR
          CALL LIT(XBID,W,IBID,TEXTLU(I),32,'CH',NFIC,'STD',ISTAT)
10      CONTINUE
      ENDIF
!
!   LEC/ECR 4   : LIST OF 10 INTEGER PARAMETERS
!
      CALL LIT(XBID,W,IB,CBID,10,'I ',NFIC,'STD',ISTAT)
      IF(PRESENT(NPLAN)) NPLAN=IB(7)
      IF(IB(10).EQ.1) THEN
        CALL LIT(XBID,W,IB,CBID,1,'I ',NFIC,'STD',ISTAT)
      ENDIF
!
!   LEC/ECR 5 : 4 INTEGERS
!
      CALL LIT(XBID,W,IB,CBID,4,'I ',NFIC,'STD',ISTAT)
      NPOIN = IB(2)
!
!   LEC/ECR 6 : IKLE
!
      CALL LIT(XBID,W,IB,CBID,1,'I ',NFIC,'STD',ISTAT)
!
!   LEC/ECR 7 : IPOBO (FILES WITHOUT PARALLELISM)
!
      CALL LIT(XBID,W,IB,CBID,1,'I ',NFIC,'STD',ISTAT)
!
!   LEC/ECR  8 AND 9 : X AND Y COORDINATES OF THE MESH POINTS
!
      CALL LIT(XBID,W,IBID,CBID,1,'R4',NFIC,'STD',ISTAT)
      CALL LIT(XBID,W,IBID,CBID,1,'R4',NFIC,'STD',ISTAT)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

!                    *******************
                     SUBROUTINE READGEO2
!                    *******************
!
     &(NPOIN,NELEM,NPTFR,NDP,IKLES,IPOBO,IB,NFIC)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS OR COMPUTES THE VALUES OF NPOIN, NELEM, NPTFR.
!+                READS THE CONNECTIVITY TABLE AND NUMBERING FOR THE
!+                BOUNDARY NODES.
!
!warning  USER SUBROUTINE (MAY BE REWRITTEN FOR ANOTHER FILE FORMAT)
!
!history  J-M HERVOUET (LNH)     ; REGINA NEBAUER; LAM MINH PHUONG
!+        29/04/04
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
!| IB             |-->| SERIES OF 10 INTEGERS IN THE SELAFIN FORMAT
!| IKLES          |-->| LIKE CONNECTIVITY TABLE BUT IN SELAFIN FORMAT
!|                |   | IKLES(3,NELEM) INSTEAD OF IKLE(NELEM,3)
!| IPOBO          |<--| INTEGER ARRAY. WHEN 0: INNER POINT
!|                |   | IF NOT 0, BOUNDARY POINT NUMBER
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NFIC           |-->| LOGICAL UNIT OF GEOMETRY FILE
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_READGEO2 => READGEO2
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(OUT) :: NPTFR
      INTEGER, INTENT(IN)  :: NFIC,NPOIN,NELEM,NDP,IB(10)
      INTEGER, INTENT(OUT) :: IKLES(NDP*NELEM)
      INTEGER, INTENT(OUT) :: IPOBO(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XB(2)
      REAL RB(2)
      INTEGER ISTAT,I
      CHARACTER(LEN=1)  :: CB
!
!-----------------------------------------------------------------------
!
!     HAS ALREADY READ THE 1ST PART OF THE FILE
!
!     REWIND NFIC
!
!     6: IKLES (LIKE IKLE BUT INDICES EXCHANGED)
!
      CALL LIT(XB,RB,IKLES,CB,NELEM*NDP,'I ',NFIC,'STD',ISTAT)
!
!     7: IPOBO (SCALAR MODE)
!
      IF(IB(8).EQ.0.AND.IB(9).EQ.0) THEN
!
        CALL LIT(XB,RB,IPOBO,CB,NPOIN,'I ',NFIC,'STD',ISTAT)
!
        NPTFR = 0
!
        IF(NPOIN.GE.1) THEN
          DO 22 I = 1 , NPOIN
            IF(IPOBO(I).NE.0) NPTFR = NPTFR + 1
22        CONTINUE
        ENDIF
!
      ELSE
!
!       PARALLEL MODE,
!       CASE WHERE KNOLG REPLACES IPOBO:
!       IPOBO IS NOT READ HERE, WILL BE IN READGEO2
!       BUT NPTFR, MXPTVS AND MXELVS NEED TO BE COMPUTED
        NPTFR = IB(8)
        IF(NPOIN.GE.1) THEN
          DO 122 I = 1 , NPOIN
            IPOBO(I)=1
122       CONTINUE
        ENDIF
!       IPOBO SET TO 1: MXPTVS WILL HAVE 1 TOO MANY
!       BUT WOULD OTHERWISE NEED TO BUILD THE TRUE IPOBO AND ALSO
!       CONSIDER INTERFACE POINTS.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

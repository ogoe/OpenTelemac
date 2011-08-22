!                    **********************
                     SUBROUTINE FIND_IN_SEL
!                    **********************
!
     &(RES,NAME,NFIC,W,OK,RECORD,NP,TIME)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    LOOKS FOR A RESULT ARRAY IN A SELAFIN FILE.
!
!history  J-M HERVOUET (LNH)
!+        08/08/98
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
!| NAME           |-->| NAME OF VARIABLE (16 CHARACTERS)
!| NFIC           |-->| NUMERO DU CANAL DU FICHIER
!| NP             |<--| NUMBER OF POINTS (OPTIONAL)
!| OK             |<--| TRUE IF ARRAY IS FOUND
!| RECORD         |-->| NUMBER OF THE REQUESTED RECORD
!| RES            |<--| WHERE TO PUT THE RESULT
!| TIME           |<--| TIME OF RECORD (OPTIONAL)
!| W              |<->| REAL WORK ARRAY OF DIMENSION AT LEAST NPOIN.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FIND_IN_SEL => FIND_IN_SEL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: RES
      CHARACTER(LEN=16), INTENT(IN) :: NAME
      LOGICAL, INTENT(OUT)          :: OK
      REAL, INTENT(INOUT)           :: W(*)
      INTEGER, INTENT(IN) :: NFIC
      INTEGER, INTENT(IN),  OPTIONAL          :: RECORD
      INTEGER, INTENT(OUT), OPTIONAL          :: NP
      DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: TIME
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOIN,ISTAT,I,NVAR,IB(10),REC,IREC
!
      DOUBLE PRECISION XB(2)
      REAL RB(2)
!
      CHARACTER*1 CB
      CHARACTER*32 TEXTLU(36)
!
!-----------------------------------------------------------------------
!
      IF(PRESENT(RECORD)) THEN
        REC = RECORD
      ELSE
        REC = 1
      ENDIF
!
      OK = .FALSE.
!
!-----------------------------------------------------------------------
!
!     'QUICKLY' READS UNTIL REACHES A TIME RECORD
!
!
!     GOES TO THE BEGINNING OF THE FILE
!
      REWIND NFIC
!
!     1: TITLE
      CALL LIT(XB,RB,IB,CB,1,'CH',NFIC,'STD',ISTAT)
!
!     2: NUMBER OF ARRAYS IN THE RESULT FILE
      CALL LIT(XB,RB,IB,CB,2,'I ',NFIC,'STD',ISTAT)
      NVAR =  IB(1)  +  IB(2)
!
!     3: NAMES AND UNITS OF VARIABLES
      IF(NVAR.GE.1) THEN
        DO I=1,NVAR
           CALL LIT(XB,RB,IB,TEXTLU(I),32,'CH',NFIC,'STD',ISTAT)
        ENDDO
      ENDIF
!
!     4: LIST OF 10 INTEGER PARAMETERS
      CALL LIT(XB,RB,IB,CB,10,'I ',NFIC,'STD',ISTAT)
!     CASE WHERE DATE AND TIME ARE IN THE FILE
      IF(IB(10).EQ.1) CALL LIT(XB,RB,IB,CB,6,'I ',NFIC,'STD',ISTAT)
!
!     5: 4 INTEGERS
      CALL LIT(XB,RB,IB,CB,4,'I ',NFIC,'STD',ISTAT)
      NPOIN = IB(2)
!
      IF(PRESENT(NP)) NP = NPOIN
!
!     6: IKLES (LIKE IKLE BUT INDICES EXCHANGED)
      CALL LIT(XB,RB,IB,CB,1,'I ',NFIC,'STD',ISTAT)
!
!     7: IPOBO OR KNOLG
      CALL LIT(XB,RB,IB,CB,1,'I ',NFIC,'STD',ISTAT)
!
!     8 AND 9: X AND Y
      CALL LIT(XB,W,IB,CB,1,'R4',NFIC,'STD',ISTAT)
      CALL LIT(XB,W,IB,CB,1,'R4',NFIC,'STD',ISTAT)
!
!-----------------------------------------------------------------------
!
      IREC = 0
500   IREC = IREC + 1
      IF (NVAR.GE.1) THEN
!
!       TIME RECORD
!
        CALL LIT(XB,W,IB,CB,1,'R4',NFIC,'STD',ISTAT)
!       NOTE JMH : THE FOLLOWING INSTRUCTION RAISES PROBLEMS
!       WHEN TIME IS NOT PRESENT, WITH NAG COMPILER AND OPTION -O4
        IF(PRESENT(TIME)) TIME=XB(1)
!
        DO I=1,NVAR
!
!         READS THE VARIABLE, OR SKIPS THE RECORD
          IF(TEXTLU(I)(1:16).EQ.NAME.AND.REC.EQ.IREC) THEN
            CALL LIT(RES%R,W,IB,CB,NPOIN,'R4',NFIC,'STD',ISTAT)
            OK=.TRUE.
          ELSE
            CALL LIT(XB,W,IB,CB,1,'R4',NFIC,'STD',ISTAT)
          ENDIF
!
        ENDDO
!
      ENDIF
      IF(IREC.NE.REC) GO TO 500
!
!-----------------------------------------------------------------------
!
      RETURN
      END

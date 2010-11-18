C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       LOOKS FOR A RESULT ARRAY IN A SELAFIN FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NAME, NFIC, NP, OK, RECORD, RES, TIME, W
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CB, I, IB, IREC, ISTAT, NPOIN, NVAR, RB, REC, TEXTLU, XB
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FIND_IN_SEL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LIT()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FONSTR(), MESURES(), PROSOU(), TRISOU()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> 08/08/98
!> </td><td> J-M HERVOUET (LNH) 30 71 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NAME
!></td><td>--></td><td>NAME OF VARIABLE (16 CHARACTERS)
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>NUMERO DU CANAL DU FICHIER
!>    </td></tr>
!>          <tr><td>NP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OK
!></td><td><--</td><td>TRUE IF ARRAY IS FOUND
!>    </td></tr>
!>          <tr><td>PARAMETRES OPTIONNELS
!></td><td>---</td><td>:
!>    </td></tr>
!>          <tr><td>RECORD
!></td><td>--></td><td>NUMBER OF THE REQUESTED RECORD
!>    </td></tr>
!>          <tr><td>RES
!></td><td><--</td><td>WHERE TO PUT THE RESULT
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>TABLEAU DE TRAVAIL REEL DE DIMENSION NPOIN.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FIND_IN_SEL
     &(RES,NAME,NFIC,W,OK,RECORD,NP,TIME)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NAME           |-->| NAME OF VARIABLE (16 CHARACTERS)
C| NFIC           |-->| NUMERO DU CANAL DU FICHIER
C| NP             |---| 
C| OK             |<--| TRUE IF ARRAY IS FOUND
C| PARAMETRES OPTI|---| :
C| RECORD         |-->| NUMBER OF THE REQUESTED RECORD
C| RES            |<--| WHERE TO PUT THE RESULT
C| TIME           |---| 
C| W             |---| TABLEAU DE TRAVAIL REEL DE DIMENSION NPOIN.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FIND_IN_SEL => FIND_IN_SEL
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: RES
      CHARACTER(LEN=16), INTENT(IN) :: NAME
      LOGICAL, INTENT(OUT)          :: OK
      REAL, INTENT(INOUT)           :: W(*)
      INTEGER, INTENT(IN) :: NFIC
      INTEGER, INTENT(IN),  OPTIONAL          :: RECORD
      INTEGER, INTENT(OUT), OPTIONAL          :: NP
      DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: TIME
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NPOIN,ISTAT,I,NVAR,IB(10),REC,IREC
C
      DOUBLE PRECISION XB(2)
      REAL RB(2)
C
      CHARACTER*1 CB
      CHARACTER*32 TEXTLU(36)
C
C-----------------------------------------------------------------------
C
      IF(PRESENT(RECORD)) THEN
        REC = RECORD
      ELSE
        REC = 1
      ENDIF
C
      OK = .FALSE.
C
C-----------------------------------------------------------------------
C
C     'QUICKLY' READS UNTIL REACHES A TIME RECORD
C
C
C     GOES TO THE BEGINNING OF THE FILE
C
      REWIND NFIC
C
C     1: TITLE
      CALL LIT(XB,RB,IB,CB,1,'CH',NFIC,'STD',ISTAT)
C
C     2: NUMBER OF ARRAYS IN THE RESULT FILE
      CALL LIT(XB,RB,IB,CB,2,'I ',NFIC,'STD',ISTAT)
      NVAR =  IB(1)  +  IB(2)
C
C     3: NAMES AND UNITS OF VARIABLES
      IF(NVAR.GE.1) THEN
        DO I=1,NVAR
           CALL LIT(XB,RB,IB,TEXTLU(I),32,'CH',NFIC,'STD',ISTAT)
        ENDDO
      ENDIF
C
C     4: LIST OF 10 INTEGER PARAMETERS
      CALL LIT(XB,RB,IB,CB,10,'I ',NFIC,'STD',ISTAT)
C     CASE WHERE DATE AND TIME ARE IN THE FILE
      IF(IB(10).EQ.1) CALL LIT(XB,RB,IB,CB,6,'I ',NFIC,'STD',ISTAT)
C
C     5: 4 INTEGERS
      CALL LIT(XB,RB,IB,CB,4,'I ',NFIC,'STD',ISTAT)
      NPOIN = IB(2)
C
      IF(PRESENT(NP)) NP = NPOIN
C
C     6: IKLES (LIKE IKLE BUT INDICES EXCHANGED)
      CALL LIT(XB,RB,IB,CB,1,'I ',NFIC,'STD',ISTAT)
C
C     7: IPOBO OR KNOLG
      CALL LIT(XB,RB,IB,CB,1,'I ',NFIC,'STD',ISTAT)
C
C     8 AND 9: X AND Y
      CALL LIT(XB,W,IB,CB,1,'R4',NFIC,'STD',ISTAT)
      CALL LIT(XB,W,IB,CB,1,'R4',NFIC,'STD',ISTAT)
C
C-----------------------------------------------------------------------
C
      IREC = 0
500   IREC = IREC + 1
      IF (NVAR.GE.1) THEN
C
C       TIME RECORD
C
        CALL LIT(XB,W,IB,CB,1,'R4',NFIC,'STD',ISTAT)
C       NOTE JMH : THE FOLLOWING INSTRUCTION RAISES PROBLEMS
C       WHEN TIME IS NOT PRESENT, WITH NAG COMPILER AND OPTION -O4
        IF(PRESENT(TIME)) TIME=XB(1)
C
        DO I=1,NVAR
C
C         READS THE VARIABLE, OR SKIPS THE RECORD
          IF(TEXTLU(I)(1:16).EQ.NAME.AND.REC.EQ.IREC) THEN
            CALL LIT(RES%R,W,IB,CB,NPOIN,'R4',NFIC,'STD',ISTAT)
            OK=.TRUE.
          ELSE
            CALL LIT(XB,W,IB,CB,1,'R4',NFIC,'STD',ISTAT)
          ENDIF
C
        ENDDO
C
      ENDIF
      IF(IREC.NE.REC) GO TO 500
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
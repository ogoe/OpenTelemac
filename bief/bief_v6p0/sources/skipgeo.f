C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SKIPS THE GEOMETRY IN A SELAFIN FILE.
!>  @code
!>    LIST OF RECORDS IN THE GEOMETRY FILE:<br>
!>      1    : TITLE
!>      2    : NUMBER OF FUNCTIONS READ ON GRIDS 1 AND 2
!>      3    : VARIABLE NAMES AND UNITS
!>      4    : 1,0,0,0,0,0,0,0,0,N
!>      4.1  : DATE(3 INTEGERS) AND TIME(3 INTEGERS) IF N=1
!>      5    : NELEM,NPOIN,NDP,1
!>      6    : IKLE
!>      7    : ARRAY IPOBO (SIZE NPOIN), 0 FOR INTERNAL POINTS
!>             A NUMBER OTHERWISE
!>      8    : X
!>      9    : Y
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NFIC, NPLAN, NPOIN, NVAR, TEXTLU, TITFIC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CBID, I, IB, IBID, ISTAT, W, XBID
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LIT()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D(), SUITE_SERAFIN()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 18/11/04
!> </td><td> J-M HERVOUET (LNH) 01 30 71 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>LOGICAL UNIT OF FILE TO READ
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td><--</td><td>NUMBER OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>NVAR
!></td><td><--</td><td>NUMBER OF VARIABLES IN THE FILE
!>    </td></tr>
!>          <tr><td>TEXTLU
!></td><td><--</td><td>NAMES OF VARIABLES (32 CHARACTERS FOR EACH)
!>                  16 FIRST : NAME  16 LAST : UNIT
!>    </td></tr>
!>          <tr><td>TITFIC
!></td><td><--</td><td>TITLE OF FILE (FIRST RECORD)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SKIPGEO
     &(NFIC,TITFIC,NPOIN,NVAR,TEXTLU,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NFIC           |-->| LOGICAL UNIT OF FILE TO READ
C| NPLAN          |---| 
C| NPOIN          |<--| NUMBER OF POINTS IN THE MESH
C| NVAR           |<--| NUMBER OF VARIABLES IN THE FILE
C| TEXTLU         |<--| NAMES OF VARIABLES (32 CHARACTERS FOR EACH)
C|                |   | 16 FIRST : NAME  16 LAST : UNIT
C| TITFIC         |<--| TITLE OF FILE (FIRST RECORD)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NFIC
      INTEGER, INTENT(OUT), OPTIONAL :: NPLAN
      INTEGER, INTENT(OUT)           :: NPOIN,NVAR
      CHARACTER(LEN=72), INTENT(OUT) :: TITFIC
      CHARACTER(LEN=32), INTENT(OUT) :: TEXTLU(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XBID(1)
      REAL W(1)
      INTEGER IB(10),ISTAT,I,IBID(1)
      CHARACTER*1 CBID
C
C-----------------------------------------------------------------------
C
C   BEGINNING OF FILE
C
      REWIND NFIC
C
C   LEC/ECR 1   : NAME OF GEOMETRY FILE
C
      CALL LIT(XBID,W,IBID,TITFIC,72,'CH',NFIC,'STD',ISTAT)
C
C   LEC/ECR 2   : NUMBER OF DISCRETISATION FUNCTIONS 1 AND 2
C
      CALL LIT(XBID,W,IB,CBID,2,'I ',NFIC,'STD',ISTAT)
      NVAR = IB(1)+IB(2)
C
C   LEC/ECR 3 : VARIABLE NAMES AND UNITS
C
      IF(NVAR.GE.1) THEN
        DO 10 I=1,NVAR
          CALL LIT(XBID,W,IBID,TEXTLU(I),32,'CH',NFIC,'STD',ISTAT)
10      CONTINUE
      ENDIF
C
C   LEC/ECR 4   : LIST OF 10 INTEGER PARAMETERS
C
      CALL LIT(XBID,W,IB,CBID,10,'I ',NFIC,'STD',ISTAT)
      IF(PRESENT(NPLAN)) NPLAN=IB(7)
      IF(IB(10).EQ.1) THEN
        CALL LIT(XBID,W,IB,CBID,1,'I ',NFIC,'STD',ISTAT)
      ENDIF
C
C   LEC/ECR 5 : 4 INTEGERS
C
      CALL LIT(XBID,W,IB,CBID,4,'I ',NFIC,'STD',ISTAT)
      NPOIN = IB(2)
C
C   LEC/ECR 6 : IKLE
C
      CALL LIT(XBID,W,IB,CBID,1,'I ',NFIC,'STD',ISTAT)
C
C   LEC/ECR 7 : IPOBO (FILES WITHOUT PARALLELISM)
C
      CALL LIT(XBID,W,IB,CBID,1,'I ',NFIC,'STD',ISTAT)
C
C   LEC/ECR  8 AND 9 : X AND Y COORDINATES OF THE MESH POINTS
C
      CALL LIT(XBID,W,IBID,CBID,1,'R4',NFIC,'STD',ISTAT)
      CALL LIT(XBID,W,IBID,CBID,1,'R4',NFIC,'STD',ISTAT)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
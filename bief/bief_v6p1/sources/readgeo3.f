C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS OR COMPUTES THE VALUES OF NPOIN, NELEM, NPTFR,
!>                MXPTVS, MXELVS IN THE GEOMETRY FILE (CHANNEL NGEO).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE (MAY BE REWRITTEN FOR ANOTHER FILE FORMAT)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IB, KNOLG, NFIC, NPOIN, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CB, ERR, ISTAT, RB, RBID, XB
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_READGEO3
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CORRXY(), LIT()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALMESH()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 19/10/05
!> </td><td> EMILE RAZAFINDRAKOTO
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 19/10/03
!> </td><td> J-M HERVOUET (LNH) 01 30 71 80 18; REGINA NEBAUER; LAM MINH PHUONG
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNOLG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td><--</td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td><--</td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td><--</td><td>NOMBRE DE POINTS FRONTIERE DU DOMAINE.
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE READGEO3
     &(KNOLG,X,Y,NPOIN,NFIC,IB,Z)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IB             |---| 
C| KNOLG          |---| 
C| NELEM          |<--| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NFIC           |---| 
C| NPOIN          |<--| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |<--| NOMBRE DE POINTS FRONTIERE DU DOMAINE.
C| X             |---| 
C| Y             |---| 
C| Z             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_READGEO3 => READGEO3
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NPOIN,NFIC
      INTEGER, INTENT(INOUT)        :: IB(10)
      INTEGER, INTENT(OUT)          :: KNOLG(NPOIN)
      DOUBLE PRECISION, INTENT(OUT) :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: Z(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XB(2)
      REAL, ALLOCATABLE :: RB(:)
      REAL RBID(1)
      INTEGER ISTAT,ERR
      CHARACTER(LEN=1)  :: CB
C
C-----------------------------------------------------------------------
C
C     HAS ALREADY READ THE 1ST PART OF THE FILE IN READGEO1
C
C     REWIND NFIC
C
C     7 : KNOLG REPLACES IPOBO (PARALLEL MODE)
C
      IF(IB(8).NE.0.OR.IB(9).NE.0) THEN
C       PARALLEL MODE,
C       CASE WHERE KNOLG REPLACES IPOBO
        CALL LIT(XB,RBID,KNOLG,CB,NPOIN,'I ',NFIC,'STD',ISTAT)
      ENDIF
C
C     8 AND 9: X AND Y COORDINATES
C
      ALLOCATE(RB(NPOIN),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'READGEO3 : ALLOCATION DE RB DEFECTUEUSE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'READGEO3 : WRONG ALLOCATION OF RB'
        ENDIF
        STOP
      ENDIF
C
      CALL LIT(X   ,RB,IB,CB,NPOIN,'R4',NFIC,'STD',ISTAT)
      CALL LIT(Y   ,RB,IB,CB,NPOIN,'R4',NFIC,'STD',ISTAT)
C
C     SPECIAL FORMAT FOR TETRAHEDRONS : Z AFTER X AND Y
C     A RECORD FOR TIME IS PRESENT WITH THE SELAFIN FORMAT
C     WHEN Z IS GIVEN AS VARIABLE IN TIME, BUT THIS IS NEVER USED.
C
      IF(PRESENT(Z)) THEN
C       RECORD FOR TIME
C       CALL LIT(Z,RB,IB,CB,1,'R4',NFIC,'STD',ISTAT)
C       RECORD FOR Z (FIRST VARIABLE IN SELAFIN FORMAT)
        CALL LIT(Z,RB,IB,CB,NPOIN,'R4',NFIC,'STD',ISTAT)
      ENDIF
C
C-----------------------------------------------------------------------
C
CMODIFICATION ER 19/10/2005
      CALL CORRXY(X,Y,NPOIN)
CEND OF MODIFICATION ER 19/10/2005
C
      DEALLOCATE(RB)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
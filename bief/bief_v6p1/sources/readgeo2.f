C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS OR COMPUTES THE VALUES OF NPOIN, NELEM, NPTFR.
!>                READS THE CONNECTIVITY TABLE AND NUMBERING FOR THE
!>                BOUNDARY NODES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE (MAY BE REWRITTEN FOR ANOTHER FILE FORMAT)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IB, IKLES, IPOBO, NDP, NELEM, NFIC, NPOIN, NPTFR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CB, I, ISTAT, RB, XB
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_READGEO2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LIT()
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
!>      <td><center> 5.5                                       </center>
!> </td><td> 29/04/04
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
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>IKLES
!></td><td><--</td><td>TABLE DE CONNECTIVITE
!>    </td></tr>
!>          <tr><td>IPOBO
!></td><td><--</td><td>TABLE NUEROS NOEUDS DE BORD
!>    </td></tr>
!>          <tr><td>NDP
!></td><td>--></td><td>NOMBRE DE POINTS PAR ELEMENT
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>UNITE LOGIQUE DU FICHIER GEO
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE DU DOMAINE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE READGEO2
     &(NPOIN,NELEM,NPTFR,NDP,IKLES,IPOBO,IB,NFIC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IB             |-->| 
C| IKLES          |<--| TABLE DE CONNECTIVITE
C| IPOBO          |<--| TABLE NUEROS NOEUDS DE BORD
C| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NFIC           |-->| UNITE LOGIQUE DU FICHIER GEO
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE DU DOMAINE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_READGEO2 => READGEO2
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(OUT) :: NPTFR
      INTEGER, INTENT(IN)  :: NFIC,NPOIN,NELEM,NDP,IB(10)
      INTEGER, INTENT(OUT) :: IKLES(NDP*NELEM)
      INTEGER, INTENT(OUT) :: IPOBO(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XB(2)
      REAL RB(2)
      INTEGER ISTAT,I
      CHARACTER(LEN=1)  :: CB
C
C-----------------------------------------------------------------------
C
C     HAS ALREADY READ THE 1ST PART OF THE FILE
C
C     REWIND NFIC
C
C     6: IKLES (LIKE IKLE BUT INDICES EXCHANGED)
C
      CALL LIT(XB,RB,IKLES,CB,NELEM*NDP,'I ',NFIC,'STD',ISTAT)
C
C     7: IPOBO (SCALAR MODE)
C
      IF(IB(8).EQ.0.AND.IB(9).EQ.0) THEN
C
        CALL LIT(XB,RB,IPOBO,CB,NPOIN,'I ',NFIC,'STD',ISTAT)
C
        NPTFR = 0
C
        IF(NPOIN.GE.1) THEN
          DO 22 I = 1 , NPOIN
            IF(IPOBO(I).NE.0) NPTFR = NPTFR + 1
22        CONTINUE
        ENDIF
C
      ELSE
C
C       PARALLEL MODE,
C       CASE WHERE KNOLG REPLACES IPOBO:
C       IPOBO IS NOT READ HERE, WILL BE IN READGEO2
C       BUT NPTFR, MXPTVS AND MXELVS NEED TO BE COMPUTED
        NPTFR = IB(8)
        IF(NPOIN.GE.1) THEN
          DO 122 I = 1 , NPOIN
            IPOBO(I)=1
122       CONTINUE
        ENDIF
C       IPOBO SET TO 1: MXPTVS WILL HAVE 1 TOO MANY
C       BUT WOULD OTHERWISE NEED TO BUILD THE TRUE IPOBO AND ALSO
C       CONSIDER INTERFACE POINTS.
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C
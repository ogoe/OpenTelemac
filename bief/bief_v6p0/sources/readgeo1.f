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
!>    </th><td> IB, NDP, NELEBD, NELEM, NFIC, NPOIN, NPTFR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CB, I, IB6, ISTAT, NVAR, RB, TITRE, XB
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_READGEO1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LIT(), PLANTE()
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
!></td><td><--</td><td>10 INTEGERS, SEE SELAFIN FILE STANDARD
!>    </td></tr>
!>          <tr><td>NDP
!></td><td><--</td><td>NOMBRE DE NOEUD PAR ELEMENT
!>    </td></tr>
!>          <tr><td>NELEBD
!></td><td><--</td><td>NOMBRE D'ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td><--</td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>UNITE LOGIQUE FICHIER GEO
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td><--</td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td><--</td><td>NOMBRE DE POINTS FRONTIERE DU DOMAINE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE READGEO1
     &(NPOIN,NELEM,NPTFR,NDP,IB,NFIC,NELEBD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IB             |<--| 10 INTEGERS, SEE SELAFIN FILE STANDARD
C| NDP            |<--| NOMBRE DE NOEUD PAR ELEMENT
C| NELEBD         |<--| NOMBRE D'ELEMENTS DE BORD
C| NELEM          |<--| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NFIC           |-->| UNITE LOGIQUE FICHIER GEO
C| NPOIN          |<--| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |<--| NOMBRE DE POINTS FRONTIERE DU DOMAINE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_READGEO1 => READGEO1
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(OUT)         :: NPOIN  ! NUMBER OF MESH NODES
      INTEGER, INTENT(OUT)         :: NELEM  ! NUMBER OF ELEMENTS
      INTEGER, INTENT(OUT)         :: NDP    ! NUMBER OF ELEMENT FACES
      INTEGER, INTENT(OUT)         :: IB(10) ! INTEGER ARRAY
      INTEGER, INTENT(OUT)         :: NPTFR  ! NUMBER OF BOUNDARY NODES
      INTEGER, INTENT(IN)          :: NFIC   ! FILE TO READ
      INTEGER,OPTIONAL,INTENT(OUT) :: NELEBD ! NUMBER OF BOUNDARY ELEMENTS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION  :: XB(2)
      REAL              :: RB(2)
      INTEGER           :: ISTAT
      INTEGER           :: NVAR
      INTEGER           :: I,IB6(6)
      CHARACTER(LEN=2)  :: CB
      CHARACTER(LEN=72) :: TITRE
C
C-----------------------------------------------------------------------
C
C   GOES TO THE BEGINNING OF THE FILE
C
      REWIND NFIC
C
C     1: TITLE
      CALL LIT(XB,RB,IB,TITRE,72,'CH',NFIC,'STD',ISTAT)
C
C     2: NUMBER OF ARRAYS IN THE RESULT FILE
      CALL LIT(XB,RB,IB,CB,2,'I ',NFIC,'STD',ISTAT)
      NVAR =  IB(1)  +  IB(2)
C     3: NAMES AND UNITS OF VARIABLES
      IF(NVAR.GE.1) THEN
        DO I=1,NVAR
          CALL LIT(XB,RB,IB,CB,2,'CH',NFIC,'STD',ISTAT)
        ENDDO
      ENDIF
C
C     4: LIST OF 10 INTEGER PARAMETERS
      CALL LIT(XB,RB,IB,CB,10,'I ',NFIC,'STD',ISTAT)
C
C     CASE WHERE DATE AND TIME ARE IN THE FILE
      IF(IB(10).EQ.1) CALL LIT(XB,RB,IB6,CB,6,'I ',NFIC,'STD',ISTAT)
C
C     READS THE NUMBER OF BOUNDARY ELEMENTS FOR 3D MESH
      IF(IB(7).NE.0.AND.PRESENT(NELEBD)) THEN
        NELEBD = IB(7)
      END IF
C     CASE WHERE KNOLG IS GIVEN INSTEAD OF IPOBO (PARALLEL MODE)
      IF(IB(8).NE.0) THEN
        NPTFR=IB(8)
C       NOTE JMH : NEXT LINE MOVED AFTER ENDIF ON 22/07/02
C                  SUBDOMAINS MAY HAVE NPTFR=0
C       NPTIR=IB(9)
      ENDIF
      NPTIR=IB(9)
C
C     5: 4 INTEGERS
      CALL LIT(XB,RB,IB6,CB,4,'I ',NFIC,'STD',ISTAT)
C
      NELEM = IB6(1)
      NPOIN = IB6(2)
      NDP   = IB6(3)
C
C-----------------------------------------------------------------------
C
C  PRINTOUT FORMATS:
C
      IF(LNG.EQ.1) WRITE(LU,300) TITRE
      IF(LNG.EQ.1) WRITE(LU,500) NELEM,NPOIN
      IF(LNG.EQ.2) WRITE(LU,301) TITRE
      IF(LNG.EQ.2) WRITE(LU,501) NELEM,NPOIN
C
      IF(NPOIN.LT.3) THEN
        IF(LNG.EQ.1) WRITE(LU,23) NPOIN
        IF(LNG.EQ.2) WRITE(LU,24) NPOIN
        CALL PLANTE(1)
        STOP
      ENDIF
C
23    FORMAT(1X,'READGEO1 : NOMBRE DE POINTS DU MAILLAGE : ',1I6,/,1X,
     &          '           NOMBRE DE POINTS DE FRONTIERE: ',1I6,/,1X,
     &          '           DONNEES ERRONEES, ARRET DU PROGRAMME')
24    FORMAT(1X,'READGEO1 : NUMBER OF POINTS IN THE MESH: ',1I6,/,1X,
     &          '           NUMBER OF BOUNDARY POINTS: ',1I6,/,1X,
     &          '           WRONG DATA, PROGRAMME STOPPED')
300   FORMAT(1X,//,1X,'READGEO1 : TITRE= ',A72,/)
301   FORMAT(1X,//,1X,'READGEO1: TITLE= ',A72,/)
500   FORMAT(1X,'NOMBRE D''ELEMENTS:',1I6,/,
     &       1X,'NOMBRE REEL DE POINTS:',1I6)
501   FORMAT(1X,'NUMBER OF ELEMENTS:',1I6,/,
     &       1X,'NUMBER OF POINTS:',1I6)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE MAXIMUM NUMBER OF POINTS AND ELEMENTS
!>                NEIGHBOURING A POINT FOR A GIVEN TRIANGULAR MESH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  ALLOCATES ITRAV HERE, INTERNALLY.
!>         IT'S A LOCAL WORKING VARIABLE ANYWAY.
!>         COULD ALSO PASS THE ELEMENT TYPE IN ARGUMENT TO TREAT
!>         THE 3D SPECIFICALLY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM, IKLES, IPOBO, LISTIN, MXELVS, MXPTVS, NDP, NELEM, NPOIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IELEM, ITRAV, J
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> MXPTEL31()
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 24/08/95
!> </td><td> J-M HERVOUET (LNH) 30 71 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLES
!></td><td>--></td><td>TABLE DE CONNECTIVITE (DU FORMAT SELAFIN)
!>    </td></tr>
!>          <tr><td>IPOBO
!></td><td>--></td><td>TABLEEAU QUI VAUT 0 POUR LES POINTS INTERIEURS
!>                  ET NON NUL POUR LES POINTS DE BORD.
!>                  POUR SE PLACER SUR LES ENREGISTREMENTS DES
!>    </td></tr>
!>          <tr><td>ITRAV
!></td><td>--></td><td>TABLEAU DE TRAVAIL ENTIER DE DIMENSION NPOIN
!>    </td></tr>
!>          <tr><td>LISTIN
!></td><td>--></td><td>LOGIQUE : IMPRESSION DE MXELVS ET MXPTVS
!>    </td></tr>
!>          <tr><td>MXELVS
!></td><td><--</td><td>NOMBRE MAXIMUM D'ELEMENTS VOISINS.
!>    </td></tr>
!>          <tr><td>MXPTVS
!></td><td><--</td><td>NOMBRE MAXIMUM DE POINTS VOISINS.
!>    </td></tr>
!>          <tr><td>NDP
!></td><td>--></td><td>NOMBRE DE POINTS PAR ELEMENT.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MXPTEL
     &(MXPTVS,MXELVS,IKLES,IELM,NPOIN,NELEM,NDP,IPOBO,LISTIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |---| 
C| IKLES          |-->| TABLE DE CONNECTIVITE (DU FORMAT SELAFIN)
C| IPOBO          |-->| TABLEEAU QUI VAUT 0 POUR LES POINTS INTERIEURS
C|                |   | ET NON NUL POUR LES POINTS DE BORD.
C|                |   | POUR SE PLACER SUR LES ENREGISTREMENTS DES
C| ITRAV          |-->| TABLEAU DE TRAVAIL ENTIER DE DIMENSION NPOIN
C| LISTIN         |-->| LOGIQUE : IMPRESSION DE MXELVS ET MXPTVS
C| MXELVS         |<--| NOMBRE MAXIMUM D'ELEMENTS VOISINS.
C| MXPTVS         |<--| NOMBRE MAXIMUM DE POINTS VOISINS.
C| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(INOUT) :: MXPTVS,MXELVS
      INTEGER, INTENT(IN)    :: IELM,NDP,NPOIN,NELEM
      INTEGER, INTENT(IN)    :: IKLES(NDP,NELEM),IPOBO(NPOIN)
      LOGICAL, INTENT(IN)    :: LISTIN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C                       ITRAV(NPOIN): AUTOMATIC ARRAY
      INTEGER I,J,IELEM,ITRAV(NPOIN)
C
C-----------------------------------------------------------------------
C
C 1) INITIALISES THE NUMBER OF NEIGHBOURING ELEMENTS TO 0:
C
      DO 10 I = 1 , NPOIN
        ITRAV(I) = 0
10    CONTINUE
C
C 2) COUNTS THE NUMBER OF NEIGHBOURING ELEMENTS PER ASSEMBLY OPERATION:
C
      DO 22 J = 1, NDP
        DO 20 IELEM = 1 , NELEM
          ITRAV(IKLES(J,IELEM)) = ITRAV(IKLES(J,IELEM)) + 1
20      CONTINUE
22    CONTINUE
C
C 3) LOOKS FOR THE MAXIMUM :
C
      MXELVS = ITRAV(1)
      DO 30 I = 2 , NPOIN
        MXELVS = MAX(MXELVS,ITRAV(I))
30    CONTINUE
C
C 4) NUMBER OF NEIGHBOURING POINTS: NEED TO ADD 1 TO THIS NUMBER
C                                   FOR BOUNDARY NODES.
C    SIMULTANEOUSLY LOOKS FOR THE MAXIMUM
C
      IF (IELM.EQ.31) THEN
        CALL MXPTEL31(NELEM,NPOIN,MXELVS,IKLES,MXPTVS)
      ELSE
        MXPTVS = MXELVS
        DO 40 I = 1 , NPOIN
          IF(IPOBO(I).NE.0) MXPTVS = MAX(MXPTVS,ITRAV(I)+1)
40      CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(LISTIN) THEN
        IF(LNG.EQ.1) WRITE(LU,97) MXELVS,MXPTVS
        IF(LNG.EQ.2) WRITE(LU,98) MXELVS,MXPTVS
      ENDIF
97    FORMAT(1X,'MXPTEL (BIEF) : NOMBRE MAXIMUM D''ELEMENTS VOISINS D''
     &UN POINT : ',1I3,/,1X,
     &          '                NOMBRE MAXIMUM DE POINTS VOISINS D''UN
     &POINT : ',1I3)
98    FORMAT(1X,'MXPTEL (BIEF) : MAXIMUM NUMBER OF ELEMENTS AROUND A POI
     &NT: ',1I3,/,1X,
     &          '                MAXIMUM NUMBER OF POINTS AROUND A POINT
     &: ',1I3)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
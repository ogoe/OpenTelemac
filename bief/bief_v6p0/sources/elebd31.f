C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS NELBOR, NULONE, IKLBORD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM, IFABOR, IKLBOR, IKLE, NBOR, NELBOR, NELEB, NELEM, NELMAX, NPOIN, NPTFR, NULONE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEB, IELEM, IPOBO, IPOIN, J, K, SOMFAC
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ELEBD31
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 09/04/04
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> LAM MINH-PHUONG
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT.
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>TABLEAU DES VOISINS DES FACES.
!>    </td></tr>
!>          <tr><td>IKLBOR
!></td><td><--</td><td>NUMERO LOCAL DES NOEUDS A PARTIR D'UN ELEMENT
!>                  DE BORD
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMERO GLOBAL D'UN NOEUD A PARTIR DU NUMERO LOCAL
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td><--</td><td>NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT
!>    </td></tr>
!>          <tr><td>NELEB
!></td><td>--></td><td>NOMBRE D'ELEMENTS DE BORD.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE TOTAL DE POINTS DU DOMAINE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td><--</td><td>NUMERO LOCAL D'UN POINT DE BORD DANS
!>                  L'ELEMENT ADJACENT DONNE PAR NELBOR
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ELEBD31
     &(NELBOR,NULONE,IKLBOR,IFABOR,NBOR,IKLE,
     & NELEM,NELEB,NELMAX,NPOIN,NPTFR,IELM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |-->| TYPE D'ELEMENT.
C| IFABOR         |-->| TABLEAU DES VOISINS DES FACES.
C| IKLBOR         |<--| NUMERO LOCAL DES NOEUDS A PARTIR D'UN ELEMENT
C|                |   | DE BORD
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
C| NBOR           |-->| NUMERO GLOBAL D'UN NOEUD A PARTIR DU NUMERO LOCAL
C| NELBOR         |<--| NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT
C| NELEB          |-->| NOMBRE D'ELEMENTS DE BORD.
C| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |---| 
C| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| NULONE         |<--| NUMERO LOCAL D'UN POINT DE BORD DANS
C|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ELEBD31 => ELEBD31
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM,NELEB,NELMAX
      INTEGER, INTENT(IN)    :: NPOIN,NPTFR,IELM
      INTEGER, INTENT(IN)    :: NBOR(NPTFR)
      INTEGER, INTENT(IN)    :: IFABOR(NELMAX,4)
      INTEGER, INTENT(IN)    :: IKLE(NELEM,4)
      INTEGER, INTENT(OUT)   :: NELBOR(NELEB),NULONE(NELEB,3)
      INTEGER, INTENT(OUT)   :: IKLBOR(NELEB,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER   :: IELEM, IELEB, J,K,IPOIN
      INTEGER   :: IPOBO(NPOIN)
!
      INTEGER SOMFAC(3,4)
      DATA SOMFAC / 1,2,3 , 4,1,2 , 2,3,4 , 3,4,1  /
C     SIDE NUMBER:    1       2       3       4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!

      IF(IELM /= 31) THEN

        IF(LNG.EQ.1) WRITE(LU,98) IELM
        IF(LNG.EQ.2) WRITE(LU,99) IELM
98      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
99      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF


C BUILDS IPOBO TO GO FROM GLOBAL NUMBERING TO LOCAL NUMBERING

      DO IPOIN=1,NPOIN
        IPOBO(IPOIN) = 0
      ENDDO

      DO K = 1, NPTFR
         IPOBO(NBOR(K)) = K
      ENDDO


C BUILDS NELBOR, NULONE, IKLBORD
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IELEB = 0

      DO IELEM = 1,NELEM
         DO J = 1,4
            IF (IFABOR(IELEM,J)== 0) THEN
               IELEB           = IELEB + 1
               IF ( IELEB .GT. NELEB ) THEN
                 IF(LNG.EQ.1) WRITE(LU,101)
                 IF(LNG.EQ.2) WRITE(LU,102)
101              FORMAT(1X,'ELEBD31 : ERREUR DANS LE MAILLAGE')
102              FORMAT(1X,'ELEBD31 : ERROR IN MESH')
                 CALL PLANTE(1)
                 STOP
               END IF
               NELBOR(IELEB)   = IELEM
               NULONE(IELEB,1) = SOMFAC(1,J)
               NULONE(IELEB,2) = SOMFAC(2,J)
               NULONE(IELEB,3) = SOMFAC(3,J)
               IKLBOR(IELEB,1) = IPOBO(IKLE(NELBOR(IELEB),SOMFAC(1,J)))
               IKLBOR(IELEB,2) = IPOBO(IKLE(NELBOR(IELEB),SOMFAC(2,J)))
               IKLBOR(IELEB,3) = IPOBO(IKLE(NELBOR(IELEB),SOMFAC(3,J)))
            END IF
         END DO
      END DO

!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE ELEBD31
C
C#######################################################################
C
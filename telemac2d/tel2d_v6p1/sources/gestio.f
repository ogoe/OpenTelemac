C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MANAGES THE ALLOCATION OF ARRAYS
!>                DEPENDING ON THE SELECTED EQUATIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  DOES NOT WORK IF USES SCHEMES OTHER THAN
!>            CHARACTERISTICS FOR ADVECTION

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AK, AKTILD, C, CONVV, CTILD, EP, EPTILD, IETAPE, ITURB, PROPA, T, TRAC, TTILD, U, UTILD, V, VTILD
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!> </td><td> 17/08/1994
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AK
!></td><td><-></td><td>ENERGIE TURBULENTE A LA FIN DE L'ETAPE TRAITEE
!>    </td></tr>
!>          <tr><td>AKTILD
!></td><td>--></td><td>ENERGIE TURBULENTE AVANT L'ETAPE TRAITEE
!>    </td></tr>
!>          <tr><td>C
!></td><td><-></td><td>CELERITE A LA FIN DE L'ETAPE TRAITEE.
!>    </td></tr>
!>          <tr><td>CONVV
!></td><td>--></td><td>LOGIQUES INDIQUANT LES VARIABLES QU'ON NE
!>                  VEUT PAS CONVECTER.
!>    </td></tr>
!>          <tr><td>CTILD
!></td><td>--></td><td>CELERITE AVANT L'ETAPE TRAITEE.
!>    </td></tr>
!>          <tr><td>EP
!></td><td><-></td><td>DISSIPASSION A FIN DE L'ETAPE TRAITEE.
!>    </td></tr>
!>          <tr><td>EPTILD
!></td><td>--></td><td>DISSIPASSION AVANT L'ETAPE TRAITEE.
!>    </td></tr>
!>          <tr><td>IETAPE
!></td><td>--></td><td>INDICATEUR D'AVANCEMENT DANS LE PROGRAMME .
!>    </td></tr>
!>          <tr><td>ITURB
!></td><td>--></td><td>MODELE DE TURBULENCE  1 : LAMINAIRE
!>                  2 : LONGUEUR DE MELANGE
!>                  3 : K-EPSILON
!>    </td></tr>
!>          <tr><td>PROPA
!></td><td>--></td><td>SI PROPA=.FALSE. : PAS DE PROPAGATION.
!>    </td></tr>
!>          <tr><td>T
!></td><td><-></td><td>TRACEUR A LA FIN DE L'ETAPE TRAITEE.
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
!>    </td></tr>
!>          <tr><td>TTILD
!></td><td>--></td><td>TRACEUR AVANT L'ETAPE TRAITEE.
!>    </td></tr>
!>          <tr><td>U,V
!></td><td><-></td><td>VITESSE A LA FIN DE L'ETAPE TRAITEE.
!>    </td></tr>
!>          <tr><td>UTILD,VTILD
!></td><td>--></td><td>VITESSE AVANT L'ETAPE TRAITEE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GESTIO
     &(U,V,C,T,AK,EP,UTILD,VTILD,CTILD,TTILD,AKTILD,EPTILD,
     & TRAC,PROPA,CONVV,ITURB,IETAPE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK             |<->| ENERGIE TURBULENTE A LA FIN DE L'ETAPE TRAITEE
C| AKTILD         |-->| ENERGIE TURBULENTE AVANT L'ETAPE TRAITEE
C| C             |<->| CELERITE A LA FIN DE L'ETAPE TRAITEE.
C| CONVV          |-->| LOGIQUES INDIQUANT LES VARIABLES QU'ON NE
C|                |   | VEUT PAS CONVECTER.
C| CTILD          |-->| CELERITE AVANT L'ETAPE TRAITEE.
C| EP             |<->| DISSIPASSION A FIN DE L'ETAPE TRAITEE.
C| EPTILD         |-->| DISSIPASSION AVANT L'ETAPE TRAITEE.
C| IETAPE         |-->| INDICATEUR D'AVANCEMENT DANS LE PROGRAMME .
C| ITURB          |-->| MODELE DE TURBULENCE  1 : LAMINAIRE
C|                |   | 2 : LONGUEUR DE MELANGE
C|                |   | 3 : K-EPSILON
C| PROPA          |-->| SI PROPA=.FALSE. : PAS DE PROPAGATION.
C| T             |<->| TRACEUR A LA FIN DE L'ETAPE TRAITEE.
C| TRAC           |-->| LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
C| TTILD          |-->| TRACEUR AVANT L'ETAPE TRAITEE.
C| U,V            |<->| VITESSE A LA FIN DE L'ETAPE TRAITEE.
C| UTILD,VTILD    |-->| VITESSE AVANT L'ETAPE TRAITEE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: ITURB,IETAPE
      LOGICAL, INTENT(IN)           :: TRAC,CONVV(4),PROPA
      TYPE(BIEF_OBJ), INTENT(IN)    :: T,AK,EP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: U,V,C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UTILD,VTILD,CTILD,TTILD
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AKTILD,EPTILD
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C----------------------------------------------------------------------
C    ARRAYS DEPENDING ON THE SELECTED EQUATIONS
C-----------------------------------------------------------------------
C
C    ADVECTION
C
      IF(IETAPE.EQ.3) THEN
C
        IF(.NOT.CONVV(1)) THEN
          CALL OS( 'X=Y     ' , X=UTILD , Y=U )
          CALL OS( 'X=Y     ' , X=VTILD , Y=V )
        ENDIF
        IF(.NOT.CONVV(2)) THEN
          CALL OS( 'X=Y     ' , X=CTILD , Y=C )
        ENDIF
        IF(TRAC.AND.(.NOT.CONVV(3))) THEN
          CALL OS( 'X=Y     ' , X=TTILD , Y=T )
        ENDIF
        IF(ITURB.EQ.3.AND.(.NOT.CONVV(4))) THEN
          CALL OS( 'X=Y     ' , X=AKTILD , Y=AK )
          CALL OS( 'X=Y     ' , X=EPTILD , Y=EP )
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C    PROPAGATION
C
      IF(IETAPE.EQ.6) THEN
C
            IF(.NOT.PROPA) THEN
C
                   CALL OS( 'X=Y     ' , X=U , Y=UTILD )
                   CALL OS( 'X=Y     ' , X=V , Y=VTILD )
                   CALL OS( 'X=Y     ' , X=C , Y=CTILD )
C
            ENDIF
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
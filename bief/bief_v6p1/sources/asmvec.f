C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MULTIPLICATIVE ASSEMBLY FOR A VECTOR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THIS VECTOR IS INITIALISED TO 1 IF INIT = .TRUE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, INIT, LV, NDP, NELEM, NELMAX, NPOIN, W, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IDP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ASMVEC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ASMVE1(), OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DLDU11(), DLDU21(), DLDU41()

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
!> </td><td> 17/08/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CORRESPONDANCES NUMEROTATION LOCALE-GLOBALE
!>    </td></tr>
!>          <tr><td>INIT
!></td><td>--></td><td>LOGIQUE : SI VRAI : X EST INITIALISE A 0
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>NDP
!></td><td>--></td><td>DEUXIEME DIMENSION DE IKLE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>PREMIERE DIMENSION DE IKLE ET W.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>DIMENSION DU TABLEAU X
!>    </td></tr>
!>          <tr><td>W
!></td><td>--></td><td>TABLEAUX DE TRAVAIL CONTENANT LE VECTEUR SOUS
!>                  FORME NON ASSEMBLEE
!>                  W EST DE DIMENSION NELMAX * NDP(IELM)
!>                  NDP EST LE NOMBRE DE POINTS DE L'ELEMENT
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>VECTEUR ASSEMBLE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ASMVEC
     &(X, IKLE,NPOIN,NELEM,NELMAX,NDP,W,INIT,LV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| CORRESPONDANCES NUMEROTATION LOCALE-GLOBALE
C| INIT           |-->| LOGIQUE : SI VRAI : X EST INITIALISE A 0
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| NDP            |-->| DEUXIEME DIMENSION DE IKLE.
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPOIN          |-->| DIMENSION DU TABLEAU X
C| W             |-->| TABLEAUX DE TRAVAIL CONTENANT LE VECTEUR SOUS
C|                |   | FORME NON ASSEMBLEE
C|                |   | W EST DE DIMENSION NELMAX * NDP(IELM)
C|                |   | NDP EST LE NOMBRE DE POINTS DE L'ELEMENT
C| X             |<->| VECTEUR ASSEMBLE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ASMVEC => ASMVEC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: NELMAX,NPOIN,NELEM,NDP,LV
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX,NDP)
      LOGICAL         , INTENT(IN)    :: INIT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IDP
C
C-----------------------------------------------------------------------
C   INITIALISES VECTOR X TO 1 IF(INIT)
C-----------------------------------------------------------------------
C
      IF(INIT) CALL OV( 'X=C     ' , X , X , X , 1.D0 , NPOIN )
C
C-----------------------------------------------------------------------
C   ASSEMBLES, CONTRIBUTION OF LOCAL POINTS 1,... TO NDP
C-----------------------------------------------------------------------
C
      DO IDP = 1 , NDP
C
        CALL ASMVE1(X, IKLE(1,IDP),W(1,IDP),NPOIN,NELEM,NELMAX,LV)
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       VECTOR ASSEMBLY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THIS VECTOR IS ONLY INITIALISED TO 0 IF INIT = .TRUE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM, IKLE, INIT, LV, MASKEL, MSK, NELEM, NELMAX, NPOIN, W, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IDP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ASSVEC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ASSVE1(), NBPEL(), OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CFLPSI(), MATRIX(), MATVCT(), TRISOU(), VECTOS()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 29/02/08
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT (VOIR CI-DESSUS)
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CORRESPONDANCES NUMEROTATION LOCALE-GLOBALE
!>    </td></tr>
!>          <tr><td>INIT
!></td><td>--></td><td>LOGIQUE : SI VRAI : X EST INITIALISE A 0
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
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
                        SUBROUTINE ASSVEC
     &(X, IKLE,NPOIN,NELEM,NELMAX,IELM,W,INIT,LV,MSK,MASKEL,NDP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |-->| TYPE D'ELEMENT (VOIR CI-DESSUS)
C| IKLE           |-->| CORRESPONDANCES NUMEROTATION LOCALE-GLOBALE
C| INIT           |-->| LOGIQUE : SI VRAI : X EST INITIALISE A 0
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NDP            |-->| NUMBER OF POINTS PER ELEMENT
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPOIN          |-->| DIMENSION DU TABLEAU X
C| W              |-->| TABLEAUX DE TRAVAIL CONTENANT LE VECTEUR SOUS
C|                |   | FORME NON ASSEMBLEE
C|                |   | W EST DE DIMENSION NELMAX * NDP(IELM)
C|                |   | NDP EST LE NOMBRE DE POINTS DE L'ELEMENT
C| X              |<->| VECTEUR ASSEMBLE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ASSVEC => ASSVEC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,NPOIN,IELM,LV,NDP
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX,NDP),MASKEL(NELMAX)
      LOGICAL         , INTENT(IN)    :: INIT,MSK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IDP
C
C-----------------------------------------------------------------------
C   INITIALISES VECTOR X TO 0 IF(INIT)
C-----------------------------------------------------------------------
C
      IF(INIT) CALL OV( 'X=C     ' , X , X , X , 0.D0 , NPOIN )
C
C-----------------------------------------------------------------------
C   ASSEMBLES, CONTRIBUTION OF LOCAL POINTS 1,... TO NDP
C-----------------------------------------------------------------------
C
      DO IDP = 1 , NDP
C
        CALL ASSVE1(X,IKLE(1,IDP),W(1,IDP),NELEM,NELMAX,LV,MSK,MASKEL)
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

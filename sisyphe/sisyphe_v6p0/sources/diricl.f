C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DETERMINES THE BOUNDARY CONDITIONS ON E
!>                FOR DIRICHLET POINTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> EBOR, KENT, LIEBOR, NBOR, NPOIN, NPTFR, ZF, ZF1
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> K, N
!>   </td></tr>
!>     </table>

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
!> </td><td> **/10/97
!> </td><td> C.LE NORMANT 01-30-87-78-54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>EBOR
!></td><td><-></td><td>EVOLUTION AUX POINTS DE BORD
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>TYPE DE CONDITION LIMITE
!>    </td></tr>
!>          <tr><td>LIEBOR
!></td><td><-></td><td>TYPES DE CONDITIONS AUX LIMITES SUR E
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>TABLEAU DES NUMEROS GLOBAUX DES POINTS
!>                  DE BORD
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND
!>    </td></tr>
!>          <tr><td>ZF1
!></td><td><-></td><td>COTE DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIRICL
     &( ZF1 , ZF , EBOR , LIEBOR , NBOR , NPOIN  , NPTFR  , KENT )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| EBOR           |<->| EVOLUTION AUX POINTS DE BORD
C| KENT           |-->| TYPE DE CONDITION LIMITE
C| LIEBOR         |<->| TYPES DE CONDITIONS AUX LIMITES SUR E
C| NBOR           |-->| TABLEAU DES NUMEROS GLOBAUX DES POINTS
C|                |   | DE BORD
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
C| ZF             |-->| COTE DU FOND
C| ZF1            |<->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN):: KENT,NPOIN,NPTFR
      INTEGER, INTENT(IN):: NBOR(NPTFR)
      INTEGER, INTENT(IN):: LIEBOR(NPTFR)
C
      DOUBLE PRECISION, INTENT(IN)::  ZF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: ZF1(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: EBOR(NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K, N
C
C-----------------------------------------------------------------------
C
      DO 10 K=1,NPTFR
C
        N = NBOR(K)
C
        IF (LIEBOR(K).EQ.KENT) THEN
          ZF1(N)   = EBOR(K)+ZF(N)
        ENDIF
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE DIRICL

C
C#######################################################################
C
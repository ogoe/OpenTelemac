C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES A VECTOR, USED BY THE METHOD:
!>                STREAMLINE UPWIND PETROV GALERKIN (SUPG)
!>                WITH AN OFF-CENTERING OF 1.
!>  @code
!>                    DX   U
!>             KX = -----------
!>                   2 NORM(U)
!>
!>                    DY   V
!>             KY = -----------
!>                   2 NORM(U)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> KX, KY, MESH, U, V, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_KSUPG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> KSPG11(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVDFTR(), KEPSIL(), PROPAG()

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
!> </td><td> 08/12/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>KX,KY
!></td><td>--></td><td>COORDONNEES DU VECTEUR UNITAIRE.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE.
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE.
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE KSUPG
     &(KX,KY,XMUL,U,V,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| KX,KY          |-->| COORDONNEES DU VECTEUR UNITAIRE.
C| MESH           |-->| MAILLAGE.
C| U,V            |-->| COMPOSANTES DE LA VITESSE.
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_KSUPG => KSUPG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: KX,KY
      TYPE(BIEF_OBJ), INTENT(IN)    :: U,V
C
      DOUBLE PRECISION, INTENT(IN)  :: XMUL
C
C-----------------------------------------------------------------------
C
      IF(U%ELM.EQ.11.OR.U%ELM.EQ.12.OR.U%ELM.EQ.13) THEN
C
        CALL KSPG11(KX%R,KY%R,MESH%XEL%R,MESH%YEL%R,U%R,V%R,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX,XMUL)
C
C  ELEMENT NOT IMPLEMENTED: ERROR
C
      ELSE
        IF (LNG.EQ.1) WRITE(LU,100) U%ELM
        IF (LNG.EQ.2) WRITE(LU,101) U%ELM
100     FORMAT(1X,'KSUPG (BIEF) : U%ELM = ',1I6,' ELEMENT NON PREVU')
101     FORMAT(1X,'KSUPG (BIEF): U%ELM = ',1I6,' ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C
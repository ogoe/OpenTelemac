C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COURANT NUMBER AT EACH POINT OF THE MESH
!>                AND FOR EACH TIMESTEP.
!><br>            THE STABILITY CRITERION OF THE DISTRIBUTIVE SCHEME N
!>                IS HERE USED TO EVALUATE THE COURANT NUMBER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, NELEM, NELMAX, U, V, W1, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, K1, K2, K3, L12, L13, L21, L23, L31, L32, SUR6, U1, U2, U3, USUR2, V1, V2, V3, VSUR2, X2, X3, Y2, Y3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CFLPSI()

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
!> </td><td> JMH
!> </td><td> MODIFICATIONS
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> C MOULIN   (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS DES NOEUDS DE CHAQUE ELEMENT.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE D'ELEMENTS MAXIMUM DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF).
!>    </td></tr>
!>          <tr><td>U
!></td><td>--></td><td>VITESSE SUIVANT X.
!>    </td></tr>
!>          <tr><td>V
!></td><td>--></td><td>VITESSE SUIVANT Y.
!>    </td></tr>
!>          <tr><td>W1
!></td><td>--></td><td>RESULTAT PARTIEL
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>ABSCISSES DES POINTS DU MAILLAGE PAR ELEMENTS
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>ORDONNEES DES POINTS DU MAILLAGE PAR ELEMENTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CFLP11
     &(U,V,X,Y,IKLE,NELEM,NELMAX,W1)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| NUMEROS DES NOEUDS DE CHAQUE ELEMENT.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE D'ELEMENTS MAXIMUM DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF).
C| U             |-->| VITESSE SUIVANT X.
C| V             |-->| VITESSE SUIVANT Y.
C| W1             |-->| RESULTAT PARTIEL
C| X             |-->| ABSCISSES DES POINTS DU MAILLAGE PAR ELEMENTS
C| Y             |-->| ORDONNEES DES POINTS DU MAILLAGE PAR ELEMENTS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)  :: NELEM,NELMAX
      DOUBLE PRECISION, INTENT(IN)  :: U(*),V(*)
      DOUBLE PRECISION, INTENT(IN)  :: X(NELMAX,*),Y(NELMAX,*)
      INTEGER         , INTENT(IN)  :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(OUT) :: W1(NELMAX,*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
C
      DOUBLE PRECISION U1,U2,U3,V1,V2,V3,USUR2,VSUR2
      DOUBLE PRECISION SUR6,K1,K2,K3,L12,L13,L21,L23,L31,L32
      DOUBLE PRECISION X2,X3,Y2,Y3
C
      INTRINSIC MAX,MIN
C
C-----------------------------------------------------------------------
C
C
      SUR6 = 1.D0 / 6.D0
C
C LOOP ON THE ELEMENTS
C
        DO 10 IELEM = 1, NELEM
C
          X2 = X(IELEM,2)
          X3 = X(IELEM,3)
          Y2 = Y(IELEM,2)
          Y3 = Y(IELEM,3)
C
          U1 = U(IKLE(IELEM,1))
          U2 = U(IKLE(IELEM,2))
          U3 = U(IKLE(IELEM,3))
          V1 = V(IKLE(IELEM,1))
          V2 = V(IKLE(IELEM,2))
          V3 = V(IKLE(IELEM,3))
C
          USUR2 = (U1+U2+U3)*SUR6
          VSUR2 = (V1+V2+V3)*SUR6
C
          K1 = USUR2 * (Y2-Y3) - VSUR2 * (X2-X3)
          K2 = USUR2 * (Y3   ) - VSUR2 * (X3   )
          K3 = USUR2 * (  -Y2) - VSUR2 * (  -X2)
C
          L12 = MAX(  MIN(K1,-K2) , 0.D0 )
          L13 = MAX(  MIN(K1,-K3) , 0.D0 )
          L21 = MAX(  MIN(K2,-K1) , 0.D0 )
          L23 = MAX(  MIN(K2,-K3) , 0.D0 )
          L31 = MAX(  MIN(K3,-K1) , 0.D0 )
          L32 = MAX(  MIN(K3,-K2) , 0.D0 )
C
          W1(IELEM,1) = L12 + L13
          W1(IELEM,2) = L21 + L23
          W1(IELEM,3) = L31 + L32
C
10      CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C
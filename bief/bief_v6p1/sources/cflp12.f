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
!>    </th><td> IAD1, IAD2, IAD3, IELEM, IG1, IG2, IG3, IL, IT, K1, K2, K3, L12, L13, L21, L23, L31, L32, SUR6, TIERS, USUR2, VSUR2, X1, X2, X3, Y1, Y2, Y3
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
!>      <td><center> 5.6                                       </center>
!> </td><td> 29/12/05
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
!></td><td>---</td><td>
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
                        SUBROUTINE CFLP12
     &(U,V,X,Y,IKLE,NELEM,NELMAX,W1)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| NUMEROS DES NOEUDS DE CHAQUE ELEMENT.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE D'ELEMENTS MAXIMUM DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF).
C| U             |-->| VITESSE SUIVANT X.
C| V             |-->| VITESSE SUIVANT Y.
C| W1             |---| 
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
      DOUBLE PRECISION, INTENT(IN)  :: X(NELMAX*3),Y(NELMAX*3)
      INTEGER         , INTENT(IN)  :: IKLE(NELMAX*4)
      DOUBLE PRECISION, INTENT(OUT) :: W1(NELMAX*4)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IT,IAD1,IAD2,IAD3,IG1,IG2,IG3
C
      DOUBLE PRECISION USUR2,VSUR2
      DOUBLE PRECISION SUR6,K1,K2,K3,L12,L13,L21,L23,L31,L32
      DOUBLE PRECISION X1,X2,X3,Y1,Y2,Y3,TIERS
C
      INTRINSIC MAX,MIN
C
C-----------------------------------------------------------------------
C
C     FOR A QUASI-BUBBLE TRIANGLE : NUMBERS OF THE VERTICES OF THE
C     SUB-TRIANGLES IN THE INITIAL TRIANGLE
C     IL(NUMBER OF THE SUB-TRIANGLE,LOCAL NUMBER IN THE SUB-TRIANGLE)
C
      INTEGER IL(3,3)
      DATA IL /1,2,3,2,3,1,4,4,4/
C
C-----------------------------------------------------------------------
C
      TIERS= 1.D0 / 3.D0
      SUR6 = 1.D0 / 6.D0
C
C     INITIALISES W
C
      DO 32 IELEM = 1 , 4*NELMAX
        W1(IELEM) = 0.D0
32    CONTINUE
C
C     USING THE PSI SCHEME,
C     LOOP ON THE 3 SUB-TRIANGLES AND PRE-ASSEMBLY
C
      DO 10 IT=1,3
CDIR$ IVDEP
      DO 33 IELEM = 1 , NELEM
C
C       ADDRESSES IN AN ARRAY (NELMAX,*)
        IAD1= IELEM + (IL(IT,1)-1)*NELMAX
        IAD2= IELEM + (IL(IT,2)-1)*NELMAX
        IAD3= IELEM + (IL(IT,3)-1)*NELMAX
C       GLOBAL NUMBERS IN THE INITIAL TRIANGLE
        IG1 = IKLE(IAD1)
        IG2 = IKLE(IAD2)
        IG3 = IKLE(IAD3)
C       COORDINATES OF THE SUB-TRIANGLE VERTICES
        X1 = X(IAD1)
        X2 = X(IAD2) - X1
        Y1 = Y(IAD1)
        Y2 = Y(IAD2) - Y1
C       POINT 3 IS ALWAYS AT THE CENTRE OF THE INITIAL TRIANGLE
        X3=TIERS*(X(IELEM)+X(IELEM+NELMAX)+X(IELEM+2*NELMAX))-X1
        Y3=TIERS*(Y(IELEM)+Y(IELEM+NELMAX)+Y(IELEM+2*NELMAX))-Y1
C
        USUR2 = (U(IG1)+U(IG2)+U(IG3))*SUR6
        VSUR2 = (V(IG1)+V(IG2)+V(IG3))*SUR6
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
        W1(IAD1) = W1(IAD1) + L12 + L13
        W1(IAD2) = W1(IAD2) + L21 + L23
        W1(IAD3) = W1(IAD3) + L31 + L32
C
33    CONTINUE
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>  EXAMPLE WITH ICOORD = 1<br>
!>                  /           D
!> A(I,J)= XMUL *  /  PSI2(I) * --( PSI1(J) ) D(OMEGA)
!>                /OMEGA        DX<br>
!>  ICOORD=2 WOULD GIVE A DERIVATIVE WRT Y<br>
!>  PSI1: BASES OF TYPE P2 TRIANGLE
!>  PSI2: BASES OF TYPE P2 TRIANGLE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A14, A15, A16, A21, A22, A23, A24, A25, A26, A31, A32, A33, A34, A35, A36, A41, A42, A43, A44, A45, A46, A51, A52, A53, A54, A55, A56, A61, A62, A63, A64, A65, A66, ICOORD, NELEM, NELMAX, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, X2, X3, XSUR10, XSUR15, XSUR30, Y2, Y3
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MATRIY()

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
!> </td><td> 16/06/08
!> </td><td> ALGIANE FROEHLY (MATMECA) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A11,A12
!></td><td><--</td><td>ELEMENTS DE LA MATRICE
!>    </td></tr>
!>          <tr><td>A13
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A14
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A15
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A16
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A21
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A24
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A25
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A26
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A31
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A32
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A33
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A34
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A35
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A36
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A41
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A42
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A43
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A44
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A45
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A46
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A51
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A52
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A53
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A54
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A55
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A56
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A61
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A62
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A63
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A64
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A65
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A66
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>ICOORD
!></td><td>--></td><td>1: DERIVEE SUIVANT X, 2:SUIVANT Y
!>    </td></tr>
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DE F,G ET H.
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DE U,V ET W.
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>XEL,YEL,ZEL
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT13CC
     &(  A11 , A12 , A13 , A14 , A15 , A16 ,
     &   A21 , A22 , A23 , A24 , A25 , A26 ,
     &   A31 , A32 , A33 , A34 , A35 , A36 ,
     &   A41 , A42 , A43 , A44 , A45 , A46 ,
     &   A51 , A52 , A53 , A54 , A55 , A56 ,
     &   A61 , A62 , A63 , A64 , A65 , A66 ,
     &   XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A14            |---| 
C| A15            |---| 
C| A16            |---| 
C| A21            |---| 
C| A22            |---| 
C| A23            |---| 
C| A24            |---| 
C| A25            |---| 
C| A26            |---| 
C| A31            |---| 
C| A32            |---| 
C| A33            |---| 
C| A34            |---| 
C| A35            |---| 
C| A36            |---| 
C| A41            |---| 
C| A42            |---| 
C| A43            |---| 
C| A44            |---| 
C| A45            |---| 
C| A46            |---| 
C| A51            |---| 
C| A52            |---| 
C| A53            |---| 
C| A54            |---| 
C| A55            |---| 
C| A56            |---| 
C| A61            |---| 
C| A62            |---| 
C| A63            |---| 
C| A64            |---| 
C| A65            |---| 
C| A66            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| ICOORD         |-->| 1: DERIVEE SUIVANT X, 2:SUIVANT Y
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| XEL,YEL,ZEL    |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF!, EX_MT13CC => MT13CC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
C
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A24(*),A25(*),A26(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A34(*),A35(*),A36(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A51(*),A52(*),A53(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A54(*),A55(*),A56(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A61(*),A62(*),A63(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A64(*),A65(*),A66(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
C
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION XSUR10,XSUR15,XSUR30
C
C-----------------------------------------------------------------------
C
      XSUR10 = XMUL/10.D0
      XSUR15 = XMUL/15.D0
      XSUR30 = XMUL/30.D0
C
C================================
C  DERIVATIVE WRT X  =
C================================
C
        IF(ICOORD.EQ.1) THEN
C
C   LOOP ON THE ELEMENTS
C
        DO 1 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
C   DIAGONAL TERMS
C
        A22(IELEM) =   Y3 * XSUR15
        A33(IELEM) = - Y2 * XSUR15
        A44(IELEM) = - 4.D0 *   A33(IELEM)
        A55(IELEM) =   4.D0 * ( A22(IELEM) + A33(IELEM) )
        A66(IELEM) = - 4.D0 *   A22(IELEM)
        A11(IELEM) = - A22(IELEM) - A33(IELEM)
C
C   EXTRADIAGONAL TERMS
C
        A12(IELEM) = - Y3 * XSUR30
        A13(IELEM) =   Y2 * XSUR30
        A42(IELEM) =   Y3 * XSUR10
        A53(IELEM) = - Y2 * XSUR10
        A14(IELEM) = - A13(IELEM) + A42(IELEM)
        A15(IELEM) =   A12(IELEM) + A13(IELEM)
        A16(IELEM) = - A12(IELEM) + A53(IELEM)
        A21(IELEM) = - A15(IELEM)
        A23(IELEM) =   A13(IELEM)
        A24(IELEM) = - A42(IELEM) - A33(IELEM)
        A25(IELEM) =   A12(IELEM) + A33(IELEM)
        A26(IELEM) = - A12(IELEM)
        A31(IELEM) = - A15(IELEM)
        A32(IELEM) =   A12(IELEM)
        A34(IELEM) = - A13(IELEM)
        A35(IELEM) =   A22(IELEM) + A13(IELEM)
        A36(IELEM) = - A53(IELEM) - A22(IELEM)
        A41(IELEM) = - A42(IELEM) - A53(IELEM)
        A43(IELEM) =   A13(IELEM)
        A45(IELEM) = 2.D0 *   A22(IELEM) - A44(IELEM)
        A46(IELEM) = 2.D0 * ( A33(IELEM) - A22(IELEM) )
        A51(IELEM) =   A21(IELEM)
        A52(IELEM) =   A42(IELEM)
        A54(IELEM) = - A45(IELEM)
        A56(IELEM) =   A66(IELEM) - 2.D0 * A33(IELEM)
        A61(IELEM) =   A41(IELEM)
        A62(IELEM) =   A12(IELEM)
        A63(IELEM) =   A53(IELEM)
        A64(IELEM) = - A46(IELEM)
        A65(IELEM) = - A56(IELEM)
C
1     CONTINUE
C
        ELSEIF(ICOORD.EQ.2) THEN
C
C================================
C  DERIVATIVE WRT Y  =
C================================
C
        DO 2 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
C
C   DIAGONAL TERMS
C
        A22(IELEM) = - X3 * XSUR15
        A33(IELEM) =   X2 * XSUR15
        A44(IELEM) = - 4.D0 *   A33(IELEM)
        A55(IELEM) =   4.D0 * ( A22(IELEM) + A33(IELEM) )
        A66(IELEM) = - 4.D0 *   A22(IELEM)
        A11(IELEM) = - A22(IELEM) - A33(IELEM)
C
C   EXTRADIAGONAL TERMS
C
        A12(IELEM) =   X3 * XSUR30
        A13(IELEM) = - X2 * XSUR30
        A42(IELEM) = - X3 * XSUR10
        A53(IELEM) =   X2 * XSUR10
        A14(IELEM) = - A13(IELEM) + A42(IELEM)
        A15(IELEM) =   A12(IELEM) + A13(IELEM)
        A16(IELEM) = - A12(IELEM) + A53(IELEM)
        A21(IELEM) = - A15(IELEM)
        A23(IELEM) =   A13(IELEM)
        A24(IELEM) = - A42(IELEM) - A33(IELEM)
        A25(IELEM) =   A12(IELEM) + A33(IELEM)
        A26(IELEM) = - A12(IELEM)
        A31(IELEM) = - A15(IELEM)
        A32(IELEM) =   A12(IELEM)
        A34(IELEM) = - A13(IELEM)
        A35(IELEM) =   A22(IELEM) + A13(IELEM)
        A36(IELEM) = - A53(IELEM) - A22(IELEM)
        A41(IELEM) = - A42(IELEM) - A53(IELEM)
        A43(IELEM) =   A13(IELEM)
        A45(IELEM) = 2.D0 *   A22(IELEM) - A44(IELEM)
        A46(IELEM) = 2.D0 * ( A33(IELEM) - A22(IELEM) )
        A51(IELEM) =   A21(IELEM)
        A52(IELEM) =   A42(IELEM)
        A54(IELEM) = - A45(IELEM)
        A56(IELEM) =   A66(IELEM) - 2.D0 * A33(IELEM)
        A61(IELEM) =   A41(IELEM)
        A62(IELEM) =   A12(IELEM)
        A63(IELEM) =   A53(IELEM)
        A64(IELEM) = - A46(IELEM)
        A65(IELEM) = - A56(IELEM)
C
C
2       CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(1)
C
        ENDIF
C
200       FORMAT(1X,'MT13CC (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'MT13CC (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
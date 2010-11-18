C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>  EXAMPLE WITH ICOORD = 1<br>
!>                  /           D
!> A(I,J)= XMUL *  /  PSI1(I) * --( PSI2(J) ) D(OMEGA)
!>                /OMEGA        DX<br>
!>  ICOORD=2 WOULD GIVE A DERIVATIVE WRT Y<br>
!>  PSI1: BASES OF TYPE QUASI-BUBBLE TRIANGLE
!>  PSI2: BASES OF TYPE IELM2
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THE EXPRESSION OF THIS MATRIX IS THE TRANSPOSE
!>         OF THAT IN MT13BB.

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A21, A22, A23, A31, A32, A33, A41, A42, A43, ICOORD, NELEM, NELMAX, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, X2, X3, XSUR6, XSUR9, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT13BA
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 06/02/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; C MOULIN (LNH) 30 87 83 81
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
!>          <tr><td>A21
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
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
!>          <tr><td>A41
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A42
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A43
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
                        SUBROUTINE MT13BA
     &(  A11 , A12 , A13 ,
     &   A21 , A22 , A23 ,
     &   A31 , A32 , A33 ,
     &   A41 , A42 , A43 ,
     &   XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A21            |---| 
C| A22            |---| 
C| A23            |---| 
C| A31            |---| 
C| A32            |---| 
C| A33            |---| 
C| A41            |---| 
C| A42            |---| 
C| A43            |---| 
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
      USE BIEF, EX_MT13BA => MT13BA
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
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
C
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION XSUR9,XSUR6
C
C-----------------------------------------------------------------------
C
      XSUR6 = XMUL/6.D0
      XSUR9 = XMUL/9.D0
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
C   EXTRADIAGONAL TERMS
C
        A12(IELEM) =  Y3*XSUR9
        A13(IELEM) = -Y2*XSUR9
        A21(IELEM) = -(Y3-Y2)*XSUR9
        A23(IELEM) = -Y2*XSUR9
        A31(IELEM) = -(Y3-Y2)*XSUR9
        A32(IELEM) =  Y3*XSUR9
        A41(IELEM) = -(Y3-Y2)*XSUR6
        A42(IELEM) =  Y3*XSUR6
        A43(IELEM) = -Y2*XSUR6
C
C   DIAGONAL TERMS
C   THE SUM OF THE MATRIX COLUMNS IS 0 (VECTOR)
C
        A11(IELEM) = - A12(IELEM) - A13(IELEM)
        A22(IELEM) = - A21(IELEM) - A23(IELEM)
        A33(IELEM) = - A31(IELEM) - A32(IELEM)
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
C   EXTRADIAGONAL TERMS
C
        A12(IELEM) = -X3*XSUR9
        A13(IELEM) =  X2*XSUR9
        A21(IELEM) = -(X2-X3)*XSUR9
        A23(IELEM) =  X2*XSUR9
        A31(IELEM) = -(X2-X3)*XSUR9
        A32(IELEM) = -X3*XSUR9
        A41(IELEM) = -(X2-X3)*XSUR6
        A42(IELEM) = -X3*XSUR6
        A43(IELEM) =  X2*XSUR6
C
C   DIAGONAL TERMS
C   THE SUM OF THE MATRIX COLUMNS IS 0 (VECTOR)
C
        A11(IELEM) = - A12(IELEM) - A13(IELEM)
        A22(IELEM) = - A21(IELEM) - A23(IELEM)
        A33(IELEM) = - A31(IELEM) - A32(IELEM)
C
2       CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(0)
C
        ENDIF
C
200       FORMAT(1X,'MT13BA (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'MT13BA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
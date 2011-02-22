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

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>  MEANING OF IELM :
!>
!>  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!>
!>  11 : P1 TRIANGLE            3                       YES
!>  12 : P2 TRIANGLE            6
!>  21 : Q1 QUADRILATERAL       4                       YES
!>  41 : TELEMAC-3D PRISMS      6
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, KX, KY, NELEM, NELMAX, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, A2, A3, C1, C2, C3, GP1X, GP1Y, GP2X, GP2Y, GP3X, GP3Y, H, H1, H2, H3, I1, I2, I3, IELEM, SUNORM, SURFAC, UMOY, UNORM, VMOY, VNORM, X2, X3, Y2, Y3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>KSUPG()

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
!>          <tr><td>IKLE
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>KX,KY
!></td><td>--></td><td>COORDONNEES DU VECTEUR UNITAIRE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE.
!>    </td></tr>
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFICIENT MULTIPLICATEUR
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE KSPG11
     &(KX,KY,XEL,YEL,U,V,IKLE,NELEM,NELMAX,XMUL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| KX,KY          |-->| COORDONNEES DU VECTEUR UNITAIRE.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| U,V,W          |-->| COMPOSANTES DE LA VITESSE.
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFICIENT MULTIPLICATEUR
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: KX(NELEM),KY(NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: U(*),V(*),XMUL
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,I1,I2,I3
C
      DOUBLE PRECISION UMOY,VMOY,H,SUNORM,X2,X3,Y2,Y3
      DOUBLE PRECISION SURFAC,GP1X,GP1Y,GP2X,GP2Y,GP3X,GP3Y
      DOUBLE PRECISION A1,A2,A3,H1,H2,H3,C1,C2,C3,UNORM,VNORM
C
      INTRINSIC MAX,SQRT
C
C-----------------------------------------------------------------------
C
      DO 10 IELEM = 1 , NELEM
C
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
C
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
        GP1X = Y2-Y3
        GP1Y = X3-X2
        SUNORM = 1.D0 / SQRT(GP1X**2+GP1Y**2)
        GP1X = GP1X * SUNORM
        GP1Y = GP1Y * SUNORM
C
        GP2X = Y3
        GP2Y =   -X3
        SUNORM = 1.D0 / SQRT(GP2X**2+GP2Y**2)
        GP2X = GP2X * SUNORM
        GP2Y = GP2Y * SUNORM
C
        GP3X =   -Y2
        GP3Y = X2
        SUNORM = 1.D0 / SQRT(GP3X**2+GP3Y**2)
        GP3X = GP3X * SUNORM
        GP3Y = GP3Y * SUNORM
C
        C3 = SQRT(  X2**2     +  Y2**2 )
        C1 = SQRT( (X3-X2)**2 + (Y3-Y2)**2 )
        C2 = SQRT(  X3**2     +  Y3**2 )
C
        SURFAC = 0.5D0 * (X2*Y3 - X3*Y2)
C
        H1 = 2*SURFAC/C1
        H2 = 2*SURFAC/C2
        H3 = 2*SURFAC/C3
C
        H = MAX(H1,H2,H3)
C
        UMOY = U(I1) + U(I2) + U(I3)
        VMOY = V(I1) + V(I2) + V(I3)
C
        SUNORM = 1.D0 / MAX ( SQRT(UMOY**2+VMOY**2) , 1.D-10 )
C
        UNORM = UMOY * SUNORM
        VNORM = VMOY * SUNORM
C
        A1 = GP1X * UNORM + GP1Y * VNORM
        A2 = GP2X * UNORM + GP2Y * VNORM
        A3 = GP3X * UNORM + GP3Y * VNORM
C
        IF(A1*H.GT.H1) H = H1
        IF(A2*H.GT.H2) H = H2
        IF(A3*H.GT.H3) H = H3
C
        KX(IELEM) = 0.33333333D0 * XMUL * H * UNORM
        KY(IELEM) = 0.33333333D0 * XMUL * H * VNORM
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C
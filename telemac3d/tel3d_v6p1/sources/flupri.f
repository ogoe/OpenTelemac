C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FLUX THROUGH THE BOTTOM AND THE FREE SURFACE IN 3D:
!>  @code
!>                          /  ->  ->
!>      VEC = VEC  +  XMUL /   U . N  D(GAMMA)
!>                        /
!>                       /GAMMA<br>
!>      HERE GAMMA IS THE BOTTOM AND THE SURFACE<br>
!>      THE RESULT IS ADDED TO VECTOR VEC !!!!!!
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
!>    </th><td> IKLE, NELEM, NELEM2D, NELMAX, NPOIN2, NPOIN3, T1, T2, T3, U, V, VEC, W, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F123, F2, F3, I1, I2, I3, IELEM, NX, NY, NZ, X2, X3, XSUR24, Y2, Y3, Z2, Z3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PREDIV()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 14/03/06
!> </td><td> J.-M. HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F
!></td><td>--></td><td>FONCTION F, ICI LE FOND OU LA SURFACE
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELEM2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SF
!></td><td>--></td><td>STRUCTURE DE LA FONCTION F
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DES FONCTIONS U,V ET W
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>T1,2,3
!></td><td>--></td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR
!>                  INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>VEC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUPRI
     &( VEC,XMUL,U,V,W,X,Y,Z,IKLE,
     &  NELEM,NELMAX,NELEM2D,NPOIN2,NPOIN3,T1,T2,T3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F             |-->| FONCTION F, ICI LE FOND OU LA SURFACE
C| IKLE           |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELEM2D        |---| 
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPOIN2         |---| 
C| NPOIN3         |---| 
C| SF             |-->| STRUCTURE DE LA FONCTION F
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| T1,2,3         |-->| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| T2             |---| 
C| T3             |---| 
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| VEC            |---| 
C| X,Y            |-->| COORDONNEES DES POINTS
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C| Z             |---| 
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
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NELEM2D,NPOIN2,NPOIN3
      DOUBLE PRECISION, INTENT(INOUT) :: VEC(NPOIN3)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
C
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3),Y(NPOIN3),Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(INOUT) :: T1(NELEM2D)
      DOUBLE PRECISION, INTENT(INOUT) :: T2(NELEM2D)
      DOUBLE PRECISION, INTENT(INOUT) :: T3(NELEM2D)
C
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION X2,X3,Y2,Y3,NX,NY,NZ,Z2,Z3,F1,F2,F3,XSUR24,F123
C
      INTEGER I1,I2,I3,IELEM
C
C**********************************************************************
C
      XSUR24 = XMUL/24.D0
C
C     LOOP ON THE 2D ELEMENTS
C
      DO IELEM = 1,NELEM2D
C
C       BOTTOM
C
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
C
        X2 = X(I2) - X(I1)
        X3 = X(I3) - X(I1)
        Y2 = Y(I2) - Y(I1)
        Y3 = Y(I3) - Y(I1)
        Z2 = Z(I2) - Z(I1)
        Z3 = Z(I3) - Z(I1)
C       OUTGOING NORMAL (NOT NORMALISED)
        NX =  (Z2*Y3-Z3*Y2)
        NY =  (X2*Z3-X3*Z2)
        NZ = -(X2*Y3-X3*Y2)
C
        F1 = U(I1)*NX+V(I1)*NY+W(I1)*NZ
        F2 = U(I2)*NX+V(I2)*NY+W(I2)*NZ
        F3 = U(I3)*NX+V(I3)*NY+W(I3)*NZ
C
        F123 = F1 + F2 + F3
C
        T1(IELEM) = XSUR24 * ( F123 + F1 )
        T2(IELEM) = XSUR24 * ( F123 + F2 )
        T3(IELEM) = XSUR24 * ( F123 + F3 )
C
C       ASSEMBLY
C
        VEC(I1) = VEC(I1) + T1(IELEM)
        VEC(I2) = VEC(I2) + T2(IELEM)
        VEC(I3) = VEC(I3) + T3(IELEM)
C
C       FREE SURFACE (IDEM EXCEPT FOR POINTS NUMBERS AND REVERSED NORMAL)
C
        I1 = I1 + NPOIN3-NPOIN2
        I2 = I2 + NPOIN3-NPOIN2
        I3 = I3 + NPOIN3-NPOIN2
C
C       X2 = X(I2) - X(I1)
C       X3 = X(I3) - X(I1)
C       Y2 = Y(I2) - Y(I1)
C       Y3 = Y(I3) - Y(I1)
        Z2 = Z(I2) - Z(I1)
        Z3 = Z(I3) - Z(I1)
C       OUTGOING NORMAL (NOT NORMALISED)
        NX = -(Z2*Y3-Z3*Y2)
        NY = -(X2*Z3-X3*Z2)
        NZ = +(X2*Y3-X3*Y2)
C
        F1 = U(I1)*NX+V(I1)*NY+W(I1)*NZ
        F2 = U(I2)*NX+V(I2)*NY+W(I2)*NZ
        F3 = U(I3)*NX+V(I3)*NY+W(I3)*NZ
C
        F123 = F1 + F2 + F3
C
        T1(IELEM) = XSUR24 * ( F123 + F1 )
        T2(IELEM) = XSUR24 * ( F123 + F2 )
        T3(IELEM) = XSUR24 * ( F123 + F3 )
C
C       ASSEMBLY
C
        VEC(I1) = VEC(I1) + T1(IELEM)
        VEC(I2) = VEC(I2) + T2(IELEM)
        VEC(I3) = VEC(I3) + T3(IELEM)
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
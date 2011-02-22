C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE FOLLOWING MATRIX FOR P2 TRIANGLES:
!>  @code
!>    ->--->
!>    U.GRAD
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A14, A15, A16, A21, A22, A23, A24, A25, A26, A31, A32, A33, A34, A35, A36, A41, A42, A43, A44, A45, A46, A51, A52, A53, A54, A55, A56, A61, A62, A63, A64, A65, A66, FORMUL, IKLE1, IKLE2, IKLE3, IKLE4, IKLE5, IKLE6, NELEM, NELMAX, SU, SV, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, IELMU, IELMV, U1, U2, U3, U4, U5, U6, V1, V2, V3, V4, V5, V6, X2, X3, XSU360, XSUR2520, XSUR45, XSUR630, XSUR90, Y2, Y3
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
!> </td><td> 09/07/2008
!> </td><td> ALGIANE FROEHLY; C MOULIN (LNH) 30 87 83 81
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
!>          <tr><td>FORMUL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE6
!></td><td>---</td><td>
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
                        SUBROUTINE MT05CC
     &( A11 , A12 , A13 , A14 , A15 , A16 ,
     &  A21 , A22 , A23 , A24 , A25 , A26 ,
     &  A31 , A32 , A33 , A34 , A35 , A36 ,
     &  A41 , A42 , A43 , A44 , A45 , A46 ,
     &  A51 , A52 , A53 , A54 , A55 , A56 ,
     &  A61 , A62 , A63 , A64 , A65 , A66 ,
     &  XMUL,SU,SV,U,V,
     &  XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     &  NELEM,NELMAX,FORMUL)
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
C| FORMUL         |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| IKLE5          |---| 
C| IKLE6          |---| 
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
      USE BIEF!, EX_MT05CC => MT05CC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
      INTEGER, INTENT(IN) :: IKLE5(NELMAX),IKLE6(NELMAX)
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
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
C
C
C     STRUCTURES OF      U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV
C
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
C
      CHARACTER(LEN=16) :: FORMUL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELEM,IELMU,IELMV
C
      DOUBLE PRECISION X2,X3
      DOUBLE PRECISION Y2,Y3
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6
      DOUBLE PRECISION V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION XSU360,XSUR90,XSUR45
      DOUBLE PRECISION XSUR2520,XSUR630

C=======================================================================
C
C     EXTRACTS THE TYPE OF ELEMENT FOR VELOCITY
C
      IELMU = SU%ELM
      IELMV = SV%ELM
C
C-----------------------------------------------------------------------
C
      XSU360   = XMUL / 360.D0
      XSUR45   = XMUL /  45.D0
      XSUR90   = XMUL /  90.D0
      XSUR2520 = XMUL /  2520.D0
      XSUR630  = XMUL /  630.D0
C
C-----------------------------------------------------------------------
C
C     CASE WHERE U AND V ARE CONSTANT BY ELEMENT
C
      IF(IELMU.EQ.10.AND.IELMV.EQ.10) THEN
C
C-----------------------------------------------------------------------
C
C  P1 DISCRETISATION OF THE VELOCITY:
C
      DO 1 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
C
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
C
         U1 = U(IELEM)
         U2 = U(IELEM)
         U3 = U(IELEM)
         V1 = V(IELEM)
         V2 = V(IELEM)
         V3 = V(IELEM)
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM)=(( 5.D0*V2+V3+V1*6.D0)*X3 +
     &              ( -U1*6.D0-U3-U2*5.D0)*Y3) * XSU360
         A13(IELEM)=((-V2-V3*5.D0-V1*6.D0)*X2 +
     &              (  U1*6.D0+U3*5.D0+U2)*Y2) * XSU360
         A14(IELEM)=(( V2*2.D0+V3)*X2+(-V2*2.D0-V3-V1*6.D0)*X3 +
     &              ( -U2*2.D0-U3)*Y2+(U1*6.D0+U3+U2*2.D0)*Y3) * XSUR90
         A15(IELEM)=((-V3-V2*2.D0)*X2+(V2+V3*2.D0)*X3 +
     &              (U2*2.D0+U3)*Y2+(-U2-U3*2.D0)*Y3) * XSUR90
         A16(IELEM)=((V2+V3*2.D0+V1*6.D0)*X2+(-V2-V3*2.D0)*X3 +
     &              (-U2-U3*2.D0-U1*6.D0)*Y2+(U2+U3*2.D0)*Y3) * XSUR90
C
         A21(IELEM)=((V2*6.D0+V3+V1*5.D0)*X2 +
     &              (-V2*6.D0-V3-V1*5.D0)*X3 +
     &              (-U2*6.D0-U3-U1*5.D0)*Y2 +
     &              (U1*5.D0+U3+U2*6.D0)*Y3) * XSU360
         A23(IELEM)=((-V2*6.D0-V3*5.D0-V1)*X2 +
     &              (  U1+U3*5.D0+U2*6.D0)*Y2) * XSU360
         A24(IELEM)=(-V2*X2*6.D0+(V2*6.D0+V3+V1*2.D0)*X3+U2*Y2*6.D0 +
     &              (-U2*6.D0-U3-U1*2.D0)*Y3) * XSUR90
         A25(IELEM)=(V2*X2*6.D0+(V3*2.D0+V1)*X3-U2*Y2*6.D0 +
     &              (-U3*2.D0-U1)*Y3) * XSUR90
         A26(IELEM)=((V3-V1)*X2+(-V3*2.D0-V1)*X3+(-U3+U1)*Y2 +
     &              (U1+U3*2.D0)*Y3) * XSUR90
C
         A31(IELEM)=((V2+V3*6.D0+V1*5.D0)*X2 +
     &              (-V2-V3*6.D0-V1*5.D0)*X3 +
     &              (-U2-U3*6.D0-U1*5.D0)*Y2 +
     &              (U1*5.D0+U3*6.D0+U2)*Y3) * XSU360
         A32(IELEM)=((V2*5.D0+V3*6.D0+V1)*X3 +
     &              (-U1-U3*6.D0-U2*5.D0)*Y3) * XSU360
         A34(IELEM)=((V2*2.D0+V1)*X2+(-V2+V1)*X3+(-U1-U2*2.D0)*Y2+
     &              (U2-U1)*Y3)*XSUR90
         A35(IELEM)=((-V2*2.D0-V1)*X2-V3*X3*6.D0 +
     &               ( U2*2.D0+U1)*Y2+U3*Y3*6.D0) * XSUR90
         A36(IELEM)=((-V3*6.D0-V2-V1*2.D0)*X2+V3*X3*6.D0 +
     &               ( U2+U3*6.D0+U1*2.D0)*Y2-U3*Y3*6.D0) * XSUR90
C
         A41(IELEM)=((-V2*2.D0-V3-V1*6.D0)*X2 +
     &               ( V2*2.D0+V3+V1*6.D0)*X3 +
     &               ( U1*6.D0+U3+U2*2.D0)*Y2 +
     &               (-U1*6.D0-U3-U2*2.D0)*Y3) * XSUR90
         A42(IELEM)=((-V2*6.D0-V3-V1*2.D0)*X3 +
     &               ( U1*2.D0+U3+U2*6.D0)*Y3) * XSUR90
         A43(IELEM)=((-V2*2.D0+V3-V1*2.D0)*X2 +
     &               ( U1*2.D0-U3+U2*2.D0)*Y2) * XSUR90
         A45(IELEM)=(( 6.D0*V2+2.D0*V3+4.D0*V1)*X2 +
     &               (-2.D0*V2-2.D0*V3-2.D0*V1)*X3 +
     &               (-6.D0*U2-2.D0*U3-4.D0*U1)*Y2 +
     &               (2.D0*U1+2.D0*U3+2.D0*U2)*Y3) * XSUR45
         A46(IELEM)=((2.D0*V2+4.D0*V1)*X2 +
     &               (2.D0*V3+2.D0*V2+2.D0*V1)*X3 +
     &               (-4.D0*U1-2.D0*U2)*Y2 +
     &               (-2.D0*U2-2.D0*U3-2.D0*U1)*Y3) * XSUR45
C
         A51(IELEM)=((V2*2.D0+V3*2.D0-V1)*X2 +
     &              (-V2*2.D0-V3*2.D0+V1)*X3 +
     &              (-U2*2.D0-U3*2.D0+U1)*Y2 +
     &              (-U1+U3*2.D0+U2*2.D0)*Y3) * XSUR90
         A52(IELEM)=((-V2*6.D0-V3*2.D0-V1)*X3 +
     &              (U1+U3*2.D0+U2*6.D0)*Y3) * XSUR90
         A53(IELEM)=((V2*2.D0+V3*6.D0+V1)*X2 +
     &              (-U1-U3*6.D0-U2*2.D0)*Y2) * XSUR90
         A54(IELEM)=((-6.D0*V2-4.D0*V3-2.D0*V1)*X2 +
     &              (4.D0*V2+2.D0*V3)*X3 +
     &              (6.D0*U2+4.D0*U3+2.D0*U1)*Y2 +
     &              (-4.D0*U2-2.D0*U3)*Y3) * XSUR45
         A56(IELEM)=((-2.D0*V2-4.D0*V3)*X2 +
     &              (4.D0*V2+6.D0*V3+2.D0*V1)*X3 +
     &              (2.D0*U2+4.D0*U3)*Y2 +
     &              (-2.D0*U1-6.D0*U3-4.D0*U2)*Y3) * XSUR45
C
         A61(IELEM)=((-V2-V3*2.D0-V1*6.D0)*X2 +
     &              (V2+V3*2.D0+V1*6.D0)*X3 +
     &              (U2+U3*2.D0+U1*6.D0)*Y2 +
     &              (-U2-U3*2.D0-U1*6.D0)*Y3) * XSUR90
         A62(IELEM)=((-V2+V3*2.D0+V1*2.D0)*X3 +
     &              (-U1*2.D0-U3*2.D0+U2)*Y3) * XSUR90
         A63(IELEM)=((V2+V3*6.D0+V1*2.D0)*X2 +
     &              (-U1*2.D0-U3*6.D0-U2)*Y2) * XSUR90
         A64(IELEM)=((-2.D0*V2-2.D0*V3-2.D0*V1)*X2 +
     &              (-4.D0*V1-2.D0*V3)*X3 +
     &              (2.D0*U1+2.D0*U3+2.D0*U2)*Y2 +
     &              (4.D0*U1+2.D0*U3)*Y3) * XSUR45
         A65(IELEM)=((2.D0*V3+2.D0*V2+2.D0*V1)*X2 +
     &              (-2.D0*V2-6.D0*V3-4.D0*V1)*X3 +
     &              (-2.D0*U2-2.D0*U3-2.D0*U1)*Y2 +
     &              (4.D0*U1+6.D0*U3+2.D0*U2)*Y3) * XSUR45
C
C  DIAGONAL TERMS:
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
     &                - A15(IELEM) - A16(IELEM)
         A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
     &                - A25(IELEM) - A26(IELEM)
         A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
     &                - A35(IELEM) - A36(IELEM)
         A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
     &                - A45(IELEM) - A46(IELEM)
         A55(IELEM) = - A51(IELEM) - A52(IELEM) - A53(IELEM)
     &                - A54(IELEM) - A56(IELEM)
         A66(IELEM) = - A61(IELEM) - A62(IELEM) - A63(IELEM)
     &                - A64(IELEM) - A65(IELEM)
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
C     CASE WHERE U AND V ARE LINEAR
C
      ELSEIF(IELMU.EQ.11.AND.IELMV.EQ.11) THEN
C
C-----------------------------------------------------------------------
C
C  P1 DISCRETISATION OF THE VELOCITY:
C
      DO 2 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
C
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
C
         U1 = U(IKLE1(IELEM))
         U2 = U(IKLE2(IELEM))
         U3 = U(IKLE3(IELEM))
         V1 = V(IKLE1(IELEM))
         V2 = V(IKLE2(IELEM))
         V3 = V(IKLE3(IELEM))
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM)=(( 5.D0*V2+V3+V1*6.D0)*X3 +
     &              ( -U1*6.D0-U3-U2*5.D0)*Y3) * XSU360
         A13(IELEM)=((-V2-V3*5.D0-V1*6.D0)*X2 +
     &              (  U1*6.D0+U3*5.D0+U2)*Y2) * XSU360
         A14(IELEM)=(( V2*2.D0+V3)*X2+(-V2*2.D0-V3-V1*6.D0)*X3 +
     &              ( -U2*2.D0-U3)*Y2+(U1*6.D0+U3+U2*2.D0)*Y3) * XSUR90
         A15(IELEM)=((-V3-V2*2.D0)*X2+(V2+V3*2.D0)*X3 +
     &              (U2*2.D0+U3)*Y2+(-U2-U3*2.D0)*Y3) * XSUR90
         A16(IELEM)=((V2+V3*2.D0+V1*6.D0)*X2+(-V2-V3*2.D0)*X3 +
     &              (-U2-U3*2.D0-U1*6.D0)*Y2+(U2+U3*2.D0)*Y3) * XSUR90
C
         A21(IELEM)=((V2*6.D0+V3+V1*5.D0)*X2 +
     &              (-V2*6.D0-V3-V1*5.D0)*X3 +
     &              (-U2*6.D0-U3-U1*5.D0)*Y2 +
     &              (U1*5.D0+U3+U2*6.D0)*Y3) * XSU360
         A23(IELEM)=((-V2*6.D0-V3*5.D0-V1)*X2 +
     &              (  U1+U3*5.D0+U2*6.D0)*Y2) * XSU360
         A24(IELEM)=(-V2*X2*6.D0+(V2*6.D0+V3+V1*2.D0)*X3+U2*Y2*6.D0 +
     &              (-U2*6.D0-U3-U1*2.D0)*Y3) * XSUR90
         A25(IELEM)=(V2*X2*6.D0+(V3*2.D0+V1)*X3-U2*Y2*6.D0 +
     &              (-U3*2.D0-U1)*Y3) * XSUR90
         A26(IELEM)=((V3-V1)*X2+(-V3*2.D0-V1)*X3+(-U3+U1)*Y2 +
     &              (U1+U3*2.D0)*Y3) * XSUR90
C
         A31(IELEM)=((V2+V3*6.D0+V1*5.D0)*X2 +
     &              (-V2-V3*6.D0-V1*5.D0)*X3 +
     &              (-U2-U3*6.D0-U1*5.D0)*Y2 +
     &              (U1*5.D0+U3*6.D0+U2)*Y3) * XSU360
         A32(IELEM)=((V2*5.D0+V3*6.D0+V1)*X3 +
     &              (-U1-U3*6.D0-U2*5.D0)*Y3) * XSU360
         A34(IELEM)=((V2*2.D0+V1)*X2+(-V2+V1)*X3+(-U1-U2*2.D0)*Y2+
     &              (U2-U1)*Y3)*XSUR90
         A35(IELEM)=((-V2*2.D0-V1)*X2-V3*X3*6.D0 +
     &               ( U2*2.D0+U1)*Y2+U3*Y3*6.D0) * XSUR90
         A36(IELEM)=((-V3*6.D0-V2-V1*2.D0)*X2+V3*X3*6.D0 +
     &               ( U2+U3*6.D0+U1*2.D0)*Y2-U3*Y3*6.D0) * XSUR90
C
         A41(IELEM)=((-V2*2.D0-V3-V1*6.D0)*X2 +
     &               ( V2*2.D0+V3+V1*6.D0)*X3 +
     &               ( U1*6.D0+U3+U2*2.D0)*Y2 +
     &               (-U1*6.D0-U3-U2*2.D0)*Y3) * XSUR90
         A42(IELEM)=((-V2*6.D0-V3-V1*2.D0)*X3 +
     &               ( U1*2.D0+U3+U2*6.D0)*Y3) * XSUR90
         A43(IELEM)=((-V2*2.D0+V3-V1*2.D0)*X2 +
     &               ( U1*2.D0-U3+U2*2.D0)*Y2) * XSUR90
         A45(IELEM)=(( 6.D0*V2+2.D0*V3+4.D0*V1)*X2 +
     &               (-2.D0*V2-2.D0*V3-2.D0*V1)*X3 +
     &               (-6.D0*U2-2.D0*U3-4.D0*U1)*Y2 +
     &               (2.D0*U1+2.D0*U3+2.D0*U2)*Y3) * XSUR45
         A46(IELEM)=((2.D0*V2+4.D0*V1)*X2 +
     &               (2.D0*V3+2.D0*V2+2.D0*V1)*X3 +
     &               (-4.D0*U1-2.D0*U2)*Y2 +
     &               (-2.D0*U2-2.D0*U3-2.D0*U1)*Y3) * XSUR45
C
         A51(IELEM)=((V2*2.D0+V3*2.D0-V1)*X2 +
     &              (-V2*2.D0-V3*2.D0+V1)*X3 +
     &              (-U2*2.D0-U3*2.D0+U1)*Y2 +
     &              (-U1+U3*2.D0+U2*2.D0)*Y3) * XSUR90
         A52(IELEM)=((-V2*6.D0-V3*2.D0-V1)*X3 +
     &              (U1+U3*2.D0+U2*6.D0)*Y3) * XSUR90
         A53(IELEM)=((V2*2.D0+V3*6.D0+V1)*X2 +
     &              (-U1-U3*6.D0-U2*2.D0)*Y2) * XSUR90
         A54(IELEM)=((-6.D0*V2-4.D0*V3-2.D0*V1)*X2 +
     &              (4.D0*V2+2.D0*V3)*X3 +
     &              (6.D0*U2+4.D0*U3+2.D0*U1)*Y2 +
     &              (-4.D0*U2-2.D0*U3)*Y3) * XSUR45
         A56(IELEM)=((-2.D0*V2-4.D0*V3)*X2 +
     &              (4.D0*V2+6.D0*V3+2.D0*V1)*X3 +
     &              (2.D0*U2+4.D0*U3)*Y2 +
     &              (-2.D0*U1-6.D0*U3-4.D0*U2)*Y3) * XSUR45
C
         A61(IELEM)=((-V2-V3*2.D0-V1*6.D0)*X2 +
     &              (V2+V3*2.D0+V1*6.D0)*X3 +
     &              (U2+U3*2.D0+U1*6.D0)*Y2 +
     &              (-U2-U3*2.D0-U1*6.D0)*Y3) * XSUR90
         A62(IELEM)=((-V2+V3*2.D0+V1*2.D0)*X3 +
     &              (-U1*2.D0-U3*2.D0+U2)*Y3) * XSUR90
         A63(IELEM)=((V2+V3*6.D0+V1*2.D0)*X2 +
     &              (-U1*2.D0-U3*6.D0-U2)*Y2) * XSUR90
         A64(IELEM)=((-2.D0*V2-2.D0*V3-2.D0*V1)*X2 +
     &              (-4.D0*V1-2.D0*V3)*X3 +
     &              (2.D0*U1+2.D0*U3+2.D0*U2)*Y2 +
     &              (4.D0*U1+2.D0*U3)*Y3) * XSUR45
         A65(IELEM)=((2.D0*V3+2.D0*V2+2.D0*V1)*X2 +
     &              (-2.D0*V2-6.D0*V3-4.D0*V1)*X3 +
     &              (-2.D0*U2-2.D0*U3-2.D0*U1)*Y2 +
     &              (4.D0*U1+6.D0*U3+2.D0*U2)*Y3) * XSUR45
C
C  DIAGONAL TERMS:
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
     &                - A15(IELEM) - A16(IELEM)
         A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
     &                - A25(IELEM) - A26(IELEM)
         A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
     &                - A35(IELEM) - A36(IELEM)
         A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
     &                - A45(IELEM) - A46(IELEM)
         A55(IELEM) = - A51(IELEM) - A52(IELEM) - A53(IELEM)
     &                - A54(IELEM) - A56(IELEM)
         A66(IELEM) = - A61(IELEM) - A62(IELEM) - A63(IELEM)
     &                - A64(IELEM) - A65(IELEM)
2     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMU.EQ.13.AND.IELMV.EQ.13) THEN
C
C-----------------------------------------------------------------------
C
C  P2 DISCRETISATION OF THE VELOCITY:
C
      IF(FORMUL(16:16).EQ.'N') THEN
C
C  N SCHEME
C
        IF (LNG.EQ.1) WRITE(LU,12)
        IF (LNG.EQ.2) WRITE(LU,13)
        CALL PLANTE(1)
      ELSE
C
      DO 3 IELEM = 1 , NELEM
C
C  TRADITIONAL METHOD
C
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
C
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
C
         U1 = U(IKLE1(IELEM))
         U2 = U(IKLE2(IELEM))
         U3 = U(IKLE3(IELEM))
         U4 = U(IKLE4(IELEM))
         U5 = U(IKLE5(IELEM))
         U6 = U(IKLE6(IELEM))
         V1 = V(IKLE1(IELEM))
         V2 = V(IKLE2(IELEM))
         V3 = V(IKLE3(IELEM))
         V4 = V(IKLE4(IELEM))
         V5 = V(IKLE5(IELEM))
         V6 = V(IKLE6(IELEM))
C
C  EXTRADIAGONAL TERMS
C
      A12(IELEM)= ((V5*20.D0+4.D0*8.D0*V4-11.D0*V3+16.D0*V6 +
     &             V1*18.D0+V2*9.D0)*X3+(-16.D0*U6-U5*20.D0+
     &             11.D0*U3-32.D0*U4-U1*18.D0-U2*9.D0)*Y3) * XSUR2520
      A13(IELEM)= ((-16.D0*V4-V5*20.D0-4.D0*8.D0*V6-V3*9.D0 -
     &             V1*18.D0+11.D0*V2)*X2+(U1*18.D0+U5*20.D0 +
     &             U3*9.D0+32.D0*U6+16.D0*U4-11.D0*U2)*Y2) * XSUR2520
      A14(IELEM)= ((-V3+8.D0*V4+4.D0*V6-V1*6.D0+12.D0*V5       +
     &              2.D0*2.D0*V2)*X2+(-20.D0*V4+V3*5.D0-16.D0*V6-
     &              24.D0*V1-8.D0*V5)*X3+(-4.D0*U2-8.D0*U4-12.D0*U5+
     &              U3+U1*6.D0-4.D0*U6)*Y2+(16.D0*U6+20.D0*U4+
     &              24.D0*U1-U3*5.D0+4.D0*2.D0*U5)*Y3) * XSUR630
      A15(IELEM)= ((V3-12.D0*V5-4.D0*V6+V1*6.D0-8.D0*V4-4.D0*V2)*X2 +
     &             (4.D0*V4-V1*6.D0-V2+12.D0*V5+4.D0*V3+8.D0*V6)*X3 +
     &             (4.D0*U2+12.D0*U5-U1*6.D0+8.D0*U4+4.D0*U6-U3)*Y2 +
     &             (-8.D0*U6-4.D0*U4+U1*6.D0+U2-4.D0*U3-12.D0*U5)*Y3)
     &             * XSUR630
      A16(IELEM)= (( 16.D0*V4+20.D0*V6+24.D0*V1+8.D0*V5-V2*5.D0)*X2 +
     &             (-8.D0*V6-4.D0*V4+V1*6.D0-4.D0*V3-12.D0*V5+V2)*X3+
     &             (-24.D0*U1-8.D0*U5+U2*5.D0-20.D0*U6-16.D0*U4)*Y2 +
     &             (8.D0*U6+4.D0*U4-U1*6.D0+4.D0*U3+12.D0*U5-U2)*Y3)
     &             * XSUR630
C
      A21(IELEM)= ((-11.D0*V3+2.D0*8.D0*V5+V6*20.D0+V1*9.D0+32.D0*V4+
     &             V2*18.D0)*X2+(-4.D0*8.D0*V4-V1*9.D0-V2*18.D0-
     &             2.D0*8.D0*V5+11.D0*V3-V6*20.D0)*X3+(-U2*18.D0-
     &             2.D0*8.D0*U5-U1*9.D0-32.D0*U4-U6*20.D0+11.D0*U3)*Y2+
     &             (U6*20.D0+4.D0*8.D0*U4+U1*9.D0+U2*18.D0-11.D0*U3+
     &             2.D0*8.D0*U5)*Y3) * XSUR2520
      A23(IELEM)= ((-2.D0*8.D0*V4-4.D0*8.D0*V5-V6*20.D0-V3*9.D0 +
     &             11.D0*V1-V2*18.D0)*X2+(-11.D0*U1+4.D0*8.D0*U5 +
     &             U3*9.D0+U6*20.D0+2.D0*8.D0*U4+U2*18.D0)*Y2)
     &             *XSUR2520
      A24(IELEM)=((4.D0*V3-12.D0*V4+4.D0*V1+4.D0*V6-12.D0*V5-
     &            V2*30.D0)*X2+(20.D0*V4-V3*5.D0+8.D0*V6+24.D0*V2+
     &            16.D0*V5)*X3+(U2*30.D0+12.D0*U4+12.D0*U5-4.D0*U3-
     &            4.D0*U1-4.D0*U6)*Y2+(-4.D0*2.D0*U6-20.D0*U4-24.D0*U2+
     &            U3*5.D0-16.D0*U5)*Y3)*XSUR630
      A25(IELEM)=((-4.D0*V3+12.D0*V5-4.D0*V6-4.D0*V1+12.D0*V4+
     &            V2*30.D0)*X2+(4.D0*V4-V1-V2*6.D0+8.D0*V5+4.D0*V3+
     &            12.D0*V6)*X3+(-U2*30.D0-12.D0*U5+4.D0*U1-12.D0*U4+
     &            4.D0*U6+4.D0*U3)*Y2+(-12.D0*U6-4.D0*U4+U1+U2*6.D0-
     &            4.D0*U3-8.D0*U5)*Y3)*XSUR630
      A26(IELEM)=((4.D0*V5-4.D0*V4-V1*5.D0+V3*5.D0)*X2+(-12.D0*V6+
     &            V2*6.D0-4.D0*V4+V1-4.D0*V3-8.D0*V5)*X3+(-4.D0*U5-
     &            U3*5.D0+U1*5.D0+4.D0*U4)*Y2+(12.D0*U6+4.D0*U4-U2*6.D0+
     &            8.D0*U5+4.D0*U3-U1)*Y3)*XSUR630
C
      A31(IELEM)=((V3*18.D0+16.D0*V5+32.D0*V6+V1*9.D0+V4*20.D0-
     &            11.D0*V2)*X2+(-V4*20.D0-V1*9.D0+11.D0*V2-2.D0*8.D0*V5-
     &            V3*18.D0-4.D0*8.D0*V6)*X3+(11.D0*U2-2.D0*8.D0*U5-
     &            U1*9.D0-U4*20.D0-4.D0*8.D0*U6-U3*18.D0)*Y2+
     &            (4.D0*8.D0*U6+U4*20.D0+U1*9.D0-11.D0*U2+U3*18.D0+
     &            16.D0*U5)*Y3)*XSUR2520
      A32(IELEM)=((32.D0*V5+V4*20.D0+V3*18.D0+16.D0*V6-11.D0*V1+
     &            V2*9.D0)*X3+(-16.D0*U6-32.D0*U5-U3*18.D0-U4*20.D0+
     &            11.D0*U1-U2*9.D0)*Y3)*XSUR2520
      A34(IELEM)=((-V3*6.D0+8.D0*V5+4.D0*V6+12.D0*V4-V1+4.D0*V2)*X2+
     &           (-V2*5.D0-4.D0*V5+V1*5.D0+4.D0*V6)*X3+(U1-8.D0*U5+
     &           U3*6.D0-4.D0*U6-4.D0*U2-12.D0*U4)*Y2+(-4.D0*U6+U2*5.D0+
     &           4.D0*U5-U1*5.D0)*Y3)*XSUR630
      A35(IELEM)=((V3*6.D0-8.D0*V5-4.D0*V6+V1-12.D0*V4-4.D0*V2)*X2+
     &           (4.D0*V4+4.D0*V1+4.D0*V2-12.D0*V5-V3*30.D0-
     &           12.D0*V6)*X3+(4.D0*U2+8.D0*U5-U1+12.D0*U4+4.D0*U6-
     &           U3*6.D0)*Y2+(12.D0*U6-4.D0*U4-4.D0*U1-4.D0*U2+U3*30.D0+
     &           12.D0*U5)*Y3)*XSUR630
      A36(IELEM)=((-24.D0*V3-8.D0*V4-20.D0*V6-16.D0*V5+V2*5.D0)*X2+
     &           (-4.D0*V4+V3*30.D0+12.D0*V6-4.D0*V2+12.D0*V5-
     &           4.D0*V1)*X3+(-U2*5.D0+16.D0*U5+24.D0*U3+8.D0*U4+
     &           20.D0*U6)*Y2+(-12.D0*U6+4.D0*U4+4.D0*U2-U3*30.D0+
     &           4.D0*U1-12.D0*U5)*Y3)*XSUR630
C
      A41(IELEM)=(-U3*Y2*5.D0-20.D0*V6*X2-V3*X3*5.D0+12.*V1*X3-
     &           12.D0*V1*X2-12.D0*U1*Y3+12.D0*U1*Y2-20.D0*U6*Y3+
     &           V3*X2*5.D0+U3*Y3*5.D0+20.D0*V6*X3+20.*U6*Y2-
     &           40.D0*U4*Y3+40.*V4*X3-4.D0*U5*Y3+4.D0*V5*X3+
     &           40.D0*U4*Y2+4.D0*U5*Y2-4.D0*V5*X2-40.*V4*X2-
     &           8.D0*V2*X3+8.D0*U2*Y3+8.D0*V2*X2-
     &           8.D0*U2*Y2)*XSUR630
      A42(IELEM)=((-20.D0*V5-40.D0*V4+5.D0*V3-4.D0*V6+8.D0*V1-
     &           12.D0*V2)*X3+(4.D0*U6+20.D0*U5-5.D0*U3+40.D0*U4-
     &           8.D0*U1+12.D0*U2)*Y3) * XSUR630
      A43(IELEM)=((-24.D0*V4+4.D0*V5+4.D0*V6+3.D0*V3-4.D0*V1-
     &           4.D0*V2)*X2+(4.D0*U1-4.D0*U5-3.D0*U3-4.D0*U6+
     &           24.D0*U4+4.D0*U2)*Y2) * XSUR630
      A45(IELEM)=(12.D0*U3*Y2+32.D0*V6*X2+4.D0*V3*X3+4.D0*V1*X3-
     &           8.D0*V1*X2-4.D0*U1*Y3+8.D0*U1*Y2+32.D0*U6*Y3-
     &           12.D0*V3*X2-4.D0*U3*Y3-32.D0*V6*X3-32.D0*U6*Y2+
     &           32.D0*U4*Y3-32.D0*V4*X3+32.D0*U5*Y3-32.D0*V5*X3-
     &           96.D0*U4*Y2-48.D0*U5*Y2+48.D0*V5*X2+96.D0*V4*X2+
     &           4.D0*V2*X3-4.D0*U2*Y3+12.D0*V2*X2-12.D0*U2*Y2)
     &           * XSUR630
      A46(IELEM)=((-8.D0*V3+16.D0*V6+16.D0*V1+64.D0*V4-4.D0*V2)*X2+
     &           (-4.D0*V1+32.D0*V4-4.D0*V3+32.D0*V6+32.D0*V5-
     &           4.D0*V2)*X3+(8.D0*U3-16.D0*U1+4.D0*U2-16.D0*U6-
     &           64.D0*U4)*Y2+(-32.D0*U6+4.D0*U1-32.D0*U5+4.D0*U3-
     &           32.D0*U4+4.D0*U2)*Y3)*XSUR630
C
      A51(IELEM)=((4.D0*V3+24.D0*V5-4.D0*V6-V1*3.D0-4.D0*V4+
     &           4.D0*V2)*X2+(4.D0*V4+V1*3.D0-4.D0*V2-24.D0*V5-
     &           4.D0*V3+4.D0*V6)*X3+(-4.D0*U2-24.D0*U5+U1*3.D0+
     &           4.D0*U4+4.D0*U6-4.D0*U3)*Y2+(-4.D0*U6-4.D0*U4-U1*3.D0+
     &           4.D0*U2+4.D0*U3+24.D0*U5)*Y3)*XSUR630
      A52(IELEM)=((-40.D0*V5-20.D0*V4+8.D0*V3-4.D0*V6+V1*5.D0-
     &            12.D0*V2)*X3+(4.D0*U6+40.D0*U5-8.D0*U3+
     &            20.D0*U4-U1*5.D0+12.D0*U2)*Y3)*XSUR630
      A53(IELEM)=((4.D0*V4+40.D0*V5+20.D0*V6+2.D0*6.D0*V3-V1*5.D0-
     &           8.D0*V2)*X2+(U1*5.D0-40.D0*U5-12.D0*U3-20.D0*U6-
     &           4.D0*U4+8.D0*U2)*Y2)*XSUR630
      A54(IELEM)=((8.D0*V3-96.D0*V5-32.D0*V6+12.D0*V1-48.D0*V4-
     &           12.D0*V2)*X2+(16.D0*V4-8.D0*V1+16.D0*V2+64.D0*V5-
     &           4.D0*V3)*X3+(-12.D0*U1+96.D0*U5-8.D0*U3+12.D0*U2+
     &           32.D0*U6+48.D0*U4)*Y2+(8.D0*U1+4.D0*U3-16.D0*U4-
     &           64.D0*U5-16.D0*U2)*Y3)*XSUR630
      A56(IELEM)=((-16.D0*V3-16.D0*V6+8.D0*V1-64.D0*V5+4.D0*V2)*X2+
     &           (32.D0*V4+12.D0*V3-12.D0*V1+48.D0*V6+96.D0*V5-
     &           8.D0*V2)*X3+(-8.D0*U1-4.D0*U2+16.D0*U3+16.D0*U6+
     &           64.D0*U5)*Y2+(-48.D0*U6+12.D0*U1+8.D0*U2-12.D0*U3-
     &           32.D0*U4-96.D0*U5)*Y3)*XSUR630
C
      A61(IELEM)=((8.D0*V3-4.D0*V5-40.D0*V6-12.D0*V1-20.D0*V4+
     &           V2*5.D0)*X2+(20.D0*V4+12.D0*V1-V2*5.D0+4.D0*V5-
     &           8.D0*V3+40.D0*V6)*X3+(-U2*5.D0+4.D0*U5+12.D0*U1+
     &           20.D0*U4+40.D0*U6-8.D0*U3)*Y2+(-40.D0*U6-20.D0*U4-
     &           12.D0*U1+U2*5.D0+8.D0*U3-4.D0*U5)*Y3)*XSUR630
      A62(IELEM)=((-4.D0*V5-4.D0*V4+4.D0*V3+24.D0*V6+4.D0*V1-
     &           V2*3.D0)*X3+(-24.D0*U6+4.D0*U5-4.D0*U3+4.D0*U4-
     &           4.D0*U1+U2*3.D0)*Y3)*XSUR630
      A63(IELEM)=((4.D0*V4+20.D0*V5+40.D0*V6+12.D0*V3-8.D0*V1-
     &           V2*5.D0)*X2+(8.D0*U1-20.D0*U5-12.D0*U3-40.D0*U6-
     &           4.D0*U4+U2*5.D0)*Y2)*XSUR630
      A64(IELEM)=((-32.D0*V4+4.D0*V1+4.D0*V2-32.D0*V5+4.D0*V3-
     &           32.D0*V6)*X2+(-16.D0*V4-16.D0*V1+8.D0*V2+4.D0*V3-
     &           64.D0*V6)*X3+(32.D0*U6+32.D0*U4-4.D0*U1-4.D0*U2-
     &           4.D0*U3+32.D0*U5)*Y2+(64.D0*U6-8.D0*U2+16.D0*U1-
     &           4.D0*U3+16.D0*U4)*Y3)*XSUR630
      A65(IELEM)=((-4.D0*V1+32.D0*V4-4.D0*V3+32.D0*V6+32.D0*V5-
     &           4.D0*V2)*X2+(-32.D0*V4+8.D0*V1+12.D0*V2-48.D0*V5-
     &           12.D0*V3-96.D0*V6)*X3+(-32.D0*U6+4.D0*U1-32.D0*U5+
     &           4.D0*U3-32.D0*U4+4.D0*U2)*Y2+(96.D0*U6+32.D0*U4-
     &           8.D0*U1-12.D0*U2+12.D0*U3+48.D0*U5)*Y3)*XSUR630
C
C  THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE 'MAGIC SQUARE':
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
     &                - A15(IELEM) - A16(IELEM)
         A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
     &                - A25(IELEM) - A26(IELEM)
         A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
     &                - A35(IELEM) - A36(IELEM)
         A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
     &                - A45(IELEM) - A46(IELEM)
         A55(IELEM) = - A51(IELEM) - A52(IELEM) - A53(IELEM)
     &                - A54(IELEM) - A56(IELEM)
         A66(IELEM) = - A61(IELEM) - A62(IELEM) - A63(IELEM)
     &                - A64(IELEM) - A65(IELEM)
C
3     CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,10) IELMU,IELMV
        IF (LNG.EQ.2) WRITE(LU,11) IELMU,IELMV
10     FORMAT(1X,'MT05CC (BIEF) : TYPES DE VITESSES NON PREVU : ',2I6)
11     FORMAT(1X,
     &  'MT05CC (BIEF) : TYPES OF VELOCITIES NOT AVAILABLE : ',2I6)
12     FORMAT(1X,'MT05CC (BIEF) : SCHEMA N NON PREVU  ')
13     FORMAT(1X,
     &  'MT05CC (BIEF) : N SCHEMES NOT AVAILABLE  ')
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
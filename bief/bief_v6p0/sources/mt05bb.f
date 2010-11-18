C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE FOLLOWING MATRIX FOR QUASI-BUBBLE TRIANGLES:
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
!>    </th><td> A11, A12, A13, A14, A21, A22, A23, A24, A31, A32, A33, A34, A41, A42, A43, A44, FORMUL, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SU, SV, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, IELMU, IELMV, K1T1, K1T2, K1T3, K2T1, K2T2, K2T3, K3T1, K3T2, K3T3, L12T1, L12T2, L12T3, L13T1, L13T2, L13T3, L21T1, L21T2, L21T3, L23T1, L23T2, L23T3, L31T1, L31T2, L31T3, L32T1, L32T2, L32T3, TIERS, U1, U2, U3, U4, US2T1, US2T2, US2T3, V1, V2, V3, V4, VS2T1, VS2T2, VS2T3, X1T1, X1T2, X1T3, X2, X2T1, X2T2, X2T3, X3, X3T1, X3T2, X3T3, X4, XSU216, XSUR24, XSUR6, XSUR72, Y1T1, Y1T2, Y1T3, Y2, Y2T1, Y2T2, Y2T3, Y3, Y3T1, Y3T2, Y3T3, Y4
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT05BB
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
!> </td><td> 10/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; C   MOULIN (LNH) 30 87 83 81
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
                        SUBROUTINE MT05BB
     &( A11 , A12 , A13 , A14 ,
     &  A21 , A22 , A23 , A24 ,
     &  A31 , A32 , A33 , A34 ,
     &  A41 , A42 , A43 , A44 ,
     &  XMUL,SU,SV,U,V,
     &  XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,FORMUL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A14            |---| 
C| A21            |---| 
C| A22            |---| 
C| A23            |---| 
C| A24            |---| 
C| A31            |---| 
C| A32            |---| 
C| A33            |---| 
C| A34            |---| 
C| A41            |---| 
C| A42            |---| 
C| A43            |---| 
C| A44            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| FORMUL         |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
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
      USE BIEF, EX_MT05BB => MT05BB
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
C
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*),A44(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
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
      DOUBLE PRECISION XSUR6,TIERS
      DOUBLE PRECISION K1T1,K2T1,K3T1,US2T1,VS2T1
      DOUBLE PRECISION L12T1,L13T1,L21T1,L23T1,L31T1,L32T1
      DOUBLE PRECISION K1T2,K2T2,K3T2,US2T2,VS2T2
      DOUBLE PRECISION L12T2,L13T2,L21T2,L23T2,L31T2,L32T2
      DOUBLE PRECISION K1T3,K2T3,K3T3,US2T3,VS2T3
      DOUBLE PRECISION L12T3,L13T3,L21T3,L23T3,L31T3,L32T3
C
C-----------------------------------------------------------------------
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELEM,IELMU,IELMV
C
      DOUBLE PRECISION X2,X3,X4,Y2,Y3,Y4,U1,U2,U3,U4,V1,V2,V3,V4
      DOUBLE PRECISION XSU216,XSUR72,XSUR24
      DOUBLE PRECISION X1T1,X2T1,X3T1,Y1T1,Y2T1,Y3T1
      DOUBLE PRECISION X1T2,X2T2,X3T2,Y1T2,Y2T2,Y3T2
      DOUBLE PRECISION X1T3,X2T3,X3T3,Y1T3,Y2T3,Y3T3
C
C=======================================================================
C
C     EXTRACTS THE TYPE OF ELEMENT FOR VELOCITY
C
      IELMU = SU%ELM
      IELMV = SV%ELM
C
C-----------------------------------------------------------------------
C
      TIERS  = 1.D0 /   6.D0
      XSUR6  = XMUL /   6.D0
      XSU216 = XMUL / 216.D0
      XSUR72 = XMUL /  72.D0
      XSUR24 = XMUL /  24.D0
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
         A12(IELEM)= (X2*(-V3-4*V2-7*V1)+X3*(-V3-4*V2-7*V1)+Y2*(
     &               U3+4*U2+7*U1)+Y3*(U3+4*U2+7*U1))*XSU216
C
         A13(IELEM)= (X2*(4*V3+V2+7*V1)+X3*(4*V3+V2+7*V1)+Y2*(-
     &               4*U3-U2-7*U1)+Y3*(-4*U3-U2-7*U1))*XSU216
C
         A14(IELEM)= (X2*(V3+4*V2+7*V1)+X3*(-4*V3-V2-7*V1)+Y2*(-
     &               U3-4*U2-7*U1)+Y3*(4*U3+U2+7*U1))*XSUR72
C
         A21(IELEM)= (2*X2*(-V3-7*V2-4*V1)+X3*(V3+7*V2+4*V1)+2
     &               *Y2*(U3+7*U2+4*U1)+Y3*(-U3-7*U2-4*U1))*XSU216
C
         A23(IELEM)= (2*X2*(4*V3+7*V2+V1)+X3*(-4*V3-7*V2-V1)+2
     &               *Y2*(-4*U3-7*U2-U1)+Y3*(4*U3+7*U2+U1))*XSU216
C
         A24(IELEM)= (3*X2*(-V3+V1)+X3*(4*V3+7*V2+V1)+3*Y2*(U3-
     &               U1)+Y3*(-4*U3-7*U2-U1))*XSUR72
C
         A31(IELEM)= (X2*(-7*V3-V2-4*V1)+2*X3*(7*V3+V2+4*V1)+Y2
     &               *(7*U3+U2+4*U1)+2*Y3*(-7*U3-U2-4*U1))*XSU216
C
         A32(IELEM)= (X2*(7*V3+4*V2+V1)+2*X3*(-7*V3-4*V2-V1)+Y2
     &               *(-7*U3-4*U2-U1)+2*Y3*(7*U3+4*U2+U1))*XSU216
C
         A34(IELEM)= (X2*(-7*V3-4*V2-V1)+3*X3*(V2-V1)+Y2*(7*U3+
     &               4*U2+U1)+3*Y3*(-U2+U1))*XSUR72
C
         A41(IELEM)= (X2*(-3*V3-4*V2-5*V1)+X3*(4*V3+3*V2+5*V1)
     &               +Y2*(3*U3+4*U2+5*U1)
     &               +Y3*(-4*U3-3*U2-5*U1))*XSUR72
C
         A42(IELEM)= (X2*(V3-V1)+X3*(-4*V3-5*V2-3*V1)+Y2*(-U3+U1)
     &               +Y3*(4*U3+5*U2+3*U1))*XSUR72
C
         A43(IELEM)= (X2*(5*V3+4*V2+3*V1)+X3*(-V2+V1)+Y2*(-5*U3-
     &               4*U2-3*U1)+Y3*(U2-U1))*XSUR72
C
C  THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE 'MAGIC SQUARE':
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
         A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
         A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
         A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
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
         A12(IELEM)= (X2*(-V3-4*V2-7*V1)+X3*(-V3-4*V2-7*V1)+Y2*(
     &               U3+4*U2+7*U1)+Y3*(U3+4*U2+7*U1))*XSU216
C
         A13(IELEM)= (X2*(4*V3+V2+7*V1)+X3*(4*V3+V2+7*V1)+Y2*(-
     &               4*U3-U2-7*U1)+Y3*(-4*U3-U2-7*U1))*XSU216
C
         A14(IELEM)= (X2*(V3+4*V2+7*V1)+X3*(-4*V3-V2-7*V1)+Y2*(-
     &               U3-4*U2-7*U1)+Y3*(4*U3+U2+7*U1))*XSUR72
C
         A21(IELEM)= (2*X2*(-V3-7*V2-4*V1)+X3*(V3+7*V2+4*V1)+2
     &               *Y2*(U3+7*U2+4*U1)+Y3*(-U3-7*U2-4*U1))*XSU216
C
         A23(IELEM)= (2*X2*(4*V3+7*V2+V1)+X3*(-4*V3-7*V2-V1)+2
     &               *Y2*(-4*U3-7*U2-U1)+Y3*(4*U3+7*U2+U1))*XSU216
C
         A24(IELEM)= (3*X2*(-V3+V1)+X3*(4*V3+7*V2+V1)+3*Y2*(U3-
     &               U1)+Y3*(-4*U3-7*U2-U1))*XSUR72
C
         A31(IELEM)= (X2*(-7*V3-V2-4*V1)+2*X3*(7*V3+V2+4*V1)+Y2
     &               *(7*U3+U2+4*U1)+2*Y3*(-7*U3-U2-4*U1))*XSU216
C
         A32(IELEM)= (X2*(7*V3+4*V2+V1)+2*X3*(-7*V3-4*V2-V1)+Y2
     &               *(-7*U3-4*U2-U1)+2*Y3*(7*U3+4*U2+U1))*XSU216
C
         A34(IELEM)= (X2*(-7*V3-4*V2-V1)+3*X3*(V2-V1)+Y2*(7*U3+
     &               4*U2+U1)+3*Y3*(-U2+U1))*XSUR72
C
         A41(IELEM)= (X2*(-3*V3-4*V2-5*V1)+X3*(4*V3+3*V2+5*V1)
     &               +Y2*(3*U3+4*U2+5*U1)
     &               +Y3*(-4*U3-3*U2-5*U1))*XSUR72
C
         A42(IELEM)= (X2*(V3-V1)+X3*(-4*V3-5*V2-3*V1)+Y2*(-U3+U1)
     &               +Y3*(4*U3+5*U2+3*U1))*XSUR72
C
         A43(IELEM)= (X2*(5*V3+4*V2+3*V1)+X3*(-V2+V1)+Y2*(-5*U3-
     &               4*U2-3*U1)+Y3*(U2-U1))*XSUR72
C
C  THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE 'MAGIC SQUARE':
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
         A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
         A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
         A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
C
2     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMU.EQ.12.AND.IELMV.EQ.12) THEN
C
C-----------------------------------------------------------------------
C
C  QUASI-BUBBLE DISCRETISATION OF THE VELOCITY:
C
      IF(FORMUL(16:16).EQ.'N') THEN
C
C  N SCHEME
C
      DO 33 IELEM = 1 , NELEM
C
C     COORDINATES OF THE SUB-TRIANGLE VERTICES
      X1T1 = XEL(IELEM,1)
      X2T1 = XEL(IELEM,2) - X1T1
      Y1T1 = YEL(IELEM,1)
      Y2T1 = YEL(IELEM,2) - Y1T1
C
      X1T2 = XEL(IELEM,2)
      X2T2 = XEL(IELEM,3) - X1T2
      Y1T2 = YEL(IELEM,2)
      Y2T2 = YEL(IELEM,3) - Y1T2
C
      X1T3 = XEL(IELEM,3)
      X2T3 = XEL(IELEM,1) - X1T3
      Y1T3 = YEL(IELEM,3)
      Y2T3 = YEL(IELEM,1) - Y1T3
C     POINT 3 IS ALWAYS THE CENTRE OF THE INITIAL TRIANGLE
      X4   = TIERS * (XEL(IELEM,1)+XEL(IELEM,2)+XEL(IELEM,3))
      Y4   = TIERS * (YEL(IELEM,1)+YEL(IELEM,2)+YEL(IELEM,3))
      X3T1 = X4 - X1T1
      Y3T1 = Y4 - Y1T1
      X3T2 = X4 - X1T2
      Y3T2 = Y4 - Y1T2
      X3T3 = X4 - X1T3
      Y3T3 = Y4 - Y1T3
C
      U1 = U(IKLE1(IELEM))
      U2 = U(IKLE2(IELEM))
      U3 = U(IKLE3(IELEM))
      U4 = U(IKLE4(IELEM))
      V1 = V(IKLE1(IELEM))
      V2 = V(IKLE2(IELEM))
      V3 = V(IKLE3(IELEM))
      V4 = V(IKLE4(IELEM))
C
      US2T1 = (U1+U2+U4)*XSUR6
      VS2T1 = (V1+V2+V4)*XSUR6
      US2T2 = (U2+U3+U4)*XSUR6
      VS2T2 = (V2+V3+V4)*XSUR6
      US2T3 = (U3+U1+U4)*XSUR6
      VS2T3 = (V3+V1+V4)*XSUR6
C
      K1T1 = US2T1 * (Y2T1-Y3T1) - VS2T1 * (X2T1-X3T1)
      K2T1 = US2T1 * (Y3T1     ) - VS2T1 * (X3T1     )
      K3T1 = US2T1 * (    -Y2T1) - VS2T1 * (    -X2T1)
C
      K1T2 = US2T2 * (Y2T2-Y3T2) - VS2T2 * (X2T2-X3T2)
      K2T2 = US2T2 * (Y3T2     ) - VS2T2 * (X3T2     )
      K3T2 = US2T2 * (    -Y2T2) - VS2T2 * (    -X2T2)
C
      K1T3 = US2T3 * (Y2T3-Y3T3) - VS2T3 * (X2T3-X3T3)
      K2T3 = US2T3 * (Y3T3     ) - VS2T3 * (X3T3     )
      K3T3 = US2T3 * (    -Y2T3) - VS2T3 * (    -X2T3)
C
      L12T1 = MAX( MIN(K1T1,-K2T1) , 0.D0 )
      L13T1 = MAX( MIN(K1T1,-K3T1) , 0.D0 )
      L21T1 = MAX( MIN(K2T1,-K1T1) , 0.D0 )
      L23T1 = MAX( MIN(K2T1,-K3T1) , 0.D0 )
      L31T1 = MAX( MIN(K3T1,-K1T1) , 0.D0 )
      L32T1 = MAX( MIN(K3T1,-K2T1) , 0.D0 )
C
      L12T2 = MAX( MIN(K1T2,-K2T2) , 0.D0 )
      L13T2 = MAX( MIN(K1T2,-K3T2) , 0.D0 )
      L21T2 = MAX( MIN(K2T2,-K1T2) , 0.D0 )
      L23T2 = MAX( MIN(K2T2,-K3T2) , 0.D0 )
      L31T2 = MAX( MIN(K3T2,-K1T2) , 0.D0 )
      L32T2 = MAX( MIN(K3T2,-K2T2) , 0.D0 )
C
      L12T3 = MAX( MIN(K1T3,-K2T3) , 0.D0 )
      L13T3 = MAX( MIN(K1T3,-K3T3) , 0.D0 )
      L21T3 = MAX( MIN(K2T3,-K1T3) , 0.D0 )
      L23T3 = MAX( MIN(K2T3,-K3T3) , 0.D0 )
      L31T3 = MAX( MIN(K3T3,-K1T3) , 0.D0 )
      L32T3 = MAX( MIN(K3T3,-K2T3) , 0.D0 )
C
C  EXTRADIAGONAL TERMS
C
      A12(IELEM) = - L12T1
      A13(IELEM) = - L21T3
      A14(IELEM) = - L13T1 - L23T3
      A21(IELEM) = - L21T1
      A23(IELEM) = - L12T2
      A24(IELEM) = - L13T2 - L23T1
      A31(IELEM) = - L12T3
      A32(IELEM) = - L21T2
      A34(IELEM) = - L13T3 - L23T2
      A41(IELEM) = - L31T1 - L32T3
      A42(IELEM) = - L31T2 - L32T1
      A43(IELEM) = - L31T3 - L32T2
C
      A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
      A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
      A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
      A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
C
33    CONTINUE
C
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
         V1 = V(IKLE1(IELEM))
         V2 = V(IKLE2(IELEM))
         V3 = V(IKLE3(IELEM))
         V4 = V(IKLE4(IELEM))
C
C  EXTRADIAGONAL TERMS
C
         A12(IELEM)= (X2*(-V4-V2-2*V1)+X3*(-V4-V2-2*V1)+Y2*(U4+U2+
     &               2*U1)+Y3*(U4+U2+2*U1))*XSUR72
C
         A13(IELEM)= (X2*(V3+V4+2*V1)+X3*(V3+V4+2*V1)+Y2*(-U3-U4-
     &               2*U1)+Y3*(-U3-U4-2*U1))*XSUR72
C
         A14(IELEM)= (X2*(V4+V2+2*V1)+X3*(-V3-V4-2*V1)+Y2*(-U4-U2-
     &               2*U1)+Y3*(U3+U4+2*U1))*XSUR24
C
         A21(IELEM)= (2*X2*(-V4-2*V2-V1)+X3*(V4+2*V2+V1)+2*Y2*(
     &               U4+2*U2+U1)+Y3*(-U4-2*U2-U1))*XSUR72
C
         A23(IELEM)= (2*X2*(V3+V4+2*V2)+X3*(-V3-V4-2*V2)+2*Y2*(-
     &               U3-U4-2*U2)+Y3*(U3+U4+2*U2))*XSUR72
C
         A24(IELEM)= (X2*(-V3+V1)+X3*(V3+V4+2*V2)+Y2*(U3-U1)+Y3*(-
     &               U3-U4-2*U2))*XSUR24
C
         A31(IELEM)= (X2*(-2*V3-V4-V1)+2*X3*(2*V3+V4+V1)+Y2*(2*
     &               U3+U4+U1)+2*Y3*(-2*U3-U4-U1))*XSUR72
C
         A32(IELEM)= (X2*(2*V3+V4+V2)+2*X3*(-2*V3-V4-V2)+Y2*(-2*
     &               U3-U4-U2)+2*Y3*(2*U3+U4+U2))*XSUR72
C
         A34(IELEM)= (X2*(-2*V3-V4-V2)+X3*(V2-V1)+Y2*(2*U3+U4+U2)+
     &               Y3*(-U2+U1))*XSUR24
C
         A41(IELEM)= (X2*(-V3-6*V4-2*V2-3*V1)+X3*(2*V3+6*V4+V2+
     &               3*V1)+Y2*(U3+6*U4+2*U2+3*U1)
     &               +Y3*(-2*U3-6*U4-U2-3*U1))*XSUR72
C
         A42(IELEM)= (X2*(V3-V1)+X3*(-2*V3-6*V4-3*V2-V1)+Y2*(-U3+
     &               U1)+Y3*(2*U3+6*U4+3*U2+U1))*XSUR72
C
         A43(IELEM)= (X2*(3*V3+6*V4+2*V2+V1)+X3*(-V2+V1)+Y2*(-3*
     &               U3-6*U4-2*U2-U1)+Y3*(U2-U1))*XSUR72
C
C  THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE 'MAGIC SQUARE':
C
         A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
         A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
         A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
         A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
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
10      FORMAT(1X,'MT05BB (BIEF) : TYPES DE VITESSES NON PREVU : ',2I6)
11      FORMAT(1X,
     &  'MT05BB (BIEF) : TYPES OF VELOCITIES NOT AVAILABLE : ',2I6)
        CALL PLANTE(0)
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
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /                  DF      DF
!>      V  =  XMUL   /       PSII  * ( U --  + V -- )   D(OMEGA)
!>       I          /OMEGA               DX      DY<br>
!>    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FORMUL, IKLE, NELEM, NELMAX, SF, SU, SV, U, V, W1, W2, W3, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BETAN1, BETAN2, BETAN3, F1, F1MF3, F2, F2MF1, F3, IELEM, IELMF, IELMU, IELMV, K1, K2, K3, L12, L13, L21, L23, L31, L32, PHIT, SUR6, U1, U2, U3, U4, U5, U6, USUR2, V1, V2, V3, V4, V5, V6, VSUR2, X2, X3, XSU216, XSUR120, XSUR24, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC08AA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VECTOS()

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
!> </td><td> 01/07/08
!> </td><td> A FROEHLY (MATMECA) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>FORMUL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DES FONCTIONS F,G ET H
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DES FONCTIONS U,V ET W
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR
!>                  INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>W1,2,3
!></td><td>--></td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>W2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC08AA
     &( XMUL,SF,SU,SV,F,U,V,XEL,YEL,IKLE,
     &  NELEM,NELMAX,W1,W2,W3 , FORMUL )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| FORMUL         |---| 
C| IKLE           |---| 
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G ET H
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W1,2,3         |-->| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF   !, EX_VC08AA => VC08AA
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
C
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
C
C     STRUCTURES OF F, U, V AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN)   :: SF,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
C
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF,IELMU,IELMV
C
      INTRINSIC MIN,MAX
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION X2,Y2,X3,Y3,F1,F2,F3
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION SUR6,XSUR24,XSUR120,XSU216,F1MF3,F2MF1
      DOUBLE PRECISION K1,K2,K3,USUR2,VSUR2,PHIT
      DOUBLE PRECISION L12,L13,L21,L23,L31,L32,BETAN1,BETAN2,BETAN3
C
C-----------------------------------------------------------------------
C
      SUR6 = 1.D0 / 6.D0
      XSUR24 = XMUL/24.D0
      XSUR120= XMUL/120.D0
      XSU216 = XMUL/216.D0
C
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
C
C-----------------------------------------------------------------------
C
C     FUNCTION F AND VECTOR U ARE LINEAR
C
      IF(IELMF.EQ.11.AND.IELMU.EQ.11.AND.IELMV.EQ.11) THEN
C
      IF(FORMUL(14:16).EQ.'PSI') THEN
C
C     PSI SCHEME
C
      DO 33 IELEM = 1 , NELEM
C
         X2 = XEL(IELEM,2)
         X3 = XEL(IELEM,3)
         Y2 = YEL(IELEM,2)
         Y3 = YEL(IELEM,3)
C
         F1 = F(IKLE(IELEM,1))
         F2 = F(IKLE(IELEM,2))
         F3 = F(IKLE(IELEM,3))
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
         BETAN1 = L12*(F1-F2) + L13*(F1-F3)
         BETAN2 = L21*(F2-F1) + L23*(F2-F3)
         BETAN3 = L31*(F3-F1) + L32*(F3-F2)
C
         PHIT = BETAN1 + BETAN2 + BETAN3
C
         IF(PHIT.GT.0.D0) THEN
           W1(IELEM) =   XMUL * MAX( MIN( BETAN1, PHIT),0.D0 )
           W2(IELEM) =   XMUL * MAX( MIN( BETAN2, PHIT),0.D0 )
           W3(IELEM) =   XMUL * MAX( MIN( BETAN3, PHIT),0.D0 )
         ELSE
           W1(IELEM) = - XMUL * MAX( MIN(-BETAN1,-PHIT),0.D0 )
           W2(IELEM) = - XMUL * MAX( MIN(-BETAN2,-PHIT),0.D0 )
           W3(IELEM) = - XMUL * MAX( MIN(-BETAN3,-PHIT),0.D0 )
         ENDIF
C
33    CONTINUE
C
      ELSE
C
C     NORMAL CENTERED SCHEME
C
      DO 3 IELEM = 1 , NELEM
C
         X2 = XEL(IELEM,2)
         X3 = XEL(IELEM,3)
         Y2 = YEL(IELEM,2)
         Y3 = YEL(IELEM,3)
C
         F1MF3 = F(IKLE(IELEM,1)) - F(IKLE(IELEM,3))
         F2MF1 = F(IKLE(IELEM,2)) - F(IKLE(IELEM,1))
C
         U1 = U(IKLE(IELEM,1))
         U2 = U(IKLE(IELEM,2))
         U3 = U(IKLE(IELEM,3))
         V1 = V(IKLE(IELEM,1))
         V2 = V(IKLE(IELEM,2))
         V3 = V(IKLE(IELEM,3))
C
         W1(IELEM)=(  ( Y2*F1MF3 + Y3*F2MF1 ) * (U1+U1+U2+U3)
     &              - ( X2*F1MF3 + X3*F2MF1 ) * (V1+V1+V2+V3)  )*XSUR24
C
         W2(IELEM)=(  ( Y2*F1MF3 + Y3*F2MF1 ) * (U1+U2+U2+U3)
     &              - ( X2*F1MF3 + X3*F2MF1 ) * (V1+V2+V2+V3)  )*XSUR24
C
         W3(IELEM)=(  ( Y2*F1MF3 + Y3*F2MF1 ) * (U1+U2+U3+U3)
     &              - ( X2*F1MF3 + X3*F2MF1 ) * (V1+V2+V3+V3)  )*XSUR24
C
3     CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     FUNCTION F IS LINEAR AND VECTOR U QUASI-BUBBLE
C
      ELSEIF(IELMF.EQ.11.AND.IELMU.EQ.12.AND.IELMV.EQ.12) THEN
C
      IF(FORMUL(14:16).EQ.'PSI') THEN
C
C  PSI SCHEME (U4 DISCARDED HERE)
C             (AS IF U WAS P1   )
C
      DO 4 IELEM = 1 , NELEM
C
         X2 = XEL(IELEM,2)
         X3 = XEL(IELEM,3)
         Y2 = YEL(IELEM,2)
         Y3 = YEL(IELEM,3)
C
         F1 = F(IKLE(IELEM,1))
         F2 = F(IKLE(IELEM,2))
         F3 = F(IKLE(IELEM,3))
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
         BETAN1 = L12*(F1-F2) + L13*(F1-F3)
         BETAN2 = L21*(F2-F1) + L23*(F2-F3)
         BETAN3 = L31*(F3-F1) + L32*(F3-F2)
C
         PHIT = BETAN1 + BETAN2 + BETAN3
C
         IF(PHIT.GT.0.D0) THEN
           W1(IELEM) =   XMUL * MAX( MIN( BETAN1, PHIT),0.D0 )
           W2(IELEM) =   XMUL * MAX( MIN( BETAN2, PHIT),0.D0 )
           W3(IELEM) =   XMUL * MAX( MIN( BETAN3, PHIT),0.D0 )
         ELSE
           W1(IELEM) = - XMUL * MAX( MIN(-BETAN1,-PHIT),0.D0 )
           W2(IELEM) = - XMUL * MAX( MIN(-BETAN2,-PHIT),0.D0 )
           W3(IELEM) = - XMUL * MAX( MIN(-BETAN3,-PHIT),0.D0 )
         ENDIF
C
4     CONTINUE
C
      ELSE
C
C  NORMAL CENTERED SCHEME
C
      DO 44 IELEM = 1 , NELEM
C
         X2 = XEL(IELEM,2)
         X3 = XEL(IELEM,3)
         Y2 = YEL(IELEM,2)
         Y3 = YEL(IELEM,3)
C
         F1 = F(IKLE(IELEM,1))
         F2 = F(IKLE(IELEM,2)) - F1
         F3 = F(IKLE(IELEM,3)) - F1
C
         U1 = U(IKLE(IELEM,1))
         U2 = U(IKLE(IELEM,2))
         U3 = U(IKLE(IELEM,3))
         U4 = U(IKLE(IELEM,4))
         V1 = V(IKLE(IELEM,1))
         V2 = V(IKLE(IELEM,2))
         V3 = V(IKLE(IELEM,3))
         V4 = V(IKLE(IELEM,4))
C
         W1(IELEM)=(5*X2*F3*V3+12*X2*F3*V4+5*X2*F3*V2+14*X2*F3*V1-5
     &    *X3*F2*V3-12*X3*F2*V4-5*X3*F2*V2-14*X3*F2*V1-5*F3*U3*
     &    Y2-12*F3*U4*Y2-5*F3*U2*Y2-14*F3*U1*Y2+5*F2*U3*Y3+12*
     &    F2*U4*Y3+5*F2*U2*Y3+14*F2*U1*Y3)*XSU216
         W2(IELEM)=(5*X2*F3*V3+12*X2*F3*V4+14*X2*F3*V2+5*X2*F3*V1-5
     &    *X3*F2*V3-12*X3*F2*V4-14*X3*F2*V2-5*X3*F2*V1-5*F3*U3*
     &    Y2-12*F3*U4*Y2-14*F3*U2*Y2-5*F3*U1*Y2+5*F2*U3*Y3+12*
     &    F2*U4*Y3+14*F2*U2*Y3+5*F2*U1*Y3)*XSU216
         W3(IELEM)=(14*X2*F3*V3+12*X2*F3*V4+5*X2*F3*V2+5*X2*F3*V1-
     &    14*X3*F2*V3-12*X3*F2*V4-5*X3*F2*V2-5*X3*F2*V1-14*F3*
     &    U3*Y2-12*F3*U4*Y2-5*F3*U2*Y2-5*F3*U1*Y2+14*F2*U3*Y3+
     &    12*F2*U4*Y3+5*F2*U2*Y3+5*F2*U1*Y3)*XSU216
C
44    CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     FUNCTION F IS LINEAR AND VECTOR U P2
C
      ELSEIF(IELMF.EQ.11.AND.IELMU.EQ.13.AND.IELMV.EQ.13) THEN
C
      IF(FORMUL(14:16).EQ.'PSI') THEN
C
C  PSI SCHEME (U4,U5 AND U6 DISCARDED HERE)
C             (AS IF U WAS P1             )
C
      DO 5 IELEM = 1 , NELEM
C
         X2 = XEL(IELEM,2)
         X3 = XEL(IELEM,3)
         Y2 = YEL(IELEM,2)
         Y3 = YEL(IELEM,3)
C
         F1 = F(IKLE(IELEM,1))
         F2 = F(IKLE(IELEM,2))
         F3 = F(IKLE(IELEM,3))
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
         BETAN1 = L12*(F1-F2) + L13*(F1-F3)
         BETAN2 = L21*(F2-F1) + L23*(F2-F3)
         BETAN3 = L31*(F3-F1) + L32*(F3-F2)
C
         PHIT = BETAN1 + BETAN2 + BETAN3

C
         IF(PHIT.GT.0.D0) THEN
           W1(IELEM) =   XMUL * MAX( MIN( BETAN1, PHIT),0.D0 )
           W2(IELEM) =   XMUL * MAX( MIN( BETAN2, PHIT),0.D0 )
           W3(IELEM) =   XMUL * MAX( MIN( BETAN3, PHIT),0.D0 )
         ELSE
           W1(IELEM) = - XMUL * MAX( MIN(-BETAN1,-PHIT),0.D0 )
           W2(IELEM) = - XMUL * MAX( MIN(-BETAN2,-PHIT),0.D0 )
           W3(IELEM) = - XMUL * MAX( MIN(-BETAN3,-PHIT),0.D0 )
         ENDIF
C
5     CONTINUE
C
      ELSE
C
C  NORMAL CENTERED SCHEME
C
      DO 55 IELEM = 1 , NELEM
C
         X2 = XEL(IELEM,2)
         X3 = XEL(IELEM,3)
         Y2 = YEL(IELEM,2)
         Y3 = YEL(IELEM,3)
C
         F1 = F(IKLE(IELEM,1))
         F2 = F(IKLE(IELEM,2)) - F1
         F3 = F(IKLE(IELEM,3)) - F1
C
         U1 = U(IKLE(IELEM,1))
         U2 = U(IKLE(IELEM,2))
         U3 = U(IKLE(IELEM,3))
         U4 = U(IKLE(IELEM,4))
         U5 = U(IKLE(IELEM,5))
         U6 = U(IKLE(IELEM,6))
         V1 = V(IKLE(IELEM,1))
         V2 = V(IKLE(IELEM,2))
         V3 = V(IKLE(IELEM,3))
         V4 = V(IKLE(IELEM,4))
         V5 = V(IKLE(IELEM,5))
         V6 = V(IKLE(IELEM,6))
C
      W1(IELEM)=(2.D0*U1*Y3*F2-2.D0*U1*Y2*F3-V2*X2*F3-8.D0*V4*X3*F2+
     &           8.D0*U4*Y3*F2+V3*X3*F2-U2*Y3*F2-V3*X2*F3+4.D0*U5*Y3*F2-
     &           4.D0*U5*Y2*F3+U3*Y2*F3-4.D0*V5*X3*F2+V2*X3*F2-
     &           8.D0*U6*Y2*F3-8.D0*U4*Y2*F3+4.D0*V5*X2*F3+
     &           2.D0*V1*X2*F3-U3*Y3*F2+8.D0*V6*X2*F3-8.D0*V6*X3*F2+
     &           8.D0*V4*X2*F3-2.D0*V1*X3*F2+8.D0*U6*Y3*F2+
     &           U2*Y2*F3)*XSUR120
C
      W2(IELEM)=-(-8.D0*V5*X2*F3-4.D0*U6*Y3*F2-V1*X3*F2+4.D0*V6*X3*F2+
     &            8.D0*U4*Y2*F3-4.D0*V6*X2*F3+2.D0*U2*Y2*F3+
     &            8.D0*U5*Y2*F3+V3*X2*F3+8.D0*V5*X3*F2+2.D0*V2*X3*F2-
     &            2.D0*U2*Y3*F2-8.D0*U5*Y3*F2+V1*X2*F3-2.D0*V2*X2*F3-
     &            V3*X3*F2-8.D0*V4*X2*F3+U3*Y3*F2+U1*Y3*F2-U1*Y2*F3+
     &            4.D0*U6*Y2*F3-8.D0*U4*Y3*F2+8.D0*V4*X3*F2-U3*Y2*F3)
     &           *XSUR120
C
      W3(IELEM) = (-V5*X3*F2*8.D0+V2*X3*F2-U6*Y2*F3*8.D0-U4*Y2*F3*4.D0+
     &             V5*X2*F3*8.D0-V1*X2*F3+U3*Y3*F2*2.D0+V6*X2*F3*8.D0-
     &             V6*X3*F2*8.D0+V4*X2*F3*4.D0+V1*X3*F2+U6*Y3*F2*8.D0+
     &             U2*Y2*F3-U1*Y3*F2+U1*Y2*F3-V2*X2*F3-V4*X3*F2*4.D0+
     &             U4*Y3*F2*4.D0-V3*X3*F2*2.D0-U2*Y3*F2+V3*X2*F3*2.D0+
     &             U5*Y3*F2*8.D0-U5*Y2*F3*8.D0-U3*Y2*F3*2.D0)*XSUR120
C
55    CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.1) WRITE(LU,200) IELMU,SU%NAME
       IF (LNG.EQ.1) WRITE(LU,300)
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,201) IELMU,SU%NAME
       IF (LNG.EQ.2) WRITE(LU,301)
100    FORMAT(1X,'VC08AA (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,
     &        1X,'NOM REEL : ',A6)
200    FORMAT(1X,'DISCRETISATION DE U : ',1I6,
     &        1X,'NOM REEL : ',A6)
300    FORMAT(1X,'CAS NON PREVU')
101    FORMAT(1X,'VC08AA (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F:',1I6,
     &        1X,'REAL NAME: ',A6)
201    FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &        1X,'REAL NAME: ',A6)
301    FORMAT(1X,'CASE NOT IMPLEMENTED')
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

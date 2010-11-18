C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /                     DF      DF
!>      V  =  XMUL   /   K GRAD(PSII) * ( U --  + V -- )   D(OMEGA)
!>       I          /OMEGA                  DX      DY<br>
!>    PSI(I) IS A BASE OF TYPE QUASI-BUBBLE TRIANGLE<br>
!>    F, U AND V ARE VECTORS
!>    K IS A VECTOR WITH COMPONENTS G AND H
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
!>    </th><td> F, G, H, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, SG, SH, SU, SURFAC, SV, U, V, W1, W2, W3, W4, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F2, F3, F4, GEL, HEL, IELEM, IELMF, IELMG, IELMH, IELMU, IELMV, U1, U2, U3, U4, V1, V2, V3, V4, X2, X3, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC03BB
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 13/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; C MOULIN  (LNH)  30 87 78 54
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
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
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
!>          <tr><td>W4
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
                        SUBROUTINE VC03BB
     &( XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,XEL,YEL,SURFAC,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3,W4 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
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
C| W4             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_VC03BB => VC03BB
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
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
C
C     STRUCTURES OF F, G, H, U, V AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN)   :: SF,SG,SH,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),H(*),U(*),V(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF,IELMG,IELMU,IELMV,IELMH
      DOUBLE PRECISION X2,Y2,X3,Y3,F1,F2,F3,F4
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4,GEL,HEL
C
C-----------------------------------------------------------------------
C
      IELMF=SF%ELM
      IELMG=SG%ELM
      IELMH=SH%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
C
C-----------------------------------------------------------------------
C
C     F IS QUASI-BUBBLE; G AND H (NOT CHECKED) P0; AND U, V (NOT CHECKED) P1
C
      IF(      IELMF.EQ.12.AND.IELMG.EQ.10.AND.IELMH.EQ.10
     &    .AND.IELMU.EQ.11.AND.IELMV.EQ.11                 ) THEN
C
      DO 3 IELEM = 1 , NELEM
C
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM)) - F1
      F3 = F(IKLE3(IELEM)) - F1
      F4 = F(IKLE4(IELEM)) - F1
C
      U1 = U(IKLE1(IELEM))
      U2 = U(IKLE2(IELEM))
      U3 = U(IKLE3(IELEM))
C
      V1 = V(IKLE1(IELEM))
      V2 = V(IKLE2(IELEM))
      V3 = V(IKLE3(IELEM))
C
      GEL = G(IELEM)
      HEL = H(IELEM)
C
      W1(IELEM) = (-(3*((2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V3+4
     &   *X2*V2+4*X2*V1-U3*Y2-4*U2*Y2-4*U1*Y2)-(X2*HEL-2*X3*HEL+
     &   2*GEL*Y3-GEL*Y2)*(4*X3*V3+X3*V2+4*X3*V1-4*U3*Y3-U2*Y3-4
     &   *U1*Y3))*F4-(2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V3+4*X2*
     &   V2+4*X2*V1+X3*V3+4*X3*V2+4*X3*V1-U3*Y3-U3*Y2-4*U2*Y3-
     &   4*U2*Y2-4*U1*Y3-4*U1*Y2)*F2+(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL
     &   *Y2)*(4*X2*V3+X2*V2+4*X2*V1+4*X3*V3+X3*V2+4*X3*V1-4*
     &   U3*Y3-4*U3*Y2-U2*Y3-U2*Y2-4*U1*Y3-4*U1*Y2)*F3))
     &   *XMUL/(54*(X2*Y3-X3*Y2))
C
      W2(IELEM) = (F2*((X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*(X2*V3+4*X2*V2+
     &   4*X2*V1+X3*V3+4*X3*V2+4*X3*V1-U3*Y3-U3*Y2-4*U2*Y3-4*
     &   U2*Y2-4*U1*Y3-4*U1*Y2)+(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL*Y2)*
     &   (4*X2*V3+4*X2*V2+X2*V1-8*X3*V3-8*X3*V2-2*X3*V1+8*U3
     &   *Y3-4*U3*Y2+8*U2*Y3-4*U2*Y2+2*U1*Y3-U1*Y2))+F3*(X2*HEL
     &   -2*X3*HEL+2*GEL*Y3-GEL*Y2)*(8*X2*V3+8*X2*V2+2*X2*V1-4*
     &   X3*V3-4*X3*V2-X3*V1+4*U3*Y3-8*U3*Y2+4*U2*Y3-8*U2*Y2+
     &   U1*Y3-2*U1*Y2)-3*F4*((X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*(X2*V3+
     &   4*X2*V2+4*X2*V1-U3*Y2-4*U2*Y2-4*U1*Y2)+(X2*HEL-2*X3*HEL
     &   +2*GEL*Y3-GEL*Y2)*(4*X2*V3+4*X2*V2+X2*V1-4*X3*V3-4*X3*
     &   V2-X3*V1+4*U3*Y3-4*U3*Y2+4*U2*Y3-4*U2*Y2+U1*Y3-U1*Y2)
     &   ))*XMUL/(54*(X2*Y3-X3*Y2))
C
      W3(IELEM) = (F2*(2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(4*X2*V3+4
     &   *X2*V2+X2*V1-8*X3*V3-8*X3*V2-2*X3*V1+8*U3*Y3-4*U3*Y2
     &   +8*U2*Y3-4*U2*Y2+2*U1*Y3-U1*Y2)+F3*((2*X2*HEL-X3*HEL+GEL
     &   *Y3-2*GEL*Y2)*(8*X2*V3+8*X2*V2+2*X2*V1-4*X3*V3-4*X3*
     &   V2-X3*V1+4*U3*Y3-8*U3*Y2+4*U2*Y3-8*U2*Y2+U1*Y3-2*U1*
     &   Y2)+(X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*(4*X2*V3+X2*V2+4*X2*V1+4
     &   *X3*V3+X3*V2+4*X3*V1-4*U3*Y3-4*U3*Y2-U2*Y3-U2*Y2-4*U1
     &   *Y3-4*U1*Y2))-3*F4*((2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(4
     &   *X2*V3+4*X2*V2+X2*V1-4*X3*V3-4*X3*V2-X3*V1+4*U3*Y3-4
     &   *U3*Y2+4*U2*Y3-4*U2*Y2+U1*Y3-U1*Y2)+(X2*HEL+X3*HEL-GEL*Y3-
     &   GEL*Y2)*(4*X3*V3+X3*V2+4*X3*V1-4*U3*Y3-U2*Y3-4*U1*Y3))
     &   )*XMUL/(54*(X2*Y3-X3*Y2))
C
      W4(IELEM) = (-(((X2*HEL-X3*HEL+GEL*Y3-GEL*Y2)*(8*X2*V3+8*X2*V2
     &   +2*X2*V1-4*X3*V3-4*X3*V2-X3*V1+4*U3*Y3-8*U3*Y2+4*U2
     &   *Y3-8*U2*Y2+U1*Y3-2*U1*Y2)+(4*X2*V3+X2*V2+4*X2*V1+4*
     &   X3*V3+X3*V2+4*X3*V1-4*U3*Y3-4*U3*Y2-U2*Y3-U2*Y2-4*U1*
     &   Y3-4*U1*Y2)*(X3*HEL-GEL*Y3))*F3-3*((X2*HEL-X3*HEL+GEL*Y3-GEL*
     &   Y2)*(4*X2*V3+4*X2*V2+X2*V1-4*X3*V3-4*X3*V2-X3*V1+4*
     &   U3*Y3-4*U3*Y2+4*U2*Y3-4*U2*Y2+U1*Y3-U1*Y2)+(X2*HEL-GEL*
     &   Y2)*(X2*V3+4*X2*V2+4*X2*V1-U3*Y2-4*U2*Y2-4*U1*Y2)+(X3
     &   *HEL-GEL*Y3)*(4*X3*V3+X3*V2+4*X3*V1-4*U3*Y3-U2*Y3-4*U1*
     &   Y3))*F4+((X2*HEL-X3*HEL+GEL*Y3-GEL*Y2)*(4*X2*V3+4*X2*V2+X2*
     &   V1-8*X3*V3-8*X3*V2-2*X3*V1+8*U3*Y3-4*U3*Y2+8*U2*Y3-
     &   4*U2*Y2+2*U1*Y3-U1*Y2)+(X2*HEL-GEL*Y2)*(X2*V3+4*X2*V2+4*
     &   X2*V1+X3*V3+4*X3*V2+4*X3*V1-U3*Y3-U3*Y2-4*U2*Y3-4*U2*
     &   Y2-4*U1*Y3-4*U1*Y2))*F2))*XMUL/(18*(X2*Y3-X3*Y2))

C
3     CONTINUE
C
C-----------------------------------------------------------------------
C
C     F IS QUASI-B; G AND H (NOT CHECKED) P0; AND U, V (NOT CHECKED) QUASI-B
C
      ELSEIF(IELMF.EQ.12.AND.IELMG.EQ.10.AND.IELMH.EQ.10.
     &                   AND.IELMU.EQ.12.AND.IELMV.EQ.12  ) THEN
C
      DO 4 IELEM = 1 , NELEM
C
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM)) - F1
      F3 = F(IKLE3(IELEM)) - F1
      F4 = F(IKLE4(IELEM)) - F1
C
      U1 = U(IKLE1(IELEM))
      U2 = U(IKLE2(IELEM))
      U3 = U(IKLE3(IELEM))
      U4 = U(IKLE4(IELEM))
C
      V1 = V(IKLE1(IELEM))
      V2 = V(IKLE2(IELEM))
      V3 = V(IKLE3(IELEM))
      V4 = V(IKLE4(IELEM))
C
      GEL = G(IELEM)
      HEL = H(IELEM)
C
      W1(IELEM) = (-(3*((2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V4+X2
     &   *V2+X2*V1-U4*Y2-U2*Y2-U1*Y2)-(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL*
     &   Y2)*(X3*V3+X3*V4+X3*V1-U3*Y3-U4*Y3-U1*Y3))*F4-(2*X2*HEL-
     &   X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V4+X2*V2+X2*V1+X3*V4+X3*V2+X3*
     &   V1-U4*Y3-U4*Y2-U2*Y3-U2*Y2-U1*Y3-U1*Y2)*F2+(X2*HEL-2*X3*
     &   HEL+2*GEL*Y3-GEL*Y2)*(X2*V3+X2*V4+X2*V1+X3*V3+X3*V4+X3*V1-
     &   U3*Y3-U3*Y2-U4*Y3-U4*Y2-U1*Y3-U1*Y2)*F3))
     &   *XMUL/(18*(X2*Y3-X3*Y2))
C
      W2(IELEM) = (F2*((X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*(X2*V4+X2*V2+X2*
     &   V1+X3*V4+X3*V2+X3*V1-U4*Y3-U4*Y2-U2*Y3-U2*Y2-U1*Y3-U1*Y2)
     &   +(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL*Y2)*(X2*V3+X2*V4+X2*V2-2*X3
     &   *V3-2*X3*V4-2*X3*V2+2*U3*Y3-U3*Y2+2*U4*Y3-U4*Y2+2*U2
     &   *Y3-U2*Y2))+F3*(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL*Y2)*(2*X2*V3+
     &   2*X2*V4+2*X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-2*U3*Y2+U4*Y3-
     &   2*U4*Y2+U2*Y3-2*U2*Y2)-3*F4*((X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*
     &   (X2*V4+X2*V2+X2*V1-U4*Y2-U2*Y2-U1*Y2)+(X2*HEL-2*X3*HEL+2*
     &   GEL*Y3-GEL*Y2)*(X2*V3+X2*V4+X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-
     &   U3*Y2+U4*Y3-U4*Y2+U2*Y3-U2*Y2)))*XMUL/(18*(X2*Y3-X3*Y2))
C
      W3(IELEM) = (F2*(2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V3+X2*V4
     &   +X2*V2-2*X3*V3-2*X3*V4-2*X3*V2+2*U3*Y3-U3*Y2+2*U4*Y3
     &   -U4*Y2+2*U2*Y3-U2*Y2)+F3*((2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2
     &   )*(2*X2*V3+2*X2*V4+2*X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-2*
     &   U3*Y2+U4*Y3-2*U4*Y2+U2*Y3-2*U2*Y2)+(X2*HEL+X3*HEL-GEL*Y3-
     &   GEL*Y2)*(X2*V3+X2*V4+X2*V1+X3*V3+X3*V4+X3*V1-U3*Y3-U3*Y2-
     &   U4*Y3-U4*Y2-U1*Y3-U1*Y2))-3*F4*((2*X2*HEL-X3*HEL+GEL*Y3-2
     &   *GEL*Y2)*(X2*V3+X2*V4+X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-U3*Y2+
     &   U4*Y3-U4*Y2+U2*Y3-U2*Y2)+(X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*
     &   (X3*V3+X3*V4+X3*V1-U3*Y3-U4*Y3-U1*Y3)))
     &   *XMUL/(18*(X2*Y3-X3*Y2))
C
      W4(IELEM) = (-(((X2*HEL-X3*HEL+GEL*Y3-GEL*Y2)*(2*X2*V3+2*X2*V4
     &   +2*X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-2*U3*Y2+U4*Y3-2*U4*Y2
     &   +U2*Y3-2*U2*Y2)+(X2*V3+X2*V4+X2*V1+X3*V3+X3*V4+X3*V1-U3*
     &   Y3-U3*Y2-U4*Y3-U4*Y2-U1*Y3-U1*Y2)*(X3*HEL-GEL*Y3))*F3-3*((
     &   X2*HEL-X3*HEL+GEL*Y3-GEL*Y2)*(X2*V3+X2*V4+X2*V2-X3*V3-X3*V4-
     &   X3*V2+U3*Y3-U3*Y2+U4*Y3-U4*Y2+U2*Y3-U2*Y2)+(X2*HEL-GEL*Y2)*
     &   (X2*V4+X2*V2+X2*V1-U4*Y2-U2*Y2-U1*Y2)+(X3*HEL-GEL*Y3)*(X3*
     &   V3+X3*V4+X3*V1-U3*Y3-U4*Y3-U1*Y3))*F4+((X2*HEL-X3*HEL+GEL*Y3
     &   -GEL*Y2)*(X2*V3+X2*V4+X2*V2-2*X3*V3-2*X3*V4-2*X3*V2+2*
     &   U3*Y3-U3*Y2+2*U4*Y3-U4*Y2+2*U2*Y3-U2*Y2)+(X2*HEL-GEL*Y2)*
     &   (X2*V4+X2*V2+X2*V1+X3*V4+X3*V2+X3*V1-U4*Y3-U4*Y2-U2*Y3-U2
     &   *Y2-U1*Y3-U1*Y2))*F2))*XMUL/(6*(X2*Y3-X3*Y2))
C
4     CONTINUE
C
C-----------------------------------------------------------------------
      ELSE
C-----------------------------------------------------------------------
C
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.1) WRITE(LU,110) IELMG,SG%NAME
       IF (LNG.EQ.1) WRITE(LU,200) IELMU,SU%NAME
       IF (LNG.EQ.1) WRITE(LU,300)
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,111) IELMG,SG%NAME
       IF (LNG.EQ.1) WRITE(LU,201) IELMU,SU%NAME
       IF (LNG.EQ.1) WRITE(LU,301)
100    FORMAT(1X,'VC03BB (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,
     &        1X,'NOM REEL : ',A6)
110    FORMAT(1X,'DISCRETISATION DE G : ',1I6,
     &        1X,'NOM REEL : ',A6)
200    FORMAT(1X,'DISCRETISATION DE U : ',1I6,
     &        1X,'NOM REEL : ',A6)
300    FORMAT(1X,'CAS NON PREVU')
101    FORMAT(1X,'VC03BB (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F:',1I6,
     &        1X,'REAL NAME: ',A6)
111    FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &        1X,'REAL NAME: ',A6)
201    FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &        1X,'REAL NAME: ',A6)
301    FORMAT(1X,'CASE NOT IMPLEMENTED')
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
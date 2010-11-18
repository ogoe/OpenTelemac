C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)<br>
!>                       /            DF
!>    VEC(I)  =  XMUL   /     ( P  *( --  )) D(OMEGA)
!>                     /OMEGA    I    DX<br>
!>    P   IS A LINEAR BASE
!>     I<br>
!>    F IS A VECTOR OF TYPE P1 OR OTHER
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!>                     HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!>                     P1, BUT GIVEN BY ELEMENTS.
!>                     THE SIZE OF F SHOULD THEN BE : F(NELMAX,3).

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FORMUL, ICOORD, IKLE1, IKLE2, IKLE3, IKLE4, IKLE5, IKLE6, NELEM, NELMAX, SF, W1, W2, W3, W4, W5, W6, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F2, F3, F4, F5, F6, I1, I2, I3, I4, I5, I6, IELEM, IELMF, X2, X3, XMU, XS144, XS48, Y2, Y3, Z1, Z2, Z3, Z4, Z5, Z6
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 19/05/2010
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td> NEW FILTER FOR PARTLY CRUSHED ELEMENTS
!>           IF FORMUL(7:7).EQ.'2'
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
!></td><td>--></td><td>SEE AT THE END OF THE SUBROUTINE
!>                  THE USE OF FORMUL(6:6) AND FORMUL(7:7)
!>                  FOR APPLYING FILTERS.
!>    </td></tr>
!>          <tr><td>ICOORD
!></td><td>--></td><td>COORDONNEE SUIVANT LAQUELLE ON DERIVE.
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
!>          <tr><td>IKLE5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE6
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
!></td><td><--</td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
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
!>          <tr><td>W5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC13PP
     &(XMUL,SF,F,X,Y,Z,
     & IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     & W1,W2,W3,W4,W5,W6,ICOORD,FORMUL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| FORMUL         |-->| SEE AT THE END OF THE SUBROUTINE
C|                |   | THE USE OF FORMUL(6:6) AND FORMUL(7:7)
C|                |   | FOR APPLYING FILTERS.
C| ICOORD         |-->| COORDONNEE SUIVANT LAQUELLE ON DERIVE.
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| IKLE5          |---| 
C| IKLE6          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G ET H
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W1,2,3         |<--| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| W4             |---| 
C| W5             |---| 
C| W6             |---| 
C| X             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C| Y             |---| 
C| Z             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF  !, EX_VC13PP => VC13PP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) ::W4(NELMAX),W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
C
C     STRUCTURE OF F AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XS48,XS144,F1,F2,F3,F4,F5,F6,XMU
      DOUBLE PRECISION X2,X3,Y2,Y3,Z1,Z2,Z3,Z4,Z5,Z6
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,IELMF
C
      INTRINSIC MAX,MIN
C
C-----------------------------------------------------------------------
C
      XS48  = XMUL/48.D0
      XS144 = XMUL/144.D0
C
C-----------------------------------------------------------------------
C
      IELMF=SF%ELM
C
C=======================================================================
C
C     F IS LINEAR
C
      IF(IELMF.EQ.41) THEN
C
      IF(ICOORD.EQ.1) THEN
C
C-----------------------------------------------------------------------
C
C  DERIVATIVE WRT X
C
      DO 3 IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
         I5 = IKLE5(IELEM)
         I6 = IKLE6(IELEM)
C
         F1 = F(I1)
         F2 = F(I2)
         F3 = F(I3)
         F4 = F(I4)
         F5 = F(I5)
         F6 = F(I6)
C
C  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
C
         Y2  =  Y(I2) - Y(I1)
         Y3  =  Y(I3) - Y(I1)
         Z2  =  Z(I2) - Z(I1)
         Z3  =  Z(I3) - Z(I1)
         Z4  =  Z(I4) - Z(I1)
         Z5  =  Z(I5) - Z(I1)
         Z6  =  Z(I6) - Z(I1)
C
      W1(IELEM)=( (2*F1-F6)*Y2*( Z5+3*Z4-3*Z3  -Z2)
     &           +(2*F1-F5)*Y3*(-Z6-3*Z4  +Z3+3*Z2)
     &                  +F2*Y3*(2*Z6+3*Z5+3*Z4-2*Z3)
     &                  +F3*Y2*(-3*Z6-2*Z5-3*Z4+2*Z2)
     &             +(F3-F6)*Y3*(Z5-Z4+2*Z2)
     &                  +F4*Y2*(3*Z6+Z5+3*Z3-Z2)
     &                  +F4*Y3*(-Z6-3*Z5+Z3-3*Z2)
     &             +(F5-F2)*Y2*(Z6-Z4+2*Z3) )*XS144
      W2(IELEM)=(   F1*Y2*(Z6+4*Z5+3*Z4-4*Z3-4*Z2)
     &             +F1*Y3*(-2*Z6-3*Z5-3*Z4+2*Z3+6*Z2)
     &      +(2*F2-F4)*Y3*(Z6+3*Z5-Z3)
     &             +F3*Y2*(-3*Z6-4*Z5-Z4+4*Z2)
     &             +F4*Y2*(2*Z6+2*Z5+Z3-2*Z2)
     &      +2*(F5-F2)*Y2*(Z6-Z4+2*Z3)
     &             +F5*Y3*(Z6+3*Z4-Z3-6*Z2)
     &             +F6*Y2*(-2*Z5-2*Z4+3*Z3+2*Z2)
     &        +(F6-F3)*Y3*(-Z5+Z4-2*Z2) )*XS144
      W3(IELEM)=(   F1*Y2*(3*Z6+2*Z5+3*Z4-6*Z3-2*Z2)
     &             +F1*Y3*(-4*Z6-Z5-3*Z4+4*Z3+4*Z2)
     &             +F2*Y3*(4*Z6+3*Z5+Z4-4*Z3)
     &      +(2*F3-F4)*Y2*(-3*Z6-Z5+Z2)
     &             +F4*Y3*(-2*Z6-2*Z5+2*Z3-Z2)
     &        +(F5-F2)*Y2*(Z6-Z4+2*Z3)
     &             +F5*Y3*(2*Z6+2*Z4-2*Z3-3*Z2)
     &             +F6*Y2*(-Z5-3*Z4+6*Z3+Z2)
     &      +2*(F6-F3)*Y3*(-Z5+Z4-2*Z2) )*XS144
      W4(IELEM)=(   F1*Y2*(-3*Z6+Z5+6*Z4-3*Z3-Z2)
     &             +F1*Y3*(-Z6+3*Z5-6*Z4+Z3+3*Z2)
     &             +F2*Y3*(Z6+3*Z5-Z3)
     &      +(2*F4-F3)*Y2*(3*Z6+Z5-Z2)
     &           +2*F4*Y3*(-Z6-3*Z5+Z3)
     &        +(F5-F2)*Y2*(2*Z6-2*Z4+Z3)
     &             +F5*Y3*(2*Z6+6*Z4-2*Z3-3*Z2)
     &             +F6*Y2*(-2*Z5-6*Z4+3*Z3+2*Z2)
     &        +(F6-F3)*Y3*(-2*Z5+2*Z4-Z2)  )*XS144
      W5(IELEM)=(   F1*Y2*(-Z6+2*Z5+3*Z4-2*Z3-2*Z2)
     &             +F2*Y3*(Z6+6*Z5-3*Z4-Z3)
     &             +F3*Y2*(-3*Z6-2*Z5+Z4+2*Z2)
     &             +F4*Y2*(4*Z6+4*Z5-Z3-4*Z2)
     &             +F4*Y3*(-2*Z6-6*Z5+2*Z3+3*Z2)
     &      +2*(F5-F2)*Y2*(2*Z6-2*Z4+Z3)
     &      +(2*F5-F1)*Y3*(Z6+3*Z4-Z3-3*Z2)
     &             +F6*Y2*(-4*Z5-4*Z4+3*Z3+4*Z2)
     &        +(F6-F3)*Y3*(-2*Z5+2*Z4-Z2) )*XS144
      W6(IELEM)=(  +F1*Y3*(-2*Z6+Z5-3*Z4+2*Z3+2*Z2)
     &             +F2*Y3*(2*Z6+3*Z5-Z4-2*Z3)
     &             +F3*Y2*(-6*Z6-Z5+3*Z4+Z2)
     &             +F4*Y2*(6*Z6+2*Z5-3*Z3-2*Z2)
     &             +F4*Y3*(-4*Z6-4*Z5+4*Z3+Z2)
     &        +(F5-F2)*Y2*(2*Z6-2*Z4+Z3)
     &             +F5*Y3*(4*Z6+4*Z4-4*Z3-3*Z2)
     &      +(2*F6-F1)*Y2*(-Z5-3*Z4+3*Z3+Z2)
     &      +2*(F6-F3)*Y3*(-2*Z5+2*Z4-Z2) )*XS144
C
3     CONTINUE
C
      ELSEIF(ICOORD.EQ.2) THEN
C
C-----------------------------------------------------------------------
C
C  DERIVATIVE WRT Y
C
      DO 4 IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
         I5 = IKLE5(IELEM)
         I6 = IKLE6(IELEM)
C
         F1 = F(I1)
         F2 = F(I2)
         F3 = F(I3)
         F4 = F(I4)
         F5 = F(I5)
         F6 = F(I6)
C
C  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
C
         X2  =  X(I2) - X(I1)
         X3  =  X(I3) - X(I1)
         Z2  =  Z(I2) - Z(I1)
         Z3  =  Z(I3) - Z(I1)
         Z4  =  Z(I4) - Z(I1)
         Z5  =  Z(I5) - Z(I1)
         Z6  =  Z(I6) - Z(I1)
C
      W1(IELEM)=( (2*F1-F6)*X2*(-Z5-3*Z4+3*Z3+Z2)
     &                +2*F1*X3*(Z6+3*Z4-Z3-3*Z2)
     &                  +F2*X3*(-2*Z6-3*Z5-3*Z4+2*Z3)
     &                  +F3*X2*(3*Z6+2*Z5+3*Z4-2*Z2)
     &                  +F4*X2*(-3*Z6-Z5-3*Z3+Z2)
     &                  +F4*X3*(Z6+3*Z5-Z3+3*Z2)
     &             +(F5-F2)*X2*(-Z6+Z4-2*Z3)
     &                  +F5*X3*(-Z6-3*Z4+Z3+3*Z2)
     &             +(F6-F3)*X3*(Z5-Z4+2*Z2) )*XS144
      W2(IELEM)=( F1*X2*(-Z6-4*Z5-3*Z4+4*Z3+4*Z2)
     &           +F1*X3*(2*Z6+3*Z5+3*Z4-2*Z3-6*Z2)
     &    +(2*F2-F4)*X3*(-Z6-3*Z5+Z3)
     &           +F3*X2*(3*Z6+4*Z5+Z4-4*Z2)
     &           +F4*X2*(-2*Z6-2*Z5-Z3+2*Z2)
     &    +2*(F5-F2)*X2*(-Z6+Z4-2*Z3)
     &           +F5*X3*(-Z6-3*Z4+Z3+6*Z2)
     &           +F6*X2*(2*Z5+2*Z4-3*Z3-2*Z2)
     &      +(F6-F3)*X3*(Z5-Z4+2*Z2) )*XS144
      W3(IELEM)=( F1*X2*(-3*Z6-2*Z5-3*Z4+6*Z3+2*Z2)
     &           +F1*X3*(4*Z6+Z5+3*Z4-4*Z3-4*Z2)
     &           +F2*X3*(-4*Z6-3*Z5-Z4+4*Z3)
     &    +(2*F3-F4)*X2*(3*Z6+Z5-Z2)
     &           +F4*X3*(2*Z6+2*Z5-2*Z3+Z2)
     &      +(F5-F2)*X2*(-Z6+Z4-2*Z3)
     &           +F5*X3*(-2*Z6-2*Z4+2*Z3+3*Z2)
     &           +F6*X2*(Z5+3*Z4-6*Z3-Z2)
     &    +2*(F6-F3)*X3*(Z5-Z4+2*Z2) )*XS144
      W4(IELEM)=( F1*X2*(3*Z6-Z5-6*Z4+3*Z3+Z2)
     &           +F1*X3*(Z6-3*Z5+6*Z4-Z3-3*Z2)
     &    +(2*F4-F3)*X2*(-3*Z6-Z5+Z2)
     &    +(2*F4-F2)*X3*(Z6+3*Z5-Z3)
     &      +(F5-F2)*X2*(-2*Z6+2*Z4-Z3)
     &           +F5*X3*(-2*Z6-6*Z4+2*Z3+3*Z2)
     &           +F6*X2*(2*Z5+6*Z4-3*Z3-2*Z2)
     &      +(F6-F3)*X3*(2*Z5-2*Z4+Z2) )*XS144
      W5(IELEM)=( F1*X2*(Z6-2*Z5-3*Z4+2*Z3+2*Z2)
     &           +F2*X3*(-Z6-6*Z5+3*Z4+Z3)
     &           +F3*X2*(3*Z6+2*Z5-Z4-2*Z2)
     &           +F4*X2*(-4*Z6-4*Z5+Z3+4*Z2)
     &           +F4*X3*(2*Z6+6*Z5-2*Z3-3*Z2)
     &    +2*(F5-F2)*X2*(-2*Z6+2*Z4-Z3)
     &    +(2*F5-F1)*X3*(-Z6-3*Z4+Z3+3*Z2)
     &           +F6*X2*(4*Z5+4*Z4-3*Z3-4*Z2)
     &      +(F6-F3)*X3*(2*Z5-2*Z4+Z2)  )*XS144
      W6(IELEM)=(+F1*X3*(2*Z6-Z5+3*Z4-2*Z3-2*Z2)
     &           +F2*X3*(-2*Z6-3*Z5+Z4+2*Z3)
     &           +F3*X2*(6*Z6+Z5-3*Z4-Z2)
     &           +F4*X2*(-6*Z6-2*Z5+3*Z3+2*Z2)
     &           +F4*X3*(4*Z6+4*Z5-4*Z3-Z2)
     &      +(F5-F2)*X2*(-2*Z6+2*Z4-Z3)
     &           +F5*X3*(-4*Z6-4*Z4+4*Z3+3*Z2)
     &    +(2*F6-F1)*X2*(Z5+3*Z4-3*Z3-Z2)
     &    +2*(F6-F3)*X3*(2*Z5-2*Z4+Z2)  )*XS144
C
C
4        CONTINUE
C
      ELSEIF(ICOORD.EQ.3) THEN
C
C-----------------------------------------------------------------------
C
C  DERIVATIVE WRT Z
C
      DO 5 IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
         I5 = IKLE5(IELEM)
         I6 = IKLE6(IELEM)
C
         F1 = F(I1)
         F2 = F(I2)
         F3 = F(I3)
         F4 = F(I4)
         F5 = F(I5)
         F6 = F(I6)
C
C  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
C
         X2  =  X(I2) - X(I1)
         X3  =  X(I3) - X(I1)
         Y2  =  Y(I2) - Y(I1)
         Y3  =  Y(I3) - Y(I1)
C
         XMU  = XS48*(X2*Y3-X3*Y2)
C
C        NOT LUMPED VERSION
C        DIFF = (F4+F5+F6) - (F1+F2+F3)
C        W1(IELEM)=(F4-F1+DIFF)*XMU
C        W2(IELEM)=(F5-F2+DIFF)*XMU
C        W3(IELEM)=(F6-F3+DIFF)*XMU
C        LUMPED VERSION (LIKE THE DIFFUSION MATRIX)
C        SEE W COMPUTATION IN TELEMAC-3D IN PROVEL
         W1(IELEM)=4*(F4-F1)*XMU
         W2(IELEM)=4*(F5-F2)*XMU
         W3(IELEM)=4*(F6-F3)*XMU
!
         W4(IELEM)=W1(IELEM)
         W5(IELEM)=W2(IELEM)
         W6(IELEM)=W3(IELEM)
C
5     CONTINUE
C
      ELSE
C
C-----------------------------------------------------------------------
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
200       FORMAT(1X,'VC13PP (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'VC13PP (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(1)
          STOP
C
      ENDIF
C
C=======================================================================
C
      ELSE
C
C=======================================================================
C
       IF (LNG.EQ.1) WRITE(LU,101) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,102) IELMF,SF%NAME
101    FORMAT(1X,'VC13PP (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,' CAS NON PREVU',/,
     &        1X,'NOM REEL DE F : ',A6)
102    FORMAT(1X,'VC13PP (BIEF) :',/,
     &        1X,'DISCRETISATION OF F : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF F: ',A6)
       CALL PLANTE(1)
       STOP
C
      ENDIF
C
C=======================================================================
C
C     HYDROSTATIC INCONSISTENCIES
C
C     COMMON TREATMENT FOR FILTERS 2,3 AND 4
C
      IF(FORMUL(6:6).EQ.'2'.OR.FORMUL(6:6).EQ.'3'.OR.
     &   FORMUL(6:6).EQ.'4'     ) THEN
C
        DO IELEM = 1 , NELEM
C
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
C
          IF(MAX(Z(I1),Z(I2),Z(I3)).GT.MIN(Z(I4),Z(I5),Z(I6))) THEN
            W1(IELEM)=0.D0
            W2(IELEM)=0.D0
            W3(IELEM)=0.D0
            W4(IELEM)=0.D0
            W5(IELEM)=0.D0
            W6(IELEM)=0.D0
          ENDIF
C
        ENDDO
C
      ENDIF
C
C     FILTER 3
C
      IF(FORMUL(6:6).EQ.'3'.AND.(ICOORD.EQ.1.OR.ICOORD.EQ.2)) THEN
C
        DO IELEM = 1 , NELEM
C
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
C
          F1 = F(I1)
          F2 = F(I2)
          F3 = F(I3)
          F4 = F(I4)
          F5 = F(I5)
          F6 = F(I6)
C
C         IF THERE IS A POSSIBILITY OF STRATIFICATION
C         GRADIENTS CANCELLED
C
          IF( MIN(MAX(F1,F4),MAX(F2,F5),MAX(F3,F6)).GE.
     &        MAX(MIN(F1,F4),MIN(F2,F5),MIN(F3,F6))     ) THEN
            W1(IELEM)=0.D0
            W2(IELEM)=0.D0
            W3(IELEM)=0.D0
            W4(IELEM)=0.D0
            W5(IELEM)=0.D0
            W6(IELEM)=0.D0
          ENDIF
C
        ENDDO
C
      ENDIF
C
C     FILTER 4
C
      IF(FORMUL(6:6).EQ.'4'.AND.(ICOORD.EQ.1.OR.ICOORD.EQ.2)) THEN
C
        DO IELEM = 1 , NELEM
C
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
C
          F1 = F(I1)
          F2 = F(I2)
          F3 = F(I3)
          F4 = F(I4)
          F5 = F(I5)
          F6 = F(I6)
C
          Z1 = Z(I1)
          Z2 = Z(I2)
          Z3 = Z(I3)
          Z4 = Z(I4)
          Z5 = Z(I5)
          Z6 = Z(I6)
C
C         CHECKS IF A STRATIFICATION IS IMPOSSIBLE
C         IN THIS CASE (GO TO 1000) GRADIENTS ARE KEPT
C
C         1 IN BETWEEN 3 AND 6
          IF(Z1.GE.Z3.AND.Z1.LE.Z6) THEN
            IF(F1.LT.MIN(F3,F6).OR.F1.GT.MAX(F3,F6)) GO TO 1000
          ENDIF
C         1 IN BETWEEN 2 AND 5
          IF(Z1.GE.Z2.AND.Z1.LE.Z5) THEN
            IF(F1.LT.MIN(F2,F5).OR.F1.GT.MAX(F2,F5)) GO TO 1000
          ENDIF
C         2 IN BETWEEN 1 AND 4
          IF(Z2.GE.Z1.AND.Z2.LE.Z4) THEN
            IF(F2.LT.MIN(F1,F4).OR.F2.GT.MAX(F1,F4)) GO TO 1000
          ENDIF
C         2 IN BETWEEN 3 AND 6
          IF(Z2.GE.Z3.AND.Z2.LE.Z6) THEN
            IF(F2.LT.MIN(F3,F6).OR.F2.GT.MAX(F3,F6)) GO TO 1000
          ENDIF
C         3 IN BETWEEN 1 AND 4
          IF(Z3.GE.Z1.AND.Z3.LE.Z4) THEN
            IF(F3.LT.MIN(F1,F4).OR.F3.GT.MAX(F1,F4)) GO TO 1000
          ENDIF
C         3 IN BETWEEN 2 AND 5
          IF(Z3.GE.Z2.AND.Z3.LE.Z5) THEN
            IF(F3.LT.MIN(F2,F5).OR.F3.GT.MAX(F2,F5)) GO TO 1000
          ENDIF
C         4 IN BETWEEN 2 AND 5
          IF(Z4.GE.Z2.AND.Z4.LE.Z5) THEN
            IF(F4.LT.MIN(F2,F5).OR.F4.GT.MAX(F2,F5)) GO TO 1000
          ENDIF
C         4 IN BETWEEN 3 AND 6
          IF(Z4.GE.Z3.AND.Z4.LE.Z6) THEN
            IF(F4.LT.MIN(F3,F6).OR.F4.GT.MAX(F3,F6)) GO TO 1000
          ENDIF
C         5 IN BETWEEN 1 AND 4
          IF(Z5.GE.Z1.AND.Z5.LE.Z4) THEN
            IF(F5.LT.MIN(F1,F4).OR.F5.GT.MAX(F1,F4)) GO TO 1000
          ENDIF
C         5 IN BETWEEN 3 AND 6
          IF(Z5.GE.Z3.AND.Z5.LE.Z6) THEN
            IF(F5.LT.MIN(F3,F6).OR.F5.GT.MAX(F3,F6)) GO TO 1000
          ENDIF
C         6 IN BETWEEN 1 AND 4
          IF(Z6.GE.Z1.AND.Z6.LE.Z4) THEN
            IF(F6.LT.MIN(F1,F4).OR.F6.GT.MAX(F1,F4)) GO TO 1000
          ENDIF
C         6 IN BETWEEN 2 AND 5
          IF(Z6.GE.Z2.AND.Z6.LE.Z5) THEN
            IF(F6.LT.MIN(F2,F5).OR.F6.GT.MAX(F2,F5)) GO TO 1000
          ENDIF
C
C         SO THERE IS A POSSIBILITY OF STRATIFICATION
C         GRADIENTS CANCELLED
C
          W1(IELEM)=0.D0
          W2(IELEM)=0.D0
          W3(IELEM)=0.D0
          W4(IELEM)=0.D0
          W5(IELEM)=0.D0
          W6(IELEM)=0.D0
C
1000      CONTINUE
C
        ENDDO
C
      ENDIF
C
C     FILTER FOR PARTLY CRUSHED PRISMS
C
      IF(FORMUL(7:7).EQ.'2') THEN
C
        DO IELEM = 1 , NELEM
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
          IF(Z(I4)-Z(I1).LT.1.D-3.OR.
     &       Z(I5)-Z(I2).LT.1.D-3.OR.
     &       Z(I6)-Z(I3).LT.1.D-3     ) THEN
            W1(IELEM)=0.D0
            W2(IELEM)=0.D0
            W3(IELEM)=0.D0
            W4(IELEM)=0.D0
            W5(IELEM)=0.D0
            W6(IELEM)=0.D0
          ENDIF
        ENDDO
C
      ENDIF
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C
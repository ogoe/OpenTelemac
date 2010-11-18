C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)<br>
!>                       /            DF
!>    VEC(I)  =  XMUL   /  ( G  P  *( --  )) D(OMEGA)
!>                     /OMEGA    I    DX<br><br>
!>    P   IS A LINEAR BASE
!>     I<br>
!>    F IS A VECTOR OF TYPE P1 OR OTHER
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!>  <br>               HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!>                     P1, BUT GIVEN BY ELEMENTS.
!>  <br>               THE SIZE OF F SHOULD THEN BE : F(NELMAX,4).

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, G, ICOORD, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, SG, W1, W2, W3, W4, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F2, F2MF1, F3, F3MF1, F4, F4MF1, G1, G2, G2MG1, G3, G3MG1, G4, G4MG1, I1, I2, I3, I4, IELEM, IELMF, IELMG, X2, X3, X4, XSUR120, Y2, Y3, Y4, Z2, Z3, Z4
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
!>      <td><center> 5.3                                       </center>
!> </td><td> 25/03/02
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/04/98
!> </td><td> ARNAUD DESITTER - UNIVERSITY OF BRISTOL
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
!>          <tr><td>W1,2,3,4
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
                        SUBROUTINE VC11TT
     &( XMUL,SF,SG,F,G,X,Y,Z,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,
     &  W1,W2,W3,W4,ICOORD )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| ICOORD         |-->| COORDONNEE SUIVANT LAQUELLE ON DERIVE.
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
C| W1,2,3,4       |<--| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| W4             |---| 
C| X             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C| Y             |---| 
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
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*),XMUL
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
C
C     STRUCTURES OF F, G, H, U, V, W AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C LOCAL VARIABLES
C
      INTEGER IELEM,IELMF,IELMG
      DOUBLE PRECISION F1,F2,F3,F4
      DOUBLE PRECISION G1,G2,G3,G4,X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4
      INTEGER I1,I2,I3,I4
C
      DOUBLE PRECISION XSUR120,F2MF1,F3MF1,F4MF1,G2MG1,G3MG1,G4MG1
C
C-----------------------------------------------------------------------
C INITIALISES
C
      XSUR120 = XMUL/120.D0
C
      IELMF = SF%ELM
      IELMG = SG%ELM
C
C-----------------------------------------------------------------------
C     F AND G ARE LINEAR
C
      IF ((IELMF.EQ.31.AND.IELMG.EQ.31).OR.
     &    (IELMF.EQ.51.AND.IELMG.EQ.51)     ) THEN
C
      IF (ICOORD.EQ.1) THEN
C
C-----------------------------------------------------------------------
C  DERIVATIVE WRT X
C
      DO  IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
C
         F1 = F(I1)
         F2 = F(I2)
         F3 = F(I3)
         F4 = F(I4)
C
         G1 = G(I1)
         G2 = G(I2)
         G3 = G(I3)
         G4 = G(I4)
C
         F2MF1 = F2-F1
         F3MF1 = F3-F1
         F4MF1 = F4-F1
         G2MG1 = G2-G1
         G3MG1 = G3-G1
         G4MG1 = G4-G1
C
C  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
C
         Y2  =  Y(I2) - Y(I1)
         Y3  =  Y(I3) - Y(I1)
         Y4  =  Y(I4) - Y(I1)
         Z2  =  Z(I2) - Z(I1)
         Z3  =  Z(I3) - Z(I1)
         Z4  =  Z(I4) - Z(I1)
C
         W1(IELEM) = (
     & (5*F2MF1*G1+F2MF1*G2MG1+F2MF1*G3MG1+F2MF1*G4MG1)*(Y3*Z4-Y4*Z3)
     &+(5*F3MF1*G1+F3MF1*G2MG1+F3MF1*G3MG1+F3MF1*G4MG1)*(Z2*Y4-Y2*Z4)
     &+(5*F4MF1*G1+F4MF1*G2MG1+F4MF1*G3MG1+F4MF1*G4MG1)*(Y2*Z3-Z2*Y3)
     &               ) * XSUR120
C
         W2(IELEM) = (
     &-F4MF1*Z2*Y3*G4MG1+F4MF1*Y2*Z3*G4MG1+F3MF1*Z2*Y4*G4MG1
     &-F3MF1*Y2*Z4*G4MG1+F2MF1*Y3*Z4*G4MG1-F2MF1*Y4*Z3*G4MG1
     &+5*F2MF1*Y3*Z4*G1+2*F2MF1*Y3*Z4*G2MG1+F2MF1*Y3*Z4*G3MG1
     &-5*F2MF1*Y4*Z3*G1-2*F2MF1*Y4*Z3*G2MG1-F2MF1*Y4*Z3*G3MG1
     &-5*F3MF1*Y2*Z4*G1-2*F3MF1*Y2*Z4*G2MG1-F3MF1*Y2*Z4*G3MG1
     &+5*F3MF1*Z2*Y4*G1+2*F3MF1*Z2*Y4*G2MG1+F3MF1*Z2*Y4*G3MG1
     &+5*F4MF1*Y2*Z3*G1+2*F4MF1*Y2*Z3*G2MG1+F4MF1*Y2*Z3*G3MG1
     &-5*F4MF1*Z2*Y3*G1-2*F4MF1*Z2*Y3*G2MG1-F4MF1*Z2*Y3*G3MG1
     &               ) * XSUR120
         W3(IELEM) = (
     &-(-F2MF1*Y3*Z4+F2MF1*Y4*Z3+F3MF1*Y2*Z4
     &  -F3MF1*Z2*Y4-F4MF1*Y2*Z3+F4MF1*Z2*Y3)
     &               *(2*G3MG1+G2MG1+G4MG1+5*G1)
     &               ) * XSUR120
         W4(IELEM) = (
     &-2*F4MF1*Z2*Y3*G4MG1
     &+2*F4MF1*Y2*Z3*G4MG1
     &+2*F3MF1*Z2*Y4*G4MG1
     &-2*F3MF1*Y2*Z4*G4MG1
     &+2*F2MF1*Y3*Z4*G4MG1
     &-2*F2MF1*Y4*Z3*G4MG1
     &+5*F2MF1*Y3*Z4*G1
     &+F2MF1*Y3*Z4*G2MG1
     &+F2MF1*Y3*Z4*G3MG1-5*F2MF1*Y4*Z3*G1-F2MF1*Y4*Z3*G2MG1
     &-F2MF1*Y4*Z3*G3MG1-5*F3MF1*Y2*Z4*G1-F3MF1*Y2*Z4*G2MG1
     &-F3MF1*Y2*Z4*G3MG1+5*F3MF1*Z2*Y4*G1+F3MF1*Z2*Y4*G2MG1
     &+F3MF1*Z2*Y4*G3MG1+5*F4MF1*Y2*Z3*G1+F4MF1*Y2*Z3*G2MG1
     &+F4MF1*Y2*Z3*G3MG1-5*F4MF1*Z2*Y3*G1-F4MF1*Z2*Y3*G2MG1
     &-F4MF1*Z2*Y3*G3MG1
     &               ) * XSUR120
C
      ENDDO
C
      ELSE IF (ICOORD.EQ.2) THEN
C
C-----------------------------------------------------------------------
C  DERIVATIVE WRT Y
C
      DO   IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
C
         F1 = F(I1)
         F2 = F(I2)
         F3 = F(I3)
         F4 = F(I4)
C
         G1 = G(I1)
         G2 = G(I2)
         G3 = G(I3)
         G4 = G(I4)
C
         F2MF1 = F2-F1
         F3MF1 = F3-F1
         F4MF1 = F4-F1
         G2MG1 = G2-G1
         G3MG1 = G3-G1
         G4MG1 = G4-G1
C
C  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
C
         X2  =  X(I2) - X(I1)
         X3  =  X(I3) - X(I1)
         X4  =  X(I4) - X(I1)
         Z2  =  Z(I2) - Z(I1)
         Z3  =  Z(I3) - Z(I1)
         Z4  =  Z(I4) - Z(I1)
C
         W1(IELEM) = (
     &-F2MF1*X3*Z4*G2MG1+F3MF1*X2*Z4*G3MG1+5*F3MF1*X2*Z4*G1
     &-F2MF1*X3*Z4*G3MG1-5*F2MF1*X3*Z4*G1
     &+F2MF1*X4*Z3*G2MG1+F2MF1*X4*Z3*G3MG1+5*F2MF1*X4*Z3*G1
     &+F3MF1*X2*Z4*G2MG1+F4MF1*Z2*X3*G2MG1+F4MF1*Z2*X3*G3MG1
     &+5*F4MF1*Z2*X3*G1-F4MF1*X2*Z3*G2MG1-F4MF1*X2*Z3*G3MG1
     &-5*F4MF1*X2*Z3*G1-F3MF1*Z2*X4*G2MG1-F3MF1*Z2*X4*G3MG1
     &-5*F3MF1*Z2*X4*G1+F2MF1*X4*Z3*G4MG1+F3MF1*X2*Z4*G4MG1
     &-F4MF1*X2*Z3*G4MG1-F3MF1*Z2*X4*G4MG1-F2MF1*X3*Z4*G4MG1
     &+F4MF1*Z2*X3*G4MG1 ) * XSUR120
         W2(IELEM) = (
     &         -2*F2MF1*X3*Z4*G2MG1+F3MF1*X2*Z4*G3MG1+5*F3MF1*X2*Z4*G1-F
     &2MF1*X3*Z4*G3MG1-5*F2MF1*X3*Z4*G1+2*F2MF1*X4*Z3*G2MG1+F2MF1*X4*Z3*
     &G3MG1+5*F2MF1*X4*Z3*G1+2*F3MF1*X2*Z4*G2MG1+2*F4MF1*Z2*X3*G2MG1+F4M
     &F1*Z2*X3*G3MG1+5*F4MF1*Z2*X3*G1-2*F4MF1*X2*Z3*G2MG1-F4MF1*X2*Z3*G3
     &MG1-5*F4MF1*X2*Z3*G1-2*F3MF1*Z2*X4*G2MG1-F3MF1*Z2*X4*G3MG1-5*F3MF1
     &*Z2*X4*G1+F2MF1*X4*Z3*G4MG1+F3MF1*X2*Z4*G4MG1-F4MF1*X2*Z3*G4MG1-F3
     &MF1*Z2*X4*G4MG1-F2MF1*X3*Z4*G4MG1+F4MF1*Z2*X3*G4MG1 ) * XSUR120
         W3(IELEM) = (
     &         -(F2MF1*X3*Z4-F2MF1*X4*Z3-F3MF1*X2*Z4+F3MF1*Z2*X4+F4MF1*X
     &2*Z3-F4MF1*Z2*X3)*(2*G3MG1+G2MG1+G4MG1+5*G1) ) * XSUR120
         W4(IELEM) = (
     &         -F2MF1*X3*Z4*G2MG1+F3MF1*X2*Z4*G3MG1+5*F3MF1*X2*Z4*G1-F2M
     &F1*X3*Z4*G3MG1-5*F2MF1*X3*Z4*G1+F2MF1*X4*Z3*G2MG1+F2MF1*X4*Z3*G3MG
     &1+5*F2MF1*X4*Z3*G1+F3MF1*X2*Z4*G2MG1+F4MF1*Z2*X3*G2MG1+F4MF1*Z2*X3
     &*G3MG1+5*F4MF1*Z2*X3*G1-F4MF1*X2*Z3*G2MG1-F4MF1*X2*Z3*G3MG1-5*F4MF
     &1*X2*Z3*G1-F3MF1*Z2*X4*G2MG1-F3MF1*Z2*X4*G3MG1-5*F3MF1*Z2*X4*G1+2*
     &F2MF1*X4*Z3*G4MG1+2*F3MF1*X2*Z4*G4MG1-2*F4MF1*X2*Z3*G4MG1-2*F3MF1*
     &Z2*X4*G4MG1-2*F2MF1*X3*Z4*G4MG1+2*F4MF1*Z2*X3*G4MG1 ) * XSUR120
C
      ENDDO
C
      ELSE IF (ICOORD.EQ.3) THEN
C-----------------------------------------------------------------------
C  DERIVATIVE WRT Z
C
      DO   IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
C
         F1 = F(I1)
         F2 = F(I2)
         F3 = F(I3)
         F4 = F(I4)
C
         G1 = G(I1)
         G2 = G(I2)
         G3 = G(I3)
         G4 = G(I4)
C
         F2MF1 = F2-F1
         F3MF1 = F3-F1
         F4MF1 = F4-F1
         G2MG1 = G2-G1
         G3MG1 = G3-G1
         G4MG1 = G4-G1
C
C  REAL COORDINATES OF THE POINTS OF THE ELEMENT
C
         X2  =  X(I2) - X(I1)
         X3  =  X(I3) - X(I1)
         X4  =  X(I4) - X(I1)
         Y2  =  Y(I2) - Y(I1)
         Y3  =  Y(I3) - Y(I1)
         Y4  =  Y(I4) - Y(I1)
C
         W1(IELEM) = (
     &         5*F2MF1*X3*Y4*G1+F2MF1*X3*Y4*G2MG1+F2MF1*X3*Y4*G3MG1-5*F2
     &MF1*X4*Y3*G1-F2MF1*X4*Y3*G2MG1-F2MF1*X4*Y3*G3MG1-5*F3MF1*X2*Y4*G1-
     &F3MF1*X2*Y4*G2MG1-F3MF1*X2*Y4*G3MG1+5*F3MF1*Y2*X4*G1+F3MF1*Y2*X4*G
     &2MG1+F3MF1*Y2*X4*G3MG1+5*F4MF1*X2*Y3*G1+F4MF1*X2*Y3*G2MG1-5*F4MF1*
     &Y2*X3*G1-F4MF1*Y2*X3*G2MG1-F4MF1*Y2*X3*G3MG1+F4MF1*X2*Y3*G3MG1-F4M
     &F1*Y2*X3*G4MG1-F3MF1*X2*Y4*G4MG1+F4MF1*X2*Y3*G4MG1+F2MF1*X3*Y4*G4M
     &G1+F3MF1*Y2*X4*G4MG1-F2MF1*X4*Y3*G4MG1 ) * XSUR120
         W2(IELEM) = (
     &         5*F2MF1*X3*Y4*G1+2*F2MF1*X3*Y4*G2MG1+F2MF1*X3*Y4*G3MG1-5*
     &F2MF1*X4*Y3*G1-2*F2MF1*X4*Y3*G2MG1-F2MF1*X4*Y3*G3MG1-5*F3MF1*X2*Y4
     &*G1-2*F3MF1*X2*Y4*G2MG1-F3MF1*X2*Y4*G3MG1+5*F3MF1*Y2*X4*G1+2*F3MF1
     &*Y2*X4*G2MG1+F3MF1*Y2*X4*G3MG1+5*F4MF1*X2*Y3*G1+2*F4MF1*X2*Y3*G2MG
     &1-5*F4MF1*Y2*X3*G1-2*F4MF1*Y2*X3*G2MG1-F4MF1*Y2*X3*G3MG1+F4MF1*X2*
     &Y3*G3MG1-F4MF1*Y2*X3*G4MG1-F3MF1*X2*Y4*G4MG1+F4MF1*X2*Y3*G4MG1+F2M
     &F1*X3*Y4*G4MG1+F3MF1*Y2*X4*G4MG1-F2MF1*X4*Y3*G4MG1 ) * XSUR120
         W3(IELEM) = (
     &         -(-F2MF1*X3*Y4+F2MF1*X4*Y3+F3MF1*X2*Y4-F3MF1*Y2*X4-F4MF1*
     &X2*Y3+F4MF1*Y2*X3)*(2*G3MG1+G2MG1+G4MG1+5*G1) ) * XSUR120
         W4(IELEM) = (
     &         5*F2MF1*X3*Y4*G1+F2MF1*X3*Y4*G2MG1+F2MF1*X3*Y4*G3MG1-5*F2
     &MF1*X4*Y3*G1-F2MF1*X4*Y3*G2MG1-F2MF1*X4*Y3*G3MG1-5*F3MF1*X2*Y4*G1-
     &F3MF1*X2*Y4*G2MG1-F3MF1*X2*Y4*G3MG1+5*F3MF1*Y2*X4*G1+F3MF1*Y2*X4*G
     &2MG1+F3MF1*Y2*X4*G3MG1+5*F4MF1*X2*Y3*G1+F4MF1*X2*Y3*G2MG1-5*F4MF1*
     &Y2*X3*G1-F4MF1*Y2*X3*G2MG1-F4MF1*Y2*X3*G3MG1+F4MF1*X2*Y3*G3MG1-2*F
     &4MF1*Y2*X3*G4MG1-2*F3MF1*X2*Y4*G4MG1+2*F4MF1*X2*Y3*G4MG1+2*F2MF1*X
     &3*Y4*G4MG1+2*F3MF1*Y2*X4*G4MG1-2*F2MF1*X4*Y3*G4MG1 ) * XSUR120
C
      ENDDO
C
      ELSE
C
C-----------------------------------------------------------------------
C
         IF (LNG.EQ.1) WRITE(LU,200) ICOORD
         IF (LNG.EQ.2) WRITE(LU,201) ICOORD
 200     FORMAT(1X,'VC11TT (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &        1I6,' VERIFIER ICOORD')
 201     FORMAT(1X,'VC11TT (BIEF) : IMPOSSIBLE COMPONENT ',
     &        1I6,' CHECK ICOORD')
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C ERROR
C
      ELSE
C
C-----------------------------------------------------------------------
C
         IF (LNG.EQ.1) WRITE(LU,1100) IELMF,SF%NAME
         IF (LNG.EQ.1) WRITE(LU,1200) IELMG,SG%NAME
         IF (LNG.EQ.1) WRITE(LU,1300)
         IF (LNG.EQ.2) WRITE(LU,1101) IELMF,SF%NAME
         IF (LNG.EQ.2) WRITE(LU,1201) IELMG,SG%NAME
         IF (LNG.EQ.2) WRITE(LU,1301)
         CALL PLANTE(1)
         STOP
 1100    FORMAT(1X,'VC11TT (BIEF) :',/,
     &          1X,'DISCRETISATION DE F : ',1I6,
     &          1X,'NOM REEL : ',A6)
 1200    FORMAT(1X,'DISCRETISATION DE G : ',1I6,
     &          1X,'NOM REEL : ',A6)
 1300    FORMAT(1X,'CAS NON PREVU')
 1101    FORMAT(1X,'VC11TT (BIEF) :',/,
     &          1X,'DISCRETIZATION OF F:',1I6,
     &          1X,'REAL NAME: ',A6)
 1201    FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &          1X,'REAL NAME: ',A6)
 1301    FORMAT(1X,'CASE NOT IMPLEMENTED')
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
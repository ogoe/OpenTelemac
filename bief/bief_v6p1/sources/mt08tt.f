C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code<br>
!>               /
!>  A    = XMUL / P   F . GRAD(P ) * J(X,Y) DXDY
!>   I J       /S  J            I<br>
!>  BY ELEMENTARY CELL; THE ELEMENT IS THE P1 TRIANGLE<br>
!>  J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  ONLY THE Z COMPONENT IS TREATED HERE !!

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, IKLE, NELEM, NELMAX, SF, T, X, XM, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F2, F3, F4, I1, I2, I3, I4, IELEM, X2, X3, X4, XSUR120, Y2, Y3, Y4
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT08TT
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
!>      <td><center> 5.3                                       </center>
!> </td><td> 21/03/02
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
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
!>          <tr><td>T,XM
!></td><td><--</td><td>ELEMENTS DE LA MATRICE
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT08TT
     &( T,XM,XMUL,X,Y,Z,SF,F,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| T,XM           |<--| ELEMENTS DE LA MATRICE
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT08TT => MT08TT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,INTENT(IN) :: NELEM,NELMAX
      INTEGER,INTENT(IN) :: IKLE(NELMAX,4)
C
      DOUBLE PRECISION,INTENT(INOUT) :: T(NELMAX,4),XM(NELMAX,12)
C
      DOUBLE PRECISION,INTENT(IN) :: XMUL
      DOUBLE PRECISION,INTENT(IN) :: F(*),X(*),Y(*),Z(*)
C
C     STRUCTURE OF F
C
      TYPE(BIEF_OBJ),INTENT(IN) :: SF
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      DOUBLE PRECISION X2,X3,X4,Y2,Y3,Y4,F1,F2,F3,F4,XSUR120
C
      INTEGER I1,I2,I3,I4,IELEM
C
C**********************************************************************
C
      XSUR120 = XMUL/120.D0
C
      IF(SF%ELM.NE.31.AND.SF%ELM.NE.51) THEN
        IF (LNG.EQ.1) WRITE(LU,1000) SF%ELM
        IF (LNG.EQ.2) WRITE(LU,1001) SF%ELM
1000    FORMAT(1X,'MT08TT (BIEF) : TYPE DE F NON PREVU : ',I6)
1001    FORMAT(1X,'MT08TT (BIEF): TYPE OF F NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C   LOOP ON THE ELEMENTS
C
      DO 1 IELEM=1,NELEM
C
         I1 = IKLE(IELEM,1)
         I2 = IKLE(IELEM,2)
         I3 = IKLE(IELEM,3)
         I4 = IKLE(IELEM,4)
C
         X2 = X(I2)-X(I1)
         X3 = X(I3)-X(I1)
         X4 = X(I4)-X(I1)
C
         Y2 = Y(I2)-Y(I1)
         Y3 = Y(I3)-Y(I1)
         Y4 = Y(I4)-Y(I1)
C
         F1 = F(I1)
         F2 = F(I2)
         F3 = F(I3)
         F4 = F(I4)
C
         T(IELEM,1)=(
     & X2*Y4*F3-X3*Y4*F3+X4*Y3*F2-Y2*X4*F2+X2*Y4*F2+2*Y2*X3*F1
     &-2*X3*Y4*F1+Y2*X3*F2+Y2*X3*F3+2*X4*Y3*F1+2*X2*Y4*F1-2*Y2*X4*F1
     &-2*X2*Y3*F1-Y2*X4*F3-X2*Y3*F2-X2*Y3*F3+X4*Y3*F3-X3*Y4*F4
     &+X2*Y4*F4-Y2*X4*F4-X2*Y3*F4+Y2*X3*F4+X4*Y3*F4-X3*Y4*F2 )*XSUR120
C
         T(IELEM,2)=(
     & X3*Y4*F3-2*X4*Y3*F2+X3*Y4*F1-X4*Y3*F1-X4*Y3*F3+X3*Y4*F4
     &-X4*Y3*F4+2*X3*Y4*F2                        )*XSUR120
C
         T(IELEM,3)=(-X2*Y4+Y2*X4)*(F1+2*F3+F2+F4) *XSUR120
C
         T(IELEM,4)=(
     & -Y2*X3*F1-Y2*X3*F2-Y2*X3*F3+X2*Y3*F1+X2*Y3*F2+X2*Y3*F3+
     &2*X2*Y3*F4-2*Y2*X3*F4                       )*XSUR120
C
         XM(IELEM,01)=(
     & X2*Y4*F3-X3*Y4*F3+2*X4*Y3*F2-2*Y2*X4*F2+2*X2*Y4*F2+Y2*X3*F1
     &-X3*Y4*F1+2*Y2*X3*F2+Y2*X3*F3+X4*Y3*F1+X2*Y4*F1-Y2*X4*F1-X2*Y3
     &*F1-Y2*X4*F3-2*X2*Y3*F2-X2*Y3*F3+X4*Y3*F3
     &-X3*Y4*F4+X2*Y4*F4-Y2*X4*F4
     &-X2*Y3*F4+Y2*X3*F4+X4*Y3*F4-2*X3*Y4*F2    )*XSUR120
C
         XM(IELEM,02)=(
     &-X3*Y4+X4*Y3+X2*Y4-Y2*X4-X2*Y3+Y2*X3)*(F1+2*F3+F2+F4)*XSUR120
C
         XM(IELEM,03)=(
     & X2*Y4*F3-X3*Y4*F3+X4*Y3*F2-Y2*X4*F2+X2*Y4*F2+Y2*X3*F1-X3*Y4*F1
     &+Y2*X3*F2+Y2*X3*F3+X4*Y3*F1+X2*Y4*F1-Y2*X4*F1-X2*Y3*F1-Y2*X4*F3
     &-X2*Y3*F2-X2*Y3*F3+X4*Y3*F3-2*X3*Y4*F4+2*X2*Y4*F4-2*Y2*X4*F4-2
     &*X2*Y3*F4+2*Y2*X3*F4+2*X4*Y3*F4-X3*Y4*F2)*XSUR120
C
         XM(IELEM,04)= -(-X3*Y4+X4*Y3)*(F1+2*F3+F2+F4)*XSUR120
C
         XM(IELEM,05)=( X3*Y4*F3-X4*Y3*F2+X3*Y4*F1-X4*Y3*F1-X4*Y3*F3
     &                  +2*X3*Y4*F4-2*X4*Y3*F4+X3*Y4*F2)*XSUR120
C
         XM(IELEM,06)=( -X2*Y4*F3+Y2*X4*F2-X2*Y4*F2-X2*Y4*F1+Y2*X4*F1
     &                  +Y2*X4*F3-2*X2*Y4*F4+2*Y2*X4*F4)*XSUR120
C
         XM(IELEM,07)=( X3*Y4*F3-X4*Y3*F2+2*X3*Y4*F1-2*X4*Y3*F1
     &                 -X4*Y3*F3+X3*Y4*F4-X4*Y3*F4+X3*Y4*F2)*XSUR120
C
         XM(IELEM,08)=( -X2*Y4*F3+Y2*X4*F2-X2*Y4*F2-2*X2*Y4*F1
     &                  +2*Y2*X4*F1+Y2*X4*F3-X2*Y4*F4+Y2*X4*F4)*XSUR120
C
         XM(IELEM,09)=( -X2*Y4*F3+2*Y2*X4*F2-2*X2*Y4*F2-X2*Y4*F1
     &                  +Y2*X4*F1+Y2*X4*F3-X2*Y4*F4+Y2*X4*F4 )*XSUR120
C
         XM(IELEM,10)=( -2*Y2*X3*F1-Y2*X3*F2-Y2*X3*F3+2*X2*Y3*F1
     &                  +X2*Y3*F2+X2*Y3*F3+X2*Y3*F4-Y2*X3*F4)*XSUR120
C
         XM(IELEM,11)=(-Y2*X3*F1-2*Y2*X3*F2-Y2*X3*F3+X2*Y3*F1
     &                 +2*X2*Y3*F2+X2*Y3*F3+X2*Y3*F4-Y2*X3*F4)*XSUR120
C
         XM(IELEM,12)= -(-X2*Y3+Y2*X3)*(F1+2*F3+F2+F4)*XSUR120
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
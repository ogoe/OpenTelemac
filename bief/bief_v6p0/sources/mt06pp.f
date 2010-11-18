C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE MASS MATRIX FOR P1 PRISMS.
!>  @code
!>     COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:<br>
!>                              /
!>                    A    =   /  F * (P *P )*J(X,Y) DXDY
!>                     I J    /S        I  J<br>
!>     BY ELEMENTARY CELL; THE ELEMENT IS THE P1 PRISM<br>
!>     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
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
!>    </th><td> F, IKLE, NELEM, NELMAX, SF, SURFAC, T, XM, XMUL, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COEF, F1, F2, F3, F4, F5, F6, H1, H2, H3, IELEM, IELMF, SUR2160
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT06PP
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
!> </td><td> **/04/98
!> </td><td> ARNAUD DESITTER - UNIVERSITY OF BRISTOL
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 28/11/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
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
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>T
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT06PP
     &( T,XM,XMUL,SF,F,Z,SURFAC,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| T             |---| 
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XM             |---| 
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT06PP => MT06PP
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
C
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6), XM(NELMAX,30)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
C
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C     STRUCTURE OF F
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
C
      DOUBLE PRECISION, INTENT(IN) :: Z(*)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      DOUBLE PRECISION SUR2160,COEF,H1,H2,H3,F1,F2,F3,F4,F5,F6
C
      INTEGER IELEM,IELMF
C
C-----------------------------------------------------------------------
C
      SUR2160 = XMUL / 2160.D0
C
C-----------------------------------------------------------------------
C
      IELMF=SF%ELM
      IF(IELMF.NE.41) THEN
        IF (LNG.EQ.1) WRITE(LU,100) IELMF
        IF (LNG.EQ.2) WRITE(LU,101) IELMF
100     FORMAT(1X,'MT06PP (BIEF) :',/,
     &         1X,'DISCRETISATION DE F : ',1I6,' NON PREVUE')
101     FORMAT(1X,'MT06PP (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C LOOP ON THE ELEMENTS
C
      DO IELEM = 1,NELEM
C
         COEF = SURFAC(IELEM)* SUR2160
C
         H1 = (Z(IKLE(IELEM,4)) - Z(IKLE(IELEM,1))) * COEF
         H2 = (Z(IKLE(IELEM,5)) - Z(IKLE(IELEM,2))) * COEF
         H3 = (Z(IKLE(IELEM,6)) - Z(IKLE(IELEM,3))) * COEF
C
         F1 = F(IKLE(IELEM,1))
         F2 = F(IKLE(IELEM,2))
         F3 = F(IKLE(IELEM,3))
         F4 = F(IKLE(IELEM,4))
         F5 = F(IKLE(IELEM,5))
         F6 = F(IKLE(IELEM,6))
C
C-----------------------------------------------------------------------
C
C  EXTRA-DIAGONAL TERMS
C
      XM(IELEM,01) =
     &     (9*F1+6*F2+3*F3+3*F4+2*F5+F6)*H1+
     &     (6*F1+9*F2+3*F3+2*F4+3*F5+F6)*H2+
     &     (3*F1+3*F2+3*F3+F4+F5+F6)*H3
      XM(IELEM,02) =
     &     (9*F1+3*F2+6*F3+3*F4+F5+2*F6)*H1+
     &     (3*F1+3*F2+3*F3+F4+F5+F6)*H2+
     &     (6*F1+3*F2+9*F3+2*F4+F5+3*F6)*H3
      XM(IELEM,03) =
     &     (12*F1+3*F2+3*F3+12*F4+3*F5+3*F6)*H1+
     &     (3*F1+2*F2+F3+3*F4+2*F5+F6)*H2+
     &     (3*F1+F2+2*F3+3*F4+F5+2*F6)*H3
      XM(IELEM,04) =
     &     (3*F1+2*F2+F3+3*F4+2*F5+F6)*H1+
     &     (2*F1+3*F2+F3+2*F4+3*F5+F6)*H2+
     &     (F1+F2+F3+F4+F5+F6)*H3
      XM(IELEM,05) =
     &     (3*F1+F2+2*F3+3*F4+F5+2*F6)*H1+
     &     (F1+F2+F3+F4+F5+F6)*H2+
     &     (2*F1+F2+3*F3+2*F4+F5+3*F6)*H3
      XM(IELEM,06) =
     &     (3*F1+3*F2+3*F3+F4+F5+F6)*H1+
     &     (3*F1+9*F2+6*F3+F4+3*F5+2*F6)*H2+
     &     (3*F1+6*F2+9*F3+F4+2*F5+3*F6)*H3
      XM(IELEM,07) =
     &     (3*F1+2*F2+F3+3*F4+2*F5+F6)*H1+
     &     (2*F1+3*F2+F3+2*F4+3*F5+F6)*H2+
     &     (F1+F2+F3+F4+F5+F6)*H3
      XM(IELEM,08) =
     &     (2*F1+3*F2+F3+2*F4+3*F5+F6)*H1+
     &     (3*F1+12*F2+3*F3+3*F4+12*F5+3*F6)*H2+
     &     (F1+3*F2+2*F3+F4+3*F5+2*F6)*H3
      XM(IELEM,09) =
     &     (F1+F2+F3+F4+F5+F6)*H1+
     &     (F1+3*F2+2*F3+F4+3*F5+2*F6)*H2+
     &     (F1+2*F2+3*F3+F4+2*F5+3*F6)*H3
      XM(IELEM,10) =
     &     (3*F1+F2+2*F3+3*F4+F5+2*F6)*H1+
     &     (F1+F2+F3+F4+F5+F6)*H2+
     &     (2*F1+F2+3*F3+2*F4+F5+3*F6)*H3
      XM(IELEM,11) =
     &     (F1+F2+F3+F4+F5+F6)*H1+
     &     (F1+3*F2+2*F3+F4+3*F5+2*F6)*H2+
     &     (F1+2*F2+3*F3+F4+2*F5+3*F6)*H3
      XM(IELEM,12) =
     &     (2*F1+F2+3*F3+2*F4+F5+3*F6)*H1+
     &     (F1+2*F2+3*F3+F4+2*F5+3*F6)*H2+
     &     (3*F1+3*F2+12*F3+3*F4+3*F5+12*F6)*H3
      XM(IELEM,13) =
     &     (3*F1+2*F2+F3+9*F4+6*F5+3*F6)*H1+
     &     (2*F1+3*F2+F3+6*F4+9*F5+3*F6)*H2+
     &     (F1+F2+F3+3*F4+3*F5+3*F6)*H3
      XM(IELEM,14) =
     &     (3*F1+F2+2*F3+9*F4+3*F5+6*F6)*H1+
     &     (F1+F2+F3+3*F4+3*F5+3*F6)*H2+
     &     (2*F1+F2+3*F3+6*F4+3*F5+9*F6)*H3
      XM(IELEM,15) =
     &     (F1+F2+F3+3*F4+3*F5+3*F6)*H1+
     &     (F1+3*F2+2*F3+3*F4+9*F5+6*F6)*H2+
     &     (F1+2*F2+3*F3+3*F4+6*F5+9*F6)*H3
C
C  DIAGONAL TERMS
C
      T(IELEM,1) =
     &     (36*F1+9*F2+9*F3+12*F4+3*F5+3*F6)*H1+
     &     (9*F1+6*F2+3*F3+3*F4+2*F5+F6)*H2+
     &     (9*F1+3*F2+6*F3+3*F4+F5+2*F6)*H3
      T(IELEM,2) =
     &     (6*F1+9*F2+3*F3+2*F4+3*F5+F6)*H1+
     &     (9*F1+36*F2+9*F3+3*F4+12*F5+3*F6)*H2+
     &     (3*F1+9*F2+6*F3+F4+3*F5+2*F6)*H3
      T(IELEM,3) =
     &     (6*F1+3*F2+9*F3+2*F4+F5+3*F6)*H1+
     &     (3*F1+6*F2+9*F3+F4+2*F5+3*F6)*H2+
     &     (9*F1+9*F2+36*F3+3*F4+3*F5+12*F6)*H3
      T(IELEM,4) =
     &     (12*F1+3*F2+3*F3+36*F4+9*F5+9*F6)*H1+
     &     (3*F1+2*F2+F3+9*F4+6*F5+3*F6)*H2+
     &     (3*F1+F2+2*F3+9*F4+3*F5+6*F6)*H3
      T(IELEM,5) =
     &     (2*F1+3*F2+F3+6*F4+9*F5+3*F6)*H1+
     &     (3*F1+12*F2+3*F3+9*F4+36*F5+9*F6)*H2+
     &     (F1+3*F2+2*F3+3*F4+9*F5+6*F6)*H3
      T(IELEM,6) =
     &     (2*F1+F2+3*F3+6*F4+3*F5+9*F6)*H1+
     &     (F1+2*F2+3*F3+3*F4+6*F5+9*F6)*H2+
     &     (3*F1+3*F2+12*F3+9*F4+9*F5+36*F6)*H3
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
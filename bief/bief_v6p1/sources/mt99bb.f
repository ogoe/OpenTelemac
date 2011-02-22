C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>                       /     DF             D
!>          A    = XMUL /  F * -- * PSI2(J) * --( PSI1(I) ) D(O
!>           I J       /S      DX             DX<br>
!>     BY ELEMENT - THE ELEMENT IS THE QUASI-BUBBLE TRIANGLE<br>
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
!>    </th><td> A11, A12, A13, A14, A21, A22, A23, A24, A31, A32, A33, A34, A41, A42, A43, A44, F, FORMUL, IKLE1, IKLE2, IKLE3, NELEM, NELMAX, SF, SURFAC, TDIA, TEXT, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F2, F3, IELEM, S, X2, X3, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT99BB
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
!> </td><td> 28/11/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F LEPEINTRE (LNH) 30 87 78 54
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
!>          <tr><td>TDIA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEXT
!></td><td>---</td><td>
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
                        SUBROUTINE MT99BB
     &( A11 , A12 , A13 , A14 ,
     &  A21 , A22 , A23 , A24 ,
     &  A31 , A32 , A33 , A34 ,
     &  A41 , A42 , A43 , A44 ,
     &  XMUL,SF,F,XEL,YEL,
     &  SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,FORMUL,TDIA,TEXT)
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
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| TDIA           |---| 
C| TEXT           |---| 
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| XEL,YEL,ZEL    |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT99BB => MT99BB
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
C
      CHARACTER(LEN=1), INTENT(INOUT) :: TDIA,TEXT
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
C
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*),A44(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C     STRUCTURE OF F
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
C
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELEM
      DOUBLE PRECISION F1,F2,F3,X2,X3,Y2,Y3,S
C
C=======================================================================
C
C     ONLY ONE CASE IMPLEMENTED FOR NOW: LINEAR F
C
      IF(SF%ELM.NE.11) THEN
C
        IF (LNG.EQ.1) WRITE(LU,2000) SF%ELM
        IF (LNG.EQ.2) WRITE(LU,2001) SF%ELM
2000    FORMAT(1X,'MT99BB (BIEF) : TYPE DE F : ',I6,' NON PREVU')
2001    FORMAT(1X,'MT99BB (BIEF) : TYPE OF F:',I6,' NOT IMPLEMENTED')
        CALL PLANTE(0)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(FORMUL(8:16).EQ.'     0XX0') THEN
C
      TDIA='Q'
      TEXT='Q'
C
C   LOOP ON THE ELEMENTS
C
      DO 1 IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      A11(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     & (7*Y2*F1+3*Y2*F2+2*Y2*F3
     & -7*Y3*F1-2*Y3*F2-3*Y3*F3)/S/144
C
      A12(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(Y2+Y3)*
     & (7*F1+4*F2+F3)/S/432
C
      A13(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(Y2+Y3)*
     & (4*F3+7*F1+F2)/S/432
C
      A14(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     & (7*Y2*F1+4*Y2*F2+Y2*F3-4*Y3*F3-7*Y3*F1-Y3*F2)/S/144
C
      A21(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(2*Y2-Y3)*
     &  (4*F1+7*F2+F3)/S/432
C
      A22(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (Y2*F1-Y2*F3+2*Y3*F1+7*Y3*F2+3*Y3*F3)/S/144
C
      A23(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(2*Y2-Y3)*
     &  (7*F2+4*F3+F1)/S/432
C
      A24(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (3*Y2*F1-3*Y2*F3+7*Y3*F2+4*Y3*F3+Y3*F1)/S/144
C
      A31(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(Y2-2*Y3)*
     &  (7*F3+4*F1+F2)/S/432
C
      A32(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(Y2-2*Y3)*
     &  (4*F2+F1+7*F3)/S/432
C
      A33(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (3*Y2*F2+2*Y2*F1+7*Y2*F3-Y3*F2+Y3*F1)/S/144
C
      A34(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (-3*Y3*F2+3*Y3*F1+4*Y2*F2+Y2*F1+7*Y2*F3)/S/144
C
      A41(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     & (5*Y2*F1+4*Y2*F2+3*Y2*F3-5*Y3*F1-3*Y3*F2-4*Y3*F3)
     & /S/144
C
      A42(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (Y2*F1-Y2*F3+3*Y3*F1+5*Y3*F2+4*Y3*F3)/S/144
C
      A43(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (4*Y2*F2+5*Y2*F3+3*Y2*F1-Y3*F2+Y3*F1)/S/144
C
      A44(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)**2/S/48
C
C
C
C   END OF THE LOOP ON THE ELEMENTS
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'     0YY0') THEN
C
      TDIA='Q'
      TEXT='Q'
C
C   LOOP ON THE ELEMENTS
C
      DO 2 IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
C  ELEMENTS OUTSIDE OF THE DIAGONAL
C
      A11(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(-7*X2*F1-
     & 3*X2*F2-2*X2*F3+7*X3*F1+2*X3*F2+3*X3*F3)/S/144

      A12(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(X2+X3)*
     & (7*F1+4*F2+F3)/S/432

      A13(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(X2+X3)*
     & (4*F3+7*F1+F2)/S/432

      A14(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-7*X2*F1-
     & 4*X2*F2-X2*F3+4*X3*F3+7*X3*F1+X3*F2)/S/144

      A21(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(-2*X2+X3)*
     & (4*F1+7*F2+F3)/S/432

      A22(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(X2*F1-X2*F3+2*
     & X3*F1+7*X3*F2+3*X3*F3)/S/144

      A23(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-2*X2+X3)*
     & (7*F2+4*F3+F1)/S/432

      A24(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(3*X2*F1-3*X2*F3+
     & 7*X3*F2+4*X3*F3+X3*F1)/S/144

      A31(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(-X2+2*X3)*
     & (7*F3+4*F1+F2)/S/432

      A32(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-X2+2*X3)*
     & (4*F2+F1+7*F3)/S/432

      A33(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-3*X2*F2-2*X2*F1-
     & 7*X2*F3+X3*F2-X3*F1)/S/144

      A34(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(3*X3*F2-3*X3*F1-
     & 4*X2*F2-X2*F1-7*X2*F3)/S/144

      A41(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(-5*X2*F1-4*X2*F2-
     & 3*X2*F3+5*X3*F1+3*X3*F2+4*X3*F3)/S/144

      A42(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(X2*F1-X2*F3+3*X3*
     & F1+5*X3*F2+4*X3*F3)/S/144

      A43(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-4*X2*F2-5*X2*F3-
     & 3*X2*F1+X3*F2-X3*F1)/S/144

      A44(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)**2/S/48
C
C   END OF THE LOOP ON THE ELEMENTS
C
2     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'     XX00') THEN
C
      TDIA='Q'
      TEXT='Q'
C     SYMMETRY NOT TAKEN INTO ACCOUNT
C     TEXT='S'
C
C   LOOP ON THE ELEMENTS
C
      DO 3 IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
C
      A11(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/36
      A12(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A13(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A14(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A21(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A22(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/36
      A23(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A24(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A31(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A32(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A33(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/36
      A34(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A41(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A42(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A43(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A44(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/24
C
C
C
C   END OF THE LOOP ON THE ELEMENTS
C
3     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'     0X0Y') THEN
C
      TDIA='Q'
      TEXT='Q'
C
C   LOOP ON THE ELEMENTS
C
      DO 5 IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      A11(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-7*X2*F1-3*X2*F2-2*X2*F3+7*X3*F1+2*X3*F2+3*X3*F3)
     & /S/144
C
      A12(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-2*X2+X3)*
     & (4*F1+7*F2+F3)/S/432
C
      A13(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-X2+2*X3)*
     & (7*F3+4*F1+F2)/S/432
C
      A14(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-5*X2*F1-4*X2*F2-
     & 3*X2*F3+5*X3*F1+3*X3*F2+4*X3*F3)/S/144
C
      A21(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2+X3)*
     & (7*F1+4*F2+F3)/S/432
C
      A22(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X2*F3+2*X3*F1+
     & 7*X3*F2+3*X3*F3)/S/144
C
      A23(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-X2+2*X3)*
     & (4*F2+F1+7*F3)/S/432
C
      A24(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X2*F3+3*X3*F1+
     & 5*X3*F2+4*X3*F3)/S/144
C
      A31(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2+X3)*
     & (4*F3+7*F1+F2)/S/432
C
      A32(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-2*X2+X3)*
     & (7*F2+4*F3+F1)/S/432
C
      A33(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(3*X2*F2+2*X2*F1+
     & 7*X2*F3-X3*F2+X3*F1)/S/144
C
      A34(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(4*X2*F2+5*X2*F3+
     & 3*X2*F1-X3*F2+X3*F1)/S/144
C
      A41(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-7*X2*F1-4*X2*F2-
     & X2*F3+4*X3*F3+7*X3*F1+X3*F2)/S/144
C
      A42(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(3*X2*F1-3*X2*F3+
     & 7*X3*F2+4*X3*F3+X3*F1)/S/144
C
      A43(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-3*X3*F2+3*X3*F1+
     & 4*X2*F2+X2*F1+7*X2*F3)/S/144
C
      A44(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-X2*F1+X2*F3-X3*F2+
     & X3*F1)/S/48
C
C
C   END OF THE LOOP ON THE ELEMENTS
C
5     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'     XY00') THEN
C
      TDIA='Q'
      TEXT='Q'
C     SYMMETRY NOT TAKEN INTO ACCOUNT
C     TEXT='S'
C
C   LOOP ON THE ELEMENTS
C
      DO 6 IELEM = 1 , NELEM
C
      S =  SURFAC(IELEM)
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      A11(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/36
C
      A12(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
C
      A13(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
C
      A14(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
C
      A21(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
C
      A22(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/36
C
      A23(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
C
      A24(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
C
      A31(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
C
      A32(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
C
      A33(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/36
C
      A34(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
C
      A41(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
C
      A42(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
C
      A43(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
C
      A44(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/24
C
C   END OF THE LOOP ON THE ELEMENTS
C
6     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'     YY00') THEN
C
      TDIA='Q'
      TEXT='Q'
C     SYMMETRY NOT TAKEN INTO ACCOUNT
C     TEXT='S'
C
C   LOOP ON THE ELEMENTS
C
      DO 7 IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      A11(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/36
      A12(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A13(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A14(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A21(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A22(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/36
      A23(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A24(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A31(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A32(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A33(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/36
      A34(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A41(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A42(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A43(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A44(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/24
C
C
C   END OF THE LOOP ON THE ELEMENTS
C
7     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'     0Y0X') THEN
C
      TDIA='Q'
      TEXT='Q'
C
C   LOOP ON THE ELEMENTS
C
      DO 9 IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      A11(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*(-7*Y2*F1-
     & 3*Y2*F2-2*Y2*F3+7*Y3*F1+2*Y3*F2+3*Y3*F3)/S/144
C
      A12(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (-2*Y2+Y3)*(4*F1+7*F2+F3)/S/432
C
      A13(IELEM) = -(-Y2+2*Y3)*(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (7*F3+4*F1+F2)/S/432
C
      A14(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (-5*Y2*F1-4*Y2*F2-3*Y2*F3+5*Y3*F1+3*Y3*F2+4*Y3*F3)/S/144
C
      A21(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(Y2+Y3)*
     & (7*F1+4*F2+F3)/S/432
C
      A22(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(Y2*F1-
     & Y2*F3+2*Y3*F1+7*Y3*F2+3*Y3*F3)/S/144
C
      A23(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (-Y2+2*Y3)*(4*F2+F1+7*F3)/S/432
C
      A24(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (Y2*F1-Y2*F3+3*Y3*F1+5*Y3*F2+4*Y3*F3)/S/144
C
      A31(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*(Y2+Y3)*
     & (4*F3+7*F1+F2)/S/432
C
      A32(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(-2*Y2+Y3)*
     & (7*F2+4*F3+F1)/S/432
C
      A33(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (3*Y2*F2+2*Y2*F1+7*Y2*F3-Y3*F2+Y3*F1)/S/144
C
      A34(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (4*Y2*F2+5*Y2*F3+3*Y2*F1-Y3*F2+Y3*F1)/S/144
C
      A41(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(-7*Y2*F1-4*Y2*F2-
     & Y2*F3+4*Y3*F3+7*Y3*F1+Y3*F2)/S/144
C
      A42(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*(3*Y2*F1-3*Y2*F3+
     & 7*Y3*F2+4*Y3*F3+Y3*F1)/S/144
C
      A43(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(4*Y2*F2+Y2*F1+
     & 7*Y2*F3-3*Y3*F2+3*Y3*F1)/S/144
C
      A44(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)/S/48
C
C
C
C   END OF THE LOOP ON THE ELEMENTS
C
9     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'00XX+00YY') THEN
C
      TDIA='Q'
      TEXT='Q'
C     SYMMETRY NOT TAKEN INTO ACCOUNT
C     TEXT='S'
C
C   LOOP ON THE ELEMENTS
C
      DO IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      A11(IELEM) = (-56*F2**2*X2*X3+25*F2*F3*X3**2-56*
     & F3**2*Y2*Y3-104*F1**2*X2*X3+37*F1*F2*Y3**2+37*F1*F2*X3**2+
     & 25*F2*F3*Y2**2+25*F2*F3*Y3**2-56*F2**2*Y2*Y3+73*F1*
     & F2*Y2**2+37*F1*F3*Y2**2-104*F1**2*Y2*Y3+73*F1*F3*Y3**2+
     & 37*F1*F3*X2**2+25*F2*F3*X2**2-56*F3**2*X2*X3+73*F1*F2*X2**2+
     & 73*F1*F3*X3**2+17*F3**2*Y2**2-40*F2*F3*Y2*Y3+65*F1**2*
     & X3**2+65*F1**2*X2**2-40*F2*F3*X2*X3+53*F3**2*Y3**2+
     & 17*F2**2*X3**2-88*F1*F3*Y2*Y3-88*F1*F3*X2*X3+17*F3**2*X2**2+
     & 53*F3**2*X3**2+53*F2**2*X2**2+53*F2**2*Y2**2+17*F2**2*Y3**2-
     & 88*F1*F2*Y2*Y3-88*F1*F2*X2*X3+65*F1**2*Y2**2+65*F1**2*Y3**2)
     & /S/648
C
      A12(IELEM) = -(13*F1**2+17*F1*F2+5*F1*F3+F3**2+
     & 13*F2**2+5*F2*F3)*
     & (-2*Y2**2-Y2*Y3+Y3**2-2*X2**2-X2*X3+X3**2)/S/648
C
      A13(IELEM) = (13*F1**2+5*F1*F2+17*F1*F3+13*F3**2+F2**2+5*F2*F3)*
     & (-Y2**2+Y2*Y3+2*Y3**2-X2**2+X2*X3+2*X3**2)/S/648
C
      A14(IELEM) = -(-7*F2**2*X2*X3+5*F2*F3*X3**2-7*F3**2*Y2*Y3-13*
     & F1**2*X2*X3+5*F1*F2*Y3**2+5*F1*F2*X3**2+5*F2*F3*Y2**2+5*F2*F3*
     & Y3**2-7*F2**2*Y2*Y3+17*F1*F2*Y2**2+5*F1*F3*Y2**2-
     & 13*F1**2*Y2*Y3+
     & 17*F1*F3*Y3**2+5*F1*F3*X2**2+5*F2*F3*X2**2-7*F3**2*X2*X3+17*F1*
     & F2*X2**2+17*F1*F3*X3**2+F3**2*Y2**2-5*F2*F3*Y2*Y3+
     & 13*F1**2*X3**2+
     & 13*F1**2*X2**2-5*F2*F3*X2*X3+13*F3**2*Y3**2+F2**2*X3**2-
     & 11*F1*F3*Y2*Y3-11*F1*F3*X2*X3+F3**2*X2**2+13*F3**2*X3**2+
     & 13*F2**2*X2**2+13*F2**2*Y2**2+F2**2*Y3**2-11*F1*F2*Y2*Y3-
     & 11*F1*F2*X2*X3+13*F1**2*Y2**2+13*F1**2*Y3**2)/S/108
C
      A22(IELEM) = (-26*F2**2*X2*X3+73*F2*F3*X3**2-50*F3**2*Y2*Y3+22*
     & F1**2*X2*X3+37*F1*F2*Y3**2+37*F1*F2*X3**2+22*F2*F3*Y2**2+
     & 73*F2*F3*Y3**2-26*F2**2*Y2*Y3+22*F1*F2*Y2**2+10*F1*F3*Y2**2+
     & 22*F1**2*Y2*Y3+25*F1*F3*Y3**2+10*F1*F3*X2**2+22*F2*F3*X2**2-
     & 50*F3**2*X2*X3+22*F1*F2*X2**2+25*F1*F3*X3**2+14*F3**2*Y2**2-
     & 58*F2*F3*Y2*Y3+17*F1**2*X3**2+14*F1**2*X2**2-58*F2*F3*X2*X3+
     & 53*F3**2*
     & Y3**2+65*F2**2*X3**2-10*F1*F3*Y2*Y3-10*F1*F3*X2*X3+
     & 14*F3**2*X2**2+
     & 53*F3**2*X3**2+26*F2**2*X2**2+26*F2**2*Y2**2+65*F2**2*Y3**2+
     & 14*F1*F2*Y2*Y3+14*F1*F2*X2*X3+14*F1**2*Y2**2+
     & 17*F1**2*Y3**2)/S/648
C
      A23(IELEM) = (13*F3**2+17*F2*F3+5*F1*F3+13*F2**2+
     & F1**2+5*F1*F2)*
     & (2*Y2**2-5*Y2*Y3+2*Y3**2+2*X2**2-5*X2*X3+2*X3**2)/S/648
C
      A24(IELEM) = -(-13*F2**2*X2*X3+17*F2*F3*X3**2-
     & 19*F3**2*Y2*Y3+5*F1**2*
     & X2*X3+5*F1*F2*Y3**2+5*F1*F2*X3**2+
     & 11*F2*F3*Y2**2+17*F2*F3*Y3**2-
     & 13*F2**2*Y2*Y3+11*F1*F2*Y2**2+5*F1*F3*Y2**2+5*F1**2*
     & Y2*Y3+5*F1*F3*
     & Y3**2+5*F1*F3*X2**2+11*F2*F3*X2**2-19*F3**2*X2*X3+
     & 11*F1*F2*X2**2+
     & 5*F1*F3*X3**2+7*F3**2*Y2**2-23*F2*F3*Y2*Y3+
     & F1**2*X3**2+7*F1**2*
     & X2**2-23*F2*F3*X2*X3+13*F3**2*Y3**2+13*F2**2*X3**2-
     & 5*F1*F3*Y2*Y3-
     & 5*F1*F3*X2*X3+7*F3**2*X2**2+13*F3**2*X3**2+13*F2**2*X2**2+
     & 13*F2**2*
     & Y2**2+13*F2**2*Y3**2+F1*F2*Y2*Y3+F1*F2*X2*X3+7*F1**2*
     & Y2**2+F1**2*Y3**2)/S/108
C
      A33(IELEM) = (-50*F2**2*X2*X3+22*F2*F3*X3**2-26*F3**2*
     & Y2*Y3+22*F1**2*X2*X3+10*F1*F2*Y3**2+10*F1*F2*X3**2+73*F2*F3*
     & Y2**2+22*F2*F3*Y3**2-
     &  50*F2**2*Y2*Y3+25*F1*F2*Y2**2+37*F1*F3*Y2**2+
     & 22*F1**2*Y2*Y3+22*F1*
     &  F3*Y3**2+37*F1*F3*X2**2+73*F2*F3*X2**2-
     & 26*F3**2*X2*X3+25*F1*
     &  F2*X2**2+22*F1*F3*X3**2+65*F3**2*Y2**2-58*F2*F3*Y2*Y3+
     & 14*F1**2*X3**2+
     &  17*F1**2*X2**2-58*F2*F3*X2*X3+26*F3**2*Y3**2+14*F2**2*
     & X3**2+14*F1*
     &  F3*Y2*Y3+14*F1*F3*X2*X3+65*F3**2*X2**2+26*F3**2*X3**2+53*
     & F2**2*X2**2+
     &  53*F2**2*Y2**2+14*F2**2*Y3**2-10*F1*F2*Y2*Y3-
     & 10*F1*F2*X2*X3+17*
     &  F1**2*Y2**2+14*F1**2*Y3**2)/S/648
C
      A34(IELEM) = -(-19*F2**2*X2*X3+11*F2*F3*X3**2-
     & 13*F3**2*Y2*Y3+5*F1**2
     &  *X2*X3+5*F1*F2*Y3**2+5*F1*F2*X3**2+17*F2*F3*Y2**2+
     & 11*F2*F3*Y3**2-
     &  19*F2**2*Y2*Y3+5*F1*F2*Y2**2+5*F1*F3*Y2**2+5*F1**2*
     & Y2*Y3+11*F1*
     &  F3*Y3**2+5*F1*F3*X2**2+17*F2*F3*X2**2-13*F3**2*X2*X3+
     & 5*F1*F2*X2**2+
     &  11*F1*F3*X3**2+13*F3**2*Y2**2-23*F2*F3*Y2*Y3+7*F1**2*X3**2+
     &  F1**2*X2**2-23*F2*F3*X2*X3+13*F3**2*Y3**2+7*F2**2*X3**2+
     & F1*F3*Y2*Y3+
     &  F1*F3*X2*X3+13*F3**2*X2**2+13*F3**2*X3**2+13*F2**2*X2**2+
     & 13*F2**2*Y2**2+
     &  7*F2**2*Y3**2-5*F1*F2*Y2*Y3-5*F1*F2*X2*X3+F1**2*Y2**2+
     & 7*F1**2*Y3**2)/S/108
C
      A44(IELEM) = (-13*F2**2*X2*X3+11*F2*F3*X3**2-
     &  13*F3**2*Y2*Y3-F1**2*X2*X3+
     &  5*F1*F2*Y3**2+5*F1*F2*X3**2+11*F2*F3*Y2**2+11*F2*F3*Y3**2-
     &  13*F2**2*Y2*Y3+11*F1*F2*Y2**2+5*F1*F3*Y2**2-F1**2*Y2*Y3+
     & 11*F1*F3*Y3**2+5*F1*F3*X2**2+11*F2*F3*X2**2-13*F3**2*
     & X2*X3+11*F1*F2*X2**2+
     &  11*F1*F3*X3**2+7*F3**2*Y2**2-17*F2*F3*Y2*Y3+
     & 7*F1**2*X3**2+7*F1**2*X2**2-17*F2*F3*X2*X3+
     & 13*F3**2*Y3**2+7*F2**2*X3**2-5*F1*F3*Y2*Y3-5*F1*F3*X2*X3+
     & 7*F3**2*X2**2+13*F3**2*X3**2+13*F2**2*X2**2+
     &  13*F2**2*Y2**2+7*F2**2*Y3**2-5*F1*F2*Y2*Y3-5*F1*F2*X2*X3+
     & 7*F1**2*Y2**2+7*F1**2*Y3**2)/S/36
C
C   TERMS OBTAINED BY SYMMETRY
C
      A21(IELEM) = A12(IELEM)
      A31(IELEM) = A13(IELEM)
      A32(IELEM) = A23(IELEM)
      A41(IELEM) = A14(IELEM)
      A42(IELEM) = A24(IELEM)
      A43(IELEM) = A34(IELEM)
C
C   END OF THE LOOP ON THE ELEMENTS
C
      ENDDO
C
C------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'     00XX') THEN
C
      TDIA='Q'
      TEXT='Q'
C     SYMMETRY NOT TAKEN INTO ACCOUNT
C     TEXT='S'
C
C   LOOP ON THE ELEMENTS
C
      DO IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      A11(IELEM)=(65*F1**2*Y2**2+65*F1**2*Y3**2+17*F3**2*Y2**2+53*F3**2*
     &Y3**2-88*F1*F2*Y2*Y3-88*F1*F3*Y2*Y3+53*F2**2*Y2**2+17*F2**2*Y3**2-
     &40*F2*F3*Y2*Y3+73*F1*F3*Y3**2-56*F2**2*Y2*Y3-56*F3**2*Y2*Y3+37*F1*
     &F3*Y2**2+37*F1*F2*Y3**2+73*F1*F2*Y2**2+25*F2*F3*Y2**2+25*F2*F3*Y3*
     &*2-104*F1**2*Y2*Y3)/S/648
C
      A12(IELEM)=(13*F1**2+17*F1*F2+5*F1*F3+F3**2+13*F2**2+5*F2*F3)*(Y2+
     &Y3)*(2*Y2-Y3)/S/648
C
      A13(IELEM)=-(13*F1**2+17*F1*F3+5*F1*F2+13*F3**2+F2**2+5*F2*F3)*(Y2
     &+Y3)*(Y2-2*Y3)/S/648
C
      A14(IELEM)=-(13*F1**2*Y2**2-13*F1**2*Y2*Y3+17*F1*F2*Y2**2-11*F1*F2
     &*Y2*Y3+5*F1*F3*Y2**2-11*F1*F3*Y2*Y3+F3**2*Y2**2-7*F3**2*Y2*Y3+13*F
     &2**2*Y2**2-7*F2**2*Y2*Y3+5*F2*F3*Y2**2-5*F2*F3*Y2*Y3+13*F1**2*Y3**
     &2+17*F1*F3*Y3**2+5*F1*F2*Y3**2+13*F3**2*Y3**2+F2**2*Y3**2+5*F2*F3*
     &Y3**2)/S/108
C
      A22(IELEM)=(14*F1**2*Y2**2+17*F1**2*Y3**2+14*F3**2*Y2**2+53*F3**2*
     &Y3**2+14*F1*F2*Y2*Y3-10*F1*F3*Y2*Y3+26*F2**2*Y2**2+65*F2**2*Y3**2-
     &58*F2*F3*Y2*Y3+25*F1*F3*Y3**2-26*F2**2*Y2*Y3-50*F3**2*Y2*Y3+10*F1*
     &F3*Y2**2+37*F1*F2*Y3**2+22*F1*F2*Y2**2+22*F2*F3*Y2**2+73*F2*F3*Y3*
     &*2+22*F1**2*Y2*Y3)/S/648
C
      A23(IELEM)=(13*F2**2+17*F2*F3+5*F1*F2+F1**2+13*F3**2+5*F1*F3)*(2*Y
     &2-Y3)*(Y2-2*Y3)/S/648
C
      A24(IELEM)=-(7*F1**2*Y2**2+F1**2*Y3**2+7*F3**2*Y2**2+13*F3**2*Y3**
     &2+F1*F2*Y2*Y3-5*F1*F3*Y2*Y3+13*F2**2*Y2**2+13*F2**2*Y3**2-23*F2*F3
     &*Y2*Y3+5*F1*F3*Y3**2-13*F2**2*Y2*Y3-19*F3**2*Y2*Y3+5*F1*F3*Y2**2+5
     &*F1*F2*Y3**2+11*F1*F2*Y2**2+11*F2*F3*Y2**2+17*F2*F3*Y3**2+5*F1**2*
     &Y2*Y3)/S/108
C
      A33(IELEM)=(17*F1**2*Y2**2+14*F1**2*Y3**2+65*F3**2*Y2**2+26*F3**2*
     &Y3**2-10*F1*F2*Y2*Y3+14*F1*F3*Y2*Y3+53*F2**2*Y2**2+14*F2**2*Y3**2-
     &58*F2*F3*Y2*Y3+22*F1*F3*Y3**2-50*F2**2*Y2*Y3-26*F3**2*Y2*Y3+37*F1*
     &F3*Y2**2+10*F1*F2*Y3**2+25*F1*F2*Y2**2+73*F2*F3*Y2**2+22*F2*F3*Y3*
     &*2+22*F1**2*Y2*Y3)/S/648
C
      A34(IELEM)=-(F1**2*Y2**2+7*F1**2*Y3**2+13*F3**2*Y2**2+13*F3**2*Y3*
     &*2-5*F1*F2*Y2*Y3+F1*F3*Y2*Y3+13*F2**2*Y2**2+7*F2**2*Y3**2-23*F2*F3
     &*Y2*Y3+11*F1*F3*Y3**2-19*F2**2*Y2*Y3-13*F3**2*Y2*Y3+5*F1*F3*Y2**2+
     &5*F1*F2*Y3**2+5*F1*F2*Y2**2+17*F2*F3*Y2**2+11*F2*F3*Y3**2+5*F1**2*
     &Y2*Y3)/S/108
C
      A44(IELEM)=(7*F1**2*Y2**2+7*F1**2*Y3**2+7*F3**2*Y2**2+13*F3**2*Y3*
     &*2-5*F1*F2*Y2*Y3-5*F1*F3*Y2*Y3+13*F2**2*Y2**2+7*F2**2*Y3**2-17*F2*
     &F3*Y2*Y3+11*F1*F3*Y3**2-13*F2**2*Y2*Y3-13*F3**2*Y2*Y3+5*F1*F3*Y2**
     &2+5*F1*F2*Y3**2+11*F1*F2*Y2**2+11*F2*F3*Y2**2+11*F2*F3*Y3**2-F1**2
     &*Y2*Y3)/S/36
C
C   TERMS OBTAINED BY SYMMETRY
C
      A21(IELEM) = A12(IELEM)
      A31(IELEM) = A13(IELEM)
      A32(IELEM) = A23(IELEM)
      A41(IELEM) = A14(IELEM)
      A42(IELEM) = A24(IELEM)
      A43(IELEM) = A34(IELEM)
C
C   END OF THE LOOP ON THE ELEMENTS
C
      ENDDO
C
C------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'     00YY') THEN
C
      TDIA='Q'
      TEXT='Q'
C     SYMMETRY NOT TAKEN INTO ACCOUNT
C     TEXT='S'
C
C   LOOP ON THE ELEMENTS
C
      DO IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      A11(IELEM)=(-56*F3**2*X2*X3+37*F1*F3*X2**2+73*F1*F3*X3**2-88*F1*F3
     &*X2*X3-88*F1*F2*X2*X3-40*F2*F3*X2*X3+73*F1*F2*X2**2-104*F1**2*X2*X
     &3-56*F2**2*X2*X3+37*F1*F2*X3**2+65*F1**2*X2**2+65*F1**2*X3**2+53*F
     &2**2*X2**2+17*F2**2*X3**2+17*F3**2*X2**2+53*F3**2*X3**2+25*F2*F3*X
     &2**2+25*F2*F3*X3**2)/S/648
C
      A12(IELEM)=-(13*F1**2+5*F1*F3+17*F1*F2+13*F2**2+F3**2+5*F2*F3)*(X2
     &+X3)*(-2*X2+X3)/S/648
C
      A13(IELEM)=(13*F1**2+17*F1*F3+5*F1*F2+13*F3**2+F2**2+5*F2*F3)*(X2+
     &X3)*(-X2+2*X3)/S/648
C
      A14(IELEM)=-(13*F1**2*X2**2-13*F1**2*X2*X3+5*F1*F3*X2**2-11*F1*F3*
     &X2*X3+17*F1*F2*X2**2-11*F1*F2*X2*X3+13*F2**2*X2**2-7*F2**2*X2*X3+F
     &3**2*X2**2-7*F3**2*X2*X3+5*F2*F3*X2**2-5*F2*F3*X2*X3+13*F1**2*X3**
     &2+17*F1*F3*X3**2+5*F1*F2*X3**2+13*F3**2*X3**2+F2**2*X3**2+5*F2*F3*
     &X3**2)/S/108
C
      A22(IELEM)=(-50*F3**2*X2*X3+10*F1*F3*X2**2+25*F1*F3*X3**2-10*F1*F3
     &*X2*X3+14*F1*F2*X2*X3-58*F2*F3*X2*X3+22*F1*F2*X2**2+22*F1**2*X2*X3
     &-26*F2**2*X2*X3+37*F1*F2*X3**2+14*F1**2*X2**2+17*F1**2*X3**2+26*F2
     &**2*X2**2+65*F2**2*X3**2+14*F3**2*X2**2+53*F3**2*X3**2+22*F2*F3*X2
     &**2+73*F2*F3*X3**2)/S/648
C
      A23(IELEM)=(13*F2**2+5*F1*F2+17*F2*F3+F1**2+13*F3**2+5*F1*F3)*(-2*
     &X2+X3)*(-X2+2*X3)/S/648
C
      A24(IELEM)=-(-19*F3**2*X2*X3+5*F1*F3*X2**2+5*F1*F3*X3**2-5*F1*F3*X
     &2*X3+F1*F2*X2*X3-23*F2*F3*X2*X3+11*F1*F2*X2**2+5*F1**2*X2*X3-13*F2
     &**2*X2*X3+5*F1*F2*X3**2+7*F1**2*X2**2+F1**2*X3**2+13*F2**2*X2**2+1
     &3*F2**2*X3**2+7*F3**2*X2**2+13*F3**2*X3**2+11*F2*F3*X2**2+17*F2*F3
     &*X3**2)/S/108
C
      A33(IELEM)=(-26*F3**2*X2*X3+37*F1*F3*X2**2+22*F1*F3*X3**2+14*F1*F3
     &*X2*X3-10*F1*F2*X2*X3-58*F2*F3*X2*X3+25*F1*F2*X2**2+22*F1**2*X2*X3
     &-50*F2**2*X2*X3+10*F1*F2*X3**2+17*F1**2*X2**2+14*F1**2*X3**2+53*F2
     &**2*X2**2+14*F2**2*X3**2+65*F3**2*X2**2+26*F3**2*X3**2+73*F2*F3*X2
     &**2+22*F2*F3*X3**2)/S/648
C
      A34(IELEM)=-(-13*F3**2*X2*X3+5*F1*F3*X2**2+11*F1*F3*X3**2+F1*F3*X2
     &*X3-5*F1*F2*X2*X3-23*F2*F3*X2*X3+5*F1*F2*X2**2+5*F1**2*X2*X3-19*F2
     &**2*X2*X3+5*F1*F2*X3**2+F1**2*X2**2+7*F1**2*X3**2+13*F2**2*X2**2+7
     &*F2**2*X3**2+13*F3**2*X2**2+13*F3**2*X3**2+17*F2*F3*X2**2+11*F2*F3
     &*X3**2)/S/108
C
      A44(IELEM)=(-13*F3**2*X2*X3+5*F1*F3*X2**2+11*F1*F3*X3**2-5*F1*F3*X
     &2*X3-5*F1*F2*X2*X3-17*F2*F3*X2*X3+11*F1*F2*X2**2-F1**2*X2*X3-13*F2
     &**2*X2*X3+5*F1*F2*X3**2+7*F1**2*X2**2+7*F1**2*X3**2+13*F2**2*X2**2
     &+7*F2**2*X3**2+7*F3**2*X2**2+13*F3**2*X3**2+11*F2*F3*X2**2+11*F2*F
     &3*X3**2)/S/36
C
C   TERMS OBTAINED BY SYMMETRY
C
      A21(IELEM) = A12(IELEM)
      A31(IELEM) = A13(IELEM)
      A32(IELEM) = A23(IELEM)
      A41(IELEM) = A14(IELEM)
      A42(IELEM) = A24(IELEM)
      A43(IELEM) = A34(IELEM)
C
C   END OF THE LOOP ON THE ELEMENTS
C
      ENDDO
C
C------------------------------------------------------------------
C
      ELSEIF(FORMUL(8:16).EQ.'     00XY') THEN
C
      TDIA='Q'
      TEXT='Q'
C
C   LOOP ON THE ELEMENTS
C
      DO IELEM = 1 , NELEM
C
      S = SURFAC(IELEM)/XMUL
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      A11(IELEM)=(44*F1*F3*Y2*X3+44*F1*F3*Y3*X2-73*F1*F3*Y3*X3-73*F1*F2*
     &Y2*X2-25*F2*F3*Y3*X3+44*F1*F2*Y3*X2-37*F1*F2*Y3*X3-25*F2*F3*Y2*X2+
     &20*F2*F3*Y2*X3+52*F1**2*Y2*X3+28*F3**2*Y2*X3-17*F2**2*Y3*X3-65*F1*
     &*2*Y2*X2+28*F2**2*Y2*X3-53*F2**2*Y2*X2+28*F3**2*Y3*X2+52*F1**2*Y3*
     &X2-65*F1**2*Y3*X3-17*F3**2*Y2*X2+28*F2**2*Y3*X2-53*F3**2*Y3*X3-37*
     &F1*F3*Y2*X2+44*F1*F2*Y2*X3+20*F2*F3*Y3*X2)/S/648
C
      A12(IELEM)=(13*F1**2+5*F1*F3+17*F1*F2+F3**2+13*F2**2+5*F2*F3)*(Y2+
     &Y3)*(-2*X2+X3)/S/648
C
      A13(IELEM)=-(13*F1**2+17*F1*F3+5*F1*F2+F2**2+13*F3**2+5*F2*F3)*(Y2
     &+Y3)*(-X2+2*X3)/S/648
C
      A14(IELEM)=-(-26*F1**2*Y2*X2+13*F1**2*Y2*X3-10*F1*F3*Y2*X2+5*F1*F3
     &*Y2*X3-34*F1*F2*Y2*X2+17*F1*F2*Y2*X3-2*F3**2*Y2*X2+F3**2*Y2*X3-26*
     &F2**2*Y2*X2+13*F2**2*Y2*X3-10*F2*F3*Y2*X2+5*F2*F3*Y2*X3+13*F1**2*Y
     &3*X2-26*F1**2*Y3*X3+17*F1*F3*Y3*X2-34*F1*F3*Y3*X3+5*F1*F2*Y3*X2-10
     &*F1*F2*Y3*X3+F2**2*Y3*X2-2*F2**2*Y3*X3+13*F3**2*Y3*X2-26*F3**2*Y3*
     &X3+5*F2*F3*Y3*X2-10*F2*F3*Y3*X3)/S/216
C
      A21(IELEM)=-(13*F1**2+5*F1*F3+17*F1*F2+F3**2+13*F2**2+5*F2*F3)*(2*
     &Y2-Y3)*(X2+X3)/S/648
C
      A22(IELEM)=-(-5*F1*F3*Y2*X3-5*F1*F3*Y3*X2+25*F1*F3*Y3*X3+22*F1*F2*
     &Y2*X2+73*F2*F3*Y3*X3+7*F1*F2*Y3*X2+37*F1*F2*Y3*X3+22*F2*F3*Y2*X2-2
     &9*F2*F3*Y2*X3+11*F1**2*Y2*X3-25*F3**2*Y2*X3+65*F2**2*Y3*X3+14*F1**
     &2*Y2*X2-13*F2**2*Y2*X3+26*F2**2*Y2*X2-25*F3**2*Y3*X2+11*F1**2*Y3*X
     &2+17*F1**2*Y3*X3+14*F3**2*Y2*X2-13*F2**2*Y3*X2+53*F3**2*Y3*X3+10*F
     &1*F3*Y2*X2+7*F1*F2*Y2*X3-29*F2*F3*Y3*X2)/S/648
C
      A23(IELEM)=(13*F2**2+5*F1*F2+17*F2*F3+F1**2+13*F3**2+5*F1*F3)*(2*Y
     &2-Y3)*(-X2+2*X3)/S/648
C
      A24(IELEM)=(-5*F1*F3*Y2*X3-5*F1*F3*Y3*X2+10*F1*F3*Y3*X3+22*F1*F2*Y
     &2*X2+34*F2*F3*Y3*X3-5*F1*F2*Y3*X2+10*F1*F2*Y3*X3+22*F2*F3*Y2*X2-29
     &*F2*F3*Y2*X3+11*F1**2*Y2*X3-25*F3**2*Y2*X3+26*F2**2*Y3*X3+14*F1**2
     &*Y2*X2-13*F2**2*Y2*X3+26*F2**2*Y2*X2-13*F3**2*Y3*X2-F1**2*Y3*X2+2*
     &F1**2*Y3*X3+14*F3**2*Y2*X2-13*F2**2*Y3*X2+26*F3**2*Y3*X3+10*F1*F3*
     &Y2*X2+7*F1*F2*Y2*X3-17*F2*F3*Y3*X2)/S/216
C
      A31(IELEM)=(13*F1**2+17*F1*F3+5*F1*F2+F2**2+13*F3**2+5*F2*F3)*(Y2-
     &2*Y3)*(X2+X3)/S/648
C
      A32(IELEM)=(13*F2**2+5*F1*F2+17*F2*F3+F1**2+13*F3**2+5*F1*F3)*(Y2-
     &2*Y3)*(-2*X2+X3)/S/648
C
      A33(IELEM)=-(7*F1*F3*Y2*X3+7*F1*F3*Y3*X2+22*F1*F3*Y3*X3+25*F1*F2*Y
     &2*X2+22*F2*F3*Y3*X3-5*F1*F2*Y3*X2+10*F1*F2*Y3*X3+73*F2*F3*Y2*X2-29
     &*F2*F3*Y2*X3+11*F1**2*Y2*X3-13*F3**2*Y2*X3+14*F2**2*Y3*X3+17*F1**2
     &*Y2*X2-25*F2**2*Y2*X3+53*F2**2*Y2*X2-13*F3**2*Y3*X2+11*F1**2*Y3*X2
     &+14*F1**2*Y3*X3+65*F3**2*Y2*X2-25*F2**2*Y3*X2+26*F3**2*Y3*X3+37*F1
     &*F3*Y2*X2-5*F1*F2*Y2*X3-29*F2*F3*Y3*X2)/S/648
C
      A34(IELEM)=-(5*F1*F3*Y2*X3-7*F1*F3*Y3*X2-22*F1*F3*Y3*X3-10*F1*F2*Y
     &2*X2-22*F2*F3*Y3*X3+5*F1*F2*Y3*X2-10*F1*F2*Y3*X3-34*F2*F3*Y2*X2+17
     &*F2*F3*Y2*X3+F1**2*Y2*X3+13*F3**2*Y2*X3-14*F2**2*Y3*X3-2*F1**2*Y2*
     &X2+13*F2**2*Y2*X3-26*F2**2*Y2*X2+13*F3**2*Y3*X2-11*F1**2*Y3*X2-14*
     &F1**2*Y3*X3-26*F3**2*Y2*X2+25*F2**2*Y3*X2-26*F3**2*Y3*X3-10*F1*F3*
     &Y2*X2+5*F1*F2*Y2*X3+29*F2*F3*Y3*X2)/S/216
C
      A41(IELEM)=-(-26*F1**2*Y2*X2+13*F1**2*Y3*X2-10*F1*F3*Y2*X2+5*F1*F3
     &*Y3*X2-34*F1*F2*Y2*X2+17*F1*F2*Y3*X2-2*F3**2*Y2*X2+F3**2*Y3*X2-26*
     &F2**2*Y2*X2+13*F2**2*Y3*X2-10*F2*F3*Y2*X2+5*F2*F3*Y3*X2+13*F1**2*Y
     &2*X3-26*F1**2*Y3*X3+17*F1*F3*Y2*X3-34*F1*F3*Y3*X3+5*F1*F2*Y2*X3-10
     &*F1*F2*Y3*X3+F2**2*Y2*X3-2*F2**2*Y3*X3+13*F3**2*Y2*X3-26*F3**2*Y3*
     &X3+5*F2*F3*Y2*X3-10*F2*F3*Y3*X3)/S/216
C
      A42(IELEM)=-(5*F1*F3*Y2*X3+5*F1*F3*Y3*X2-10*F1*F3*Y3*X3-22*F1*F2*Y
     &2*X2-34*F2*F3*Y3*X3-7*F1*F2*Y3*X2-10*F1*F2*Y3*X3-22*F2*F3*Y2*X2+17
     &*F2*F3*Y2*X3+F1**2*Y2*X3+13*F3**2*Y2*X3-26*F2**2*Y3*X3-14*F1**2*Y2
     &*X2+13*F2**2*Y2*X3-26*F2**2*Y2*X2+25*F3**2*Y3*X2-11*F1**2*Y3*X2-2*
     &F1**2*Y3*X3-14*F3**2*Y2*X2+13*F2**2*Y3*X2-26*F3**2*Y3*X3-10*F1*F3*
     &Y2*X2+5*F1*F2*Y2*X3+29*F2*F3*Y3*X2)/S/216
C
      A43(IELEM)=(7*F1*F3*Y2*X3-5*F1*F3*Y3*X2+22*F1*F3*Y3*X3+10*F1*F2*Y2
     &*X2+22*F2*F3*Y3*X3-5*F1*F2*Y3*X2+10*F1*F2*Y3*X3+34*F2*F3*Y2*X2-29*
     &F2*F3*Y2*X3+11*F1**2*Y2*X3-13*F3**2*Y2*X3+14*F2**2*Y3*X3+2*F1**2*Y
     &2*X2-25*F2**2*Y2*X3+26*F2**2*Y2*X2-13*F3**2*Y3*X2-F1**2*Y3*X2+14*F
     &1**2*Y3*X3+26*F3**2*Y2*X2-13*F2**2*Y3*X2+26*F3**2*Y3*X3+10*F1*F3*Y
     &2*X2-5*F1*F2*Y2*X3-17*F2*F3*Y3*X2)/S/216
C
      A44(IELEM)=(5*F1*F3*Y2*X3+5*F1*F3*Y3*X2-22*F1*F3*Y3*X3-22*F1*F2*Y2
     &*X2-22*F2*F3*Y3*X3+5*F1*F2*Y3*X2-10*F1*F2*Y3*X3-22*F2*F3*Y2*X2+17*
     &F2*F3*Y2*X3+F1**2*Y2*X3+13*F3**2*Y2*X3-14*F2**2*Y3*X3-14*F1**2*Y2*
     &X2+13*F2**2*Y2*X3-26*F2**2*Y2*X2+13*F3**2*Y3*X2+F1**2*Y3*X2-14*F1*
     &*2*Y3*X3-14*F3**2*Y2*X2+13*F2**2*Y3*X2-26*F3**2*Y3*X3-10*F1*F3*Y2*
     &X2+5*F1*F2*Y2*X3+17*F2*F3*Y3*X2)/S/72
C
C   END OF THE LOOP ON THE ELEMENTS
C
      ENDDO
C
C-----------------------------------------------------------------------
C
C   CASE NOT IMPLEMENTED
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
        IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
1000    FORMAT(1X,'MT99BB (BIEF) : MATRICE NON PREVUE : ',A16)
1001    FORMAT(1X,'MT99BB (BIEF) : MATRIX NOT IMPLEMENTED:',A16)
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
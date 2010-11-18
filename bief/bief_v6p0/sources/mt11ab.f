C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>  EXAMPLE WITH ICOORD=1<br>
!>             NPOIN
!>               _          /            D
!>  A(I,J)=-XMUL>_   F  *  /  PSI2(J) *  --( PSI1(K) * PSI1(I) ) D(OMEGA)
!>                    K   /OMEGA         DX
!>              K=1<br><br>
!>  BEWARE THE MINUS SIGN !!<br>
!>  PSI1: BASES OF TYPE P1 TRIANGLE
!>  PSI2: BASES OF TYPE QUASI-BUBBLE TRIANGLE<br>
!>  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
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
!>    </th><td> A11, A12, A13, A14, A21, A22, A23, A24, A31, A32, A33, A34, F, ICOORD, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F2, F3, F4, IELEM, IELMF, X2, X3, XSU216, XSUR18, XSUR72, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT11AB
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
!> </td><td> 09/12/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; C MOULIN (LNH) 30 87 83 81
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
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>ICOORD
!></td><td>--></td><td>1: DERIVEE SUIVANT X, 2:SUIVANT Y
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
                        SUBROUTINE MT11AB
     &(  A11 , A12 , A13 , A14 ,
     &   A21 , A22 , A23 , A24 ,
     &   A31 , A32 , A33 , A34 ,
     &   XMUL,SF,F,XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,
     &   NELEM,NELMAX,ICOORD)
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
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| ICOORD         |-->| 1: DERIVEE SUIVANT X, 2:SUIVANT Y
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
      USE BIEF, EX_MT11AB => MT11AB
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
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*),A34(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
C
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF
      DOUBLE PRECISION X2,X3,Y2,Y3,F1,F2,F3,F4
      DOUBLE PRECISION XSUR18,XSUR72,XSU216
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
      XSUR18 = XMUL/18.D0
      XSUR72 = XMUL/72.D0
      XSU216 = XMUL/216.D0
C
      IELMF=SF%ELM
C
C-----------------------------------------------------------------------
C  CASE WHERE F IS OF P1 DISCRETISATION
C-----------------------------------------------------------------------
C
      IF(IELMF.EQ.11) THEN
C
C================================
C  CASE OF DERIVATIVE WRT X =
C================================
C
        IF(ICOORD.EQ.1) THEN
C
C   LOOP ON THE ELEMENTS
C
        DO 1 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
C
C   EXTRADIAGONAL TERMS
C
        A12(IELEM) = (2*Y2*(-7*F2-5*F1)+Y3*(5*F3+9*F2+10*F1))*XSU216
        A13(IELEM) = (Y2*(-9*F3-5*F2-10*F1)+2*Y3*(7*F3+5*F1))*XSU216
        A14(IELEM) = (Y2*(-F2-2*F1)+Y3*(F3+2*F1))*XSUR18
        A21(IELEM) = (5*Y2*(F3-F1)+Y3*(-5*F3-10*F2-9*F1))*XSU216
        A23(IELEM) = (5*Y2*(F3-F1)+2*Y3*(-7*F3-5*F2))*XSU216
        A24(IELEM) = (Y2*(F3-F1)+Y3*(-F3-2*F2))*XSUR18
        A31(IELEM) = (Y2*(10*F3+5*F2+9*F1)+5*Y3*(-F2+F1))*XSU216
        A32(IELEM) = (2*Y2*(5*F3+7*F2)+5*Y3*(-F2+F1))*XSU216
        A34(IELEM) = (Y2*(2*F3+F2)+Y3*(-F2+F1))*XSUR18
C
C   DIAGONAL TERMS
C
        A11(IELEM) = (Y2*(9*F3-5*F2-28*F1)+Y3*(5*F3-9*F2+28*F1))*XSU216
        A22(IELEM) = (14*Y2*(F3-F1)+Y3*(-5*F3-28*F2+9*F1))*XSU216
        A33(IELEM) = (Y2*(28*F3+5*F2-9*F1)+14*Y3*(-F2+F1))*XSU216
C
1     CONTINUE
C
        ELSEIF(ICOORD.EQ.2) THEN
C
C================================
C  CASE OF DERIVATIVE WRT Y =
C================================
C
        DO 2 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
C
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
C
C   EXTRADIAGONAL TERMS
C
        A12(IELEM) = (2*X2*(7*F2+5*F1)+X3*(-5*F3-9*F2-10*F1))*XSU216
        A13(IELEM) = (X2*(9*F3+5*F2+10*F1)+2*X3*(-7*F3-5*F1))*XSU216
        A14(IELEM) = (X2*(F2+2*F1)+X3*(-F3-2*F1))*XSUR18
        A21(IELEM) = (5*X2*(-F3+F1)+X3*(5*F3+10*F2+9*F1))*XSU216
        A23(IELEM) = (5*X2*(-F3+F1)+2*X3*(7*F3+5*F2))*XSU216
        A24(IELEM) = (X2*(-F3+F1)+X3*(F3+2*F2))*XSUR18
        A31(IELEM) = (X2*(-10*F3-5*F2-9*F1)+5*X3*(F2-F1))*XSU216
        A32(IELEM) = (2*X2*(-5*F3-7*F2)+5*X3*(F2-F1))*XSU216
        A34(IELEM) = (X2*(-2*F3-F2)+X3*(F2-F1))*XSUR18
C
C   DIAGONAL TERMS
C
        A11(IELEM) = (X2*(-9*F3+5*F2+28*F1)+X3*(-5*F3+9*F2-28*
     &                F1))*XSU216
        A22(IELEM) = (14*X2*(-F3+F1)+X3*(5*F3+28*F2-9*F1))*XSU216
        A33(IELEM) = (X2*(-28*F3-5*F2+9*F1)+14*X3*(F2-F1))*XSU216
C
2       CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
        ENDIF
C
C
C-----------------------------------------------------------------------
C  CASE WHERE F IS OF QUASI-BUBBLE DISCRETISATION
C-----------------------------------------------------------------------
C
      ELSEIF(IELMF.EQ.12) THEN
C
C================================
C  CASE OF DERIVATIVE WRT X =
C================================
C
        IF(ICOORD.EQ.1) THEN
C
C   LOOP ON THE ELEMENTS
C
        DO 3 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
C
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
        F4  =  F(IKLE4(IELEM))
C
C   EXTRADIAGONAL TERMS
C
        A12(IELEM) = (Y2*(-F3+3*F4-15*F2-11*F1)+Y3*(2*F3+9*F4+
     &                6*F2+7*F1))*XSU216
        A13(IELEM) = (Y2*(-6*F3-9*F4-2*F2-7*F1)+Y3*(15*F3-3*F4
     &               +F2+11*F1))*XSU216
        A14(IELEM) = (Y2*(F3-3*F4-3*F2-7*F1)+Y3*(3*F3+3*F4-F2+
     &                                                     7*F1))*XSUR72
        A21(IELEM) = (Y2*(F3+12*F4-4*F2-9*F1)+Y3*(-2*F3-9*F4-7
     &               *F2-6*F1))*XSU216
        A23(IELEM) = (Y2*(9*F3-12*F4+4*F2-F1)+Y3*(-15*F3+3*F4-
     &                11*F2-F1))*XSU216
        A24(IELEM) = (4*Y2*(F3-F1)+Y3*(-3*F3-3*F4-7*F2+F1))*XSUR72
        A31(IELEM) = (Y2*(7*F3+9*F4+2*F2+6*F1)+Y3*(4*F3-12*F4-
     &                F2+9*F1))*XSU216
        A32(IELEM) = (Y2*(11*F3-3*F4+15*F2+F1)+Y3*(-4*F3+12*F4-
     &                9*F2+F1))*XSU216
        A34(IELEM) = (Y2*(7*F3+3*F4+3*F2-F1)+4*Y3*(-F2+F1))*XSUR72
C
C   DIAGONAL TERMS
C
        A11(IELEM) = (Y2*(4*F3+15*F4-10*F2-33*F1)+Y3*(10*F3-15
     &                                           *F4-4*F2+33*F1))*XSU216
        A22(IELEM) = (14*Y2*(F3-F1)+Y3*(-10*F3+15*F4-33*F2+4*F1))*XSU216
        A33(IELEM) = (Y2*(33*F3-15*F4+10*F2-4*F1)+14*Y3*(-F2+F1))*XSU216
C
3       CONTINUE
C
        ELSEIF(ICOORD.EQ.2) THEN
C
C================================
C  CASE OF DERIVATIVE WRT Y =
C================================
C
        DO 4 IELEM = 1 , NELEM
C
C   INITIALISES THE GEOMETRICAL VARIABLES
C
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
C
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
        F4  =  F(IKLE4(IELEM))
C
C   EXTRADIAGONAL TERMS
C
        A12(IELEM) = (X2*(F3-3*F4+15*F2+11*F1)+X3*(-2*F3-9*F4-
     &                                                6*F2-7*F1))*XSU216
        A13(IELEM) = (X2*(6*F3+9*F4+2*F2+7*F1)+X3*(-15*F3+3*F4
     &                                                -F2-11*F1))*XSU216
        A14(IELEM) = (X2*(-F3+3*F4+3*F2+7*F1)+X3*(-3*F3-3*F4+F2
     &                                                    -7*F1))*XSUR72
        A21(IELEM) = (X2*(-F3-12*F4+4*F2+9*F1)+X3*(2*F3+9*F4+7
     &                                                 *F2+6*F1))*XSU216
        A23(IELEM) = (X2*(-9*F3+12*F4-4*F2+F1)+X3*(15*F3-3*F4+
     &                                                 11*F2+F1))*XSU216
        A24(IELEM) = (4*X2*(-F3+F1)+X3*(3*F3+3*F4+7*F2-F1))*XSUR72
        A31(IELEM) = (X2*(-7*F3-9*F4-2*F2-6*F1)+X3*(-4*F3+12*
     &                                               F4+F2-9*F1))*XSU216
        A32(IELEM) = (X2*(-11*F3+3*F4-15*F2-F1)+X3*(4*F3-12*F4+
     &                                                  9*F2-F1))*XSU216
        A34(IELEM) = (X2*(-7*F3-3*F4-3*F2+F1)+4*X3*(F2-F1))*XSUR72
C
C   DIAGONAL TERMS
C
        A11(IELEM) = (X2*(-4*F3-15*F4+10*F2+33*F1)+X3*(-10*F3+
     &                                         15*F4+4*F2-33*F1))*XSU216
        A22(IELEM) = (14*X2*(-F3+F1)+X3*(10*F3-15*F4+33*F2-4*F1))*XSU216
        A33(IELEM) = (X2*(-33*F3+15*F4-10*F2+4*F1)+14*X3*(F2-F1))*XSU216
C
4       CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
       IF (LNG.EQ.1) WRITE(LU,100) IELMF
       IF (LNG.EQ.2) WRITE(LU,101) IELMF
100    FORMAT(1X,'MT11AB (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,' NON PREVUE')
101    FORMAT(1X,'MT11AB (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
       CALL PLANTE(0)
       STOP
      ENDIF
C
200       FORMAT(1X,'MT11AB (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'MT11AB (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
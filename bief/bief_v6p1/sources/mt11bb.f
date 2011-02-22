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
!>  PSI2: BASES OF TYPE IELM2<br>
!>  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @code
!>  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!>
!>      XA(IELEM,1)  ---->  A(1,2)
!>      XA(IELEM,2)  ---->  A(1,3)
!>      XA(IELEM,3)  ---->  A(2,3)
!>      XA(IELEM,4)  ---->  A(2,1)
!>      XA(IELEM,5)  ---->  A(3,1)
!>      XA(IELEM,6)  ---->  A(3,2)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A14, A21, A22, A23, A24, A31, A32, A33, A34, A41, A42, A43, A44, F, ICOORD, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F2, F3, F4, IELEM, IELMF, X2, X3, XSU216, XSUR24, XSUR72, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT11BB
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
                        SUBROUTINE MT11BB
     &(  A11 , A12 , A13 , A14 ,
     &   A21 , A22 , A23 , A24 ,
     &   A31 , A32 , A33 , A34 ,
     &   A41 , A42 , A43 , A44 ,
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
C| A41            |---| 
C| A42            |---| 
C| A43            |---| 
C| A44            |---| 
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
      USE BIEF, EX_MT11BB => MT11BB
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
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*),A44(*)
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
      DOUBLE PRECISION XSUR72,XSUR24,XSU216
C
C-----------------------------------------------------------------------
C
      XSUR72 = XMUL/72.D0
      XSUR24 = XMUL/24.D0
      XSU216 = XMUL/216.D0
C
      IELMF=SF%ELM
C
C-----------------------------------------------------------------------
C  CASE WHERE F IS OF TYPE P1
C-----------------------------------------------------------------------
C
      IF(IELMF.EQ.11) THEN
C
C================================
C  DERIVATIVE WRT X  =
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
      A12(IELEM) = (Y2*(F3-14*F2-11*F1)+Y3*(F3+4*F2+7*F1))*XSU216
      A13(IELEM) = (Y2*(-4*F3-F2-7*F1)+Y3*(14*F3-F2+11*F1))*XSU216
      A14(IELEM) = (Y2*(-F3-4*F2-7*F1)+Y3*(4*F3+F2+7*F1))*XSUR72
      A21(IELEM) = (2*Y2*(F3-2*F2-5*F1)+Y3*(-F3-7*F2-4*F1))*XSU216
      A23(IELEM) = (2*Y2*(5*F3+2*F2-F1)+Y3*(-14*F3-11*F2+F1))*XSU216
      A24(IELEM) = (3*Y2*(F3-F1)+Y3*(-4*F3-7*F2-F1))*XSUR72
      A31(IELEM) = (Y2*(7*F3+F2+4*F1)+2*Y3*(2*F3-F2+5*F1))*XSU216
      A32(IELEM) = (Y2*(11*F3+14*F2-F1)+2*Y3*(-2*F3-5*F2+F1))*XSU216
      A34(IELEM) = (Y2*(7*F3+4*F2+F1)+3*Y3*(-F2+F1))*XSUR72
      A41(IELEM) = (Y2*(3*F3+4*F2+5*F1)+Y3*(-4*F3-3*F2-5*F1))*XSUR72
      A42(IELEM) = (Y2*(-F3+F1)+Y3*(4*F3+5*F2+3*F1))*XSUR72
      A43(IELEM) = (Y2*(-5*F3-4*F2-3*F1)+Y3*(F2-F1))*XSUR72
C
C   DIAGONAL TERMS
C
        A11(IELEM) = (Y2*(2*F3-3*F2-11*F1)+Y3*(3*F3-2*F2+11*F1
     &                ))*XSUR72
        A22(IELEM) = (5*Y2*(F3-F1)+Y3*(-3*F3-11*F2+2*F1))*XSUR72
        A33(IELEM) = (Y2*(11*F3+3*F2-2*F1)+5*Y3*(-F2+F1))*XSUR72
        A44(IELEM) = (Y2*(F3-F1)+Y3*(-F2+F1))*XSUR24
C
1     CONTINUE
C
        ELSEIF(ICOORD.EQ.2) THEN
C
C================================
C  DERIVATIVE WRT Y  =
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
      A12(IELEM) = (X2*(-F3+14*F2+11*F1)+X3*(-F3-4*F2-7*F1))*XSU216
      A13(IELEM) = (X2*(4*F3+F2+7*F1)+X3*(-14*F3+F2-11*F1))*XSU216
      A14(IELEM) = (X2*(F3+4*F2+7*F1)+X3*(-4*F3-F2-7*F1))*XSUR72
      A21(IELEM) = (2*X2*(-F3+2*F2+5*F1)+X3*(F3+7*F2+4*F1))*XSU216
      A23(IELEM) = (2*X2*(-5*F3-2*F2+F1)+X3*(14*F3+11*F2-F1))*XSU216
      A24(IELEM) = (3*X2*(-F3+F1)+X3*(4*F3+7*F2+F1))*XSUR72
      A31(IELEM) = (X2*(-7*F3-F2-4*F1)+2*X3*(-2*F3+F2-5*F1))*XSU216
      A32(IELEM) = (X2*(-11*F3-14*F2+F1)+2*X3*(2*F3+5*F2-F1))*XSU216
      A34(IELEM) = (X2*(-7*F3-4*F2-F1)+3*X3*(F2-F1))*XSUR72
      A41(IELEM) = (X2*(-3*F3-4*F2-5*F1)+X3*(4*F3+3*F2+5*F1))*XSUR72
      A42(IELEM) = (X2*(F3-F1)+X3*(-4*F3-5*F2-3*F1))*XSUR72
      A43(IELEM) = (X2*(5*F3+4*F2+3*F1)+X3*(-F2+F1))*XSUR72
C
C   DIAGONAL TERMS
C
        A11(IELEM) = (X2*(-2*F3+3*F2+11*F1)+X3*(-3*F3+2*F2-11*
     &                F1))*XSUR72
        A22(IELEM) = (5*X2*(-F3+F1)+X3*(3*F3+11*F2-2*F1))*XSUR72
        A33(IELEM) = (X2*(-11*F3-3*F2+2*F1)+5*X3*(F2-F1))*XSUR72
        A44(IELEM) = (X2*(-F3+F1)+X3*(F2-F1))*XSUR24
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
C  CASE WHERE F IS OF TYPE QUASI-BUBBLE
C-----------------------------------------------------------------------
C
      ELSEIF(IELMF.EQ.12) THEN
C
C================================
C  DERIVATIVE WRT X  =
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
        A12(IELEM) = (Y2*(F4-5*F2-4*F1)+Y3*(F4+F2+2*F1))*XSUR72
        A13(IELEM) = (Y2*(-F3-F4-2*F1)+Y3*(5*F3-F4+4*F1))*XSUR72
        A14(IELEM) = (Y2*(-F4-F2-2*F1)+Y3*(F3+F4+2*F1))*XSUR24
        A21(IELEM) = (2*Y2*(F4-F2-2*F1)+Y3*(-F4-2*F2-F1))*XSUR72
        A23(IELEM) = (2*Y2*(2*F3-F4+F2)+Y3*(-5*F3+F4-4*F2))*XSUR72
        A24(IELEM) = (Y2*(F3-F1)+Y3*(-F3-F4-2*F2))*XSUR24
        A31(IELEM) = (Y2*(2*F3+F4+F1)+2*Y3*(F3-F4+2*F1))*XSUR72
        A32(IELEM) = (Y2*(4*F3-F4+5*F2)+2*Y3*(-F3+F4-2*F2))*XSUR72
        A34(IELEM) = (Y2*(2*F3+F4+F2)+Y3*(-F2+F1))*XSUR24
        A41(IELEM) = (Y2*(F3+6*F4+2*F2+3*F1)+Y3*(-2*F3-6*F4-F2-3*F1)
     &              )*XSUR72
        A42(IELEM) = (Y2*(-F3+F1)+Y3*(2*F3+6*F4+3*F2+F1))*XSUR72
        A43(IELEM) = (Y2*(-3*F3-6*F4-2*F2-F1)+Y3*(F2-F1))*XSUR72
C
C   DIAGONAL TERMS
C
        A11(IELEM) = (Y2*(F3+3*F4-4*F2-12*F1)+Y3*(4*F3-3*F4-F2+
     &               12*F1))*XSUR72
        A22(IELEM) = (5*Y2*(F3-F1)+Y3*(-4*F3+3*F4-12*F2+F1))*XSUR72
        A33(IELEM) = (Y2*(12*F3-3*F4+4*F2-F1)+5*Y3*(-F2+F1))*XSUR72
        A44(IELEM) = (Y2*(F3-F1)+Y3*(-F2+F1))*XSUR24
C
3       CONTINUE
C
        ELSEIF(ICOORD.EQ.2) THEN
C
C================================
C  DERIVATIVE WRT Y  =
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
C   EXTRADIAGONAL TERMS
C
      A12(IELEM) = (X2*(-F4+5*F2+4*F1)+X3*(-F4-F2-2*F1))*XSUR72
      A13(IELEM) = (X2*(F3+F4+2*F1)+X3*(-5*F3+F4-4*F1))*XSUR72
      A14(IELEM) = (X2*(F4+F2+2*F1)+X3*(-F3-F4-2*F1))*XSUR24
      A21(IELEM) = (2*X2*(-F4+F2+2*F1)+X3*(F4+2*F2+F1))*XSUR72
      A23(IELEM) = (2*X2*(-2*F3+F4-F2)+X3*(5*F3-F4+4*F2))*XSUR72
      A24(IELEM) = (X2*(-F3+F1)+X3*(F3+F4+2*F2))*XSUR24
      A31(IELEM) = (X2*(-2*F3-F4-F1)+2*X3*(-F3+F4-2*F1))*XSUR72
      A32(IELEM) = (X2*(-4*F3+F4-5*F2)+2*X3*(F3-F4+2*F2))*XSUR72
      A34(IELEM) = (X2*(-2*F3-F4-F2)+X3*(F2-F1))*XSUR24
      A41(IELEM) = (X2*(-F3-6*F4-2*F2-3*F1)+X3*(2*F3+6*F4+F2+3*F1)
     &            )*XSUR72
      A42(IELEM) = (X2*(F3-F1)+X3*(-2*F3-6*F4-3*F2-F1))*XSUR72
      A43(IELEM) = (X2*(3*F3+6*F4+2*F2+F1)+X3*(-F2+F1))*XSUR72
C
C   DIAGONAL TERMS
C
        A11(IELEM) = (X2*(-F3-3*F4+4*F2+12*F1)+X3*(-4*F3+3*F4+
     &                F2-12*F1))*XSUR72
        A22(IELEM) = (5*X2*(-F3+F1)+X3*(4*F3-3*F4+12*F2-F1))*XSUR72
        A33(IELEM) = (X2*(-12*F3+3*F4-4*F2+F1)+5*X3*(F2-F1))*XSUR72
        A44(IELEM) = (X2*(-F3+F1)+X3*(F2-F1))*XSUR24
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
100    FORMAT(1X,'MT11BB (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,' NON PREVUE')
101    FORMAT(1X,'MT11BB (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
       CALL PLANTE(0)
       STOP
      ENDIF
C
200       FORMAT(1X,'MT11BB (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'MT11BB (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
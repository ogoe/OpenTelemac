C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>  EXAMPLE WITH ICOORD=1<br>
!>                  /                -->  --->
!>  A(I,J) = XMUL  /   PSIJ GRAD(F)   U .GRAD(PSII) D(OMEGA)
!>                /OMEGA
!>                                 -->
!>                      F VECTOR    U  VECTOR WITH COMPONENTS U,V,W<br><br>
!>  BEWARE THE MINUS SIGN !!<br>
!>  PSI1: LINEAR
!>  PSI2: LINEAR<br>
!>  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!>  IT WOULD BE A DERIVATIVE WRT Z WITH ICOORD=3
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
!>    </th><td> A11, A12, A13, A21, A22, A23, A31, A32, A33, F, ICOORD, IKLE1, IKLE2, IKLE3, NELEM, NELMAX, SF, SU, SURFAC, SV, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DEN, F1, F2, F3, IELEM, IELMF, IELMU, IELMV, U1, U123, U2, U3, V1, V123, V2, V3, X2, X3, XSUR12, XSUR48, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT12AA
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
!>          <tr><td>A21
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
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
                        SUBROUTINE MT12AA
     &(  A11 , A12 , A13 ,
     &   A21 , A22 , A23 ,
     &   A31 , A32 , A33 ,
     &   XMUL,SF,SU,SV,F,U,V,
     &   XEL,YEL,SURFAC,
     &   IKLE1,IKLE2,IKLE3,
     &   NELEM,NELMAX,ICOORD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A21            |---| 
C| A22            |---| 
C| A23            |---| 
C| A31            |---| 
C| A32            |---| 
C| A33            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| ICOORD         |-->| 1: DERIVEE SUIVANT X, 2:SUIVANT Y
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
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
      USE BIEF, EX_MT12AA => MT12AA
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
C
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
C
C     STRUCTURES OF F, U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
C
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF,IELMU,IELMV
      DOUBLE PRECISION XSUR12,XSUR48,X2,X3,Y2,Y3,F1,F2,F3,DEN
      DOUBLE PRECISION U1,U2,U3,V1,V2,V3,U123,V123
C
C-----------------------------------------------------------------------
C
      XSUR12 = XMUL/12.D0
      XSUR48 = XMUL/48.D0
C
C-----------------------------------------------------------------------
C
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
C
C  CASE WHERE F IS OF TYPE P1 AND U P0
C
      IF(IELMF.EQ.11.AND.IELMU.EQ.10.AND.IELMV.EQ.10) THEN
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
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
C
      F1  =  F(IKLE1(IELEM))
      F2  =  F(IKLE2(IELEM)) - F1
      F3  =  F(IKLE3(IELEM)) - F1
C
      DEN = (F3*Y2 - F2*Y3) * XSUR12 / SURFAC(IELEM)
C
C   EXTRADIAGONAL TERMS
C
      A23(IELEM) = (  X3 *V(IELEM) -  Y3    *U(IELEM) )*DEN
      A31(IELEM) =-(  X2 *V(IELEM) -     Y2 *U(IELEM) )*DEN
C
      A12(IELEM) = - A23(IELEM) - A31(IELEM)
      A13(IELEM) =   A12(IELEM)
      A21(IELEM) =   A23(IELEM)
      A32(IELEM) =   A31(IELEM)
C
C DIAGONAL TERMS (THE SUM OF EACH COLUMN IS 0)
C
      A11(IELEM) = - A21(IELEM) - A31(IELEM)
      A22(IELEM) = - A12(IELEM) - A32(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
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
      Y2  =  YEL(IELEM,2)
      Y3  =  YEL(IELEM,3)
C
      F1  =  F(IKLE1(IELEM))
      F2  =  F(IKLE2(IELEM)) - F1
      F3  =  F(IKLE3(IELEM)) - F1
C
      DEN = (F3*X2 - F2*X3) * XSUR12 / SURFAC(IELEM)
C
C   EXTRADIAGONAL TERMS
C
      A23(IELEM) = -( X3*V(IELEM) - Y3*U(IELEM) ) * DEN
      A31(IELEM) =  ( X2*V(IELEM) - Y2*U(IELEM) ) * DEN
C
      A12(IELEM) = - A23(IELEM) - A31(IELEM)
      A13(IELEM) =   A12(IELEM)
      A21(IELEM) =   A23(IELEM)
      A32(IELEM) =   A31(IELEM)
C
C DIAGONAL TERMS (THE SUM OF EACH COLUMN IS 0)
C
      A11(IELEM) = - A21(IELEM) - A31(IELEM)
      A22(IELEM) = - A12(IELEM) - A32(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
2     CONTINUE
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
200       FORMAT(1X,'MT12AA (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'MT12AA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(0)
          STOP
C
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMF.EQ.11.AND.IELMU.EQ.11) THEN
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
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
C
      F1  =  F(IKLE1(IELEM))
      F2  =  F(IKLE2(IELEM)) - F1
      F3  =  F(IKLE3(IELEM)) - F1
C
      U1  =  U(IKLE1(IELEM))
      U2  =  U(IKLE2(IELEM))
      U3  =  U(IKLE3(IELEM))
C
      V1  =  V(IKLE1(IELEM))
      V2  =  V(IKLE2(IELEM))
      V3  =  V(IKLE3(IELEM))
C
      U123 = U1 + U2 + U3
      V123 = V1 + V2 + V3
C
      DEN = (F3*Y2 - F2*Y3) * XSUR48 / SURFAC(IELEM)
C
C   EXTRADIAGONAL TERMS
C
      A12(IELEM) = ( (X2-X3)*(V123+V2) + (Y3-Y2)*(U123+U2) )*DEN
C
      A13(IELEM) = ( (X2-X3)*(V123+V3) + (Y3-Y2)*(U123+U3) )*DEN
C
      A23(IELEM) = ( X3*(V123+V3) - Y3*(U123+U3) )*DEN
C
      A21(IELEM) = ( X3*(V123+V1) - Y3*(U123+U1) )*DEN
C
      A31(IELEM) =-( X2*(V123+V1) - Y2*(U123+U1) )*DEN
C
      A32(IELEM) =-( X2*(V123+V2) - Y2*(U123+U2) )*DEN
C
C DIAGONAL TERMS (THE SUM OF EACH COLUMN IS 0)
C
      A11(IELEM) = - A21(IELEM) - A31(IELEM)
      A22(IELEM) = - A12(IELEM) - A32(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
3     CONTINUE
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
      Y2  =  YEL(IELEM,2)
      Y3  =  YEL(IELEM,3)
C
      F1  =  F(IKLE1(IELEM))
      F2  =  F(IKLE2(IELEM)) - F1
      F3  =  F(IKLE3(IELEM)) - F1
C
      U1  =  U(IKLE1(IELEM))
      U2  =  U(IKLE2(IELEM))
      U3  =  U(IKLE3(IELEM))
C
      V1  =  V(IKLE1(IELEM))
      V2  =  V(IKLE2(IELEM))
      V3  =  V(IKLE3(IELEM))
C
      U123 = U1 + U2 + U3
      V123 = V1 + V2 + V3
C
      DEN = (F3*X2 - F2*X3) * XSUR48 / SURFAC(IELEM)
C
C   EXTRADIAGONAL TERMS
C
      A12(IELEM) =-( (X2-X3)*(V123+V2) + (Y3-Y2)*(U123+U2) )*DEN
C
      A13(IELEM) =-( (X2-X3)*(V123+V3) + (Y3-Y2)*(U123+U3) )*DEN
C
      A23(IELEM) =-( X3*(V123+V3) - Y3*(U123+U3) )*DEN
C
      A21(IELEM) =-( X3*(V123+V1) - Y3*(U123+U1) )*DEN
C
      A31(IELEM) = ( X2*(V123+V1) - Y2*(U123+U1) )*DEN
C
      A32(IELEM) = ( X2*(V123+V2) - Y2*(U123+U2) )*DEN
C
C DIAGONAL TERMS (THE SUM OF EACH COLUMN IS 0)
C
      A11(IELEM) = - A21(IELEM) - A31(IELEM)
      A22(IELEM) = - A12(IELEM) - A32(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
4     CONTINUE
C
        ELSE
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
        ENDIF
C
C     OTHER TYPES OF F FUNCTIONS
C
C-----------------------------------------------------------------------
C
      ELSE
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,IELMU
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,IELMU
100    FORMAT(1X,'MT12AA (BIEF) :',/,
     &        1X,'COMBINAISON DE F ET U: ',1I6,2X,1I6,' NON PREVUE')
101    FORMAT(1X,'MT12AA (BIEF) :',/,
     &        1X,'COMBINATION OF F AND U: ',1I6,2X,1I6,' NOT AVAILABLE')
       CALL PLANTE(0)
       STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
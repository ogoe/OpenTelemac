C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>                 /  -->   - -->          -->  --->
!> A(I,J) = XMUL  /   KEL . GRAD(PSI1(I)) * U . GRAD(PSI2(J)) D(OMEGA)
!>               /OMEGA
!>         -->
!>         KEL CONSTANT VECTOR ON THE ELEMENT, WITH COMPONENTS F AND G
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
!>    </th><td> A11, A12, A13, A21, A22, A23, A31, A32, A33, F, G, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SF, SG, SU, SURFAC, SV, U, V, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX1, AUX2, AUX3, AUX4, DEN, IELEM, IELMF, IELMG, IELMU, IELMV, KKX, KKY, SUR12, SUR36, U1, U123, U2, U3, U4, V1, V123, V2, V3, V4, X2, X3, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT03AA
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
!> </td><td> 12/04/93
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; C MOULIN (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A11
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A12
!></td><td>---</td><td>
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
!>                  (CAS DU MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DE F,G,H
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DE U,V,W
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
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
                        SUBROUTINE MT03AA
     &( A11 , A12 , A13 ,
     &  A21 , A22 , A23 ,
     &  A31 , A32 , A33 ,
     &  XMUL,SF,SG,SU,SV,F,G,U,V,
     &  XEL,YEL,SURFAC,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11            |---| 
C| A12            |---| 
C| A13            |---| 
C| A21            |---| 
C| A22            |---| 
C| A23            |---| 
C| A31            |---| 
C| A32            |---| 
C| A33            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS DU MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G,H
C| SU,SV,SW       |-->| STRUCTURES DE U,V,W
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT03AA => MT03AA
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
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),U(*),V(*)
C
C     STRUCTURES OF      F, G, U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SU,SV
C
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELMF,IELMG,IELMU,IELMV,IELEM
C
      DOUBLE PRECISION SUR12,SUR36,X2,X3,Y2,Y3,U1,U2,U3,U4,V1,V2,V3,V4
      DOUBLE PRECISION V123,U123,DEN,KKX,KKY,AUX1,AUX2,AUX3,AUX4
C
C-----------------------------------------------------------------------
C
      SUR12 = XMUL/12.D0
      SUR36 = XMUL/36.D0
C
C-----------------------------------------------------------------------
C
      IELMF = SF%ELM
      IELMG = SG%ELM
      IELMU = SU%ELM
      IELMV = SV%ELM
C
      IF(IELMF.EQ.10.AND.IELMG.EQ.10.AND.
     &   IELMU.EQ.11.AND.IELMV.EQ.11) THEN
C
C   LOOP ON THE ELEMENTS
C
      DO 1 IELEM = 1 , NELEM
C
      X2  =  XEL(IELEM,2)
      X3  =  XEL(IELEM,3)
      Y2  =  YEL(IELEM,2)
      Y3  =  YEL(IELEM,3)
C
      U1  =  U(IKLE1(IELEM))
      U2  =  U(IKLE2(IELEM))
      U3  =  U(IKLE3(IELEM))
      V1  =  V(IKLE1(IELEM))
      V2  =  V(IKLE2(IELEM))
      V3  =  V(IKLE3(IELEM))
C
      U123 = U1 + U2 + U3
      V123 = V1 + V2 + V3
C
      DEN = SUR12 / SURFAC(IELEM)
      KKX = F(IELEM)*DEN
      KKY = G(IELEM)*DEN
C
      AUX1 = X2 * V123 - Y2 * U123
      AUX2 = X3 * V123 - Y3 * U123
      AUX3 = X2 * KKY  - Y2 * KKX
      AUX4 = X3 * KKY  - Y3 * KKX
C
      A11(IELEM)  = ( AUX1 - AUX2 ) * ( AUX3 - AUX4 )
      A22(IELEM)  =          AUX2   *          AUX4
      A12(IELEM)  =          AUX2   * ( AUX3 - AUX4 )
      A21(IELEM)  = ( AUX1 - AUX2 ) *          AUX4
C
C  USES HERE THE 'MAGIC SQUARE' PROPERTIES OF A DIFFUSION-LIKE MATRIX
C  (SUM OF EACH LINE AND EACH COLUMN IS 0)
C
      A13(IELEM) = - A11(IELEM) - A12(IELEM)
      A23(IELEM) = - A22(IELEM) - A21(IELEM)
      A31(IELEM) = - A11(IELEM) - A21(IELEM)
      A32(IELEM) = - A22(IELEM) - A12(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELMF.EQ.10.AND.IELMG.EQ.10.AND.IELMU.EQ.12) THEN
C
C   LOOP ON THE ELEMENTS
C
      DO 2 IELEM = 1 , NELEM
C
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
C
      U1   =  U(IKLE1(IELEM))
      U2   =  U(IKLE2(IELEM))
      U3   =  U(IKLE3(IELEM))
      U4   =  U(IKLE4(IELEM))
      V1   =  V(IKLE1(IELEM))
      V2   =  V(IKLE2(IELEM))
      V3   =  V(IKLE3(IELEM))
      V4   =  V(IKLE4(IELEM))
C
      DEN = SUR36 / SURFAC(IELEM)
      KKX=F(IELEM)*DEN
      KKY=G(IELEM)*DEN
C
      A11(IELEM) = ((X2*KKY-X3*KKY+KKX*Y3-KKX*Y2)*(2*X2*V3+3*X2*V4+
     &          2*X2*V2+2*X2*V1-2*X3*V3-3*X3*V4-2*X3*V2-2*X3*V1+2*
     &          U3*Y3-2*U3*Y2+3*U4*Y3-3*U4*Y2+2*U2*Y3-2*U2*Y2+2*U1*
     &          Y3-2*U1*Y2))
      A22(IELEM) = ((X3*KKY-KKX*Y3)*(2*X3*V3+3*X3*V4+2*X3*V2+2*
     &          X3*V1-2*U3*Y3-3*U4*Y3-2*U2*Y3-2*U1*Y3))
      A12(IELEM) = ((X2*KKY-X3*KKY+KKX*Y3-KKX*Y2)*(2*X3*V3+3*X3*V4+
     &          2*X3*V2+2*X3*V1-2*U3*Y3-3*U4*Y3-2*U2*Y3-2*U1*Y3))
      A21(IELEM) = ((2*X2*V3+3*X2*V4+2*X2*V2+2*X2*V1-2*X3*V3-
     &          3*X3*V4-2*X3*V2-2*X3*V1+2*U3*Y3-2*U3*Y2+3*U4*Y3-3*
     &          U4*Y2+2*U2*Y3-2*U2*Y2+2*U1*Y3-2*U1*Y2)*(X3*KKY-KKX*Y3))
C
C  USES HERE THE 'MAGIC SQUARE' PROPERTIES OF A DIFFUSION-LIKE MATRIX
C  (SUM OF EACH LINE AND EACH COLUMN IS 0)
C
      A13(IELEM) = - A11(IELEM) - A12(IELEM)
      A23(IELEM) = - A22(IELEM) - A21(IELEM)
      A31(IELEM) = - A11(IELEM) - A21(IELEM)
      A32(IELEM) = - A22(IELEM) - A12(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
C
2     CONTINUE
C     OTHER TYPES OF FUNCTIONS F AND G
C
C-----------------------------------------------------------------------
C
      ELSE
C
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.1) WRITE(LU,110) IELMG,SG%NAME
       IF (LNG.EQ.1) WRITE(LU,200) IELMU,SU%NAME
       IF (LNG.EQ.1) WRITE(LU,300)
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,111) IELMG,SG%NAME
       IF (LNG.EQ.2) WRITE(LU,201) IELMU,SU%NAME
       IF (LNG.EQ.2) WRITE(LU,301)
100    FORMAT(1X,'MT03AA (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,
     &        1X,'NOM REEL : ',A6)
110    FORMAT(1X,'DISCRETISATION DE G : ',1I6,
     &        1X,'NOM REEL : ',A6)
200    FORMAT(1X,'DISCRETISATION DE U : ',1I6,
     &        1X,'NOM REEL : ',A6)
300    FORMAT(1X,'CAS NON PREVU')
101    FORMAT(1X,'MT03AA (BIEF) :',/,
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
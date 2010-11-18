C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>                              /
!>                    A    =   /  F (P *P )*J(X,Y) DX
!>                     I J    /L      I  J<br>
!>     BY ELEMENTARY CELL; THE ELEMENT IS THE P2 SEGMENT<br>
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
!>    </th><td> A11, A12, A13, A22, A23, A33, F, IKLE1, IKLE2, IKLE3, LGSEG, NBOR, NELEM, NELMAX, SF, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET1, F1, F2, F3, IELEM, IELMF, SUR30, SUR420, SUR60
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 01/07/08
!> </td><td> A FROEHLY (MATMECA) 01 30 87 80 18
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
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
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
!>          <tr><td>LGSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
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
                        SUBROUTINE MT06OC
     &(A11,A12,A13,A22,A23,A33,
     & XMUL,SF,F,LGSEG,IKLE1,IKLE2,IKLE3,NBOR,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A22            |---| 
C| A23            |---| 
C| A33            |---| 
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| LGSEG          |---| 
C| NBOR           |---| 
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
      USE BIEF!, EX_MT06OC => MT06OC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NBOR(NELMAX,3)
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
C
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
C
      DOUBLE PRECISION, INTENT(IN)    :: LGSEG(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A11(NELMAX),A12(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A13(NELMAX),A22(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A23(NELMAX),A33(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF
      DOUBLE PRECISION SUR30,SUR60,SUR420,DET1,F1,F2,F3
C
C-----------------------------------------------------------------------
C
      SUR30  = XMUL/30.D0
      SUR60  = XMUL/60.D0
      SUR420  = XMUL/420.D0
C
C-----------------------------------------------------------------------
C
      IELMF = SF%ELM
C
C     F CONSTANT BY SEGMENT, IN A BOUNDARY ARRAY
C
      IF(IELMF.EQ.0) THEN
C
      DO 1 IELEM = 1 , NELEM
      F1 = F(IELEM)
      DET1 = LGSEG(IELEM) * SUR30
C
      A11(IELEM) = DET1 * (4.D0*F1)
      A12(IELEM) = DET1 * (-F1)
      A13(IELEM) = DET1 * (2.D0*F1)
      A22(IELEM) = A11(IELEM)
      A23(IELEM) = A13(IELEM)
      A33(IELEM) = DET1 * (16.D0*F1)
C
1     CONTINUE
C
C     F LINEAR BY SEGMENT, IN A BOUNDARY ARRAY
C     NOTE: IKLE IS HERE A BOUNDARY IKLE
C
      ELSEIF(IELMF.EQ.1) THEN
C
      DO 2 IELEM = 1 , NELEM
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
C
      DET1 = LGSEG(IELEM) * SUR60
C
      A11(IELEM) = DET1 * (7.D0*F1+F2)
      A12(IELEM) = DET1 * (-F1-F2)
      A13(IELEM) = DET1 * (4.D0*F1)
      A22(IELEM) = DET1 * (F1+7.D0*F2)
      A23(IELEM) = DET1 * (4.D0*F2)
      A33(IELEM) = DET1 * 16.D0 * (F1+F2)
C
2     CONTINUE
C
C     F LINEAR, IN AN ARRAY DEFINED ON THE DOMAIN
C
      ELSEIF(IELMF.EQ.11.OR.IELMF.EQ.21) THEN
C
      DO 3 IELEM = 1 , NELEM
C
      F1 = F(NBOR(IELEM,1))
      F2 = F(NBOR(IELEM,2))
C
      DET1 = LGSEG(IELEM) * SUR60
C
      A11(IELEM) = DET1 * (7.D0*F1+F2)
      A12(IELEM) = DET1 * (-F1-F2)
      A13(IELEM) = DET1 * (4.D0*F1)
      A22(IELEM) = DET1 * (F1+7.D0*F2)
      A23(IELEM) = DET1 * (4.D0*F2)
      A33(IELEM) = DET1 * 16.D0 * (F1+F2)
C
3     CONTINUE
C
C     F QUADRATIC BY SEGMENT, IN A BOUNDARY ARRAY
C     NOTE: IKLE IS HERE A BOUNDARY IKLE
C
      ELSEIF(IELMF.EQ.2) THEN
C
      DO 4 IELEM = 1 , NELEM
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
      DET1 = LGSEG(IELEM) * SUR420
C
      A11(IELEM) = DET1 * (39.D0*F1-3.D0*F2+20.D0*F3)
      A12(IELEM) = DET1 * (-3.D0*F1-3.D0*F2-8.D0*F3)
      A13(IELEM) = DET1 * (20.D0*F1-8.D0*F2+16.D0*F3)
      A22(IELEM) = DET1 * (-3.D0*F1+39.D0*F2+20.D0*F3)
      A23(IELEM) = DET1 * (-8.D0*F1+20.D0*F2+16.D0*F3)
      A33(IELEM) = DET1 * 16.D0 * (F1+F2+12.D0*F3)
C
4     CONTINUE
C
C     F QUADRATIC, IN AN ARRAY DEFINED ON THE DOMAIN
C
      ELSEIF(IELMF.EQ.13) THEN
C
      DO 5 IELEM = 1 , NELEM
C
      F1 = F(NBOR(IELEM,1))
      F2 = F(NBOR(IELEM,2))
      F3 = F(NBOR(IELEM,3))
C
      DET1 = LGSEG(IELEM) * SUR420
C
      A11(IELEM) = DET1 * (39.D0*F1-3.D0*F2+20.D0*F3)
      A12(IELEM) = DET1 * (-3.D0*F1-3.D0*F2-8.D0*F3)
      A13(IELEM) = DET1 * (20.D0*F1-8.D0*F2+16.D0*F3)
      A22(IELEM) = DET1 * (-3.D0*F1+39.D0*F2+20.D0*F3)
      A23(IELEM) = DET1 * (-8.D0*F1+20.D0*F2+16.D0*F3)
      A33(IELEM) = DET1 * 16.D0 * (F1+F2+12.D0*F3)
C
5     CONTINUE
C
C     OTHER TYPES OF DISCRETISATION OF F
C
      ELSE
C
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
100    FORMAT(1X,'MT06OC (BIEF) :',/,
     &        1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &        1X,'NOM REEL : ',A6)
101    FORMAT(1X,'MT06OC (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &        1X,'REAL NAME: ',A6)
       CALL PLANTE(1)
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
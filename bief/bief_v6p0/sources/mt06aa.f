C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code
!>                              /
!>                    A    =   /  F * (P *P )*J(X,Y) DXDY
!>                     I J    /S        I  J<br>
!>     BY ELEMENTARY CELL; THE ELEMENT IS THE P1 TRIANGLE<br>
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
!>    </th><td> A11, A12, A13, A22, A23, A33, F, IKLE1, IKLE2, IKLE3, NELEM, NELMAX, SF, SURFAC, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET1, DET2, F1, F123, F2, F3, IELEM, IELMF, SUR12, SUR60
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT06AA
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
!> </td><td> 29/10/99
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
!>          <tr><td>F
!></td><td>--></td><td>FONCTION INTERVENANT DANS LE CALCUL DE LA
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
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,
!></td><td>--></td><td>STRUCTURE DE F
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT06AA
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SF,F,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| A13            |---| 
C| A22            |---| 
C| A23            |---| 
C| A33            |---| 
C| F             |-->| FONCTION INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,            |-->| STRUCTURE DE F
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT06AA => MT06AA
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
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
C
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      INTEGER IELMF,IELEM
C
      DOUBLE PRECISION SUR12,SUR60,DET1,DET2,F123,F1,F2,F3
C
C-----------------------------------------------------------------------
C
      SUR60 = XMUL/60.D0
      SUR12 = XMUL/12.D0
C
C-----------------------------------------------------------------------
C
      IELMF = SF%ELM
C
C-----------------------------------------------------------------------
C
C  CASE WHERE F IS LINEAR
C
      IF(IELMF.EQ.11) THEN
C
      DO 1 IELEM = 1 , NELEM
C
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
C
      F123 = F1 + F2 + F3
C
      DET1 = SURFAC(IELEM) * SUR60
      DET2 = DET1 + DET1
C
C***********************************************************************
C
C  ELEMENTS OFF THE DIAGONAL
C
      A12(IELEM) = DET1 * (F123+F123-F3)
      A13(IELEM) = DET1 * (F123+F123-F2)
      A23(IELEM) = DET1 * (F123+F123-F1)
C
C  DIAGONAL TERMS
C
      A11(IELEM) = DET2 * (F123+F1+F1)
      A22(IELEM) = DET2 * (F123+F2+F2)
      A33(IELEM) = DET2 * (F123+F3+F3)
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
C  CASE WHERE F IS LINEAR
C
      ELSEIF(IELMF.EQ.10) THEN
C
      DO 2 IELEM = 1 , NELEM
C
      F1 = F(IELEM)
C
      DET1 = SURFAC(IELEM) * SUR12
      DET2 = DET1 + DET1
C
C***********************************************************************
C
C  ELEMENTS OFF THE DIAGONAL
C
      A12(IELEM) = DET1 * F1
      A13(IELEM) = DET1 * F1
      A23(IELEM) = DET1 * F1
C
C  DIAGONAL TERMS
C
      A11(IELEM) = DET2 * F1
      A22(IELEM) = DET2 * F1
      A33(IELEM) = DET2 * F1
C
2     CONTINUE
C
C     OTHER TYPES OF DISCRETISATION OF F
C
C-----------------------------------------------------------------------
C
      ELSE
C
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
100    FORMAT(1X,'MT06AA (BIEF) :',/,
     &        1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &        1X,'NOM REEL : ',A6)
101    FORMAT(1X,'MT06AA (BIEF) :',/,
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
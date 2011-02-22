C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE SYSTEM L X = B (ELEMENT: P1 PRISM).
!>  @code
!>            B IS THE SAME AS X TO START WITH<br>
!>            L IS THE LOWER PART OF THE MATRIX, BUILT IN
!>            SUBROUTINE DECLDU.<br>
!>            EACH ELEMENTARY MATRIX IS DECOMPOSED IN THE FORM:<br>
!>            LE * DE * UE<br>
!>            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!>            DE : DIAGONAL
!>            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL<br>
!>                                                T
!>            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE1, IKLE2, IKLE3, IKLE4, IKLE5, IKLE6, LV, NELEM, NELMAX, NPOIN, X, XA1, XA10, XA11, XA12, XA13, XA14, XA15, XA2, XA3, XA4, XA5, XA6, XA7, XA8, XA9
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IB, IELEM
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DESCEN()

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
!> </td><td> 05/02/91
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE1,
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
!>          <tr><td>IKLE5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>DIMENSION DES TABLEAUX
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA1,
!></td><td>--></td><td>TERMES EXTRADIAGONAUX DE LA MATRICE A
!>                  CORRESPONDANT A LA PARTIE INFERIEURE
!>    </td></tr>
!>          <tr><td>XA10
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA11
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA12
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA13
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA14
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA15
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA9
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DES41
     &(X, XA1 ,XA2 ,XA3 ,XA4 ,XA5 ,
     &    XA6 ,XA7 ,XA8 ,XA9 ,XA10,
     &    XA11,XA12,XA13,XA14,XA15,
     &    IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     &    NELEM,NELMAX,NPOIN,LV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| IKLE5          |---| 
C| IKLE6          |---| 
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPOIN          |-->| DIMENSION DES TABLEAUX
C| X             |---| 
C| XA1,           |-->| TERMES EXTRADIAGONAUX DE LA MATRICE A
C|                |   | CORRESPONDANT A LA PARTIE INFERIEURE
C| XA10           |---| 
C| XA11           |---| 
C| XA12           |---| 
C| XA13           |---| 
C| XA14           |---| 
C| XA15           |---| 
C| XA2            |---| 
C| XA3            |---| 
C| XA4            |---| 
C| XA5            |---| 
C| XA6            |---| 
C| XA7            |---| 
C| XA8            |---| 
C| XA9            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          , INTENT(IN)    :: NPOIN,NELEM,NELMAX,LV
      DOUBLE PRECISION , INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION , INTENT(IN)    :: XA1(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA2(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA3(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA4(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA5(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA6(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA7(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA8(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA9(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA10(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA11(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA12(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA13(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA14(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA15(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE1(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE2(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE3(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE4(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE5(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE6(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IB
C
      INTRINSIC MIN
C
C-----------------------------------------------------------------------
C
C RESUMES INVERSIONS OF THE LOWER TRIANGULAR MATRICES
C
C-----------------------------------------------------------------------
C LOOP IN SCALAR MODE (LV=1) OR WITH FORCED VECTORISATION
C-----------------------------------------------------------------------
C
      IF(LV.EQ.1) THEN
C
C  SCALAR MODE
C
      DO 10 IELEM = 1 , NELEM
        X(IKLE2(IELEM))=X(IKLE2(IELEM))-XA1(IELEM)*X(IKLE1(IELEM))
        X(IKLE3(IELEM))=X(IKLE3(IELEM))-XA2(IELEM)*X(IKLE1(IELEM))
     &                                 -XA6(IELEM)*X(IKLE2(IELEM))
        X(IKLE4(IELEM))=X(IKLE4(IELEM))-XA3(IELEM)*X(IKLE1(IELEM))
     &                                 -XA7(IELEM)*X(IKLE2(IELEM))
     &                                -XA10(IELEM)*X(IKLE3(IELEM))
        X(IKLE5(IELEM))=X(IKLE5(IELEM))-XA4(IELEM)*X(IKLE1(IELEM))
     &                                 -XA8(IELEM)*X(IKLE2(IELEM))
     &                                -XA11(IELEM)*X(IKLE3(IELEM))
     &                                -XA13(IELEM)*X(IKLE4(IELEM))
        X(IKLE6(IELEM))=X(IKLE6(IELEM))-XA5(IELEM)*X(IKLE1(IELEM))
     &                                 -XA9(IELEM)*X(IKLE2(IELEM))
     &                                -XA12(IELEM)*X(IKLE3(IELEM))
     &                                -XA14(IELEM)*X(IKLE4(IELEM))
     &                                -XA15(IELEM)*X(IKLE5(IELEM))
10    CONTINUE
C
      ELSE
C
C  VECTOR MODE
C
      DO 20 IB = 1,(NELEM+LV-1)/LV
CVOCL LOOP,NOVREC
CDIR$ IVDEP
      DO 30 IELEM = 1+(IB-1)*LV , MIN(NELEM,IB*LV)
        X(IKLE2(IELEM))=X(IKLE2(IELEM))-XA1(IELEM)*X(IKLE1(IELEM))
        X(IKLE3(IELEM))=X(IKLE3(IELEM))-XA2(IELEM)*X(IKLE1(IELEM))
     &                                 -XA6(IELEM)*X(IKLE2(IELEM))
        X(IKLE4(IELEM))=X(IKLE4(IELEM))-XA3(IELEM)*X(IKLE1(IELEM))
     &                                 -XA7(IELEM)*X(IKLE2(IELEM))
     &                                -XA10(IELEM)*X(IKLE3(IELEM))
        X(IKLE5(IELEM))=X(IKLE5(IELEM))-XA4(IELEM)*X(IKLE1(IELEM))
     &                                 -XA8(IELEM)*X(IKLE2(IELEM))
     &                                -XA11(IELEM)*X(IKLE3(IELEM))
     &                                -XA13(IELEM)*X(IKLE4(IELEM))
        X(IKLE6(IELEM))=X(IKLE6(IELEM))-XA5(IELEM)*X(IKLE1(IELEM))
     &                                 -XA9(IELEM)*X(IKLE2(IELEM))
     &                                -XA12(IELEM)*X(IKLE3(IELEM))
     &                                -XA14(IELEM)*X(IKLE4(IELEM))
     &                                -XA15(IELEM)*X(IKLE5(IELEM))
30    CONTINUE
20    CONTINUE
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
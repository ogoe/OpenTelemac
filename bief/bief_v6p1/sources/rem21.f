C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRODUCT X = U B (BEWARE: ELEMENT BY ELEMENT).
!><br>            DEALS HERE WITH Q1 ELEMENTS OR ELEMENTS WITH 4 POINTS.
!><br>            REVERSE OPERATION FROM THAT IN SUBROUTINE REMONT,
!>                HENCE THE NAME.
!>  @code
!>            THE MATRIX U IS HERE THE RESULT OF A DECOMPOSITION
!>            DONE IN SUBROUTINE DECLDU
!>
!>            EACH ELEMENTARY MATRIX WAS FACTORISED IN THE FORM:
!>
!>            LE X DE X UE
!>
!>            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!>            DE : DIAGONAL
!>            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!>
!>                                                T
!>            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE1, IKLE2, IKLE3, IKLE4, LV, NELEM, NELMAX, NPOIN, X, XA1, XA2, XA3, XA4, XA5, XA6
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
!><br>REMONT()

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
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE1,2,34
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
!></td><td><-></td><td>VECTEUR DONNEE ET RESULTAT.
!>    </td></tr>
!>          <tr><td>XA1,
!></td><td>--></td><td>TERMES EXTRADIAGONAUX DE LA MATRICE A
!>                  CORRESPONDANT A LA PARTIE INFERIEURE
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE REM21
     &(X, XA1,XA2,XA3,XA4,XA5,XA6 , IKLE1,IKLE2,IKLE3,IKLE4,
     & NELEM,NELMAX,NPOIN,LV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE1,2,34     |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPOIN          |-->| DIMENSION DES TABLEAUX
C| X             |<->| VECTEUR DONNEE ET RESULTAT.
C| XA1,           |-->| TERMES EXTRADIAGONAUX DE LA MATRICE A
C|                |   | CORRESPONDANT A LA PARTIE INFERIEURE
C| XA2            |---| 
C| XA3            |---| 
C| XA4            |---| 
C| XA5            |---| 
C| XA6            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NELEM,NELMAX,LV
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
C
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XA1(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XA2(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XA3(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XA4(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XA5(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XA6(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IB
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
      DO 10 IELEM = NELEM , 1 , -1
        X(IKLE3(IELEM))=X(IKLE3(IELEM))-XA6(IELEM)*X(IKLE4(IELEM))
        X(IKLE2(IELEM))=X(IKLE2(IELEM))-XA5(IELEM)*X(IKLE4(IELEM))
     &                                 -XA4(IELEM)*X(IKLE3(IELEM))
        X(IKLE1(IELEM))=X(IKLE1(IELEM))-XA3(IELEM)*X(IKLE4(IELEM))
     &                                 -XA2(IELEM)*X(IKLE3(IELEM))
     &                                 -XA1(IELEM)*X(IKLE2(IELEM))
10    CONTINUE
C
      ELSE
C
C  VECTOR MODE
C
      DO 20 IB = (NELEM+LV-1)/LV , 1 , -1
CVOCL LOOP,NOVREC
CDIR$ IVDEP
      DO 30 IELEM = MIN(NELEM,IB*LV) , 1+(IB-1)*LV , -1
        X(IKLE3(IELEM))=X(IKLE3(IELEM))-XA6(IELEM)*X(IKLE4(IELEM))
        X(IKLE2(IELEM))=X(IKLE2(IELEM))-XA5(IELEM)*X(IKLE4(IELEM))
     &                                 -XA4(IELEM)*X(IKLE3(IELEM))
        X(IKLE1(IELEM))=X(IKLE1(IELEM))-XA3(IELEM)*X(IKLE4(IELEM))
     &                                 -XA2(IELEM)*X(IKLE3(IELEM))
     &                                 -XA1(IELEM)*X(IKLE2(IELEM))
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
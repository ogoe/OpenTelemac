C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE VECTOR X = U B     (ELEMENT BY ELEMENT).
!>  @code
!>            MATRIX L IS HERE THE RESULT OF A DECOMPOSITION
!>            PERFORMED IN SUBROUTINE DECLDU.<br>
!>            EACH ELEMENTARY MATRIX WAS FACTORISED IN THE FORM:<br>
!>            LE X DE X UE<br>
!>            LE : LOWER TRIANGULAR WITH 1 ON THE DIAGONAL
!>            DE : DIAGONAL
!>            UE : UPPER TRIANGULAR WITH 1 ON THE DIAGONAL<br>
!>                                                T
!>            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>-----------------------------------------------------------------------
!>  MEANING OF IELM :
!>
!>  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!>
!>  11 : P1 TRIANGLE            3                       YES
!>  12 : QUASI-BUBBLE TRIANGLE  4                       YES
!>  21 : Q1 QUADRILATERAL       4                       YES
!>  41 : TELEMAC-3D PRISMS      6                       YES
!>
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, COPY, DITR, MESH, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELM, NELEM, NELMAX, NPOIN, TYPX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PUOG1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), TNOMER()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PUOG()

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
!> </td><td> 23/12/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td><--</td><td>MATRICE A SOUS FORME LDU
!>    </td></tr>
!>          <tr><td>B
!></td><td><--</td><td>SECOND MEMBRE DU SYSTEME A RESOUDRE.
!>    </td></tr>
!>          <tr><td>COPY
!></td><td>--></td><td>SI .TRUE. B EST RECOPIE SUR X.
!>                  AU PREALABLE.
!>    </td></tr>
!>          <tr><td>DITR
!></td><td>--></td><td>CARACTERE  'D' : ON CALCULE AVEC A
!>                  'T' : ON CALCULE AVEC A TRANSPOSEE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>SOLUTION DU SYSTEME AX = B
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PUOG1
     &(X, A,B ,DITR,MESH,COPY)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<--| MATRICE A SOUS FORME LDU
C| B             |<--| SECOND MEMBRE DU SYSTEME A RESOUDRE.
C| COPY           |-->| SI .TRUE. B EST RECOPIE SUR X.
C|                |   | AU PREALABLE.
C| DITR           |-->| CARACTERE  'D' : ON CALCULE AVEC A
C|                |   | 'T' : ON CALCULE AVEC A TRANSPOSEE
C| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
C| X             |<--| SOLUTION DU SYSTEME AX = B
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PUOG1 => PUOG1
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=1), INTENT(IN) :: DITR
C
      LOGICAL, INTENT(IN) :: COPY
C
C-----------------------------------------------------------------------
C
C  VECTORS STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X,B
C
C-----------------------------------------------------------------------
C
C  MESH STRUCTURE
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C-----------------------------------------------------------------------
C
C  MATRIX STRUCTURE
C
      TYPE(BIEF_OBJ), INTENT(IN) :: A
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELM,NPOIN,NELEM,NELMAX
      CHARACTER(LEN=1) :: TYPX
C
C-----------------------------------------------------------------------
C
      TYPX  = A%TYPEXT
      NPOIN = A%D%DIM1
      IELM  = A%ELMLIN
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
      CALL CPSTVC(B,X)
C
C-----------------------------------------------------------------------
C
C 1) DESCENT WITH RECOPY OF B IN X
C
      CALL TNOMER(X%R,A%X%R,TYPX,
     &     B%R,MESH%IKLE%I,NELEM,NELMAX,NPOIN,IELM,DITR,COPY,MESH%LV)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE SYSTEM A X = B.
!><br>            THE MATRIX A IS HERE THE RESULT OF A DECOMPOSITION
!>                DONE IN SUBROUTINE DECLDU.
!>  @code
!>            EACH ELEMENTARY MATRIX WAS FACTORISED IN THE FORM :
!>
!>            LE X DE X UE
!>
!>            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!>            DE : DIAGONAL
!>            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!>
!>                                                T
!>            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!>
!>            "DE" MATRICES ARE CONSIDERED LIKE DIAGONALS OF SIZE
!>            NPOIN X NPOIN, WHICH ARE FILLED WITH 1S FOR THE POINTS
!>            WHICH DO NOT BELONG TO THE CONSIDERED ELEMENT
!>
!>            THEN PERFORMS THE PRODUCT OF ALL THESE DIAGONALS
!>            YIELDING DIAGONAL DB
!>
!>
!> !!!!!!!!!  FINALLY: DB HAS BEEN INVERTED BECAUSE THAT'S HOW
!>                     IT IS USED IN THIS SUBROUTINE
!>
!>            MATRIX A IS HERE :
!>
!>            THE PRODUCT FROM 1 TO NELEM OF ALL THE MATRICES: LE
!>
!>            MULTIPLIED BY :
!>
!>            THE DIAGONAL: DB
!>
!>            MULTIPLIED BY :
!>
!>            THE PRODUCT FROM NELEM TO 1 OF ALL THE MATRICES: UE
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
!>    </th><td> A, B, DITR, MESH, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, IELM, NELEM, NELMAX, NPOIN, TYPD, TYPX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DWNUP1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), DESCEN(), DESSEG(), OV(), REMONT(), REMSEG()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DOWNUP()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 26/02/04
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
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
                        SUBROUTINE DWNUP1
     &(X, A,B ,DITR,MESH)
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
      USE BIEF, EX_DWNUP1 => DWNUP1
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: B
      TYPE(BIEF_OBJ), INTENT(IN)    :: A
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
      CHARACTER(LEN=1), INTENT(IN)  :: DITR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELM,NPOIN,NELEM,NELMAX
C
      DOUBLE PRECISION C
C
      CHARACTER*1 TYPD,TYPX
C
C-----------------------------------------------------------------------
C
      TYPD  = A%TYPDIA
      TYPX  = A%TYPEXT
      NPOIN = A%D%DIM1
      IELM  = A%ELMLIN
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
      CALL CPSTVC(B,X)
C
C-----------------------------------------------------------------------
C
C 1) DESCENT WITH COPY OF B IN X
C
      IF(A%STO.EQ.1) THEN
        CALL DESCEN(X%R, A%X%R,TYPX,B%R,
     &       MESH%IKLE%I,NELEM,NELMAX,NPOIN,IELM,DITR,.TRUE.,MESH%LV)
      ELSEIF(A%STO.EQ.3) THEN
        CALL DESSEG(X%R, A%X%R,TYPX,B%R,
     &              MESH%GLOSEG%I,MESH%NSEG,NPOIN,DITR,.TRUE.)
      ENDIF
C
C-----------------------------------------------------------------------
C
C 2) RESUMES INVERSIONS OF DIAGONAL MATRICES
C
      IF(TYPD(1:1).NE.'I') THEN
        CALL OV( 'X=XY    ' , X%R , A%D%R , X%R , C , NPOIN )
      ENDIF
C
C-----------------------------------------------------------------------
C
C 3) TRACES BACK WITHOUT PRELIMINARY COPY OF B IN X
C
      IF(A%STO.EQ.1) THEN
        CALL REMONT(X%R, A%X%R,TYPX,B%R,
     &       MESH%IKLE%I,NELEM,NELMAX,NPOIN,IELM,DITR,.FALSE.,MESH%LV)
      ELSE
        CALL REMSEG(X%R, A%X%R,TYPX,B%R,
     &              MESH%GLOSEG%I,MESH%NSEG,NPOIN,DITR,.FALSE.)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
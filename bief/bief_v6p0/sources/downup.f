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
!>    </th><td> I, S, SA
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DOXNUP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DWNUP1(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CGSTAB(), EQUNOR(), ERRMIN(), GRACJG(), RESCJG()

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
!> </td><td> 24/04/97
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
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>B
!></td><td><--</td><td>SECOND MEMBRE DU SYSTEME A RESOUDRE.
!>    </td></tr>
!>          <tr><td>DA
!></td><td><--</td><td>DIAGONALE DE LA MATRICE A
!>    </td></tr>
!>          <tr><td>DITR
!></td><td>--></td><td>CARACTERE  'D' : ON CALCULE AVEC A
!>                  'T' : ON CALCULE AVEC A TRANSPOSEE
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
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
!>          <tr><td>TYPDIA
!></td><td><--</td><td>TYPE DE DIAGONALE ( 'Q', 'I' , OU '0' )
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>SOLUTION DU SYSTEME AX = B
!>    </td></tr>
!>          <tr><td>XA
!></td><td><--</td><td>TERMES EXTRADIAGONAUX DE LA MATRICE A
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DOWNUP
     &(X, A,B ,DITR,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |---| 
C| B             |<--| SECOND MEMBRE DU SYSTEME A RESOUDRE.
C| DA             |<--| DIAGONALE DE LA MATRICE A
C| DITR           |-->| CARACTERE  'D' : ON CALCULE AVEC A
C|                |   | 'T' : ON CALCULE AVEC A TRANSPOSEE
C| IELM           |-->| TYPE D'ELEMENT
C| IKLE           |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| MESH           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPOIN          |-->| DIMENSION DES TABLEAUX
C| TYPDIA         |<--| TYPE DE DIAGONALE ( 'Q', 'I' , OU '0' )
C| X             |<--| SOLUTION DU SYSTEME AX = B
C| XA             |<--| TERMES EXTRADIAGONAUX DE LA MATRICE A
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DOXNUP => DOWNUP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: A,B
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
      CHARACTER(LEN=1), INTENT(IN)  :: DITR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER S,SA,I
C
C-----------------------------------------------------------------------
C
      IF(X%TYPE.EQ.4) THEN
        S = X%N
      ELSE
        S = 0
      ENDIF
C
C     COVERS THE CASE WHERE THE SYSTEM IS A BLOCK BUT WHERE ONLY ONE OF
C     PRECONDITIONING MATRICES IS USED
C
      IF(A%TYPE.EQ.3) THEN
        SA = 0
      ELSEIF(A%TYPE.EQ.4) THEN
        SA = A%N
      ELSE
        IF (LNG.EQ.1) WRITE(LU,300) A%TYPE
        IF (LNG.EQ.2) WRITE(LU,400) A%TYPE
300     FORMAT(1X,'DOWNUP (BIEF) :',1I6,' TYPE DE A NON PREVU.')
400     FORMAT(1X,'DOWNUP (BIEF) :',1I6,' UNEXPECTED TYPE FOR A.')
        CALL PLANTE(0)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(S.EQ.0.AND.SA.EQ.0) THEN
C
C     CASE WHERE A IS A SIMPLE MATRIX AND X A SIMPLE VECTOR
C
        CALL DWNUP1(X, A,B ,DITR,MESH)
C
      ELSEIF(S.GT.0.AND.S.EQ.SA) THEN
C
C     CASE WHERE BLOCK A ONLY CONTAINS THE DIAGONALS
C
        DO 10 I=1,S
          CALL DWNUP1(X%ADR(I)%P,
     &                A%ADR(I)%P,
     &                B%ADR(I)%P,
     &                DITR,MESH)
10      CONTINUE
C
      ELSEIF(S.GT.0.AND.S**2.EQ.SA) THEN
C
C     CASE WHERE BLOCK A CONTAINS AS MANY MATRICES AS THERE ARE IN
C     THE COMPLETE SYSTEM: ONLY CONSIDERS THE DIAGONALS
C
        DO 11 I=1,S
          CALL DWNUP1(X%ADR(I)%P,
     &                A%ADR(1+(S+1)*(I-1))%P,
     &                B%ADR(I)%P,
     &                DITR,MESH)
11      CONTINUE
C
C     CASE WHERE A IS A SINGLE MATRIX AND X IS A BLOCK
C
      ELSEIF(S.GT.0.AND.SA.EQ.0) THEN
C
        DO 12 I=1,S
          CALL DWNUP1(X%ADR(I)%P,
     &                A,
     &                B%ADR(I)%P,
     &                DITR,MESH)
12      CONTINUE
C
      ELSE
        IF (LNG.EQ.1) WRITE(LU,301)
        IF (LNG.EQ.2) WRITE(LU,401)
301     FORMAT(1X,'DOWNUP (BIEF) : CAS NON PREVU')
401     FORMAT(1X,'DOWNUP (BIEF) : UNEXPECTED CASE')
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
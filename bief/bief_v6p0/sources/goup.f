C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE SYSTEM U X = B (ELEMENT BY ELEMENT).
!>  @code
!>            THE MATRIX U IS HERE THE RESULT OF A DECOMPOSITION
!>            DONE IN SUBROUTINE DECLDU<br>
!>            EACH ELEMENTARY MATRIX WAS FACTORISED IN THE FORM:<br>
!>            LE X DE X UE<br>
!>            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!>            DE : DIAGONAL
!>            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL<br>
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
!>    </th><td> I, S, SA
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_GOUP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> GOUP1(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GMRES()

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
                        SUBROUTINE GOUP
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
      USE BIEF, EX_GOUP => GOUP
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
      LOGICAL, INTENT(IN) :: COPY
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
C     CASE WHERE THE SYSTEM IS A BLOCK BUT WHERE ONLY ONE
C     PRECONDITIONING MATRIX IS USED
C
      IF(A%TYPE.EQ.3) THEN
        SA = 0
      ELSEIF(A%TYPE.EQ.4) THEN
        SA = A%N
      ELSE
        IF (LNG.EQ.1) WRITE(LU,300) A%TYPE
        IF (LNG.EQ.2) WRITE(LU,400) A%TYPE
300     FORMAT(1X,'GOUP (BIEF) :',1I6,' TYPE DE A NON PREVU.')
400     FORMAT(1X,'GOUP (BIEF) :',1I6,' UNEXPECTED TYPE FOR A.')
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
        CALL GOUP1(X, A,B ,DITR,MESH,COPY)
C
      ELSEIF(S.GT.0.AND.S.EQ.SA) THEN
C
C     CASE WHERE BLOCK A ONLY HAS DIAGONALS
C
        DO 10 I=1,S
          CALL GOUP1( X%ADR(I)%P,A%ADR(I)%P,B%ADR(I)%P,DITR,MESH,COPY)
10      CONTINUE
C
      ELSEIF(S.GT.0.AND.S**2.EQ.SA) THEN
C
C     CASE WHERE BLOCK A HAS AS MANY MATRICES AS THE WHOLE SYSTEM :
C     ONLY CONSIDERS THE DIAGONALS
C
        DO 11 I=1,S
          CALL GOUP1( X%ADR(I)%P,
     &                A%ADR(1+(S+1)*(I-1))%P,
     &                B%ADR(I)%P,
     &                DITR,MESH,COPY)
11      CONTINUE
C
C     CASE WHERE A IS A SINGLE MATRIX AND X A BLOCK
C
      ELSEIF(S.GT.0.AND.SA.EQ.0) THEN
C
        DO 12 I=1,S
          CALL GOUP1(X%ADR(I)%P,
     &                A,
     &                B%ADR(I)%P,
     &                DITR,MESH,COPY)
12      CONTINUE
C
      ELSE
        IF (LNG.EQ.1) WRITE(LU,301)
        IF (LNG.EQ.2) WRITE(LU,401)
301     FORMAT(1X,'GOUP (BIEF) : CAS NON PREVU')
401     FORMAT(1X,'GOUP (BIEF) : UNEXPECTED CASE')
        CALL PLANTE(1)
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
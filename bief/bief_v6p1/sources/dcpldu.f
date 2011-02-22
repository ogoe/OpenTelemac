C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       L D U FACTORISATION OF THE ELEMENTARY MATRICES
!>                IN MATRIX A.
!><br>           (A CAN ALSO BE A BLOCK OF MATRICES, IN WHICH CASE
!>                THE DIAGONAL MATRICES OF THE BLOCK ARE TREATED).
!><br>            REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY.
!>  @code
!>            EACH ELEMENTARY MATRIX IS DECOMPOSED IN THE FORM:
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
!>   ||||||   FINALLY: DB IS INVERTED BECAUSE THAT'S HOW IT WILL BE
!>                     USED IN DOWNUP.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, COPY, LV, MESH
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, SA, SB
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DCPLDU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DECLDU(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SOLVE()

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
!></td><td><--</td><td>MATRICE A.
!>    </td></tr>
!>          <tr><td>B
!></td><td><--</td><td>MATRICE RESULTAT.
!>    </td></tr>
!>          <tr><td>COPY
!></td><td>--></td><td>SI .TRUE. A EST COPIEE DANS B.
!>                  SINON ON CONSIDERE QUE B EST DEJA REMPLIE.
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DCPLDU
     &(B,A,MESH,COPY,LV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<--| MATRICE A.
C| B             |<--| MATRICE RESULTAT.
C| COPY           |-->| SI .TRUE. A EST COPIEE DANS B.
C|                |   | SINON ON CONSIDERE QUE B EST DEJA REMPLIE.
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION.
C| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF    !, EX_DCPLDU => DCPLDU
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: B
      TYPE(BIEF_OBJ) , INTENT(IN)    :: A
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL        , INTENT(IN)    :: COPY
      INTEGER        , INTENT(IN)    :: LV
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER SA,SB,I
C
C-----------------------------------------------------------------------
C
      IF(A%TYPE.EQ.3) THEN
        SA = 0
      ELSEIF(A%TYPE.EQ.4) THEN
        SA = A%N
      ELSE
        IF (LNG.EQ.1) WRITE(LU,300) A%TYPE
        IF (LNG.EQ.2) WRITE(LU,400) A%TYPE
300     FORMAT(1X,'DCPLDU (BIEF) :',1I6,' TYPE DE A NON PREVU.')
400     FORMAT(1X,'DCPLDU (BIEF) :',1I6,' UNEXPECTED TYPE FOR A.')
        CALL PLANTE(0)
        STOP
      ENDIF
C
      IF(B%TYPE.EQ.3) THEN
        SB = 0
      ELSEIF(B%TYPE.EQ.4) THEN
        SB = B%N
      ELSE
        IF (LNG.EQ.1) WRITE(LU,301) B%TYPE
        IF (LNG.EQ.2) WRITE(LU,401) B%TYPE
301     FORMAT(1X,'DCPLDU (BIEF) :',1I6,' TYPE DE B NON PREVU.')
401     FORMAT(1X,'DCPLDU (BIEF) :',1I6,' UNEXPECTED TYPE FOR B.')
        CALL PLANTE(0)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(SA.EQ.0.AND.SB.EQ.0) THEN
C
         CALL DECLDU(B,A,MESH,COPY,LV)
C
      ELSEIF(SB.GT.0.AND.SA.GT.0) THEN
C
C       TAKES THE DIAGONALS OF BLOCK A
C
        DO I=1,SB
          CALL DECLDU(B%ADR(I)%P,A%ADR(1+(SB+1)*(I-1))%P,
     &                MESH,COPY,LV)
        ENDDO
C
      ELSEIF(SA.NE.0.AND.SB.EQ.0) THEN
C
C       TAKES THE FIRST DIAGONAL OF BLOCK A
C
        CALL DECLDU(B,A%ADR(1)%P,MESH,COPY,LV)
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,302)
        IF (LNG.EQ.2) WRITE(LU,402)
302     FORMAT(1X,'DCPLDU (BIEF) : CAS NON PREVU')
402     FORMAT(1X,'DCPLDU (BIEF) : UNEXPECTED CASE')
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

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FACTORISES THE ELEMENTARY MATRICES IN MATRIX A
!>                USING THE GAUSS-SEIDEL EBE METHOD.
!><br>           (A CAN ALSO BE A BLOCK OF MATRICES; IN THIS CASE ALL
!>                THE MATRICES IN THE BLOCK ARE TREATED).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, MESH
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, SA, SB
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_GSEBE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OM(), PLANTE()
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
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GSEBE
     &(B,A,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<--| MATRICE A.
C| B             |<--| MATRICE RESULTAT.
C| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_GSEBE => GSEBE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(IN) :: A
      TYPE(BIEF_OBJ), INTENT(INOUT) :: B
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER SA,SB,I
C
      DOUBLE PRECISION C
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
300     FORMAT(1X,'GSEBE (BIEF) :',1I6,' TYPE DE A NON PREVU.')
400     FORMAT(1X,'GSEBE (BIEF) :',1I6,' UNEXPECTED TYPE FOR A.')
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
301     FORMAT(1X,'GSEBE (BIEF) :',1I6,' TYPE DE B NON PREVU.')
401     FORMAT(1X,'GSEBE (BIEF) :',1I6,' UNEXPECTED TYPE FOR B.')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(SA.EQ.0.AND.SB.EQ.0) THEN
C
C         B%D IS HERE A STRUCTURE OF VECTOR
C         USED AS DUMMY DIAGONAL
          CALL OM( 'M=N     ' , B , A , B%D , C , MESH )
          B%TYPDIA='I'
C
      ELSEIF(SA.GT.0.AND.SB.GT.0) THEN
C
C       TAKES THE DIAGONALS OF BLOCK A
C
        DO 10 I=1,SB
          CALL OM( 'M=N     ' ,  B%ADR(I)%P ,
     &              A%ADR(1+(SB+1)*(I-1))%P , B%ADR(I)%P%D ,
     &              C , MESH )
          B%ADR(I)%P%TYPDIA='I'
10      CONTINUE
C
      ELSEIF(SA.NE.0.AND.SB.EQ.0) THEN
C
C       TAKES THE 1ST DIAGONAL OF BLOCK A
C
        CALL OM( 'M=N     ' ,B,A%ADR(1)%P,B%D,C,MESH)
        B%TYPDIA='I'
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,302)
        IF (LNG.EQ.2) WRITE(LU,402)
302     FORMAT(1X,'GSEBE (BIEF) : CAS NON PREVU')
402     FORMAT(1X,'GSEBE (BIEF) : UNEXPECTED CASE')
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
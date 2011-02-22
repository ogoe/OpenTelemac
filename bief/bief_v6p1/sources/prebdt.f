C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BLOCK-DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!>               (CAN BE MADE OF 4-MATRIX OR 9-MATRIX BLOCKS).
!>  @code
!>    EXAMPLE FOR A BLOCK OF 4 :
!>
!>         (   A11   A12  ) ( X1 ) = ( B1 )
!>         (              ) (    )   (    )
!>         (              ) (    )   (    )
!>         (   A21   A22  ) ( X2 ) = ( B2 )
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, D, DIADON, MESH, PREXSM, S, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PREBDT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), PREBD4(), PREBD9()
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
!> </td><td> J.M. HERVOUET (LNH)  30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td>--></td><td>BLOC DE MATRICES
!>    </td></tr>
!>          <tr><td>B
!></td><td>--></td><td>BLOC DES SECONDS MEMBRES DU SYSTEME.
!>    </td></tr>
!>          <tr><td>D
!></td><td><--</td><td>BLOC DE DIAGONALES
!>    </td></tr>
!>          <tr><td>DIADON
!></td><td>--></td><td>.TRUE. : LES DIAGONALES SONT DONNEES.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>PREXSM
!></td><td>--></td><td>.TRUE. : ON PRECONDITIONNE X,X2,X3 ET SM
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>0 : SYSTEME NORMAL       (INTERDIT ICI)
!>                  1 : BLOC A UNE MATRICE   (INTERDIT ICI)
!>                  2 : BLOC A 4   MATRICES
!>                  3 : BLOC A 9   MATRICES
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>BLOC DES VECTEURS INCONNUS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PREBDT
     &(X,A,B,D,MESH,PREXSM,DIADON,S)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| BLOC DE MATRICES
C| B             |-->| BLOC DES SECONDS MEMBRES DU SYSTEME.
C| D             |<--| BLOC DE DIAGONALES
C| DIADON         |-->| .TRUE. : LES DIAGONALES SONT DONNEES.
C| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
C| PREXSM         |-->| .TRUE. : ON PRECONDITIONNE X,X2,X3 ET SM
C| S             |-->| 0 : SYSTEME NORMAL       (INTERDIT ICI)
C|                |   | 1 : BLOC A UNE MATRICE   (INTERDIT ICI)
C|                |   | 2 : BLOC A 4   MATRICES
C|                |   | 3 : BLOC A 9   MATRICES
C| X             |<->| BLOC DES VECTEURS INCONNUS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PREBDT => PREBDT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: S
C
      LOGICAL, INTENT(IN) :: PREXSM,DIADON
C
C-----------------------------------------------------------------------
C
C  VECTOR OR VECTOR BLOCK STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X,B,D
C
C-----------------------------------------------------------------------
C
C  MATRIX OR MATRIX BLOCK STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A
C
C-----------------------------------------------------------------------
C
C  MESH STRUCTURE
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C  4-MATRIX BLOCK:
C
      IF(S.EQ.2) THEN
C
C  4-MATRIX BLOCK:
C
        CALL PREBD4(X%ADR(1)%P,X%ADR(2)%P,
     &              A%ADR(1)%P,A%ADR(2)%P,A%ADR(3)%P,A%ADR(4)%P,
     &              B%ADR(1)%P,B%ADR(2)%P,
     &              D%ADR(1)%P,D%ADR(2)%P,D%ADR(3)%P,D%ADR(4)%P,
     &              MESH,PREXSM,DIADON)
C
      ELSEIF(S.EQ.3) THEN
C
C  9-MATRIX BLOCK:
C
        CALL PREBD9(X%ADR(1)%P,X%ADR(2)%P,X%ADR(3)%P,
     &              A%ADR(1)%P,A%ADR(2)%P,A%ADR(3)%P,
     &              A%ADR(4)%P,A%ADR(5)%P,A%ADR(6)%P,
     &              A%ADR(7)%P,A%ADR(8)%P,A%ADR(9)%P,
     &              B%ADR(1)%P,B%ADR(2)%P,B%ADR(3)%P,
     &              D%ADR(1)%P,D%ADR(2)%P,D%ADR(3)%P,
     &              D%ADR(4)%P,D%ADR(5)%P,D%ADR(6)%P,
     &              D%ADR(7)%P,D%ADR(8)%P,D%ADR(9)%P,
     &              MESH,PREXSM,DIADON)
C
      ELSE
C
C-----------------------------------------------------------------------
C
C  ERROR
C
        IF(LNG.EQ.1) WRITE(LU,100) S
        IF(LNG.EQ.2) WRITE(LU,200) S
100     FORMAT(1X,'PREBDT (BIEF) : S NON PREVU :',1I6)
200     FORMAT(1X,'PREBDT (BIEF) : UNEXPECTED S :',1I6)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C                                                            -1
      RETURN
      END


C
C#######################################################################
C
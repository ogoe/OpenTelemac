C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       END OF THE OPERATIONS FOR DIAGONAL PRECONDITIONING
!>                WITH THE CONDENSED MATRIX  (OR ANY SIMILAR OPERATION).
!>  @code
!>                   -1   PRIME
!>    OPERATION X = U    X
!>
!>    EXAMPLE OF A BLOCK OF 4:
!>
!>              (   I     D12  )
!>              (              )
!>         U =  (              )
!>              (              )
!>              (   0      I   )
!>                                   PRIME          PRIME         PRIME
!>              (   I    -D12  ) ( X1      )    ( X1    -   D12 X2 )
!>  -1  PRIME   (              ) (         )    (                  )
!> U   X     =  (              ) (         )  = (                  )
!>              (              ) (   PRIME )    (                  )
!>              (   0      I   ) ( X2      )    ( X2               )
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> D, S, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_UM1X
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), UM1X04(), UM1X09()
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
!>          <tr><td>D
!></td><td><--</td><td>BLOC DE MATRICES DIAGONALES
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>2 : BLOC A 4   MATRICES
!>                  3 : BLOC A 9   MATRICES
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>X ET X' (TRANSFORMATION SUR PLACE)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE UM1X
     &(X,D,S)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| D             |<--| BLOC DE MATRICES DIAGONALES
C| S             |-->| 2 : BLOC A 4   MATRICES
C|                |   | 3 : BLOC A 9   MATRICES
C| X             |<->| X ET X' (TRANSFORMATION SUR PLACE)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_UM1X => UM1X
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     STRUCTURES OF VECTORS OR BLOCKS OF VECTORS
C
      INTEGER, INTENT(IN)           :: S
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     BLOCKS OF 4 MATRICES:
C
      IF(S.EQ.2) THEN
C
C     BLOCKS OF 4 MATRICES:
C
        CALL UM1X04(X%ADR(1)%P,X%ADR(2)%P,D%ADR(3)%P)
C
      ELSEIF(S.EQ.3) THEN
C
C     BLOCKS OF 9 MATRICES:
C
        CALL UM1X09(X%ADR(1)%P,X%ADR(2)%P,X%ADR(3)%P,
     &              D%ADR(4)%P,D%ADR(5)%P,D%ADR(7)%P)
C
      ELSE
C
C-----------------------------------------------------------------------
C
C  ERROR
C
        IF(LNG.EQ.1) WRITE(LU,100) S
        IF(LNG.EQ.2) WRITE(LU,200) S
100     FORMAT(1X,'UM1X (BIEF) : S NON PREVU :',1I6)
200     FORMAT(1X,'UM1X (BIEF) : UNEXPECTED S :',1I6)
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
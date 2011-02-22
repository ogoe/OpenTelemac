C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!><br>           (CAN BE MADE OF A SIMPLE MATRIX, 4-MATRIX OR
!>                9-MATRIX BLOCKS).
!>  @code
!>   IF DIADON=.TRUE. THE PRECONDITIONING DIAGONALS ARE GIVEN BY THE USER
!>
!>
!>   IF DIADON=.FALSE. :
!>
!>   IF PRECON IS A MULTIPLE OF 2 : DIAGONAL PRECONDITIONING USING
!>                                  THE DIAGONAL FROM A
!>   IF PRECON IS A MULTIPLE OF 3 : BLOCK-DIAGONAL PRECONDITIONING
!>                                  THIS PRECONDITIONING STARTS IN
!>                                  SUBROUTINE PREBDT, BUT ENDS WITH
!>                                  A STANDARD DIAGONAL PRECONDITIONING,
!>                                  PERFORMED HERE
!>   IF PRECON IS A MULTIPLE OF 5 : DIAGONAL PRECONDITIONING USING
!>                                  THE ABSOLUTE VALUE OF THE DIAGONAL
!>                                  FROM A
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  SHOULD NOT FORGET TO REVERT THE CHANGE OF VARIABLE
!>            AFTER RESOLUTION

!>  @code
!>-----------------------------------------------------------------------
!>
!>   EXAMPLE FOR A 4-MATRIX BLOCK:
!>
!>        (  A11    A12  ) ( X1 ) = ( B1 )
!>        (              ) (    )   (    )
!>        (              ) (    )   (    )
!>        (  A21    A22  ) ( X2 ) = ( B2 )
!>
!>   THE DIAGONAL PRECONDITIONING MATRICES ARE D1 AND D2
!>
!>   THESE DIAGONALS ARE GIVEN BY THE USER IF DIADON=.TRUE. OR
!>   ARE THE DIAGONALS FROM A11, A22 IF PRECON IS A MULTIPLE OF 2
!>
!>                                   -1
!>   CHANGE OF VARIABLE X1PRIME =  D1    X1
!>                                   -1
!>                      X2PRIME =  D2    X2
!>
!>   THE WHOLE SYSTEM IS THEN MULTIPLIED BY D ON THE LEFT
!>
!>   PRODUCT:
!>
!>  ( D1   0  )       (  A11   A12  )     ( D1   0  )
!>  (         )       (             )     (         )
!>  (         )   X   (             )     (         )
!>  ( 0   D2  )       (  A21   A22  )  X  ( 0   D2  )
!>
!>
!>   WHICH GIVES :
!>
!>           (  D1  A11 D1       D1  A12 D2  )
!>           (                               )
!>           (                               )
!>           (  D2  A21 D1       D2  A22 D2  )
!>
!>
!>   BEWARE: ALL AIJ MATRICES ARE REMPLACED BY THEIR VALUE
!>           AFTER PRECONDITIONING
!>
!>   TREATS THE SECOND MEMBERS:
!>
!>   ( D1   0 )       ( B1 )
!>   (        )  X    (    )
!>   ( 0   D2 )       ( B2 )
!>
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, D, DIADON, MESH, PRECON, PREXSM, S, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PRECDT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), PRECD1(), PRECD4(), PRECD9()
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
!></td><td>--></td><td>MATRICE OU BLOC DE MATRICES
!>    </td></tr>
!>          <tr><td>B
!></td><td>--></td><td>SECONDS MEMBRES DU SYSTEME.
!>    </td></tr>
!>          <tr><td>D
!></td><td><--</td><td>STOCKAGE DE MATRICES DIAGONALES
!>                  (TOUJOURS DANS UN BLOC)
!>    </td></tr>
!>          <tr><td>DIADON
!></td><td>--></td><td>.TRUE. : LES DIAGONALES SONT DONNEES.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE.
!>    </td></tr>
!>          <tr><td>PRECON
!></td><td>--></td><td>VARIANTE DE PRECONDITIONNEMENT
!>                  ICI PRECON DOIT ETRE MULTIPLE DE 2,3 OU 5
!>    </td></tr>
!>          <tr><td>PREXSM
!></td><td>--></td><td>.TRUE. : ON PRECONDITIONNE X1,X2,X3 ET SM
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>0 : SYSTEME NORMAL
!>                  1 : BLOC A UNE MATRICE
!>                  2 : BLOC A 4   MATRICES
!>                  3 : BLOC A 9   MATRICES
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>VALEURS A L' ETAPE N+1.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PRECDT
     &(X,A,B,D,MESH,PRECON,PREXSM,DIADON,S)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| MATRICE OU BLOC DE MATRICES
C| B             |-->| SECONDS MEMBRES DU SYSTEME.
C| D             |<--| STOCKAGE DE MATRICES DIAGONALES
C|                |   | (TOUJOURS DANS UN BLOC)
C| DIADON         |-->| .TRUE. : LES DIAGONALES SONT DONNEES.
C| MESH           |-->| MAILLAGE.
C| PRECON         |-->| VARIANTE DE PRECONDITIONNEMENT
C|                |   | ICI PRECON DOIT ETRE MULTIPLE DE 2,3 OU 5
C| PREXSM         |-->| .TRUE. : ON PRECONDITIONNE X1,X2,X3 ET SM
C| S             |-->| 0 : SYSTEME NORMAL
C|                |   | 1 : BLOC A UNE MATRICE
C|                |   | 2 : BLOC A 4   MATRICES
C|                |   | 3 : BLOC A 9   MATRICES
C| X             |<->| VALEURS A L' ETAPE N+1.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PRECDT => PRECDT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: PRECON,S
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
C  STANDARD CASE
C
      IF(S.EQ.0) THEN
C
        CALL PRECD1(X,A,B,D%ADR(1)%P,MESH,PRECON,PREXSM,DIADON)
C
C-----------------------------------------------------------------------
C
C  CASE WHERE A X, B AND D ARE BLOCKS OF 1 OBJECT
C
      ELSEIF(S.EQ.1) THEN
C
        CALL PRECD1(X%ADR(1)%P,A%ADR(1)%P,B%ADR(1)%P,D%ADR(1)%P,MESH,
     &              PRECON,PREXSM,DIADON)
C
      ELSEIF(S.EQ.2) THEN
C
        CALL PRECD4(X%ADR(1)%P,X%ADR(2)%P,
     &              A%ADR(1)%P,A%ADR(2)%P,A%ADR(3)%P,A%ADR(4)%P,
     &              B%ADR(1)%P,B%ADR(2)%P,
     &              D%ADR(1)%P,D%ADR(2)%P,
     &              MESH,PRECON,PREXSM,DIADON)
C
      ELSEIF(S.EQ.3) THEN
C
        CALL PRECD9(X%ADR(1)%P,X%ADR(2)%P,X%ADR(3)%P,
     &              A%ADR(1)%P,A%ADR(2)%P,A%ADR(3)%P,A%ADR(4)%P,
     &              A%ADR(5)%P,A%ADR(6)%P,A%ADR(7)%P,A%ADR(8)%P,
     &              A%ADR(9)%P,
     &              B%ADR(1)%P,B%ADR(2)%P,B%ADR(3)%P,
     &              D%ADR(1)%P,D%ADR(2)%P,D%ADR(3)%P,
     &              MESH,PRECON,PREXSM,DIADON)
C
      ELSE
C
C-----------------------------------------------------------------------
C
C  ERROR
C
        IF(LNG.EQ.1) WRITE(LU,100) S
        IF(LNG.EQ.2) WRITE(LU,200) S
100     FORMAT(1X,'PRECDT (BIEF) : S NON PREVU :',1I6)
200     FORMAT(1X,'PRECDT (BIEF) : UNEXPECTED S :',1I6)
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
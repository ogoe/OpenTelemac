C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BLOCK-DIAGONAL PRECONDITIONING OF A SYSTEM A X = B.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A21, A22, B1, B2, D11, D12, D21, D22, DIADON, MESH, PREXSM, X1, X2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, NPOIN1, NPOIN2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PREBD4
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OM(), OS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PREBDT()

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
!>          <tr><td>A11,A12,A21,A22
!></td><td>--></td><td>MATRICE
!>    </td></tr>
!>          <tr><td>B1,2
!></td><td>--></td><td>SECONDS MEMBRES DU SYSTEME.
!>    </td></tr>
!>          <tr><td>B2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>D11,D12,D21,D22
!></td><td><--</td><td>STOCKAGE DE MATRICES DIAGONALES
!>    </td></tr>
!>          <tr><td>DIADON
!></td><td>--></td><td>.TRUE. : LES DIAGONALES SONT DONNEES.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>PREXSM
!></td><td>--></td><td>.TRUE. : ON PRECONDITIONNE X,X2,X3 ET SM
!>    </td></tr>
!>          <tr><td>X1,X2
!></td><td><-></td><td>VALEURS A L' ETAPE N+1.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PREBD4
     &(X1,X2,A11,A12,A21,A22,B1,B2,D11,D12,D21,D22,
     & MESH,PREXSM,DIADON)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12,A21,A22|-->| MATRICE
C| B1,2           |-->| SECONDS MEMBRES DU SYSTEME.
C| B2             |---| 
C| D11,D12,D21,D22|<--| STOCKAGE DE MATRICES DIAGONALES
C| DIADON         |-->| .TRUE. : LES DIAGONALES SONT DONNEES.
C| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE.
C| PREXSM         |-->| .TRUE. : ON PRECONDITIONNE X,X2,X3 ET SM
C| X1,X2          |<->| VALEURS A L' ETAPE N+1.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PREBD4 => PREBD4
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL, INTENT(IN) :: PREXSM,DIADON
C
C-----------------------------------------------------------------------
C
C  VECTOR STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,B2,D11,D12,D21,D22
      TYPE(BIEF_OBJ), INTENT(IN)    :: X2,B1
C
C-----------------------------------------------------------------------
C
C  MATRIX STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A21,A22
C
C-----------------------------------------------------------------------
C
C  MESH STRUCTURE
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,NPOIN1,NPOIN2
      DOUBLE PRECISION C
C
C-----------------------------------------------------------------------
C
      NPOIN1 = X1%DIM1
      NPOIN2 = X2%DIM1
C
      IF(NPOIN2.NE.NPOIN1) THEN
        IF(LNG.EQ.1) WRITE(LU,100)
        IF(LNG.EQ.2) WRITE(LU,200)
100     FORMAT(1X,'PREBD4 (BIEF) : MATRICES RECTANGULAIRES',/,1X,
     &  'PRECONDITIONNEMENT BLOC-DIAGONAL IMPOSSIBLE DANS CE CAS')
200     FORMAT(1X,'PREBD4 (BIEF) : RECTANGULAR MATRICES',/,1X,
     &  'BLOCK-DIAGONAL PRECONDITIONING IMPOSSIBLE IN THIS CASE')
        CALL PLANTE(0)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  PREPARES THE DIAGONALS:
C
      IF(.NOT.DIADON) THEN
C
        CALL OS( 'X=Y     ' , D11 , A11%D , D11 , C )
        CALL OS( 'X=Y     ' , D12 , A12%D , D12 , C )
        CALL OS( 'X=Y     ' , D21 , A21%D , D21 , C )
        CALL OS( 'X=Y     ' , D22 , A22%D , D22 , C )
C
C  TEST TO REDUCE TO DIAGONAL PRECONDITIONING
C
C       CALL OS( 'X=C     ' , D12 , A12%D , Z , 0.D0 )
C       CALL OS( 'X=C     ' , D21 , A21%D , Z , 0.D0 )
C
C  END OF TEST TO REDUCE TO DIAGONAL PRECONDITIONING
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  L D U FACTORISATION OF THE DIAGONAL BLOCK:
C
C     ONLY D11 INVERTED IS NOW USED
      CALL OS( 'X=1/Y   ' , D11 , D11 , D11 , C)
C
      DO 10 I = 1,NPOIN1
C
        D21%R(I) = D21%R(I) * D11%R(I)
        D22%R(I) = D22%R(I) - D21%R(I) * D12%R(I)
        D12%R(I) = D12%R(I) * D11%R(I)
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
C CHANGE OF VARIABLES:
C
      IF(PREXSM) THEN
C
        CALL OS( 'X=X+YZ  ' , X1 , X2 , D12 , C )
C
      ENDIF
C
C  COMPUTES THE SQUARE ROOT
C  INVERTS D11,D22,D33
C  (THEY ARE ONLY USED IN THIS FORM FROM NOW ON)
C
C     INVERSION OF D11 ALREADY PERFORMED
      CALL OS( 'X=1/Y   ' , D22 , D22 , D22 , C )
      CALL OS( 'X=SQR(Y)' , D11 , D11 , D11 , C )
      CALL OS( 'X=SQR(Y)' , D22 , D22 , D22 , C )
C
C=======================================================================
C MULTIPLIES A ON THE LEFT BY L INVERTED
C=======================================================================
C
C A21 :
      CALL OM( 'M=M-DN  ' , A21 , A11 , D21 , C , MESH)
C A22 :
      CALL OM( 'M=M-DN  ' , A22 , A12 , D21 , C , MESH)
C
C=======================================================================
C MULTIPLIES A ON THE RIGHT BY U INVERTED
C=======================================================================
C
C A12 :
      CALL OM( 'M=M-ND  ' , A12 , A11 , D12 , C , MESH)
C A22 :
      CALL OM( 'M=M-ND  ' , A22 , A21 , D12 , C , MESH)
C
C-----------------------------------------------------------------------
C
C NEW SECOND MEMBER
C
      IF(PREXSM) THEN
C
      DO 21 I = 1,NPOIN1
        B2%R(I) = B2%R(I) - D21%R(I) * B1%R(I)
21    CONTINUE
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
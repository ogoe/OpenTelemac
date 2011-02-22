C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!>               (SEE EXPLANATIONS IN PRECDT).
!><br>            A IS A 9-MATRIX BLOCK HERE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A11, A12, A13, A21, A22, A23, A31, A32, A33, B1, B2, B3, D1, D2, D3, DIADON, MESH, PRECON, PREXSM, X1, X2, X3
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PRECD9
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OM(), OS(), PARCOM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRECDT()

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
!> </td><td> J-M HERVOUET (LNH)  30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A11,
!></td><td>--></td><td>MATRICES COMPOSANT LE BLOC A
!>    </td></tr>
!>          <tr><td>A12
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A13
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A21
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A22
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A23
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A31
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A32
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A33
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>B1,B2,B3
!></td><td>--></td><td>SECONDS MEMBRES DU SYSTEME.
!>    </td></tr>
!>          <tr><td>D1,D2,D3
!></td><td><--</td><td>STOCKAGE DE MATRICES DIAGONALES
!>    </td></tr>
!>          <tr><td>DIADON
!></td><td>--></td><td>.TRUE. : LES DIAGONALES SONT DONNEES.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE.
!>    </td></tr>
!>          <tr><td>PRECON
!></td><td>--></td><td>VARIANTE DE PRECONDITIONNEMENT
!>    </td></tr>
!>          <tr><td>PREXSM
!></td><td>--></td><td>.TRUE. : ON PRECONDITIONNE X,X2,X3 ET SM
!>    </td></tr>
!>          <tr><td>X1,X2,X3
!></td><td><-></td><td>VALEURS A L' ETAPE N+1.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PRECD9
     &(X1,X2,X3,A11,A12,A13,A21,A22,A23,A31,A32,A33,
     & B1,B2,B3,D1,D2,D3,MESH,PRECON,PREXSM,DIADON)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,           |-->| MATRICES COMPOSANT LE BLOC A
C| A12            |---| 
C| A13            |---| 
C| A21            |---| 
C| A22            |---| 
C| A23            |---| 
C| A31            |---| 
C| A32            |---| 
C| A33            |---| 
C| B1,B2,B3       |-->| SECONDS MEMBRES DU SYSTEME.
C| D1,D2,D3       |<--| STOCKAGE DE MATRICES DIAGONALES
C| DIADON         |-->| .TRUE. : LES DIAGONALES SONT DONNEES.
C| MESH           |-->| MAILLAGE.
C| PRECON         |-->| VARIANTE DE PRECONDITIONNEMENT
C| PREXSM         |-->| .TRUE. : ON PRECONDITIONNE X,X2,X3 ET SM
C| X1,X2,X3       |<->| VALEURS A L' ETAPE N+1.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PRECD9 => PRECD9
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: PRECON
C
      LOGICAL, INTENT(IN) :: PREXSM,DIADON
C
C-----------------------------------------------------------------------
C
C  VECTOR STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,X2,X3,B1,B2,B3,D1,D2,D3
C
C-----------------------------------------------------------------------
C
C  MATRIX STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A13,A21
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A22,A23,A31,A32,A33
C
C-----------------------------------------------------------------------
C
C  MESH STRUCTURE
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION C
C
C-----------------------------------------------------------------------
C
C  PREPARES THE DIAGONALS:
C
      IF(.NOT.DIADON) THEN
C
C  COMPUTES THE SQUARE ROOTS OF THE ABSOLUTE VALUES
C
        IF(PRECON.EQ.5) THEN
C  PROBLEM IN PARALLEL MODE, SHOULD TAKE ABSOLUTE VALUE AFTER PARCOM
          CALL OS( 'X=ABS(Y)' , D1 , A11%D , D1 , C )
          CALL OS( 'X=ABS(Y)' , D2 , A22%D , D2 , C )
          CALL OS( 'X=ABS(Y)' , D3 , A33%D , D3 , C )
        ELSE
          CALL OS( 'X=Y     ' , D1 , A11%D , D1 , C )
          CALL OS( 'X=Y     ' , D2 , A22%D , D2 , C )
          CALL OS( 'X=Y     ' , D3 , A33%D , D3 , C )
        ENDIF
C
C  PARALLEL MODE: COMPLETE DIAGONAL BEFORE TAKING THE SQUARE ROOT
C
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(D1,2,MESH)
          CALL PARCOM(D2,2,MESH)
          CALL PARCOM(D3,2,MESH)
        ENDIF
C
        CALL OS( 'X=SQR(Y)' , D1 , D1 , D1 , C )
        CALL OS( 'X=SQR(Y)' , D2 , D2 , D2 , C )
        CALL OS( 'X=SQR(Y)' , D3 , D3 , D3 , C )
C
C-----------------------------------------------------------------------
C                                                    -1
C  CHANGE OF VARIABLES (D1,D2 AND D3 ACTUALLY HOLD D1 ,...)
C
        IF(PREXSM) THEN
          CALL OS( 'X=XY    ' , X1 , D1 , D1 , C )
          CALL OS( 'X=XY    ' , X2 , D2 , D2 , C )
          CALL OS( 'X=XY    ' , X3 , D3 , D3 , C )
        ENDIF
C
C-----------------------------------------------------------------------
C
C  COMPUTES THE INVERSE OF THE SQUARE ROOTS OF THE DIAGONALS
C  THIS GIVES BACK TRUE D1,D2,D3 AND NOT D1,D2,D3 INVERTED
C
        CALL OS( 'X=1/Y   ' , D1 , D1 , D1 , C , 2 , 1.D0 , 1.D-10)
        CALL OS( 'X=1/Y   ' , D2 , D2 , D2 , C , 2 , 1.D0 , 1.D-10)
        CALL OS( 'X=1/Y   ' , D3 , D3 , D3 , C , 2 , 1.D0 , 1.D-10)
C
      ELSE
C
C  CASE WHERE D1,D2,D3 ARE GIVEN, CHANGE OF VARIABLES
C  CHANGE OF VARIABLE (D1,D2,D3 REALLY HOLD D1,D2,D3)
C
        IF(PREXSM) THEN
          CALL OS( 'X=Y/Z   ' , X1 , X1 , D1 , C )
          CALL OS( 'X=Y/Z   ' , X2 , X2 , D2 , C )
          CALL OS( 'X=Y/Z   ' , X3 , X3 , D3 , C )
        ENDIF
C
      ENDIF
C
C=======================================================================
C PRECONDITIONING OF A11 :
C=======================================================================
C
      CALL OM( 'M=DMD   ' , A11,A11 ,D1,C,MESH)
C
C=======================================================================
C PRECONDITIONING OF A12 :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A12,A12 ,D1,C,MESH)
      CALL OM( 'M=MD    ' , A12,A12 ,D2,C,MESH)
C
C=======================================================================
C PRECONDITIONING OF A13 :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A13,A13 ,D1,C,MESH)
      CALL OM( 'M=MD    ' , A13,A13 ,D3,C,MESH)
C
C=======================================================================
C PRECONDITIONING OF A21 :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A21,A21 ,D2,C,MESH)
      CALL OM( 'M=MD    ' , A21,A21 ,D1,C,MESH)
C
C=======================================================================
C PRECONDITIONING OF A22 :
C=======================================================================
C
      CALL OM( 'M=DMD   ' , A22,A22 ,D2,C,MESH)
C
C=======================================================================
C PRECONDITIONING OF A23 :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A23,A23 ,D2,C,MESH)
      CALL OM( 'M=MD    ' , A23,A23 ,D3,C,MESH)
C
C=======================================================================
C PRECONDITIONING OF A31 :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A31,A31 ,D3,C,MESH)
      CALL OM( 'M=MD    ' , A31,A31 ,D1,C,MESH)
C
C=======================================================================
C PRECONDITIONING OF A32 :
C=======================================================================
C
      CALL OM( 'M=DM    ' , A32,A32 ,D3,C,MESH)
      CALL OM( 'M=MD    ' , A32,A32 ,D2,C,MESH)
C
C=======================================================================
C PRECONDITIONING OF A33 :
C=======================================================================
C
      CALL OM( 'M=DMD   ' , A33,A33 ,D3,C,MESH)
C
C=======================================================================
C
C     CASES WHERE THE DIAGONALS ARE KNOWN
C     (VALID ONLY WITH ONE SINGLE DOMAIN)
C
      IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) THEN
C
C       IF PRECON = 2 OR 3
        IF(2*(PRECON/2).EQ.PRECON.AND..NOT.DIADON) THEN
          A11%TYPDIA='I'
          A22%TYPDIA='I'
          A33%TYPDIA='I'
        ELSEIF(3*(PRECON/3).EQ.PRECON.AND..NOT.DIADON) THEN
          A11%TYPDIA='I'
          A22%TYPDIA='I'
          A33%TYPDIA='I'
          A12%TYPDIA='0'
          A13%TYPDIA='0'
          A21%TYPDIA='0'
          A23%TYPDIA='0'
          A31%TYPDIA='0'
          A32%TYPDIA='0'
        ENDIF
C
      ENDIF
C
C=======================================================================
C
C PRECONDITIONING OF THE SECOND MEMBER
C
      IF(PREXSM) THEN
        CALL OS( 'X=XY    ' , B1 , D1 , D1 , C )
        CALL OS( 'X=XY    ' , B2 , D2 , D2 , C )
        CALL OS( 'X=XY    ' , B3 , D3 , D3 , C )
      ENDIF
C
C=======================================================================
C
      RETURN
      END


C
C#######################################################################
C
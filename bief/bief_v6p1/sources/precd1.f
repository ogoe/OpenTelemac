C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!>               (SEE EXPLANATIONS IN PRECDT).
!><br>            A IS A SIMPLE MATRIX HERE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, D, DIADON, MESH, PRECON, PREXSM, X
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
!>    </th><td> EX_PRECD1
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 06/07/2009
!> </td><td> J-M HERVOUET (LNHE)  01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td>--></td><td>MATRICE
!>    </td></tr>
!>          <tr><td>B
!></td><td>--></td><td>SECONDS MEMBRES DU SYSTEME.
!>    </td></tr>
!>          <tr><td>D
!></td><td>---</td><td>
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
!>          <tr><td>X
!></td><td><-></td><td>VALEURS A L' ETAPE N+1.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PRECD1
     &(X,A,B,D,MESH,PRECON,PREXSM,DIADON)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| MATRICE
C| B             |-->| SECONDS MEMBRES DU SYSTEME.
C| D             |---| 
C| DIADON         |-->| .TRUE. : LES DIAGONALES SONT DONNEES.
C| MESH           |-->| MAILLAGE.
C| PRECON         |-->| VARIANTE DE PRECONDITIONNEMENT
C| PREXSM         |-->| .TRUE. : ON PRECONDITIONNE X,X2,X3 ET SM
C| X             |<->| VALEURS A L' ETAPE N+1.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PRECD1 => PRECD1
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X,B,D
C
C-----------------------------------------------------------------------
C
C  MATRIX STRUCTURES
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
      DOUBLE PRECISION C
C
C-----------------------------------------------------------------------
C
C  PREPARES THE DIAGONALS:
C
      IF(.NOT.DIADON) THEN
C
C  COMPUTES THE SQUARE ROOTS OF THE ABSOLUTE VALUES OR OF THE VALUES
C
        IF(PRECON.EQ.5) THEN
          CALL OS( 'X=ABS(Y)' , X=D , Y=A%D )
        ELSE
          CALL OS( 'X=Y     ' , X=D , Y=A%D )
        ENDIF
C
C  PARALLEL MODE: COMPLETE DIAGONAL BEFORE TAKING THE SQUARE ROOT
C
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(D,2,MESH)
        ENDIF
C
        CALL OS( 'X=SQR(Y)' , X=D , Y=D )
C
C-----------------------------------------------------------------------
C                                         -1
C  CHANGE OF VARIABLES (D ACTUALLY HOLDS D  )
C
        IF(PREXSM) CALL OS( 'X=XY    ' , X , D , D , C )
C
C-----------------------------------------------------------------------
C
C  COMPUTES THE INVERSE OF THE SQUARE ROOTS OF THE DIAGONALS
C  THIS GIVES BACK TRUE D AND NOT D INVERTED
C
        CALL OS( 'X=1/Y   ' , D , D , D , C , 2 , 1.D0 , 1.D-10 )
C
      ELSE
C
C  CASE WHERE D IS GIVEN, CHANGE OF VARIABLES
C  CHANGE OF VARIABLE (D REALLY HOLDS D)
C
        IF(PREXSM) THEN
          CALL OS( 'X=Y/Z   ' , X=X , Y=X , Z=D )
        ENDIF
C
      ENDIF
C
C=======================================================================
C PRECONDITIONING OF A:
C=======================================================================
C
      CALL OM( 'M=DMD   ' , A , A , D , C , MESH )
C     IF PRECON = 2 OR 3
      IF((2*(PRECON/2).EQ.PRECON.OR.3*(PRECON/3).EQ.PRECON).AND.
     &                                                 .NOT.DIADON) THEN
C       VALID ONLY WITH ONE SINGLE DOMAIN
        IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) A%TYPDIA='I'
      ENDIF
C
C=======================================================================
C
C PRECONDITIONING OF THE SECOND MEMBER
C
      IF(PREXSM) CALL OS( 'X=XY    ' , X=B , Y=D )
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C
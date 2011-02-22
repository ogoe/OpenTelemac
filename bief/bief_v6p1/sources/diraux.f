C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       HELPS PREPARE A LINEAR SYSTEM WITH DIRICHLET CONDITIONS.
!>  @code
!>             X, Y AND Z MUST BE STRUCTURES.<br>
!>             HERE X IS A VECTOR DEFINED ON THE DOMAIN
!>                  Y IS A VECTOR DEFINED ON THE DOMAIN
!>                  Z IS A VECTOR DEFINED ON THE DOMAIN OR BOUNDARY<br>
!>             INDIC IS AN ARRAY, NOT A STRUCTURE ||||||||||<br>
!>  |||||||| : THE OPERATION IS ONLY PERFORMED IF INDIC(K)=CRITER
!>             FOR A GLOBAL OR BOUNDARY NUMBER K.<br>
!>  OPERATIONS :<br>
!>             W SET TO 0.D0 FOR POINTS WHERE INDIC(K) = CRITER
!>                   TO 1.D0 OTHERWISE<br>
!>             X = Y MULTIPLIED BY Z IF INDIC(K) = CRITER<br>
!>             F = Z IF INDIC(K) = CRITER<br>
!>  THESE OPERATIONS ARE USED TO TREAT THE POINTS OF TYPE DIRICHLET.<br>
!>             X IS THE SECOND MEMBER (WILL BE EQUAL TO THE DIAGONAL
!>             MULTIPLIED BY THE DIRICHLET VALUE Z)<br>
!>             F IS THE UNKNOWN (SET TO ITS DIRICHLET VALUE)<br>
!>             W IS A WORKING ARRAY USED TO CANCEL THE MATRICES
!>             TERMS TOUCHING DIRICHLET POINTS
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CRITER, F, INDIC, MESH, W, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELMX, IELMZ, K, N, NPOIN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DIRAUX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DIRI01(), DIRI04(), DIRI09()

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
!> </td><td> 06/12/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CRITER
!></td><td>--></td><td>CRITERE POUR FAIRE L'OPERATION.
!>    </td></tr>
!>          <tr><td>F
!></td><td>--></td><td>INCONNUE MISE A SA VALEUR DIRICHLET
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td>--></td><td>TABLEAU D'INDICATEURS.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>STRUCTURE DU MAILLAGE
!>    </td></tr>
!>          <tr><td>W
!></td><td>--></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR RESULTAT
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIRAUX
     & ( X , Y , Z , W , F , INDIC , CRITER , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CRITER         |-->| CRITERE POUR FAIRE L'OPERATION.
C| F             |-->| INCONNUE MISE A SA VALEUR DIRICHLET
C| INDIC          |-->| TABLEAU D'INDICATEURS.
C| MESH           |-->| STRUCTURE DU MAILLAGE
C| W             |-->| TABLEAU DE TRAVAIL
C| X             |<--| VECTEUR RESULTAT
C| Y             |-->| VECTEUR OPERANDE
C| Z             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DIRAUX => DIRAUX
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: X,W,F
      TYPE(BIEF_OBJ) , INTENT(IN)    :: Y,Z
      INTEGER        , INTENT(IN)    :: INDIC(*),CRITER
      TYPE(BIEF_MESH), INTENT(IN)    :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,NPOIN,IELMX,IELMZ,N
C
C-----------------------------------------------------------------------
C
      NPOIN = Z%DIM1
C
C-----------------------------------------------------------------------
C
C  W SET TO 1
C
      CALL OS( 'X=C     ' , X=W , C=1.D0 )
C
C-----------------------------------------------------------------------
C
      IELMX=X%ELM
      IELMZ=Z%ELM
C
      IF(IELMX.NE.IELMZ) THEN
C
        DO K=1,NPOIN
          IF(INDIC(K).EQ.CRITER) THEN
            N = MESH%NBOR%I(K)
            X%R(N) = Y%R(N) * Z%R(K)
            W%R(N) = 0.D0
            F%R(N) = Z%R(K)
          ENDIF
        ENDDO
C
      ELSE
C
        DO K=1,NPOIN
          IF(INDIC(K).EQ.CRITER) THEN
            X%R(K) = Y%R(K) * Z%R(K)
            W%R(K) = 0.D0
            F%R(K) = Z%R(K)
          ENDIF
        ENDDO
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
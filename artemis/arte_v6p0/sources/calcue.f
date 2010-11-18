C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES AN EFFECTIVE SPEED UE FOR THE ESTIMATION
!>                OF THE FRICTION DISSIPATION COEFFICIENT FW.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::C C@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELM IELM@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_ARTEMIS::MESH MESH@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSK MSK@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHII PHII@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHIR PHIR@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink, 
!> @link DECLARATIONS_ARTEMIS::T4 T4@endlink, 
!> @link DECLARATIONS_ARTEMIS::U0 U0@endlink, 
!> @link DECLARATIONS_ARTEMIS::V0 V0@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BERKHO()

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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 02/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12; D. PAUGAM (1996 PLACEMENT) </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALCUE
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION BID
C
      INTRINSIC ABS,EXP
C
      CALL VECTOR(U0 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHIR , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK  , MASKEL )
C
      CALL VECTOR(V0 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHIR , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
C
C
C      THE OLD VARIABLE U1 IS STORED IN T1
C
      CALL VECTOR(T1, '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHII , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
C
C
C      THE OLD VARIABLE V1 IS STORED IN T2
C
      CALL VECTOR(T2, '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHII , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
C
      CALL VECTOR(T4 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , C , C , C , C , C , C ,
     &            MESH , MSK  , MASKEL )
C
      CALL OS( 'X=Y/Z   ' , U0 , U0 , T4 , BID )
      CALL OS( 'X=Y/Z   ' , V0 , V0 , T4 , BID )
      CALL OS( 'X=Y/Z   ' , T1 , T1 , T4 , BID )
      CALL OS( 'X=Y/Z   ' , T2 , T2 , T4 , BID )
C
C--------------------------------------------------------------
C             COMPUTES UE
C--------------------------------------------------------------
C
      CALL OS( 'X=C     ' , T4 , SBID , SBID , 0.D0 )
      CALL OS( 'X=YZ    ' , T4 , U0 , U0 , 0.D0 )
      CALL OS( 'X=X+YZ  ' , T4 , V0 , V0 , 0.D0 )
      CALL OS( 'X=X+YZ  ' , T4 , T1 , T1 , 0.D0 )
      CALL OS( 'X=X+YZ  ' , T4 , T2 , T2 , 0.D0 )
C
      CALL OS( 'X=CX    ' , T4 , SBID , SBID  , 0.5D0 )
      CALL OS( 'X=SQR(Y)' , T1 , T4   , SBID  , BID   )
      CALL OS( 'X=CY    ' , T4 , T1   , SBID  , 1.2D0 )
C
      RETURN
      END
C
C#######################################################################
C
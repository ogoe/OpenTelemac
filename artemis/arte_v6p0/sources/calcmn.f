C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES APPROXIMATE VALUES FOR THE MOMENTUMS M0, M1,
!>                M2 OF THE WAVE SPECTRUM TO CALCULATE THE MEAN PERIOD
!>                AND DIRECTION.
!><br>           (DEFINITIONS IN THE LIST OF PARAMETERS ESTABLISHED
!>                BY THE IAHR)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::HHO HHO@endlink, 
!> @link DECLARATIONS_ARTEMIS::INCI INCI@endlink, 
!> @link DECLARATIONS_ARTEMIS::MCOS MCOS@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSIN MSIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::NDALE NDALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPALE NPALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::PER PER@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::T01 T01@endlink, 
!> @link DECLARATIONS_ARTEMIS::T02 T02@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink, 
!> @link DECLARATIONS_ARTEMIS::TM TM@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID, FREQ, FREQ2, PONDER
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS()

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
!>    <td> 04/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
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
!>          <tr><td>PER
!></td><td>--></td><td>PERIODE DE HOULE EN COURS DE CALCUL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALCMN
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| PER            |-->| PERIODE DE HOULE EN COURS DE CALCUL
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
      DOUBLE PRECISION FREQ, FREQ2, PONDER
C
      DOUBLE PRECISION BID
C
      INTRINSIC SQRT, ATAN2, DMOD, ABS, COS, SIN
C
C-----------------------------------------------------------------------
C
C STRUCTURES
C
C-----------------------------------------------------------------------
C
      FREQ = 1.D0/PER
      FREQ2 = FREQ * FREQ
      PONDER = 1.D0/DBLE(NDALE*NPALE)
C
C=======================================================================
C M1 = INTEGRAL OF ( F * S(F) * DF )
C=======================================================================
C
      CALL OS( 'X=Y**C  ', T1, HHO , SBID , 2.D0 )
      CALL OS( 'X=CY    ', T2, T1  , SBID , FREQ )
      CALL OS( 'X=CX    ', T2, SBID , SBID , PONDER )
      CALL OS( 'X=X+Y   ', T01, T2 , SBID , 1.D0 )
C
C=======================================================================
C M2 = INTEGRAL OF ( F**2 * S(F) * DF )
C=======================================================================
C
      CALL OS( 'X=CY    ', T2, T1  , SBID , FREQ2 )
      CALL OS( 'X=CX    ', T2, SBID , SBID , PONDER )
      CALL OS( 'X=X+Y   ', T02, T2 , SBID , 1.D0 )
C
C=======================================================================
C MT1 = INTEGRAL OF ( T * S(F) * DF )
C=======================================================================
C
      CALL OS( 'X=CY    ', T2 , T1  , SBID , PER )
      CALL OS( 'X=CX    ', T2 , SBID , SBID , PONDER )
      CALL OS( 'X=X+Y   ', TM , T2  , SBID , BID )
C
C=======================================================================
C MCOS = INTEGRAL OF ( COS(INCI) * S(F) * DF )
C=======================================================================
C
      CALL OS( 'X=COS(Y)',  T2 , INCI , SBID , BID )
      CALL OS( 'X=CXY   ',  T2 , T1   , SBID , PONDER )
      CALL OS( 'X=X+Y   ', MCOS, T2   , SBID , BID )
C
C=======================================================================
C MSIN = INTEGRAL OF ( SIN(INCI) * S(F) * DF )
C=======================================================================
C
      CALL OS( 'X=SIN(Y)',  T2 , INCI , SBID , BID )
      CALL OS( 'X=CXY   ',  T2 , T1   , SBID , PONDER )
      CALL OS( 'X=X+Y   ', MSIN, T2   , SBID , BID )
C
      RETURN
      END
C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES VARIOUS ESTIMATES OF THE MEAN WAVE
!>                PERIOD :
!>                    T01 = M0/M1;
!>                    T02 = SQRT(M0/M2);
!>                    TM.
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
!> @link DECLARATIONS_ARTEMIS::HALE HALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::HHO HHO@endlink, 
!> @link DECLARATIONS_ARTEMIS::INCI INCI@endlink, 
!> @link DECLARATIONS_ARTEMIS::MCOS MCOS@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSIN MSIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::NDALE NDALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPALE NPALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::T01 T01@endlink, 
!> @link DECLARATIONS_ARTEMIS::T02 T02@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink, 
!> @link DECLARATIONS_ARTEMIS::T3 T3@endlink, 
!> @link DECLARATIONS_ARTEMIS::TM TM@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID, PONDER, RADDEG
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
!>          <tr><td>HALE
!></td><td>--></td><td>ICI, ENERGIE DE HOULE SANS LA DERN. COMPOS.
!>    </td></tr>
!>          <tr><td>HHO
!></td><td>--></td><td>HAUTEURS DE HOULE SIGNIFICATIVES
!>    </td></tr>
!>          <tr><td>NDALE
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>                  DU SPECTRE DE HOULE
!>    </td></tr>
!>          <tr><td>NPALE
!></td><td>--></td><td>NOMBRE DE PERIODES DE DISCRETISATION
!>                  DU SPECTRE DE HOULE
!>    </td></tr>
!>          <tr><td>PER
!></td><td>--></td><td>PERIODE DE HOULE EN COURS DE CALCUL
!>    </td></tr>
!>          <tr><td>T01
!></td><td><-></td><td>MOMENT D'ORDRE 1 PUIS VALEUR DE T01
!>    </td></tr>
!>          <tr><td>T02
!></td><td><-></td><td>MOMENT D'ORDRE 2 PUIS VALEUR DE T02
!>    </td></tr>
!>          <tr><td>T1
!></td><td>--></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>T2
!></td><td>--></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>T3
!></td><td>--></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TM
!></td><td><-></td><td>MOMENT D'ORDRE 1 EN PERIODE PUIS VALEUR TM
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALCTM
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| HALE           |-->| ICI, ENERGIE DE HOULE SANS LA DERN. COMPOS.
C| HHO            |-->| HAUTEURS DE HOULE SIGNIFICATIVES
C| NDALE          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C|                |   | DU SPECTRE DE HOULE
C| NPALE          |-->| NOMBRE DE PERIODES DE DISCRETISATION
C|                |   | DU SPECTRE DE HOULE
C| PER            |-->| PERIODE DE HOULE EN COURS DE CALCUL
C| T01            |<->| MOMENT D'ORDRE 1 PUIS VALEUR DE T01
C| T02            |<->| MOMENT D'ORDRE 2 PUIS VALEUR DE T02
C| T1             |-->| TABLEAU DE TRAVAIL
C| T2             |-->| TABLEAU DE TRAVAIL
C| T3             |-->| TABLEAU DE TRAVAIL
C| TM             |<->| MOMENT D'ORDRE 1 EN PERIODE PUIS VALEUR TM
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
      DOUBLE PRECISION PONDER,RADDEG
C
      DOUBLE PRECISION BID
C
      INTRINSIC SQRT, ATAN2, DMOD, ABS, COS, SIN
C
C-----------------------------------------------------------------------
C
C STRUCTURES
C
C
C-----------------------------------------------------------------------
C
      PONDER = 1.D0/DBLE(NDALE*NPALE)
      RADDEG = 180.D0/3.141592654D0
C
C=======================================================================
C COMPUTES M0 MOMENTUM AND STORES IT IN T2
C=======================================================================
C
      CALL OS( 'X=Y**C  ', T1 , HHO  , SBID  , 2.D0 )
      CALL OS( 'X=CX    ', T1 , SBID  , SBID  , PONDER )
      CALL OS( 'X=Y+Z   ', T2 , HALE , T1   , BID )
C
C=======================================================================
C T01 = M0 / M1
C=======================================================================
C
      CALL OS( 'X=Y     ', T3 , T01 , SBID  , BID )
      CALL OS( 'X=Y/Z   ', T01, T2  , T3   , BID )
C
C=======================================================================
C T02 = SQRT( M0 / M2 )
C=======================================================================
C
      CALL OS( 'X=Y     ', T3 , T02 , SBID , BID )
      CALL OS( 'X=Y/Z   ', T1 , T2  , T3  , BID )
      CALL OS( 'X=SQR(Y)', T02, T1  , SBID , BID )
C
C=======================================================================
C TM =  MT1 / M0
C=======================================================================
C
      CALL OS( 'X=Y     ', T3 , TM , SBID , BID )
      CALL OS( 'X=Y/Z   ', TM , T3 , T2  , BID )
C
C
C=======================================================================
C MEAN DIRECTION: INCI
C=======================================================================
C
      CALL OS( 'X=A(Y,Z)',INCI , MSIN, MCOS , BID )
C
      RETURN
      END
C
C#######################################################################
C
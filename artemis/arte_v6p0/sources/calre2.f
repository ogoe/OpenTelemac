C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES MEAN PARAMETERS OF THE WAVE SPECTRUM
!>               (RANDOM SEAS) :
!>                    K : MEAN WAVE NUMBER;
!>                    C : MEAN PHASE CELERITY;
!>                    CG : MEAN GROUP CELERITY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::C C@endlink, 
!> @link DECLARATIONS_ARTEMIS::CG CG@endlink, 
!> @link DECLARATIONS_ARTEMIS::GRAV GRAV@endlink, 
!> @link DECLARATIONS_ARTEMIS::H H@endlink, 
!> @link DECLARATIONS_ARTEMIS::K K@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::T01 T01@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID, DEUXPI, DHTEST, I, PI
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
!>          <tr><td>C
!></td><td><--</td><td>CELERITE DE PHASE MOYENNE
!>    </td></tr>
!>          <tr><td>CG
!></td><td><--</td><td>CELERITE DE GROUPE MOYENNE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU AU REPOS
!>    </td></tr>
!>          <tr><td>K
!></td><td><--</td><td>NOMBRE D'ONDE MOYEN
!>    </td></tr>
!>          <tr><td>T01
!></td><td>--></td><td>PERIODE MOYENNE ISSUE DU MOMENT D'ORDRE 1
!>    </td></tr>
!>          <tr><td>T02
!></td><td>--></td><td>PERIODE MOYENNE ISSUE DU MOMENT D'ORDRE 2
!>    </td></tr>
!>          <tr><td>T1,T2
!></td><td>--></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TM
!></td><td>--></td><td>PERIODE MOYENNE ISSUE DU MOMENT D'ORDRE 1
!>                  EN PERIODE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALRE2
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |<--| CELERITE DE PHASE MOYENNE
C| CG             |<--| CELERITE DE GROUPE MOYENNE
C| H             |-->| HAUTEUR D'EAU AU REPOS
C| K             |<--| NOMBRE D'ONDE MOYEN
C| T01            |-->| PERIODE MOYENNE ISSUE DU MOMENT D'ORDRE 1
C| T02            |-->| PERIODE MOYENNE ISSUE DU MOMENT D'ORDRE 2
C| T1,T2          |-->| TABLEAUX DE TRAVAIL
C| TM             |-->| PERIODE MOYENNE ISSUE DU MOMENT D'ORDRE 1
C|                |   | EN PERIODE
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
      INTEGER I
C
      DOUBLE PRECISION PI,DEUXPI, DHTEST
C
      DOUBLE PRECISION BID
C
      INTRINSIC SQRT, ATAN2, DMOD, ABS, COS, SIN
C
C-----------------------------------------------------------------------
C
      PI = 3.1415926535897932384626433D0
      DEUXPI = 2.D0*PI
      GRAV = 9.81D0
C-----------------------------------------------------------------------
C
C   COMPUTES THE WAVE NUMBER: K
C   USING AN EXPLICIT FORMULATION (SEE EDF'S EXCELLENT REPORT BY
C   F. DHELLEMMES 'PRECIS SUR LES VAGUES' )
C
C-----------------------------------------------------------------------
C
C MEAN OMEGA STORED IN T1
C
      CALL OS( 'X=1/Y   ', T1 , T01  , SBID  , BID )
      CALL OS( 'X=CX    ', T1 , SBID  , SBID  , DEUXPI )
C
C OMEGA**2 * H / GRAV
C
      CALL OS( 'X=Y**C  ', T2 , T1 , SBID , 2.D0 )
      CALL OS( 'X=CXY   ', T2 , H  , SBID , 1.D0/GRAV )
C
C     INITIALISES DHTEST
C
      DHTEST = 1.D6
C
      DO 100 I=1,NPOIN
         T1%R(I) = 1.D0 + T2%R(I) *( 0.6522D0 +
     &                  T2%R(I) *( 0.4622D0 +
     &                  T2%R(I) *
     &                  T2%R(I) *( 0.0864D0 +
     &                  T2%R(I) *( 0.0675D0 ) )))
         T1%R(I) = SQRT( T2%R(I)*(T2%R(I) + 1.D0/T1%R(I)) )
         K%R(I)  = T1%R(I)/H%R(I)
         DHTEST  = MIN( DHTEST , H%R(I) )
100   CONTINUE
C
C
C=======================================================================
C COMPUTES MEAN C
C=======================================================================
C
      CALL OS( 'X=1/Y   ', T1 , T01  , SBID  , BID )
      CALL OS( 'X=CX    ', T1 , SBID  , SBID  , DEUXPI )
      CALL OS( 'X=Y/Z   ', C  , T1   , K    , BID )
C
C
C=======================================================================
C COMPUTES MEAN CG
C=======================================================================
C
      DO 200 I=1,NPOIN
         CG%R(I) = C%R(I)/2.D0 *
     &             (1.D0 + 2.D0*K%R(I)*H%R(I)/SINH(2.D0*K%R(I)*H%R(I)))
200   CONTINUE
C
      RETURN
      END
C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE VARIABLES NOT READ FROM THE RESULTS
!>                FILE (REPLACES THE VALUES READ IN THE RESULTS FILE).
!><br>         IMPOSED VALUES OF :
!><br>         -  DEPTH-AVERAGED FLOW RATE (X,Y): QU, QV
!><br>         -  WATER DEPTH:                    H
!><br>         -  BOTTOM ELEVATION:               ZF
!><br>         -  FREE SURFACE:                   Z
!><br>         -  TOTAL BED VOLUTION:             ESOMT
!><br>         -  FLOW RATE:                      Q
!><br>         -  WAVE HEIGHT:                    HWR
!><br>         -  WAVE PERIOD:                    TWR
!><br>         -  WAVE DIRECTION (WRT OY AXIS):   THETAWR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!>  @code
!>     EXAMPLE WITH NO WAVES:
!>
!>     AMPLITUDE = 0
!>     CALL OS('X=0     ',X=HW)
!>     PERIOD = 1 S
!>     CALL OS('X=C     ',X=TW,C=1.D0)
!>     ANGLE = 0
!>     CALL OS('X=0     ',X=THETAW)
!>  @endcode

!>  @warning  CONDIM_SISYPHE IS CALLED AT EACH TIME STEP IN ORDER TO
!>            IMPOSE A VARIABLE FORCING (TIDAL CURRENT, FOR EXAMPLE)

!>  @warning  IT IS NOT SUFFICIENT TO PRESCRIBE THE FLOW RATE.
!>            THE MAIN VARIABLES ARE NOW THE 2D FLOW VELOCITY FIELD
!>            AND THE FLOW DEPTH

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, ESOMT, H, HWR, NPOIN, PMAREE, Q, QU, QV, THETAWR, TWR, U, V, X, Y, Z, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!>      <td><center>                                           </center>
!> </td><td> **/05/2006
!> </td><td>
!> </td><td> THE VARIABLES U AND V, H, MUST BE DEFINED; THE OTHER ONES ARE OPTIONAL
!> </td></tr>
!>      <tr>
!>      <td><center> 5.3                                       </center>
!> </td><td> 11/09/95
!> </td><td> E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TIME
!>    </td></tr>
!>          <tr><td>ESOMT
!></td><td><-></td><td>TOTAL BED EVOLUTION
!>    </td></tr>
!>          <tr><td>H
!></td><td><-></td><td>WATER DEPTH
!>    </td></tr>
!>          <tr><td>HW
!></td><td><-></td><td>WAVE HEIGHT (M)
!>    </td></tr>
!>          <tr><td>HWR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF 2D POINTS
!>    </td></tr>
!>          <tr><td>PMAREE
!></td><td>--></td><td>TIDAL PERIOD
!>    </td></tr>
!>          <tr><td>Q
!></td><td><-></td><td>FLOW RATE
!>    </td></tr>
!>          <tr><td>QU , QV
!></td><td><--</td><td>FLOW RATE COORDINATES
!>    </td></tr>
!>          <tr><td>TETHAW
!></td><td><-></td><td>WAVE ANGLE (DEG)
!>    </td></tr>
!>          <tr><td>THETAWR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TW
!></td><td><-></td><td>WAVE PERIOD (S)
!>    </td></tr>
!>          <tr><td>TWR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U , V
!></td><td><--</td><td>FLOW VELOCITY COORDINATES
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDINATES
!>    </td></tr>
!>          <tr><td>Z
!></td><td><-></td><td>FREE SURFACE
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><-></td><td>BED ELEVATION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CONDIM_SISYPHE
     & (U      , V       , QU    , QV   , H    , ZF , Z ,
     &  ESOMT  , THETAWR ,  Q    , HWR  , TWR  ,
     &  X      , Y       , NPOIN , AT   , PMAREE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME
C| ESOMT          |<->| TOTAL BED EVOLUTION
C| H             |<->| WATER DEPTH
C| HW             |<->| WAVE HEIGHT (M)
C| HWR            |---| 
C| NPOIN          |-->| NUMBER OF 2D POINTS
C| PMAREE         |-->| TIDAL PERIOD
C| Q             |<->| FLOW RATE
C| QU , QV        |<--| FLOW RATE COORDINATES
C| TETHAW         |<->| WAVE ANGLE (DEG)
C| THETAWR        |---| 
C| TW             |<->| WAVE PERIOD (S)
C| TWR            |---| 
C| U , V          |<--| FLOW VELOCITY COORDINATES
C| X,Y            |-->| COORDINATES
C| Z             |<->| FREE SURFACE
C| ZF             |<->| BED ELEVATION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY: HW,TW,THETAW
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)::NPOIN
C
      DOUBLE PRECISION, INTENT(IN):: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN):: AT , PMAREE
C SEDIMENT
      DOUBLE PRECISION, INTENT(INOUT) ::  ZF(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::  ESOMT(NPOIN)
C HYDRODYNAMICS
      DOUBLE PRECISION, INTENT(INOUT):: Z(NPOIN) , H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::QU(NPOIN), QV(NPOIN), Q(NPOIN)
C WAVES
      DOUBLE PRECISION, INTENT (INOUT):: HWR(NPOIN) , TWR(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT):: THETAWR(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C      INTEGER I
C-----------------------------------------------------------------------
C
C     ------------------------
C     THE USER SHOULD BE AWARE
C     ++++++++++++++++++++++++
C
C     SUBROUTINE CONDIM_SISYPHE IS CALLED AT EACH TIME STEP
C     IN ORDER TO IMPOSE A VARIABLE FORCING
C     (TIDAL CURRENT, FOR EXAMPLE)
C
C     IT IS NOT SUFFICIENT TO PRESCRIBE THE FLOW RATE
C     THE MAIN VARIABLES ARE NOW THE 2D FLOW VELOCITY FIELD
C     AND THE FLOW DEPTH
C
C-----------------------------------------------------------------------
C
C     WAVES, EXAMPLE WITH NO WAVES:
C
C     AMPLITUDE = 0
C     CALL OS('X=0     ',X=HW)
C     PERIOD = 1 S
C     CALL OS('X=C     ',X=TW,C=1.D0)
C     ANGLE = 0
C     CALL OS('X=0     ',X=THETAW)
C
C     AFTER SETTING HWR, TWR AND THETAWR, PLEASE ADD:
C
C     HW%TYPR    ='Q'
C     TW%TYPR    ='Q'
C     THETAW%TYPR='Q'
C
C     TO ENABLE THE CONTROL OF WAVE DATA
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE CONDIM_SISYPHE
C
C#######################################################################
C
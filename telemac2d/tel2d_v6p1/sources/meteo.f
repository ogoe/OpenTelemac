C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES ATMOSPHERIC PRESSURE AND WIND VELOCITY FIELDS
!>               (IN GENERAL FROM INPUT DATA FILES).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  MUST BE ADAPTED BY USER

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, ATMOS, FUAIR, FVAIR, GRAV, HN, LT, NORD, NPOIN, PATMOS, PRIVE, ROEAU, TRA01, VENT, WINDX, WINDY, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> P0, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D(), TELEMAC3D()

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
!>      <td><center> 5.4                                       </center>
!> </td><td> 02/01/2004
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT,LT
!></td><td>--></td><td>TIME, ITERATION NUMBER
!>    </td></tr>
!>          <tr><td>ATMOS
!></td><td>--></td><td>YES IF PRESSURE TAKEN INTO ACCOUNT
!>    </td></tr>
!>          <tr><td>FUAIR,FVAIR
!></td><td>--></td><td>IDEM IF WIND CONSTANT.
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITY ACCELERATION
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>DEPTH
!>    </td></tr>
!>          <tr><td>NORD
!></td><td>--></td><td>DIRECTION OF NORTH, COUNTER-CLOCK-WISE
!>                  STARTING FROM VERTICAL AXIS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>PATMOS
!></td><td><--</td><td>ATMOSPHERIC PRESSURE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>USER WORKING ARRAYS (BIEF_OBJ BLOCK)
!>    </td></tr>
!>          <tr><td>ROEAU
!></td><td>--></td><td>WATER DENSITY
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td>--></td><td>WORKING ARRAY
!>    </td></tr>
!>          <tr><td>VENT
!></td><td>--></td><td>YES IF WIND TAKEN INTO ACCOUNT
!>    </td></tr>
!>          <tr><td>WINDX,Y
!></td><td><--</td><td>TWO COMPONENTS OF WIND VELOCITY
!>    </td></tr>
!>          <tr><td>WINDY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X , Y
!></td><td>--></td><td>COORDINATES OF POINTS IN THE MESH
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE METEO
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,X,Y,AT,LT,NPOIN,VENT,ATMOS,
     & HN,TRA01,GRAV,ROEAU,NORD,PRIVE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT,LT          |-->| TIME, ITERATION NUMBER
C| ATMOS          |-->| YES IF PRESSURE TAKEN INTO ACCOUNT
C| FUAIR,FVAIR    |-->| IDEM IF WIND CONSTANT.
C| GRAV           |-->| GRAVITY ACCELERATION
C| HN             |-->| DEPTH
C| NORD           |-->| DIRECTION OF NORTH, COUNTER-CLOCK-WISE
C|                |   | STARTING FROM VERTICAL AXIS
C| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
C| PATMOS         |<--| ATMOSPHERIC PRESSURE
C| PRIVE          |-->| USER WORKING ARRAYS (BIEF_OBJ BLOCK)
C| ROEAU          |-->| WATER DENSITY
C| TRA01          |-->| WORKING ARRAY
C| VENT           |-->| YES IF WIND TAKEN INTO ACCOUNT
C| WINDX,Y        |<--| TWO COMPONENTS OF WIND VELOCITY
C| WINDY          |---| 
C| X , Y          |-->| COORDINATES OF POINTS IN THE MESH
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: LT,NPOIN
      LOGICAL, INTENT(IN)             :: ATMOS,VENT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: WINDX(NPOIN),WINDY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: PATMOS(NPOIN),TRA01(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FUAIR,FVAIR,AT,GRAV,ROEAU,NORD
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PRIVE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION P0,Z(1)
C
C-----------------------------------------------------------------------
C
C     BEWARE, HERE ONLY ONE COMPUTATION AT FIRST TIMESTEP
C
      IF(LT.EQ.0) THEN
C
C-----------------------------------------------------------------------
C
C     ATMOSPHERIC PRESSURE
C
      IF(ATMOS) THEN
        P0 = 100000.D0
        CALL OV( 'X=C     ' , PATMOS , Y , Z , P0 , NPOIN )
      ENDIF
C
C-----------------------------------------------------------------------
C
C     WIND : IN THIS CASE THE WIND IS CONSTANT,
C            VALUE GIVEN IN STEERING FILE.
C
C     MAY REQUIRE A ROTATION,
C     DEPENDING ON THE SYSTEM IN WHICH THE WIND VELOCITY WAS SUPPLIED
C
      IF(VENT) THEN
        CALL OV( 'X=C     ' , WINDX , Y , Z , FUAIR , NPOIN )
        CALL OV( 'X=C     ' , WINDY , Y , Z , FVAIR , NPOIN )
      ENDIF
C
C     END OF IF(LT.EQ.0)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C
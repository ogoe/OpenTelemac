C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE HYDROSTATIC PRESSURE FIELD PH [PA]
!>                THROUGH INTEGRATION BASED ON TRAPEZIUM RULE IN VERTICAL.
!><br>            THIS IS NEEDED FOR APPLICATIONS WHERE THE GLOBAL
!>                PRESSURE OUTPUT IS REQUIRED.
!>  @code
!>                                               S
!> PH(Z) = G * RHO0 * (S-Z) + G * RHO0 * INTEGRAL  DELTAR DZ
!>                                               Z
!> WHERE: DELTAR = (RHO-RHO0)/RHO0
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DELTAR, GRAV, NPLAN, NPOIN2, NPOIN3, PH, PRIVE, RHO0, TRA01, TRA02, Z
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, K1, K2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRERES_TELEMAC3D()

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
!> </td><td> **/04/99
!> </td><td> JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DELTAR
!></td><td>--></td><td>RELATIVE DENSITY DELTAR = (RHO-RHO0)/RHO0
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF MESH PLANES
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF 2D-POINTS
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF 3D-POINTS
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>ITS DIMENSION
!>    </td></tr>
!>          <tr><td>PH
!></td><td><--</td><td>HYDROSTATIC PRESSURE
!>    </td></tr>
!>          <tr><td>PRIV
!></td><td><-></td><td>PRIVATE FIELD NPRIV*NPOIN3
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RHO0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>WORK FIELDS
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>Z-COORDINATES OF NODES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE PHSTAT
     &   (PH, DELTAR, Z, TRA01, TRA02, RHO0, GRAV,
     &    NPOIN3, NPOIN2, NPLAN, PRIVE )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DELTAR         |-->| RELATIVE DENSITY DELTAR = (RHO-RHO0)/RHO0
C| GRAV           |---| 
C| NPLAN          |-->| NUMBER OF MESH PLANES
C| NPOIN2         |-->| NUMBER OF 2D-POINTS
C| NPOIN3         |-->| NUMBER OF 3D-POINTS
C| NPRIV          |-->| ITS DIMENSION
C| PH             |<--| HYDROSTATIC PRESSURE
C| PRIV           |<->| PRIVATE FIELD NPRIV*NPOIN3
C| PRIVE          |---| 
C| RHO0           |---| 
C| TRA01          |<->| WORK FIELDS
C| TRA02          |---| 
C| Z             |-->| Z-COORDINATES OF NODES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN3, NPOIN2, NPLAN
      DOUBLE PRECISION, INTENT(INOUT) :: PH(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: DELTAR(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION,INTENT(INOUT)  :: TRA01(NPOIN3), TRA02(NPOIN3)
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: PRIVE
      DOUBLE PRECISION, INTENT(IN)    :: RHO0, GRAV
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,K1,K2
      DOUBLE PRECISION C
!
!----------------------------------------------------------------------
C COMPUTES USING THE DENSITY VARIABLE PART
!
C                            S
C PH(Z) = G * RHO0 * INTEGRAL  DELTAR DZ
C                            Z
!
      CALL OV('X=Y     ', TRA02, DELTAR, DELTAR, C, NPOIN3)
      K1 = (NPLAN-1) * NPOIN2 + 1
      CALL OV('X=C     ', TRA01(K1), TRA01, TRA01, 0.0D0, NPOIN2)
!
      DO I=NPLAN-1,1,-1
        K2 = K1-NPOIN2
        CALL OV('X=Y-Z   ', TRA01,      Z(K1),     Z(K2), C, NPOIN2)
        CALL OV('X=X+Y   ', TRA02(K1),  TRA02(K2), Z,     C, NPOIN2)
        CALL OV('X=XY    ', TRA01,      TRA02(K1), Z,     C, NPOIN2)
        CALL OV('X=Y+Z   ', TRA01(K2),  TRA01, TRA01(K1), C, NPOIN2)
        K1 = K2
      END DO
      CALL OV('X=CY    ', PH , TRA01, TRA01, 0.5D0*GRAV*RHO0, NPOIN3)
!
C COMPUTES THE CONSTANT DENSITY PART
C PH(Z) = PH(Z) + G * RHO0 * (S-Z)
!
      K1 = (NPLAN-1) * NPOIN2 + 1
      CALL OV('X=Y-Z   ', TRA01,  Z(K1), Z(1), C, NPOIN2)
!
      DO I=1,NPLAN
        K2 = (I-1)*NPOIN2+1
        CALL OV('X=Y-Z   ', TRA01, Z(K1), Z(K2),  C, NPOIN2)
        CALL OV('X=Y+CZ  ', PH(K2), PH(K2), TRA01, GRAV*RHO0, NPOIN2)
      END DO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE PHSTAT
C
C#######################################################################
C
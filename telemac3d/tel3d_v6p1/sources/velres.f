C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FINAL, SOLENOIDAL VELOCITY FIELD (UE, VE, WE)
!>                GIVEN THE DYNAMIC PRESSURE P AND THE INTERMEDIATE
!>                VELOCITY FIELD (UP, VP, WP).
!>  @code
!>                   DPART DP
!>   UE_I = UP_I +  -----------
!>                   DPART X_I
!>
!>   U,V,W ARE UP AT THE BEGINNING
!>   U,V,W ARE UE AT THE END
!>
!>
!>   I.E. REALISES THE FINAL VELOCITY PROJECTION STEP
!>   NOTE: PHYSICAL DYNAMIC PRESSURE IS DP  MULTIPLIED BY RHO0/DT
!>        (NEEDED FOR OUTPUTS)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DP, IELM3, IPBOT, MASKEL, MESH3D, MSK, NPLAN, NPOIN2, NPOIN3, OPTBAN, PX, PY, PZ, S, SIGMAG, U, UNSV3D, V, W
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FORMUL, I2D, I3D, I3DP, IP, NP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARCOM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D(), WAVE_EQUATION()

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
!> </td><td> 21/05/2010
!> </td><td> JM HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 12/98 - 04/99
!> </td><td> JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!> </td><td> NON-HYDROSTATIC VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DP
!></td><td>--></td><td>HYDRODYNAMIC PRESSURE, TIMESTEP N+1
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IPBOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>ELEMENT MASKING
!>    </td></tr>
!>          <tr><td>MESH3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>MASKING LOGICAL FLAG
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PX,PY,PZ
!></td><td><-></td><td>H-DYN. PRESSURE PARTIAL DERIVATIVES
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIGMAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U ,V ,W
!></td><td><--</td><td>FIRST INTERMEDIATE, THEN FINAL VELOCITY FIELD
!>    </td></tr>
!>          <tr><td>UNSV3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLC
!></td><td><-></td><td>INTEGRALS OF INTERP. FUNCTIONS (BUILT HERE)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VELRES
     &(U,V,W,DP,PX,PY,PZ,MSK,MASKEL,MESH3D,S,IELM3,NPLAN,OPTBAN,
     & UNSV3D,NPOIN3,NPOIN2,SIGMAG,IPBOT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DP             |-->| HYDRODYNAMIC PRESSURE, TIMESTEP N+1
C| IELM3          |---| 
C| IPBOT          |---| 
C| MASKEL         |-->| ELEMENT MASKING
C| MESH3D         |---| 
C| MSK            |-->| MASKING LOGICAL FLAG
C| NPLAN          |---| 
C| NPOIN2         |---| 
C| NPOIN3         |---| 
C| OPTBAN         |---| 
C| PX,PY,PZ       |<->| H-DYN. PRESSURE PARTIAL DERIVATIVES
C| S             |---| 
C| SIGMAG         |---| 
C| U ,V ,W        |<--| FIRST INTERMEDIATE, THEN FINAL VELOCITY FIELD
C| UNSV3D         |---| 
C| VOLC           |<->| INTEGRALS OF INTERP. FUNCTIONS (BUILT HERE)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: IELM3,NPLAN,OPTBAN,NPOIN3,NPOIN2
      TYPE(BIEF_OBJ),  INTENT(IN)    :: S,DP,UNSV3D
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: PX,PY,PZ
      DOUBLE PRECISION, INTENT(INOUT):: U(NPOIN3),V(NPOIN3),W(NPOIN3)
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH3D
      INTEGER, INTENT(IN)            :: IPBOT(NPOIN2)
      LOGICAL, INTENT(IN)            :: MSK,SIGMAG
!
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: MASKEL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IP,I2D,I3D,I3DP
      CHARACTER(LEN=15) FORMUL
!
!-----------------------------------------------------------------------
C DYNAMIC PRESSURE DERIVATIVES AT NODES
!-----------------------------------------------------------------------
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
C       HERE GRADIENTS WITH FILTERING OF CRUSHED ELEMENTS
        FORMUL='GRADF 2        '
      ELSE
C       STANDARD GRADIENTS
        FORMUL='GRADF          '
      ENDIF
!
      CALL VECTOR(PX,'=',FORMUL//'X',IELM3,1.D0,DP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(PY,'=',FORMUL//'Y',IELM3,1.D0,DP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(PZ,'=',FORMUL//'Z',IELM3,1.D0,DP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM (PX, 2, MESH3D)
        CALL PARCOM (PY, 2, MESH3D)
        CALL PARCOM (PZ, 2, MESH3D)
      ENDIF
!
!-----------------------------------------------------------------------
C FINAL VELOCITY ( PROJECTION )
!-----------------------------------------------------------------------
!
      DO I3D=1,NPOIN3
        U(I3D)=U(I3D)-PX%R(I3D)*UNSV3D%R(I3D)
        V(I3D)=V(I3D)-PY%R(I3D)*UNSV3D%R(I3D)
        W(I3D)=W(I3D)-PZ%R(I3D)*UNSV3D%R(I3D)
      ENDDO
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        DO I2D=1,NPOIN2
          IF(IPBOT(I2D).GT.0) THEN
            I3DP=I2D+IPBOT(I2D)*NPOIN2
C           VALUE OF THE FIRST FREE POINT IS COPIED BELOW
            DO IP=0,IPBOT(I2D)-1
              I3D=I2D+IP*NPOIN2
              U(I3D)=U(I3DP)
              V(I3D)=V(I3DP)
              W(I3D)=W(I3DP)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C

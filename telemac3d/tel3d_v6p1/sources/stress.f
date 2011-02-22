C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ADDS TO OR REMOVES FROM SEM3D THE EXPLICIT STRESS
!>               (DEPENDING ON OP).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BFBORF, BFBORL, BFBORS, IELM2H, IELM2V, IELM3, MASKBR, MASKEL, MESH2D, MESH3D, MSK, NPOIN2, NPOIN3, OP, SEM3D, SV, T2, T3
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OSDB(), OV(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DIFF3D(), SEMIMP(), WAVE_EQUATION()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 20/07/05
!> </td><td> J.M. HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BFBORF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BFBORL,F,S
!></td><td>--></td><td>EXPLICIT STRESS
!>    </td></tr>
!>          <tr><td>BFBORS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM2H
!></td><td>--></td><td>DISCRETISATION TYPE FOR 2D HORIZONTAL MESH
!>    </td></tr>
!>          <tr><td>IELM2V
!></td><td>--></td><td>DISCRETISATION TYPE FOR 2D VERTICAL MESH
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE OF ELEMENT IN 3D
!>    </td></tr>
!>          <tr><td>MASKBR
!></td><td>--></td><td>MASK PER BOUNDARY ELEMENT
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASK PER ELEMENT
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>--></td><td>2D MESH
!>    </td></tr>
!>          <tr><td>MESH3D
!></td><td>--></td><td>3D MESH
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS IN 3D
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>8 CHARACTERS STRING : 'X=X+Y   ' OR 'X=X-Y   '
!>    </td></tr>
!>          <tr><td>SEM3D
!></td><td><-></td><td>RIGHT HAND SIDE OF EQUATION
!>    </td></tr>
!>          <tr><td>SV
!></td><td>--></td><td>VOID STRUCTURE
!>    </td></tr>
!>          <tr><td>T2,T3
!></td><td>--></td><td>2D AND 3D WORK BIEF_OBJ STRUCTURES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE STRESS
     & (SEM3D,OP,T2,T3,BFBORL,BFBORF,BFBORS,NPOIN2,NPOIN3,MESH2D,
     &  MESH3D,IELM3,IELM2H,IELM2V,SV,MSK,MASKBR,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BFBORF         |---| 
C| BFBORL,F,S     |-->| EXPLICIT STRESS
C| BFBORS         |---| 
C| IELM2H         |-->| DISCRETISATION TYPE FOR 2D HORIZONTAL MESH
C| IELM2V         |-->| DISCRETISATION TYPE FOR 2D VERTICAL MESH
C| IELM3          |-->| TYPE OF ELEMENT IN 3D
C| MASKBR         |-->| MASK PER BOUNDARY ELEMENT
C| MASKEL         |-->| MASK PER ELEMENT
C| MESH2D         |-->| 2D MESH
C| MESH3D         |-->| 3D MESH
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| NPOIN3         |-->| NUMBER OF POINTS IN 3D
C| OP             |-->| 8 CHARACTERS STRING : 'X=X+Y   ' OR 'X=X-Y   '
C| SEM3D          |<->| RIGHT HAND SIDE OF EQUATION
C| SV             |-->| VOID STRUCTURE
C| T2,T3          |-->| 2D AND 3D WORK BIEF_OBJ STRUCTURES
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
      TYPE(BIEF_OBJ), INTENT(IN)      :: BFBORL,BFBORF,BFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKBR,MASKEL,SV
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3,T2
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D,MESH3D
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3
      INTEGER, INTENT(IN)             :: IELM2H,IELM2V,IELM3
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SEM3D
      LOGICAL, INTENT(IN)             :: MSK
      CHARACTER(LEN=8), INTENT(IN)    :: OP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     LATERAL BOUNDARIES
!
      IF(BFBORL%TYPR.NE.'0') THEN
        CALL VECTOR(T3,'=','MASVEC          ',IELM2V,1.D0,BFBORL,
     &              SV,SV,SV,SV,SV,MESH3D,MSK,MASKBR)
        CALL OSDB(OP,SEM3D,T3,T3,1.D0,MESH3D)
      ENDIF
!
C     BOTTOM (IPBOT NOT TREATED HERE)
!
      IF(BFBORF%TYPR.NE.'0') THEN
        CALL VECTOR(T2,'=','MASVEC          ',IELM2H,1.D0,BFBORF,
     &              SV,SV,SV,SV,SV,MESH2D,MSK,MASKEL)
        CALL OV(OP,SEM3D%R(1:NPOIN2),T2%R,T2%R,0.D0,NPOIN2)
      ENDIF
!
C     FREE SURFACE
!
      IF(BFBORS%TYPR.NE.'0') THEN
        CALL VECTOR(T2,'=','MASVEC          ',IELM2H,1.D0,BFBORS,
     &              SV,SV,SV,SV,SV,MESH2D,MSK,MASKEL)
        CALL OV(OP,SEM3D%R(NPOIN3-NPOIN2+1:NPOIN3),
     &          T2%R,T2%R,0.D0,NPOIN2)
      ENDIF
!
!=======================================================================
!
      RETURN
      END
C
C#######################################################################
C
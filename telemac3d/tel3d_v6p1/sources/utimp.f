C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRINTS OUT SPECIFIC RESULTS
!>               (SEE THE LIST OF VARIABLES IN DECLARATIONS_TELEMAC3D).
!><br>            FOR BIEF_OBJ STRUCTURES, THE DOUBLE PRECISION ARRAY
!>                IS IN COMPONENT R, E.G. U%R FOR THE VELOCITY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> GRADEBL, GRAPRDL, LISDEBL, LISPRDL, LT, TIME
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), TELEMAC3D()

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
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> 25/11/97
!> </td><td> C LE NORMANT(LNH) 30 87 83 53; F LEPEINTRE (LNH) 30 87 78 54
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
!>          <tr><td>DT
!></td><td>--></td><td>TIME-STEP
!>    </td></tr>
!>          <tr><td>GRADEBL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAPRDL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>WATER DEPTH
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>--></td><td>CONNECTIVITY TABLE
!>    </td></tr>
!>          <tr><td>LISDEBL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LISPRDL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>ITERATION NUMBER
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NUMBER OF ELEMENTS IN THE 2D MESH
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NUMBER OF ELEMENTS IN THE 3D MESH
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>--></td><td>NUMBER OF TIME-STEPS
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES ON THE VERTICAL
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN THE 2D MESH
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS IN THE 3D MESH
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>NUMBER OF ARRAYS IN THE BLOCK
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NUMBER OF BOUNDARY POINTS IN 2D
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NUMBER OF TRACERS
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>BLOCK OF ARRAYS FOR THE USER
!>    </td></tr>
!>          <tr><td>RI
!></td><td>--></td><td>RICHARDSON NUMBER
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>BLOCK OF TRACERS, TRACER 2: TA%ADR(2)%P
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>3 COMPONENTS OF VELOCITY
!>    </td></tr>
!>          <tr><td>UMOY,VMOY
!></td><td>--></td><td>COMPONENTS OF DEPTH AVERAGED VELOCITY
!>    </td></tr>
!>          <tr><td>VISCTA
!></td><td>--></td><td>DIFFUSIVITY FOR TRACERS
!>    </td></tr>
!>          <tr><td>VISCVI
!></td><td>--></td><td>VISCOSITY
!>    </td></tr>
!>          <tr><td>WC
!></td><td>--></td><td>SETTLING VELOCITY
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDINATES OF MESH
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BOTTOM ELEVATION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE UTIMP
     & (LT,TIME,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME
C| DT             |-->| TIME-STEP
C| GRADEBL        |---| 
C| GRAPRDL        |---| 
C| HN             |-->| WATER DEPTH
C| IKLE3          |-->| CONNECTIVITY TABLE
C| LISDEBL        |---| 
C| LISPRDL        |---| 
C| LT             |-->| ITERATION NUMBER
C| NELEM2         |-->| NUMBER OF ELEMENTS IN THE 2D MESH
C| NELEM3         |-->| NUMBER OF ELEMENTS IN THE 3D MESH
C| NIT            |-->| NUMBER OF TIME-STEPS
C| NPLAN          |-->| NUMBER OF PLANES ON THE VERTICAL
C| NPOIN2         |-->| NUMBER OF POINTS IN THE 2D MESH
C| NPOIN3         |-->| NUMBER OF POINTS IN THE 3D MESH
C| NPRIV          |-->| NUMBER OF ARRAYS IN THE BLOCK
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
C| NTRAC          |-->| NUMBER OF TRACERS
C| PRIVE          |-->| BLOCK OF ARRAYS FOR THE USER
C| RI             |-->| RICHARDSON NUMBER
C| TA             |-->| BLOCK OF TRACERS, TRACER 2: TA%ADR(2)%P
C| TIME           |---| 
C| U,V,W          |-->| 3 COMPONENTS OF VELOCITY
C| UMOY,VMOY      |-->| COMPONENTS OF DEPTH AVERAGED VELOCITY
C| VISCTA         |-->| DIFFUSIVITY FOR TRACERS
C| VISCVI         |-->| VISCOSITY
C| WC             |-->| SETTLING VELOCITY
C| X,Y,Z          |-->| COORDINATES OF MESH
C| ZF             |-->| BOTTOM ELEVATION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER, INTENT(IN) :: LT,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTRINSIC MOD
!
!***********************************************************************
C USER OUTPUT ACCORDING TO THE VALUES: TO BE IMPLEMENTED
!
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP
C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES THE MESH, DESCRIBED BY THE BIEF_MESH STRUCTURE
!>                INTO THE FILE. BIEF_MESH STRUCTURE CONTAINS INFORMATIONS
!>                ABOUT CONNECTIVITY, COORDINATES, BOUNDARY NODES. OTHER
!>                INFORMATIONS NEEDED : THE DATE AND TIME INFORMATION, AND
!>                THE ORIGIN OF THE COORDINATE SYSTEM (X_ORIG,Y_ORIG).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, M_MED
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DATE, FFORMAT, I_ORIG, J_ORIG, MESH, NFILE, NPLAN, TIME
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), WRITE_MESH_MED(), WRITE_MESH_SERAFIN()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), ECRSPE(), SISYPHE(), TELEMAC2D(), TELEMAC3D(), WAC()

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
!> </td><td> 25/11/08
!> </td><td> R NEBAUER (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DATE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FFORMAT
!></td><td>--></td><td>FILE FORMAT
!>    </td></tr>
!>          <tr><td>I_ORIG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>J_ORIG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MESH STRUCTURE
!>    </td></tr>
!>          <tr><td>NCSIZE
!></td><td>--></td><td>NUMBER OF PROCESSORS
!>    </td></tr>
!>          <tr><td>NFILE
!></td><td>--></td><td>LOGICAL UNIT OF FILE
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES (3D)
!>    </td></tr>
!>          <tr><td>NPTIR
!></td><td>--></td><td>NUMBER OF INTERFACE POINTS
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WRITE_MESH
     &(FFORMAT,NFILE,MESH,NPLAN,DATE,TIME,I_ORIG,J_ORIG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DATE           |---| 
C| FFORMAT        |-->| FILE FORMAT
C| I_ORIG         |---| 
C| J_ORIG         |---| 
C| MESH           |-->| MESH STRUCTURE
C| NCSIZE         |-->| NUMBER OF PROCESSORS
C| NFILE          |-->| LOGICAL UNIT OF FILE
C| NPLAN          |-->| NUMBER OF PLANES (3D)
C| NPTIR          |-->| NUMBER OF INTERFACE POINTS
C| TIME           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE M_MED
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=8) ,     INTENT(IN) :: FFORMAT
      INTEGER          ,     INTENT(IN) :: NFILE,NPLAN
      TYPE(BIEF_MESH),       INTENT(IN) :: MESH
      INTEGER, DIMENSION(3), INTENT(IN) :: DATE
      INTEGER, DIMENSION(3), INTENT(IN) :: TIME
      INTEGER,               INTENT(IN) :: I_ORIG,J_ORIG
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C

!***********************************************************************
C     IF(DEBUG) CALL PROC_BEGIN('WRITE_MESH')
!***********************************************************************

      SELECT CASE (FFORMAT)
        CASE ('SERAFIN','SERAFIND')
           CALL WRITE_MESH_SERAFIN(NFILE,
     &                             MESH,
     &                             NPLAN,
     &                             DATE,
     &                             TIME,
     &                             I_ORIG,J_ORIG,
     &                             FFORMAT)
        CASE ('MED     ')
           CALL WRITE_MESH_MED(NFILE,MESH,DBLE(I_ORIG),DBLE(J_ORIG))
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'WRITE_MESH : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'WRITE_MESH: BAD FILE FORMAT : ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT

!***********************************************************************
C     IF(DEBUG) CALL PROC_END('WRITE_MESH')
!***********************************************************************

      RETURN
      END
C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES DATA VALUES ON A MESH INTO THE DATA FILE OF THE
!>                GIVEN FILE FORMAT.
!><br>            DATA VALUES ARE STORED IN A BIEF_OBJ BLOCK (BVARSOR),
!>                AND THE LOGICAL OUTVAR INDICATES FOR EACH VARIABLE IF
!>                WE SHOULD PRINT IT OUT OR NOT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, M_MED
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BVARSOR, FFORMAT, FILERES, N, NOMVAR, NVARS, OUTVAR, TIME, TIMESTEP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_WRITE_DATA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), WRITE_DATA_MED(), WRITE_DATA_SERAFIN()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BIEF_DESIMP(), ECRSPE()

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
!>          <tr><td>BVARSOR
!></td><td>--></td><td>BIEF BLOCK CONTAINING THE VARIABLES VARIABLES
!>    </td></tr>
!>          <tr><td>FFORMAT
!></td><td>--></td><td>FILE FORMAT
!>    </td></tr>
!>          <tr><td>FILERES
!></td><td>--></td><td>LOGICAL UNIT OF FILE
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>NUMBER OF VALUES (MAY BE DIFFERENT FROM
!>                  THE NUMBER OF DEGREES OF FREEDOM, E.G. FOR
!>                  QUADRATIC ELEMENTS ONLY THE LINEAR VALUES
!>                  ARE EXITED)
!>    </td></tr>
!>          <tr><td>NOMVAR
!></td><td>--></td><td>NAME OF VARIABLES
!>    </td></tr>
!>          <tr><td>NVARS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OUTVAR
!></td><td>--></td><td>VARIABLES TO BE PUT IN THE FILE
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TIMESTEP
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WRITE_DATA
     &(FFORMAT,FILERES,NVARS,TIME,TIMESTEP,OUTVAR,NOMVAR,BVARSOR,N)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BVARSOR        |-->| BIEF BLOCK CONTAINING THE VARIABLES VARIABLES
C| FFORMAT        |-->| FILE FORMAT
C| FILERES        |-->| LOGICAL UNIT OF FILE
C| N             |-->| NUMBER OF VALUES (MAY BE DIFFERENT FROM
C|                |   | THE NUMBER OF DEGREES OF FREEDOM, E.G. FOR
C|                |   | QUADRATIC ELEMENTS ONLY THE LINEAR VALUES
C|                |   | ARE EXITED)
C| NOMVAR         |-->| NAME OF VARIABLES
C| NVARS          |---| 
C| OUTVAR         |-->| VARIABLES TO BE PUT IN THE FILE
C| TIME           |---| 
C| TIMESTEP       |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE M_MED
      USE BIEF, EX_WRITE_DATA => WRITE_DATA
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=8), INTENT(IN)          :: FFORMAT
      INTEGER,          INTENT(IN)          :: FILERES,N
      INTEGER,          INTENT(IN)          :: NVARS
      DOUBLE PRECISION, INTENT(IN)          :: TIME
      INTEGER,          INTENT(IN)          :: TIMESTEP
      CHARACTER(LEN=32),DIMENSION(NVARS), INTENT(IN) :: NOMVAR
      LOGICAL, DIMENSION(NVARS), INTENT(IN) :: OUTVAR
      TYPE(BIEF_OBJ),            INTENT(IN) :: BVARSOR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C

!***********************************************************************
C     IF(DEBUG) CALL PROC_BEGIN('WRITE_DATA')
!***********************************************************************

      SELECT CASE (FFORMAT)
        CASE ('SERAFIN ','SERAFIND')
          CALL WRITE_DATA_SERAFIN(FILERES,NVARS,TIME,TIMESTEP,
     &                            OUTVAR,BVARSOR,FFORMAT,N)

        CASE ('MED     ')
          CALL WRITE_DATA_MED(FILERES,NVARS,TIME,TIMESTEP,
     &                        NOMVAR,OUTVAR,BVARSOR)

        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'WRITE_DATA : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'WRITE_DATA: BAD FILE FORMAT : ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT

!***********************************************************************
C     IF(DEBUG) CALL PROC_END('WRITE_DATA')
!***********************************************************************

      RETURN
      END
C
C#######################################################################
C
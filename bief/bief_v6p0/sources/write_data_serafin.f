C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES RECORDS OF RESULTS IN A SERAFIN FORMAT FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BVARSOR, FFORMAT, N, NFIC, NVARS, OUTVAR, TIME, TIMESTEP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CBID, IBID, ISTAT, K, RF, TTIME
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ECRI2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WRITE_DATA()

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
!> </td><td> 01/04/2009
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
!></td><td>--></td><td>BIEF_OBJ BLOCK WITH DATA VALUES
!>    </td></tr>
!>          <tr><td>FFORMAT
!></td><td>--></td><td>FILE FORMAT
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>NUMBER OF VALUES (MAY BE DIFFERENT FROM
!>                  THE NUMBER OF DEGREES OF FREEDOM, E.G. FOR
!>                  QUADRATIC ELEMENTS ONLY THE LINEAR VALUES
!>                  ARE EXITED)
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>LOGICAL UNIT OF FILE
!>    </td></tr>
!>          <tr><td>NVARS
!></td><td>--></td><td>NUMBER OF VARIABLES
!>    </td></tr>
!>          <tr><td>OUTVAR
!></td><td>--></td><td>INDICATES FOR EACH VARIABLE IF WE SHOULD
!>                  PRINT IT OUT OR NOT
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>--></td><td>LOGICAL UNIT OF FILE
!>    </td></tr>
!>          <tr><td>TIMESTEP
!></td><td>--></td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WRITE_DATA_SERAFIN
     &(NFIC,NVARS,TIME,TIMESTEP,OUTVAR,BVARSOR,FFORMAT,N)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BVARSOR        |-->| BIEF_OBJ BLOCK WITH DATA VALUES
C| FFORMAT        |-->| FILE FORMAT
C| N             |-->| NUMBER OF VALUES (MAY BE DIFFERENT FROM
C|                |   | THE NUMBER OF DEGREES OF FREEDOM, E.G. FOR
C|                |   | QUADRATIC ELEMENTS ONLY THE LINEAR VALUES
C|                |   | ARE EXITED)
C| NFIC           |-->| LOGICAL UNIT OF FILE
C| NVARS          |-->| NUMBER OF VARIABLES
C| OUTVAR         |-->| INDICATES FOR EACH VARIABLE IF WE SHOULD
C|                |   | PRINT IT OUT OR NOT
C| TIME           |-->| LOGICAL UNIT OF FILE
C| TIMESTEP       |-->| 
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
      INTEGER        ,  INTENT(IN)          :: NFIC,NVARS,N,TIMESTEP
      DOUBLE PRECISION, INTENT(IN)          :: TIME
      LOGICAL, DIMENSION(NVARS), INTENT(IN) :: OUTVAR
      TYPE(BIEF_OBJ),            INTENT(IN) :: BVARSOR
      CHARACTER(LEN=8), INTENT(IN)          :: FFORMAT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=2)               :: RF
      DOUBLE PRECISION, DIMENSION(1) :: TTIME
      INTEGER                        :: K,ISTAT
      INTEGER                        :: IBID(1)
      CHARACTER*2                    :: CBID
!
!***********************************************************************
C     IF(DEBUG) CALL PROC_BEGIN('WRITE_DATA_SERAFIN')
!***********************************************************************
!
      IF(FFORMAT.EQ.'SERAFIND') THEN
        RF = 'R8'
      ELSE
        RF = 'R4'
      ENDIF
!
      TTIME(1) = TIME
!
      CALL ECRI2(TTIME,IBID,CBID,1,RF,NFIC,'STD',ISTAT)
!
      DO K=1,NVARS
        IF(OUTVAR(K)) THEN
          ! HOPING THAT IT WILL WORK ...
          ! GIVEN THAT N IS NOT AN ARGUMENT ...
          ! N = BVARSOR%ADR(K)%P%DIM1
C  CORRECTION JMH 21/04/2009 NO, N IS GIVEN AND MAY BE DIFFERENT
C  FROM BVARSOR%ADR(K)%P%DIM1 (QUASI-BUBBLE AND QUADRATIC ELEMENTS)
          IF(ASSOCIATED(BVARSOR%ADR(K)%P%R)) THEN
            CALL ECRI2(BVARSOR%ADR(K)%P%R,IBID,CBID,N,RF,NFIC,'STD',
     &                 ISTAT)
          ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'WRITE_DATA_SERAFIN : VARIABLE NO : ',K
              WRITE(LU,*) '        PAS OU MAL ALLOUEE'
              WRITE(LU,*) '        OU POINTEUR NON ASSOCIE'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'WRITE_DATA_SERAFIN: VARIABLE NO: ',K
              WRITE(LU,*) '        NOT OR NOT WELL ALLOCATED'
              WRITE(LU,*) '        OR POINTER NOT ASSOCIATED '
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!
!***********************************************************************
C     IF(DEBUG) CALL PROC_END('WRITE_DATA_SERAFIN')
!***********************************************************************
!
      RETURN
      END
C
C#######################################################################
C
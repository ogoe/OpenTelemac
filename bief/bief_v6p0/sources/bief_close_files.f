C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, M_MED
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CODE, FILES, NFILES, PEXIT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BIEF_CLOSE_FILES
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CLOSE_FILE_MED(), P_EXIT()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ARTEMIS(), HOMERE_SISYPHE(), HOMERE_TELEMAC2D(), HOMERE_TELEMAC3D(), HOMERE_TOMAWAC()

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
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CODE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FILES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NFILES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PEXIT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BIEF_CLOSE_FILES
     &(CODE,FILES,NFILES,PEXIT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CODE           |---| 
C| FILES          |---| 
C| NFILES         |---| 
C| PEXIT          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_BIEF_CLOSE_FILES => BIEF_CLOSE_FILES
C
      USE DECLARATIONS_TELEMAC
      USE M_MED
C
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          , INTENT(IN)     :: NFILES
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      LOGICAL, INTENT(IN)               :: PEXIT
      TYPE(BIEF_FILE)   , INTENT(INOUT) :: FILES(NFILES)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
C-----------------------------------------------------------------------
C
      DO I=1,NFILES
C
        IF(FILES(I)%NAME(1:1).NE.' ') THEN
C
C         CLOSES THE FILE
C
          IF(FILES(I)%FMT.EQ.'MED     ') THEN
            CALL CLOSE_FILE_MED(FILES(I)%LU)
          ELSE
            CLOSE(FILES(I)%LU)
          ENDIF
C
        ENDIF
C
      ENDDO
C
C-----------------------------------------------------------------------
C
C     PARALLEL MODE: STOPS IF PEXIT
C
      IF(NCSIZE.GT.0.AND.PEXIT) CALL P_EXIT
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
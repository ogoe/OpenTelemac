C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE NEW SET OF FRICTION COEFFICIENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DESC, ESTIME, KFROT, NPARAM, RO, RSTART, SETSTR, SETSTR2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 22/10/2001
!> </td><td> J-M HERVOUET TEL: 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 02/10/2000
!> </td><td> A. LEOPARDI (UNINA)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 2.2                                       </center>
!> </td><td> 22/03/1994
!> </td><td> E. BARROS
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DESC
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>ESTIME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPARAM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NZONE
!></td><td>--></td><td>NUMBER OF ZONES
!>    </td></tr>
!>          <tr><td>RO
!></td><td>--></td><td>SETSTR=SETSTR2+RO*DESC
!>    </td></tr>
!>          <tr><td>RSTART
!></td><td>--></td><td>LOGICAL, RESTART COMPUTATION BECAUSE OUT OF LIMITS
!>    </td></tr>
!>          <tr><td>SETSTR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SETSTR2
!></td><td>--></td><td>OLD SET
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE NEWSTR
     &(SETSTR,SETSTR2,DESC,RO,RSTART,NPARAM,ESTIME,KFROT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DESC           |-->| 
C| ESTIME         |---| 
C| KFROT          |---| 
C| NPARAM         |---| 
C| NZONE          |-->| NUMBER OF ZONES
C| RO             |-->| SETSTR=SETSTR2+RO*DESC
C| RSTART         |-->| LOGICAL, RESTART COMPUTATION BECAUSE OUT OF LIMITS
C| SETSTR         |---| 
C| SETSTR2        |-->| OLD SET
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION , INTENT(IN)    :: RO
      TYPE (BIEF_OBJ)  , INTENT(IN)    :: DESC
      TYPE (BIEF_OBJ)  , INTENT(IN)    :: SETSTR2
      TYPE (BIEF_OBJ)  , INTENT(INOUT) :: SETSTR
      LOGICAL          , INTENT(INOUT) :: RSTART
      INTEGER          , INTENT(IN)    :: NPARAM,KFROT
      CHARACTER(LEN=72), INTENT(IN)    :: ESTIME
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
C---------------------------------------------------------------------
C
      CALL OV( 'X=Y+CZ  ',SETSTR%R,SETSTR2%R,DESC%R,RO,NPARAM)
C
C     TESTS LIMITS
C     LIMITS (1,100)
C
      RSTART=.FALSE.
      DO I=1,NPARAM
         IF (SETSTR%R(I).LT.1.D0) THEN
           SETSTR%R(I)=1.D0
           RSTART=.TRUE.
         ENDIF
         IF (SETSTR%R(I).GT.100.D0) THEN
           SETSTR%R(I)=100.D0
           RSTART=.TRUE.
         ENDIF
      ENDDO
C
C---------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
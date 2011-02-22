C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CONSTFLOW
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::CRIT_CFD CRIT_CFD@endlink, 
!> @link DECLARATIONS_SISYPHE::E E@endlink, 
!> @link DECLARATIONS_SISYPHE::ECPL ECPL@endlink, 
!> @link DECLARATIONS_SISYPHE::HCPL HCPL@endlink, 
!> @link DECLARATIONS_SISYPHE::HN HN@endlink, 
!> @link DECLARATIONS_SISYPHE::MESH MESH@endlink, 
!> @link DECLARATIONS_SISYPHE::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_SISYPHE::S S@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, NZFMAX, P_ISUM, ZFMAX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!>      <td><center> 5.5                                       </center>
!> </td><td>
!> </td><td> B. MINH DUC; F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> **/02/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/08/2003
!> </td><td> BUI MINH DUC
!> </td><td> DEVELOPED THE SUBROUTINE
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CONSTFLOW
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                 SUBROUTINE CONDIS_SISYPHE
     &(CONSTFLOW)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CONSTFLOW      |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      !
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL, INTENT(INOUT) :: CONSTFLOW
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, EXTERNAL      :: P_ISUM
!
C3/ LOCAL VARIABLES
!--------------------
!
      INTEGER          :: NZFMAX, I
      DOUBLE PRECISION :: ZFMAX, C
!
!=======================================================================!
!=======================================================================!
C                               PROGRAM                                 !
!=======================================================================!
!=======================================================================!
!
      NZFMAX = 0
!
      IF(CONSTFLOW) THEN
         CALL OS('X=X+Y   ', ECPL, E, S, C)
!
         DO I=1,NPOIN
            ZFMAX = ABS(ECPL%R(I)) - CRIT_CFD*HCPL%R(I)
            IF (ZFMAX.GT.1.D-8) NZFMAX=NZFMAX+1
         ENDDO
!
         IF (NCSIZE.GT.1) THEN
            NZFMAX=P_ISUM(NZFMAX)
            CALL PARCOM(ECPL,2,MESH)
         ENDIF
!
         IF (NZFMAX.GE.1) CONSTFLOW = .FALSE.
      ENDIF
!
      IF(.NOT.CONSTFLOW) THEN
         CALL OS('X=C     ', ECPL,  S, S, 0.D0)
         CALL OS('X=Y     ', HCPL, HN, S,    C)
!
         IF (NCSIZE.GT.1) THEN
            CALL PARCOM(ECPL,2,MESH)
            CALL PARCOM(HCPL,2,MESH)
         ENDIF
      ENDIF
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
C
C#######################################################################
C
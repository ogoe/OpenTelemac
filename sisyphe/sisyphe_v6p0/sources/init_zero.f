C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES VARIABLES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::COEFPN COEFPN@endlink, 
!> @link DECLARATIONS_SISYPHE::CS CS@endlink, 
!> @link DECLARATIONS_SISYPHE::E E@endlink, 
!> @link DECLARATIONS_SISYPHE::ESOMT ESOMT@endlink, 
!> @link DECLARATIONS_SISYPHE::FW FW@endlink, 
!> @link DECLARATIONS_SISYPHE::HN HN@endlink, 
!> @link DECLARATIONS_SISYPHE::HW HW@endlink, 
!> @link DECLARATIONS_SISYPHE::MASDEP MASDEP@endlink, 
!> @link DECLARATIONS_SISYPHE::NSICLA NSICLA@endlink, 
!> @link DECLARATIONS_SISYPHE::PRIVE PRIVE@endlink, 
!> @link DECLARATIONS_SISYPHE::QS QS@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCL QSCL@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLXC QSCLXC@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLXS QSCLXS@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLYC QSCLYC@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLYS QSCLYS@endlink, 
!> @link DECLARATIONS_SISYPHE::QSX QSX@endlink, 
!> @link DECLARATIONS_SISYPHE::QSXC QSXC@endlink, 
!> @link DECLARATIONS_SISYPHE::QSXS QSXS@endlink, 
!> @link DECLARATIONS_SISYPHE::QSY QSY@endlink, 
!> @link DECLARATIONS_SISYPHE::QSYC QSYC@endlink, 
!> @link DECLARATIONS_SISYPHE::QSYS QSYS@endlink, 
!> @link DECLARATIONS_SISYPHE::QS_C QS_C@endlink, 
!> @link DECLARATIONS_SISYPHE::QS_S QS_S@endlink, 
!> @link DECLARATIONS_SISYPHE::QU QU@endlink, 
!> @link DECLARATIONS_SISYPHE::QV QV@endlink, 
!> @link DECLARATIONS_SISYPHE::T12 T12@endlink, 
!> @link DECLARATIONS_SISYPHE::THETAW THETAW@endlink, 
!> @link DECLARATIONS_SISYPHE::TOB TOB@endlink, 
!> @link DECLARATIONS_SISYPHE::TOBW TOBW@endlink, 
!> @link DECLARATIONS_SISYPHE::TW TW@endlink, 
!> @link DECLARATIONS_SISYPHE::U2D U2D@endlink, 
!> @link DECLARATIONS_SISYPHE::UW UW@endlink, 
!> @link DECLARATIONS_SISYPHE::V2D V2D@endlink, 
!> @link DECLARATIONS_SISYPHE::ZFCL_C ZFCL_C@endlink
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
!>    </th><td> OS(), Q()
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
!>      <td><center> 5.9                                       </center>
!> </td><td>
!> </td><td> C. VILLARET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 04/06/2008
!> </td><td> JMH
!> </td><td> INITIALISATION OF MASDEP
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                         SUBROUTINE INIT_ZERO
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_SISYPHE
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
      INTEGER I
C
C-----------------------------------------------------------------------
C
C========================================================================
C                         INITIALISES
C =======================================================================
C
C---- THE WORKING ARRAYS
C
      IF(NPRIV > 0) CALL OS ('X=0     ', X=PRIVE)
      CALL OS('X=0     ', X=T12   )
      CALL OS('X=0     ', X=COEFPN)
C
C---- THE SEDIMENT VARIABLES :
C
      CALL OS('X=0     ', X=QS)
      CALL OS('X=0     ', X=QSX)
      CALL OS('X=0     ', X=QSY)
      CALL OS('X=0     ', X=QSCLXC )
      CALL OS('X=0     ', X=QSCLYC )
      CALL OS('X=0     ', X=QSCLXS )
      CALL OS('X=0     ', X=QSCLYS )
C
C 7 FOLLOWING LINES ADDED BY JMH 22/04/2005
C PROVISIONAL INITIALISATION FOR FIRST OUTPUT IN RESULTS FILE
C
      CALL OS('X=0     ', X=QSCL )  ! BLOCK OF SIZE NSICLA
      CALL OS('X=0     ', X=QS_S )
      CALL OS('X=0     ', X=QSXS )
      CALL OS('X=0     ', X=QSYS )
      CALL OS('X=0     ', X=QS_C )
      CALL OS('X=0     ', X=QSXC )
      CALL OS('X=0     ', X=QSYC )
C
C FOLLOWING LINE ADDED BY JMH 04/05/2005
C PROBABLY USEFUL ONLY IF(CHARR) AND WITH FINITE ELEMENTS
C
      CALL OS('X=0     ', X=ZFCL_C )
C
C---- THE DEPOSITION MASSES FOR EVERY CLASS IN SUSPENSION
C
      DO I=1,NSICLA
        MASDEP(I)=0.D0
      ENDDO
C
      CALL OS('X=0     ', X=E    )
      CALL OS('X=0     ', X=ESOMT)
      CALL OS('X=0     ', X=CS   )
C
C---- THE HYDRODYNAMIC VARIABLES :
C
      CALL OS('X=0     ', X=QU )
      CALL OS('X=0     ', X=QV )
      CALL OS('X=0     ', X=U2D )
      CALL OS('X=0     ', X=V2D )
      CALL OS('X=0     ', X=HN )
      CALL OS('X=0     ', X=Q  )
      CALL OS('X=0     ', X=TOB)
C
C---- THE WAVE PARAMETERS IF NEED BE
C
C     ALL INITIALISATIONS OF THE WAVES ARE TO BE REMOVED
C     WHEN ALL CHECKS WILL BE DONE
C     SEE BEDLOAD_BAILARD, DIBWAT, BIJKER AND SOULSBY
C
C
C     FW=0.3 CORRESPONDS TO NO WAVES, 0 WOULD DO A LOG(0)
      CALL OS('X=C     ', X=FW ,C=0.3D0   )   !
      CALL OS('X=0     ', X=HW    )   !
      CALL OS('X=0     ', X=TW    )   !
      CALL OS('X=C     ', X=THETAW, C=90.D0)  !
      CALL OS('X=0     ', X=UW    )   !
      CALL OS('X=0     ', X=TOBW)     !
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
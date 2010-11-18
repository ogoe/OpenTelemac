C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SISYPHE DEBUGGER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ILOOP, NAME, NLOOP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> LONGUEUR, SIZE
!>   </td></tr>
!>     </table>

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
!> </td><td> 22/11/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ILOOP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NAME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NLOOP
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE DEBUG_SISYPHE ! (_IMP_)
     & (NAME, ILOOP, NLOOP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ILOOP          |---| 
C| NAME           |---| 
C| NLOOP          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      INTEGER, PARAMETER :: SIZE =100
      CHARACTER(LEN=SIZE)  , INTENT(IN)           :: NAME
      INTEGER,   INTENT(IN), OPTIONAL :: ILOOP, NLOOP
!
      INTEGER :: LONGUEUR
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      LONGUEUR = LEN(NAME)
!
      WRITE(LU,*) NAME
      IF (PRESENT(ILOOP).AND.PRESENT(NLOOP)) THEN
         WRITE(LU,*) ILOOP, NLOOP
      ENDIF
C     JMH 03/02/05
C     MAY BE ADDED AT BAW BUT THIS IS NOT STANDARD FORTRAN
C     CALL FLUSH(LU)
!
100   FORMAT('LOOP : ',I8,'/',I8)
!
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE DEBUG_SISYPHE
C
C#######################################################################
C
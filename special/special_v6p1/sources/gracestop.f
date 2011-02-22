!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Internal(s)
!>    </th><td> BREAKER, ISIGNAL, SIGNAL, SIGUSR1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Unknown(s)
!>    </th><td> BREAKER
!>   </td></tr>
!>     </table>

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
      MODULE GRACESTOP
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE

      LOGICAL :: BREAKER = .FALSE.

CSGI      INTEGER, PARAMETER :: SIGUSR1=16
CLINUX    INTEGER, PARAMETER :: SIGUSR1=10
CIBM      INTEGER, PARAMETER :: SIGUSR1=30

      INTEGER, PARAMETER :: SIGUSR1=16

      CONTAINS

      SUBROUTINE HANDLER
        IMPLICIT NONE
C!!        WRITE(*,*) 'SIGNAL ',SIGUSR1,' CAUGHT'
        BREAKER = .TRUE.
      END SUBROUTINE HANDLER

      SUBROUTINE GRACE
        IMPLICIT NONE
        WRITE(*,*) 'I STOP GRACEFULLY'
        STOP
      END SUBROUTINE GRACE

      SUBROUTINE TRAPSIG
      IMPLICIT NONE
      INTEGER ISIGNAL, SIGNAL

CSGI        ISIGNAL = SIGNAL(SIGUSR1, HANDLER, -1)
CLINUX      ISIGNAL = SIGNAL(SIGUSR1, HANDLER)
CIBM        CALL SIGNAL(SIGUSR1, HANDLER)
!
C  CHOICE HERE : NOTHING
C       ISIGNAL = SIGNAL(SIGUSR1, HANDLER, -1)
!
      END SUBROUTINE TRAPSIG

      END MODULE GRACESTOP

C
C#######################################################################
C
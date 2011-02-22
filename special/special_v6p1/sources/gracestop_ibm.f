
      module GRACESTOP

      logical :: breaker = .false.

!sgi      integer, parameter :: sigusr1=16
!linux    integer, parameter :: sigusr1=10
!ibm      integer, parameter :: sigusr1=30

      integer, parameter :: sigusr1=30

      contains

      subroutine HANDLER
!!!        write(*,*) 'signal ',sigusr1,' caught'
        breaker = .true.
      end subroutine HANDLER

      subroutine GRACE
        write(*,*) 'I stop gracefully'
        stop
      end subroutine GRACE

      subroutine TRAPSIG
!      integer isignal, signal

!sgi        isignal = signal(sigusr1, handler, -1)
!linux      isignal = signal(sigusr1, handler)
!ibm        call signal(sigusr1, handler)

        call signal(sigusr1, handler)
      end subroutine TRAPSIG

      end module GRACESTOP


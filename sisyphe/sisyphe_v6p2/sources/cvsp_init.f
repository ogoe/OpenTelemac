!                    *********************
                     SUBROUTINE CVSP_INIT
!                    *********************
!
     &()
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    Inits a Vertical Sorting PROFILE by USER CODING
!
!history  UWE MERKEL
!+        2011-07-20
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| -              |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      USE DECLARATIONS_SISYPHE

      IMPLICIT NONE

      integer  I,J,K


      ALLOCATE(PRO_D(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_F(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_MAX(NPOIN))

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! THIS IS JUST AN EXAMPLE!!!!
        do J=1,NPOIN

           PRO_MAX(J) = PRO_MAX_MAX

        do K=1,PRO_MAX(J)
        do I=1,NSICLA

           PRO_D(J,K,I) = (ZF%R(J)-ZF%R(J))/PRO_MAX(J)*k
           PRO_F(J,K,1) = 1.D0 / NSICLA

        enddo
        enddo
        enddo
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!


        RETURN
      END SUBROUTINE CVSP_INIT


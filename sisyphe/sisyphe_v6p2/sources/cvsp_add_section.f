!                    ******************************************
                     SUBROUTINE CVSP_ADD_SECTION
!                    ******************************************
!
     &(J)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    Adds a Section to the Vertical Sorting Profile; With 0 strength!!
!
!history  UWE MERKEL
!+        2011
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: J

      integer  I

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      PRO_MAX(J)  = PRO_MAX(J) + 2


        do I=1,NSICLA
           PRO_D(J,PRO_MAX(J),I) = PRO_D(J,PRO_MAX(J)-2,I)
           PRO_F(J,PRO_MAX(J),I) = 1.D0 / NSICLA
           PRO_D(J,PRO_MAX(J)-1,I) = PRO_D(J,PRO_MAX(J)-2,I)
           PRO_F(J,PRO_MAX(J)-1,I) = 1.D0 / NSICLA
        enddo
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        RETURN
      END SUBROUTINE CVSP_ADD_SECTION
!
!

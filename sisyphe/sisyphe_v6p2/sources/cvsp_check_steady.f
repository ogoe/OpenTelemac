!                    ******************************************
                     SUBROUTINE CVSP_CHECK_STEADY(J)
!                    ******************************************
!
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    Checks Vertical Sorting Profile to be steady in PRO_D
!
!history  UWE MERKEL
!+        2012
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: J

      integer  K, JG
      doUBLE PRECISION  AT

         JG = j
         if (ncsize > 1) JG = mesh%knolg%I(J)

         AT = DT*LT/PERCOU

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        do K=1,PRO_MAX(J)-1
           if ((PRO_D(J,K+1,1) - PRO_D(J,K,1)).lt.0.D0) then
                print * ,'ERR: Unsteady VSP! ,J,K,AT',
     &                  JG, K, AT, PRO_D(J,K+1,1), PRO_D(J,K,1)
                call CVSP_P('./ERR/','Unsteady_',JG)
                call Plante(1)
           endif
        enddo
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        RETURN
      END SUBROUTINE
!

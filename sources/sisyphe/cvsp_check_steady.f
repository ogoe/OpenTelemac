!                     ****************************
                      SUBROUTINE CVSP_CHECK_STEADY
!                     ****************************
!
     &(J)
!
!***********************************************************************
! SISYPHE   V6P3                                  12/03/2013
!***********************************************************************
!
!brief   CHECKS VERTICAL SORTING PROFILE TO BE STEADY IN PRO_D
!
!history UWE MERKEL
!+        2012
!+       V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX OF A POINT IN MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: J
      INTEGER  K, JG
      DOUBLE PRECISION AT
!
!-----------------------------------------------------------------------
!
      JG = J
      IF(NCSIZE > 1) JG = MESH%KNOLG%I(J)
!
      AT = DT*LT/PERCOU
!
!-----------------------------------------------------------------------
!
      DO K=1,PRO_MAX(J)-1
        IF((PRO_D(J,K+1,1) - PRO_D(J,K,1)).LT.0.D0) THEN
          WRITE(LU,*) 'ERR: UNSTEADY VSP! ,J,K,AT',
     &           JG, K, AT, PRO_D(J,K+1,1), PRO_D(J,K,1)
          CALL CVSP_P('./','UNSTEADY_',JG)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE


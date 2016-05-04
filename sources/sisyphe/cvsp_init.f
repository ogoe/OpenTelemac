!                    ********************
                     SUBROUTINE CVSP_INIT
!                    ********************
!
!***********************************************************************
! SISYPHE   V6P3                                   12/03/2013
!***********************************************************************
!
!brief   INITS A VERTICAL SORTING PROFILE BY USER CODING
!
!history UWE MERKEL
!+        20/07/2011
!+        V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX OF A POINT IN MESH
!| I              |<--| INDEX OF A FRACTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
!
      INTEGER  I,J,K
!
!-----------------------------------------------------------------------
!
      ALLOCATE(PRO_D(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_F(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_MAX(NPOIN))
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
        PRO_MAX(J) = PRO_MAX_MAX
        DO K=1,PRO_MAX(J)
          DO I=1,NSICLA
            PRO_D(J,K,I) = (ZF%R(J)-ZF%R(J))/PRO_MAX(J)*K
            PRO_F(J,K,1) = 1.D0 / NSICLA
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_INIT


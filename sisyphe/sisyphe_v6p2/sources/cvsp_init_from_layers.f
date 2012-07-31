!                    **********************************
                     SUBROUTINE CVSP_INIT_from_LAYERS()
!                    **********************************
!
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    Inits a Vertical Sorting PROFILE using HIRANO LAYERS
!
!history  UWE MERKEL
!+        2012-04-19
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|           |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE

      IMPLICIT NONE

      integer  I,J,K,M,L, MDISC, UBS
      double precision depth

      ALLOCATE(PRO_D(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_F(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_MAX(NPOIN))

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        do J=1,NPOIN

                depth = 0                          ! Init depth of the VSP
                PRO_MAX(J) =  2* NLAYER%I(J)       ! 2 Section Points per Layer
                L = PRO_MAX(J)

            !Water / Bottom
                    do I=1,NSICLA
                       PRO_D(J,L,I) = ZF%R(J)
                       PRO_F(J,L,I) = AVAIL(J,1,I)
                    enddo


            ! Sections
            do M=1,NLAYER%I(J)-1   !For the upper 8 Layers

                depth = depth + ES(J,M)


                       L = L - 1
                    do I=1,NSICLA
                       PRO_D(J,L,I) = ZF%R(J) - depth
                       PRO_F(J,L,I) = AVAIL(J,M,I)
                    enddo
                       L = L - 1
                    do I=1,NSICLA
                       PRO_D(J,L,I) = ZF%R(J) - depth
                       PRO_F(J,L,I) = AVAIL(J,M+1,I)
                    end do

            enddo

            !Bottom / Rigid Bed
                       L = L - 1
                    do I=1,NSICLA
                       PRO_D(J,L,I) = ZR%R(J)
                       PRO_F(J,L,I) = AVAIL(J,NLAYER%I(J),I)
                    enddo


                    Call CVSP_COMPRESS_DP(J,0.00001D0)


        enddo
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!



        RETURN


      END SUBROUTINE CVSP_INIT_from_LAYERS

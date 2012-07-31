!                    *********************************
                    SUBROUTINE CVSP_COMPRESS_BRUT(J)
!                    *********************************
!
!
!***********************************************************************
! SISYPHE   V6P2                                   23/12/2011
!***********************************************************************
!
!brief    Compresses a Vertical Sorting Profile in Point J to prevent
!+        extensiv groth of section / node numbers
!
!         Brutal Version
!         In case of emergency, if no other algorithm is allowed to compress,
!         to prevent PRO_MAX_MAX overflow
!
!history  UWE MERKEL
!+        2011-12-23
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE DECLARATIONS_SISYPHE
 !
      IMPLICIT NONE

      INTEGER,           INTENT(IN)    :: J

!PAT      double precision CVSP_INTEGRATE_VOLUME
      double precision Z_LOW ,Z_HIGH, SECHIGHT, a(10)

      integer NewPro_MAX, K, I, JG
      logical db,ret !PAT,CVSP_CHECK_F
! PAT
      LOGICAL, EXTERNAL:: CVSP_CHECK_F
      DOUBLE PRECISION, EXTERNAL:: CVSP_INTEGRATE_VOLUME

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       !Temporary VERTICAL SORTING PROFILE: FRACTION FOR EACH LAYER, CLASS, POINT
       doUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::PRO_FNEW
       !Temporary VERTICAL SORTING PROFILE: Depth FOR EACH LAYER, CLASS, POINT
       doUBLE PRECISION,DIMENSION(:,:),TARGET,ALLOCATABLE::PRO_DNEW

       ALLOCATE(PRO_DNEW(PRO_MAX_MAX,NSICLA))
       ALLOCATE(PRO_FNEW(PRO_MAX_MAX,NSICLA))

       JG = J
       if (NCSIZE.gt.1) JG = mesh%knolg%I(J)


         if (db(JG,0).eqv..true.) call CVSP_P('./ERR/','BRUA',JG)


         !Works like the Make_Act layer routine! But for VSP!

           NewPro_MAX=int(
     &       max(8.0D0,(dble(PRO_MAX_MAX - 4 * NSICLA)*0.7D0)))

           !NEW VSP section height
           sechight = (PRO_D(J,PRO_MAX(J),1)-PRO_D(J,1,1))
     &          / (NewPro_MAX - 1)

         do K = 1, NewPro_MAX


         do I = 1, NSICLA
           PRO_DNEW(K,I) = (k-1)*sechight + PRO_D(J,1,1)

           Z_LOW  = PRO_DNEW(K,1) - 0.5D0*sechight
           Z_HIGH = PRO_DNEW(K,1) + 0.5D0*sechight
           if (k.eq.1) Z_LOW = PRO_D(J,1,1)
           if (k.eq.NewPro_MAX) Z_HIGH = PRO_D(J,PRO_MAX(J),1)

           PRO_FNEW(K,I) = CVSP_INTEGRATE_VOLUME(J,I, Z_HIGH, Z_LOW,a)
     &                   / sechight

           if (k.eq.1) PRO_FNEW(K,I) = PRO_FNEW(K,I) * 2.0D0
           if (k.eq.NewPro_MAX) PRO_FNEW(K,I) = PRO_FNEW(K,I) * 2.0D0
         enddo


         enddo


         !Resubstitude
         do I = 1, NSICLA
         do K = 1, NewPro_MAX
           PRO_D(J,K,I) = PRO_DNEW(K,I)
           PRO_F(J,K,I) = PRO_FNEW(K,I)
         enddo
         enddo

           PRO_MAX(J) = NewPro_MAX

            if (PRO_MAX(J).le.2) then
                print *, 'CompressBrut: Not enough PRO_MAX'
                call plante(1)
            endif

         if (db(JG,0).eqv..true.) call CVSP_P('./ERR/','BRUE',JG)


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

         DEALLOCATE(PRO_DNEW)
         DEALLOCATE(PRO_FNEW)


          do K = 1, Pro_MAX(J)
            ! removes numeric instabilities
            ret = CVSP_CHECK_F(J,K,'AfterBRUT:   ')
          enddo
            call CVSP_CHECK_STEADY(J)



       RETURN

      END SUBROUTINE CVSP_COMPRESS_BRUT


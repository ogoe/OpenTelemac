!                    ******************************
                     SUBROUTINE CVSP_COMPRESS_DP(J, threshold)
!                    ******************************
!
!
!***********************************************************************
! SISYPHE   V6P2                                   21/11/2011
!***********************************************************************
!
!brief    Compresses a Vertical Sorting Profile in Point J to prevent
!+        extensiv groth of section / node numbers
!
!         With a douglas Peuker like Algorithm
!         The algorithm is modified: instead from "Point to Line Distances"
!          to the "Sum of Fraction Errors" !!!
!
!history  UWE MERKEL
!+        2011-07-20
!
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!| threshold      |<--| share of a fraction we are willing to round of
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE DECLARATIONS_SISYPHE
!

      IMPLICIT NONE

      INTEGER,           INTENT(IN)    :: J
      double precision,   INTENT(IN)    :: threshold
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      integer  I,K, cntr_erase, MarkerMax, MarkerCnt,  ttt, NNN, JG
      integer  maxpos, m, mmm, MarkerMAXold, MarkerMAXveryold
      integer Marker(PRO_MAX_MAX), MarkerTemp(PRO_MAX_MAX)      !used to mark nodes that will be kept
      double precision Loss(PRO_MAX_MAX)                         !stores the fraction errors that will occure if the point is eleminated from current profile
      double precision MaxDist, sum_nsicla                       !stores the maximum distance of any node in the current loop
      double precision  FI, FJ, FK, DI, DJ, DK, thresh
      logical dummy, stopit, Normalize_Fraction !, db
! PAT
      LOGICAL, EXTERNAL:: DB
      character*30 debugfile
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


       ! Parallel : local to global
       JG = J
       if (NCSIZE.gt.1) JG = mesh%knolg%I(J)


       !--------------------------------------------------------------------------
        !DEBUG: Init output
          if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','V_H',JG)



       !--------------------------------------------------------------------------
        !INIT

            !Nothing to do
            if (PRO_MAX(J) <= 2) then
                Return
            endif


            !First and last point will alway be kept
            MarkerMax = 2              !Maximum used index in Marker Array
            Marker(1) = 1              !First Will always be kept
            Marker(2) = PRO_MAX(J)     !Last  Will always be kept

            MarkerCNT = 1              !Maximum used index in MarkerTEMP Array


            thresh = threshold


        !-------------------------------------------------------------------------
        !--------Extend threshold if necessary
        do NNN = 1,1 !4

            if (NNN > 1) print*, "Compress", j, NNN

            thresh = thresh * (10**(1-NNN))
            MarkerMAXveryold = MarkerMAX

        !--------------------------------------------------------------------------
        !--------------------------------------------------------------------------
        !--------------------------------------------------------------------------
        !Iterate until nothing changes anymore
        do ttt = 1, PRO_MAX(J) - 2  ! Therotical maximum number of iterations
                    MarkerMAXold = MarkerMAX
                    MarkerCNT = 1
                    MarkerTEMP(MarkerCNT) = 1


            !Loop over all sections between 2 marked nodes
            do I = 1, MarkerMax-1

                    MaxDist = 0         !Inits the maximum fraction error
                    maxpos = -1         !Inits the node which produces the maximum fraction error


                if (Marker(i+1)-Marker(i) >= 2 ) then

                !Loop over all unmarked nodes inbetween 2 marked nodes
                do M = Marker(i) + 1 , Marker(i + 1) - 1

                   !How much Volume=Fraction is lost if we eleminate this profilepoint?
                   !Using "Volume" !!! Original douglas-Peuker: Distance to Interconnection !!!
                   !"Error Triangle Volume" is calulated by Gaussian Polygonformula!

                          Loss(M) = 0.D0

                   do K = 1, NSICLA

                        if (nnn.ge.5) then
                          FI = PRO_F(J,m-1,K)
                          FJ = PRO_F(J,m+1,K)
                          FK = PRO_F(J,M,K)
                          DI = PRO_D(J,m-1,K)
                          DJ = PRO_D(J,m+1,K)
                          DK = PRO_D(J,M,K)
                        else
                          FI = PRO_F(J,Marker(i),K)
                          FJ = PRO_F(J,Marker(i+1),K)
                          FK = PRO_F(J,M,K)
                          DI = PRO_D(J,Marker(i),K)
                          DJ = PRO_D(J,Marker(i+1),K)
                          DK = PRO_D(J,M,K)
                        endif

                     Loss(M) = Loss(M) +
     &                         ABS(0.5D0 * ((FI+FJ) * (DI-DJ) +
     &                                      (FJ+FK) * (DJ-DK) +
     &                                      (FK+FI) * (DK-DI)))

                   enddo !K


                   if (Loss(M).gt.MaxDist) then
                        MaxDist = Loss(M)
                        maxpos = m
                   endif

                enddo  !m


                    ! if any point is to far out of range: add it to the marker list
                    if (maxpos > -1 .And. MaxDist > thresh) Then
                        MarkerCNT = MarkerCNT + 1
                        MarkerTEMP(MarkerCNT) = maxpos
                    End if


                endif

                    ! Add the endpoint of this section
                    MarkerCNT = MarkerCNT + 1
                    MarkerTEMP(MarkerCNT) = Marker(i+1)

            enddo !I

                do I = 1, MarkerCNT
                    Marker(I) = MarkerTEMP(I)
                enddo
                    MarkerMax = MarkerCNT

                    if (MarkerMAX - MarkerMAXOld == 0 ) exit      !Stop Iteration, as nothing changed!
        enddo ! ttt
                    if (MarkerMAX - MarkerMAXVeryOld == 0 ) exit  !Stop Iteration, as nothing changed!
        enddo ! NNN



        !--------------------------------------------------------------------------
        !Recreate the Sorting Profile with lesser number of sections
        do K = 1, NSICLA
        do I = 1, MarkerMax

            PRO_F(J,I,K) = PRO_F(J,Marker(i),K)
            PRO_D(J,I,K) = PRO_D(J,Marker(i),K)

        enddo !i
        enddo !K

            PRO_MAX(J) = MarkerMax


        !--------------------------------------------------------------------------
        ! BrutForce Compression in Case of exceptional fragmentation
        if (PRO_MAX(J) > PRO_MAX_MAX-4*NSICLA-4) then
           print*,'CVSP_COMPRESS_DP resigns and calls COMPRESS_brut:'
           call CVSP_COMPRESS_BRUT(J)
        endif


        !--------------------------------------------------------------------------
        !DEBUG: Final output
          if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','V_I',JG)


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        RETURN
      END SUBROUTINE CVSP_COMPRESS_DP


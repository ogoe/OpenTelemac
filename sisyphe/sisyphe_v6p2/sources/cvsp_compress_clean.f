!                    ******************************
                     SUBROUTINE CVSP_COMPRESS_CLEAN
!                    ******************************
!
     &(J)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/11/2011
!***********************************************************************
!
!brief    Clean a Vertical Sorting Profile in Point J after removing fractions
!+        eleminates empty layers
!
!
!history  UWE MERKEL
!+        2012-02-02
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE

      INTEGER,           INTENT(IN)    :: J

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      integer  I,K, cntr_erase, MarkerMax, MarkerCnt,  ttt, NNN, JG
      integer  maxpos, m, mmm, MarkerMAXold, MarkerMAXveryold
      integer Marker(PRO_MAX_MAX), MarkerTemp(PRO_MAX_MAX)         
!used to mark nodes that will be kept
      double precision Loss(PRO_MAX_MAX)                           
!stores the fraction errors that will occure if the point is eleminated from current profile
      double precision MaxDist, sum_nsicla, sumferr                
!stores the maximum distance of any node in the current loop
      double precision  FI, FJ, FK, DI, DJ, DK, thresh, dist, sumf
      logical dummy, stopit, Normalize_Fraction, db
      character*30 debugfile
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


       !local -> gobal / parallel stuff
       JG = J
       if (NCSIZE.gt.1) JG = mesh%knolg%I(J)



        !--------------------------------------------------------------------------
        ! Initial debugging output ....
          if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','V_W',JG)




        !--------------------------------------------------------------------------
        !--------------------------------------------------------------------------
        !--------------------------------------------------------------------------
        !INIT

            if (PRO_MAX(J) <= 2) then
                Return
            endif




            !First and last point will always be kept
            MarkerMax = 2              !Maximum used index in Marker Array
            Marker(1) = 1              !First Will always be kept

            MarkerCNT = 1              !Maximum used index in MarkerTEMP Array



        !--------------------------------------------------------------------------
        !--------------------------------------------------------------------------
        !--------------------------------------------------------------------------
        !Top to Bottom
        do ttt = 2,PRO_MAX(J)

            sumf = 0.D0
            sumferr = 0.D0
          do I = 1, NSICLA
            sumf = PRO_F(j,ttt, I) + sumf
            sumferr = abs((PRO_F(J,ttt,I)-PRO_F(J,marker(markercnt),I)))
     &                + sumferr
          enddo

            if (ttt > 1) then
               dist = abs((PRO_D(J,ttt,1)-PRO_D(J,marker(markercnt),1)))

            if ((dist.gt.zero).or.(sumferr.gt.0.D0)) then

              MarkerCnt = MarkerCnt + 1
              marker(markercnt) = ttt

            endif
            endif

        enddo !ttt


        markermax = markercnt

        !--------------------------------------------------------------------------
        !Recreate the Sorting Profile with lesser number of nodes=layers
        do K = 1, NSICLA
        do I = 1, MarkerMax

            PRO_F(J,I,K) = PRO_F(J,Marker(i),K)
            PRO_D(J,I,K) = PRO_D(J,Marker(i),K)

        enddo !i
        enddo !K


            PRO_MAX(J) = MarkerMax


        !--------------------------------------------------------------------------
        ! BrutForce Compression in Case of exceptional fragmentation
        if (PRO_MAX(J) > PRO_MAX_MAX-8*NSICLA) then
          call CVSP_COMPRESS_DP(J, 1.D-5)
        endif
        if (PRO_MAX(J) < 4) then
          call CVSP_COMPRESS_BRUT(J)
        endif
        !--------------------------------------------------------------------------

        !--------------------------------------------------------------------------
        ! Final debugging output ....
        if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','V_V',JG)


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        RETURN
      END SUBROUTINE CVSP_COMPRESS_CLEAN


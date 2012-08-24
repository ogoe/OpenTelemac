!                    **************************
                     SUBROUTINE CVSP_MAKE_ActLay()
!                    **************************
!
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    Build a new Active Layer with Data from Vertical Sorting Profile
!+        and a new Active Stratum with Data from Vertical Sorting Profile
!
!
!history  U.Merkel & REBEKKA KOPMANN
!+        2012
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE BIEF_DEF !, ONLY: IPID, NCSIZE
      USE BIEF
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      logical db
      integer  I,J,K,M, JL, JG, II
      double precision TEMP, Z_HIGH, Z_LOW, CVSP_INTEGRATE_VOLUME, AT
      double precision a1, a2 , a3,a4,a5, asum, bsum, new_alt!PAT, CVSP_ALT
!PAT
      DOUBLE PRECISION, EXTERNAL:: CVSP_ALT
      double precision sumes, sumav, ALT
      double precision a(10)
      character*30, debugfile


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!        ELAY0          = ALThickness Init
!        AVAIL(J,K,I)   = Fractions
!        ELAY%R(J)      = ALThickness Real for Point(J)
!        ESTRAT%R(J)    = ASThickness Real for Point(J)
!        ES(J,K)        = LayerThickness for Point(J) / Layer(K)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        AT = DT*LT/PERCOU

        do J=1,NPOIN

           NLAYER%I(J) = NOMBLAY

           ! GET NEW VALUES FOR ALT
           new_alt =  CVSP_ALT(J,ALT_MODEL)

           if (new_alt.gt.(ZF%R(J)-ZR%R(J))) then
               new_alt = (ZF%R(J)-ZR%R(J))
           endif


        sumes = 0.D0
        do K=1,1!NLAYER%I(J)


            if(k.eq.1) Then
                    ALT = new_alt
            else
                    ALT = ELAY0
            endif
                    sumes = sumes + ALT


            !ALT doesn't use the full depth
            ! --------------------------------------------------------------------------------
            if((ZF%R(J)-ZR%R(J)).GE.new_alt) THEN

            ! --------------------------------------------------------------------------------
            ! --------------------------------------------------------------------------------
            ! sumes < full depth
            if(sumes.LE.(ZF%R(J)-ZR%R(J))) THEN


                if(k.eq.1) Then
                    ES(J,K) = new_alt
                else
                    if(((ZF%R(J)-ZR%R(J))-sumes).gt.2.D0*ELAY0) Then
                        ES(J,K) = ELAY0
                    endif
                    if(((ZF%R(J)-ZR%R(J))-sumes).lt.2.D0*ELAY0) Then
                        ES(J,K) = (ZF%R(J)-ZR%R(J)) - sumes
                        NLAYER%I(J) = K
                    else
                        ES(J,K) = 0.D0
                    endif
                endif ! k=1

                if (k == 1) then
          !if (db(JG,0).eqv..true.) call CVSP_P('./ERR/','IVES5_',JG)
                    Z_HIGH = PRO_D(J,PRO_MAX(J),1)
                else
          !if (db(JG,0).eqv..true.) call CVSP_P('./ERR/','IVES6_',JG)
                    Z_HIGH = PRO_D(J,PRO_MAX(J),1) - sumes + ALT
                    if (Z_HIGH.gt.PRO_D(J,PRO_MAX(J),1)) then
                        Z_HIGH=PRO_D(J,PRO_MAX(J),1)
                    endif
                endif

                Z_LOW =  Z_HIGH - ES(J,K)

                !--------------------------------------------------------------
                !--------------------------------------------------------------
                ! This is the core!
                    asum = 0.D0
                    TEMP = CVSP_INTEGRATE_VOLUME(J,1,Z_HIGH,Z_LOW,a)

                !Assign Fractions
                do I=1,NSICLA

                    AVAIL(J,K,I) = a(I) / ES(J,K)
                    asum = AVAIL(J,K,I) + asum

                if ((AVAIL(J,K,I)>1+ZERO)) then
             WRITE(LU,*) "MAKE_AL_", J, K, I, AT,
     &                 AVAIL(J,K,I), a(I), ES(J,K),Z_HIGH,Z_LOW, ES(J,K)
             WRITE(LU,*) "ES,ALT,ELAY0,newalt",ES(J,K),ALT,ELAY0,new_alt
                       call plante(1)
                endif
                enddo !I=1,NSICLA
                !--------------------------------------------------------------
                !--------------------------------------------------------------

                !--------------------------------------------------------------
                !Truncation errors normalized for ALL FRACTIONS-------------------------
                if ((abs(asum - 1.D0) .ne. 0.D0)) then
                        do i=1,NSICLA
                        AVAIL(J,K,I) = AVAIL(J,K,I) / asum
                        end do
                endif
                !--------------------------------------------------------------


              !--------------------------------------------------------------------------------
              !--------------------------------------------------------------------------------
              !--------------------------------------------------------------------------------
              else ! sumes > ueberdeckung


             WRITE(LU,*) 'UHM_NOW_OBSOLETE',J, K, I, AT
                call plante(1)
                !! REMOVE THIS CASE if YOU HAVEN'T SEENN THIS FOR A LONG TIME




                  ES(J,K) = max(-1.D0*(sumes-(ZF%R(J)-ZR%R(J))), 0.D0)

                  ! layer ist auf 0
                  if(ES(J,K).EQ.0.D0) THEN

                     do i=1,NSICLA
                        AVAIL(J,K,I) = 1.D0 / NSICLA
                     end do


                  else

          if (db(JG,0).eqv..true.) call CVSP_P('./ERR/','IVES3_',JG)

                        !Assign Z-Coordinate for Upper and Lower Boundary of Layer
                        Z_LOW = PRO_D(J,PRO_MAX(J),1) !ZF%R(J)
                        do M=1,K
                            Z_HIGH = Z_LOW
                            Z_LOW = Z_LOW - ES(J,M)
                        enddo

                        if(db(JG,0).eqv..true.) print*,'UHM_L_',K,i
                        TEMP=CVSP_INTEGRATE_VOLUME(J,1,Z_HIGH,Z_LOW,a)

                     ! layerdicke ist Rest, Fraktion muss berechnet werden
                     do i=1,NSICLA

                        AVAIL(J,K,I) = a(I) / ES(J,K)
                        asum = AVAIL(J,K,I) + asum

                        !DEBUG -------------------------
                        !if (I == 1) a1 = avail(j,k,i)
                        !if (I == 2) a2 = avail(j,k,i)
                        !if (I == 3) a3 = avail(j,k,i)
                        !if (I == 4) a4 = avail(j,k,i)
                        !if (I == 5) a5 = avail(j,k,i)

               if ((AVAIL(J,K,I)>1+ZERO)) then
                            PRINT * , "MAKE_AL_", J, K, I, AT,
     &                        AVAIL(J,K,I), a(I), ES(J,K),Z_HIGH,Z_LOW
               endif

                     end do ! nsicla



                    if ((abs(asum - 1) > ZERO)) then
                            print *,  "MAKE_AL_ABS2", J, K, I, AT,
     &                         AVAIL(J,K,I), TEMP, ES(J,K),Z_HIGH,Z_LOW,
     &                         a1, a2, a3, asum

                        do i=1,NSICLA
                        AVAIL(J,K,I) = AVAIL(J,K,I) / asum
                        end do


                    endif
                  endif ! es==0
              endif ! sumes < ueberdeckung



            ! --------------------------------------------------------------------------------
            else ! rigid bed ist oberhalb vom active layer

                ! Alle anderen layer sind dann 0
                  ES(J,K) = 0.D0
                  ES(J,1) = (ZF%R(J)-ZR%R(J))
                  NLAYER%I(J) = 1
                  do i=1,nsicla
                    AVAIL(J,K,I) = 1.D0 / NSICLA
                  end do

            ! --------------------------------------------------------------------------------
            endif ! ueberdeckung > sumes


        ! --------------------------------------------------------------------------------
        ! fuer richtige Belegung von elay und estrat
        if (K == 1) ELAY%R(J) = ES(J,K)
        if (K == 2) ESTRAT%R(J) = ES(J,K)


            ! Stops if the number of layers is reduced during calculation
            if (NLAYER%I(J) == K) exit

        enddo ! k



        ! --------------------------------------------------------------------------------
        ! Not enough space for Nlayer
        do k=1,1 !NLAYER%I(J)
          if(ES(J,K).EQ.0.D0) THEN
            WRITE(LU,*) 'Not enough space for Nlayer',j
            WRITE(LU,*) 'Possible Error !!! Rigid BED? ',NLAYER%I(J)-1
            NLAYER%I(J) = NLAYER%I(J)-1
            exit
          endif
        END do


        enddo ! j





        !Legacy adaption
        !NCOUCHES(J) = NLAYER%I(J)




!--------------------------------------------------------------------
! -- CHECKS
!--------------------------------------------------------------------



       do j=1,npoin
           sumes = 0.D0
           JG = j
           if (ncsize > 1) JG = mesh%knolg%I(j)

           do k=1,9 !

              sumes = sumes+es(j,k)
              sumav = 0.D0
             do i=1,nsicla
              sumav=avail(j,k,i)+sumav
              if((avail(j,k,i).gt.1.D0+1.D-12).or.
     &           (avail(j,k,i).lt.0.D0-1.D-12)) then
                  print*,'fehler avail',j,k,i,avail(j,k,i)
                    call CVSP_P('./ERR/','ERR_AVAIL_VSP_',  JG)
                    call LAYERS_P('./ERR/ERR_AVAIL_LAY_', JG)
                    call plante(1)
              endif
             end do !i

             if((abs(sumav-1.D0).lt.0.00000000001D0).and.
     &                (abs(sumav-1.D0).gt.0.D0)) then
               !print*,'abweichung sumav',j,k,sumav
               !print*,(avail(j,k,i),i=1,nsicla)
               avail(j,k,1) = avail(j,k,1) - (1.D0 - sumav)
             endif

             if(abs(sumav-1.D0).gt.0.00000000001D0) then
               !print*,'fehler sumav',j,k,sumav
               !print*,(avail(j,k,i),i=1,nsicla)
                 !call CVSP_P('../ERR/ERR_SUM_AVAIL_VSP_', JG)
                 !call LAYERS_P('../ERR/ERR_SUM_AVAIL_LAY_',JG)

                 if(abs(sumav-1.D0).gt.0.0001D0) then
                    print*,'fehler sumav too bad:',j,k,sumav

                write(unit=debugfile, fmt='(A,I4,A)')
     &          './ERR/','SUMAV_',JG,'_VSP_'
!                    call CVSP_P(debugfile, JG)
                write(unit=debugfile, fmt='(A,I4,A)')
     &          './ERR/','SUMAV_',JG,'_LAY_'
!                    call LAYERS_P(debugfile, JG)
                    !stop
                 !else
                    !print*,'fehler sumav corrected',j,k
                    do ii=1,nsicla
                        avail(j,k,II) = avail(j,k,II) / sumav
                    enddo
                 endif
             endif
           end do !k

             if( abs(sumes-(zf%r(j)-zr%r(j))).lt.ZERO) then
              print*,'fehler summe es',j,sumes,zf%r(j)-zr%r(j)
                  call CVSP_P('./ERR/','ERR_SUM_ES_VSP_', JG)
                  call LAYERS_P('./ERR/ERR_SUM_ES_LAY_', JG)
                  call Plante(1)
             endif

       end do !j



!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        RETURN
      END SUBROUTINE CVSP_MAKE_ActLay

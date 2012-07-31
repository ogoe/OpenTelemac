!        ************************************************
         double precision Function CVSP_INTEGRATE_VOLUME
!        ************************************************
!
     &(J,I,Z_HIGH,Z_LOW,a)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    Integrates the Volume of a Fraction within the
!+        Vertical Sorting Profil between 2 Depth Z-Coordinates Z_HIGH & Z_LOW
!+
!
!
!
!history  UWE MERKEL
!+        2011
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!| I              |<--| INDEX of a Fraction in VERTICAL SORTING PROFILE
!| Z_HIGH         |<--| Higher Depth Coordinate
!| Z_LOW          |<--| Lower  Depth Coordinate
!| a1 ..a10       |<--| integrated volumes per fraction
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER,          INTENT(IN)   :: J
      INTEGER,          INTENT(IN)   :: I
      double precision, INTENT(IN)    :: Z_HIGH
      double precision, INTENT(IN)    :: Z_LOW
      double precision, INTENT(out)   :: a(10)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      doUBLE PRECISION TEMP,tempold, dhig, dlow, AT, sumup, flow, fhig
      doUBLE PRECISION TEMP2, TEMP2MAX, SUMUP2,TEMP3, TEMP3MAX, SUMUP3
      double precision correct, chsum
      integer l_cnt, mycase, f_cnt, revcnt, helper, lastcase, JG, k
      logical ret,cvsp_check_f
      LOGICAL, EXTERNAL:: DB

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


       JG = J
       if (NCSIZE.gt.1) JG = mesh%knolg%I(J)

            AT = DT*LT/PERCOU
            sumup = 0.D0
            sumup2  = 0.D0
            sumup3  = 0.D0
            mycase  = 0

      !--------------------------------------------------------------------------
      !-----doing all fractions ----------------------------
      !--------------------------------------------------------------------------
      do f_cnt = 1, NSICLA

            TEMP = 0.D0
            TEMP2 = 0.D0
            TEMP2MAX = 0.D0
            TEMP3 = 0.D0
            TEMP3MAX = 0.D0
            chsum = 0.D0


      !--------------------------------------------------------------------------
      !------Going through all sections------------------------------------------
      !--------------------------------------------------------------------------
      do l_cnt = 0,(PRO_MAX(J)-2)

            tempold = temp
            revcnt = PRO_MAX(J)-l_cnt

            !Depth coordinates of the section to check
            dhig = PRO_D(j,revcnt,f_cnt)
            dlow = PRO_D(j,revcnt-1,f_cnt)


      !Sorting profile section totally inside (Case ZddZ)
      if ( (dhig <= Z_HIGH ) .AND.
     &     (dlow >= Z_LOW  ) ) then

               flow = PRO_F(j,revcnt-1,f_cnt)
               fhig = PRO_F(j,revcnt,f_cnt)

               mycase  = mycase  + 1
               lastcase = 1

               TEMP2 = 0.5D0*(fhig+flow)*(dhig-dlow)  + TEMP2
               TEMP2MAX = Z_HIGH - dlow

               do helper = 1, NSICLA
                    chsum = PRO_F(j,revcnt, helper) + chsum
               enddo
               chsum = 1.D0 - chsum

      !Sorting profile section partially lower (Case ZdZd)
      elseif ((dhig <= Z_HIGH) .AND.
     &       (dhig >  Z_LOW ) .AND.
     &       (dlow <  Z_LOW ) )  then

               fhig = PRO_F(j,revcnt,f_cnt)
               flow = PRO_F(j,revcnt-1,f_cnt)
               flow = - ((fhig-flow)/(dhig-dlow))*(dhig-Z_LOw) + fhig

               !cut the section
               dlow = Z_low

               mycase  = mycase  + 1000
               lastcase = 2

               TEMP3 = 0.5D0*(fhig+flow)*(dhig-dlow)  + TEMP3
               TEMP3MAX = dhig - dlow


      !Sorting profile section partially higher (Case dZdZ)
      elseif ((dhig > Z_HIGH) .AND.
     &    (dlow >= Z_LOW) .AND.
     &    (dlow <  Z_HIGH) ) then

               flow = PRO_F(j,revcnt-1,f_cnt)


               fhig = PRO_F(j,revcnt,f_cnt)
               fhig = ((fhig-flow)/(dhig-dlow))*(Z_HIGH-dlow) + flow

               !cut the section
               dhig = Z_HIGH

               mycase  = mycase  + 1000000
               lastcase = 3

      !if(db(JG,0).eqv..true.) print*,'  CASE 1000000'


      !Layer totally inside one section (Case dZZd)
      elseif ((dhig >= Z_HIGH) .AND.
     &    (dlow <= Z_LOW) ) then

               fhig =
     &           - (PRO_F(j,revcnt,f_cnt)-PRO_F(j,revcnt-1,f_cnt)) /
     &             (dhig-dlow) *
     &             (dhig-Z_HIGH)
     &              + PRO_F(j,revcnt,f_cnt)

               flow =
     &           - (PRO_F(j,revcnt,f_cnt)-PRO_F(j,revcnt-1,f_cnt)) /
     &             (dhig-dlow) *
     &             (dhig-Z_LOW)
     &             + PRO_F(j,revcnt,f_cnt)

               !cut the section
               dhig = Z_HIGH
               dlow = Z_LOW

               lastcase = 8
               mycase  = mycase  + 100000000

               !if(db(JG,0).eqv..true.) print*,'  CASE 100000000'

      !Section with 0 strength
      elseif (dhig == dlow) then
               flow = 0.D0
               fhig = 0.D0

               mycase  = mycase  + 1000000000
               lastcase = 4

      !A true bug!
      elseif (Z_LOW > Z_HIGH) then
               flow = 0.D0
               fhig = 0.D0

               print*, 'UHM: Z_LOW >= Z_HIGH', dhig, dlow, z_high, z_low
               call CVSP_P('./ERR/','zLOHI',JG)
               mycase  = mycase  + 1000000000
               lastcase = 5

      !A true bug!
      elseif (dhig < dlow) then
               flow = 0.D0
               fhig = 0.D0

               print*, 'UHM: dhig <= dlow', J,dhig, dlow, z_high, z_low
               call CVSP_P('./ERR/','dLOHI',JG)
               mycase  = mycase  + 1000000000
               lastcase = 6

      !A section that is not involved / Not a bug!!
      else

               flow = 0.D0
               fhig = 0.D0

               !print*, 'UHM: IVT else', dhig, dlow, z_high, z_low

               mycase  = mycase  + 1000000000
               lastcase = 7

      endif


    !Trapezoid formula

        TEMP = 0.5D0*(fhig+flow)*(dhig-dlow)  + TEMP

      !Debug!Debug!Debug!Debug!Debug!Debug
      if (0.5D0*(fhig+flow)*(dhig-dlow) < 0.D0) then
      write(*,fmt='(A,1X,2(I11,1X),11(G20.10,1X)),1X,I11')
     &   'Integrate_Vol_ER_TMP:<0:'
     &   ,JG, I, AT, fhig,flow,dhig,dlow, dhig-dlow, revcnt,
     &         PRO_F(j,revcnt-1,f_cnt),PRO_F(j,revcnt,f_cnt),
     &         PRO_D(j,revcnt-1,f_cnt),PRO_D(j,revcnt,f_cnt),lastcase
            call CVSP_P('./ERR/','IVkT',JG)
            call plante(1)
      endif
       !Debug!Debug!Debug!Debug!Debug!Debug

      end do !section
  !--------------------------------------------------------------------------
  !--------------------------------------------------------------------------


        !adding up fractions for debugging purposes
        sumup = TEMP + sumup
        sumup2 = temp2 + sumup2
        sumup3 = temp3 + sumup3

        a(f_cnt) = TEMP


      end do !fraction

        correct = (sumup / (Z_HIGH-Z_LOW))

        CVSP_INTEGRATE_VOLUME = a(I) / correct

      !Debug!Debug!Debug!Debug!Debug!Debug
      if (CVSP_INTEGRATE_VOLUME < 0.D0) then
        call CVSP_P('./ERR/','IVk0',JG)
            write(*,fmt='(A,2(I11),14(G20.10))')'Integrate_Vol_ER:<0:'
     &  ,JG, I, AT, CVSP_INTEGRATE_VOLUME, mycase ,Z_HIGH,Z_LOW,sumup,
     &            sumup2,sumup3,chsum,a(1),a(2),a(3),a(4),a(5)
            call plante(1)
      endif
      !Debug!Debug!Debug!Debug!Debug!Debug


      !-------_CHECKSUM  OVER  ALL FRACTIONS AND LAYERS
      sumup  = sumup  - abs(Z_HIGH-Z_LOW)
      sumup2 = sumup2 - abs(temp2max)
      sumup3 = sumup3 - abs(temp3max)
      chsum =  chsum / 5.D0

               !DEBUG
 !               if (isnan(sumup)) then
 !                   call CVSP_P('./ERR/','IV_E',JG)
 !                   print *, 'Integrate Volume NAN', Sumup, JG
 !                   call plante(1)
 !               endif

               !DEBUG
               !BEWARE: 1.D-5 is up to 10g of a 1000kg but higher accuracy
               !Leads again to floating point truncation errors ...
                if ((ABS(sumup).GT.1.D-5)) then
                    call CVSP_P('./ERR/','IV_E',JG)
                    print *, 'Integrate Volume ACCURRACY!!!', Sumup, JG
                    do K = 1, Pro_MAX(J)
                        ! removes numeric instabilities
                    ret =  CVSP_CHECK_F(J,K,' IV_FiX:   ')
                    enddo
                    !call plante(1)
                endif




!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
        RETURN
      END Function CVSP_INTEGRATE_VOLUME

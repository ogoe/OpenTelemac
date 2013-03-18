
! ##############################################################################################################   

   module m_selinf
   
   implicit none
    
   !   SELAFIN FILE INFORMATION:
    character*80 :: TITLE
    character*32 :: NAMES(50)
    integer :: NBV(2),NBV1(2),IPARAM(10),NELEM,NPOIN,NDP1,EXTRA,n_names
    integer, allocatable, dimension(:,:) :: IKLE
    integer, allocatable, dimension(:)   :: IPOBO
    real, allocatable, dimension(:)      :: XI,YI
    real, allocatable, dimension(:,:)    :: UI,VI,HI,BI,EVOI,QBI
    real, allocatable, dimension(:,:)    :: CONC1I,CONC2I,CONC3I,CONC4I
    real, allocatable, dimension(:,:)    :: WHI, PPI, QSI, CONCI
    real, allocatable, dimension(:)      :: TIME
    real, allocatable, dimension(:)      :: pvalues 
    
    end module m_selinf
    
! ##############################################################################################################   

   program sisvali

!   ################################################################################################################

! Purpose:

! Read in TELEMAC2D type SELAFIN format file and perform avrious checks inc L2 error computations

!   compile with:
!   gfortran -o sisvali sisvaliFOUL.f90 -fconvert=big-endian -frecord-marker=4 -O3 -g


!   NOTE:
! For speed I've just assumed that the format of the selafin file is such that it contains
! the following output variables:
! B,E, CS1, CS2, QSBL, QS1, QS2, QS3, W, X

! 

!   ################################################################################################################

!   AUTHOR: Dr. D. M. KELLY (HRW)
!   MODIFIED: 28nd January 2013
!   SUBROUTINES CALLED: 

    use m_selinf
  
    implicit none
    
    integer :: ierr,i,ii,j,NDP,NIP,imesh,nmesh,elementcount,nodecount
    integer :: t, naly
    integer, allocatable, dimension(:,:) :: BOUND
    integer ::  md,nxi,nyi,nzi,IER

    character*100 ::  fname
    character*500 ::  junk
 
    integer :: inr,jnr,TIM,TIMF,TIMW,jnumb
    real, allocatable, dimension(:,:) :: DEPV
    real, parameter                   :: sdens = 2650.0

    integer :: npp, dvn, spt, iloc,idE(11),idF(11)
    real :: min_x, max_x, ypv, dx, hdx, wA, wB, wC, areaT
    real :: brier,bss(2),u
    real :: num, denom, sumnum, sumdenom, delvol
    real, allocatable, dimension(:)   :: susmass, xpp, valu0, valu, valua, valua0, vol
    real, allocatable, dimension(:)   :: areaN, L2errS, L2errT, totconc
    real, allocatable, dimension(:)   :: xan, xan0, valanF, valanE, xconca, conca, qsa
    real, allocatable, dimension(:,:) :: valan

    logical :: INPOLY
   
   
!   Use implicit FORTRAN 90 routine to get the file stem direct from the command line:
!    call getarg ( 1, fname )

    fname = "sis_foulness.slf" ! tmp hard coded
    
!   Open TELEMAC-2D Selafin file:
!   ============================== SELAFIN FILE READ =======================================================
    
    open (convert='big_endian',unit=100,file=trim(fname),status='old',form='unformatted')
   
    read (100,ERR=11) TITLE(1:80)
    read (100,ERR=11) NBV(1), NBV(2)
    do i=1,(NBV(1)+NBV(2))
      read (100,ERR=11) NAMES(i)(1:32)
    end do
    read (100,ERR=11) (IPARAM(i),i=1,10)
    read (100,ERR=11) NELEM, NPOIN, NDP, EXTRA
    print *, 'NDP ', NDP, ' ELEMENTS ',NELEM,' NODES ', NPOIN
    allocate(IKLE(NDP,NELEM),IPOBO(NPOIN))
!    allocate(XIS(NPOIN),YIS(NPOIN))
    allocate(XI(NPOIN),YI(NPOIN))
    TIM=500

    ! SP arrays
    allocate(UI(TIM,NPOIN),VI(TIM,NPOIN),HI(TIM,NPOIN),BI(TIM,NPOIN))
    allocate(EVOI(TIM,NPOIN),QBI(TIM,NPOIN),WHI(TIM,NPOIN),PPI(TIM,NPOIN))
    allocate(QSI(TIM,NPOIN),TIME(TIM))
    allocate(CONC1I(TIM,NPOIN),CONC2I(TIM,NPOIN),CONC3I(TIM,NPOIN),CONC4I(TIM,NPOIN))

    read (100,ERR=11) (IKLE(1:3,i),i=1,NELEM)
    read (100,ERR=11) (IPOBO(i),i=1,NPOIN)
    read (100,ERR=11) (XI(i),i=1,NPOIN)
    read (100,ERR=11) (YI(i),i=1,NPOIN)
    TIM=0
!   read data at this time: 
10  TIM=TIM+1
!    print *, "Time Record Number: ", TIM
    read (100,END=12,ERR=11) TIME(TIM)
    read (100,END=12,ERR=11) (UI(TIM,i),i=1,NPOIN)       ! Velocity x-component
    read (100,END=12,ERR=11) (VI(TIM,i),i=1,NPOIN)       ! Velocity y-component
    read (100,END=12,ERR=11) (HI(TIM,i),i=1,NPOIN)       ! Depth
    read (100,END=12,ERR=11) (CONC1I(TIM,i),i=1,NPOIN)   ! Conc Vol 1
    read (100,END=12,ERR=11) (CONC2I(TIM,i),i=1,NPOIN)   ! Conc Vol 2
    read (100,END=12,ERR=11) (CONC3I(TIM,i),i=1,NPOIN)   ! Conc Vol 3
    read (100,END=12,ERR=11) (CONC4I(TIM,i),i=1,NPOIN)   ! Conc Vol 4
    read (100,END=12,ERR=11) (QBI(TIM,i),i=1,NPOIN)      ! Bed load
    read (100,END=12,ERR=11) (QSI(TIM,i),i=1,NPOIN)      ! Suspended load
    

    GOTO 10
    
11  stop 'Error in selafin input file (produced by SISYPHE) - TERMINATING.'
   
12  print *, 'Selafin file read in for validation there were', TIM-1, 'times.'
    TIMF=TIM-1
    close(100)
    
!   ============================== END SELAFIN FILE READ ====================================================


!   ============================== SCALE RESULTS TO MODEL ===================================================
  
!   ==========================================================================================================

    allocate (totconc(TIMF))

!  Initial experimental profile data :
    allocate  (valanF(11), valanE(11))

    valanE(1)  = 25.909 ; valanF(1)  = 34.11
    valanE(2)  = 54.999; valanF(2)  = 86.723
    valanE(3)  = 89.112; valanF(3)  = 103.632
    valanE(4)  = 403.306; valanF(4)  = 474.686
    valanE(5)  = 1131.750; valanF(5)  = 1083.216
    valanE(6)  = 931.913; valanF(6)  = 1320.067
    valanE(7)  = 524.818; valanF(7)  = 1077.750
    valanE(8)  = 581.714; valanF(8)  = 838.195
    valanE(9)  = 584.588; valanF(9)  = 568.814
    valanE(10) = 498.953; valanF(10) = 190.255
    valanE(11) = 486.524; valanF(11) = 46.666
!   Finished
  
!    ============================= DATA ON CROSS-SECTIONAL PROFILE ===============================================  
    

    allocate(vol(TIMF),L2errS(2))
    
 !   ============================= PERFORM VALIDATION OPERATIONS ON DATA ===============================================  
    
 !  This operation is for the FOULNESStest case:   
    

!   Note data at times 16-26 is the FLOOD tide
!   Note data 1-16 is the EBB tide

!   data at times 

!   Dump output for R plotting    
!    do i =1,naly
!    write(101,*) xpp(i), qsa(i), valu(i)
!    end do 
!    close(101)

!   Get time series at a specific spatial location:
!    call extractts(TIMF,0.2,0.2,valu,"C1") 

!   Perform L2 error norm on time-series of qs
!   Note NODE iloc is point x = 2430m and y= 90m

    allocate(valu(TIMF),valua(TIMF))
    iloc=5113
    idE=[5:15]
    idF=[16:26]
    valu = QSI(1:TIMF,iloc) * 2650000.0 ! NB: exp data in g/m/s num data output in m^2/s

!    print *, "EBB", valu(4:15)
!    print *, '========================================================================================='
!    print *, "FLOOD", valu(4:15); stop

    valua=-1
    valua(idE)=valanE(:)*HI(idE,iloc)*sqrt(UI(idE,iloc)**2+VI(idE,iloc)**2)
    valua(idF)=valanF(:)*HI(idF,iloc)*sqrt(UI(idF,iloc)**2+VI(idF,iloc)**2)
    
    call L2error(9,valu(idE),valua(idE),L2errS(1)) ! On EBB  
    call L2error(11,valu(idF),valua(idF),L2errS(2)) ! On FLOOD

    
    open(99,FILE='transport_rates.csv')
    write(99,*) ' # u^3/h    modelled     measured'
    write(99,*) 'u,h,(u^3/h)(m^2/s^3),Sediment Transport,Sediment Transport'
    write(99,*) '-,-,-,kg/ms,kg/ms'
    do i=1,26
        u=sqrt(UI(i,iloc)**2+VI(i,iloc)**2)
        write(99,'(F6.2 ,A, F8.2 ,A, F8.2 ,A, F12.2 ,A, F12.2)') u,',',HI(i,iloc),',',u**3/HI(i,iloc),',', valu(i),',', valua(i)
    end do
    close (99)
    open(99,FILE='L2norm.csv')
    write(99,*)'EBB, FLOOD'
    write(99,*)'-, -'
    write(99,'(F6.3 ,A, F6.3)') L2errS(1),',',L2errS(2)
    close (99) 
    
!   Make the Sedconc dimensional
    CONC1I = sdens*CONC1I; CONC2I = sdens*CONC2I
    CONC4I = sdens*CONC1I; CONC4I = sdens*CONC4I

!   Clear memory:
    deallocate(XI,YI,IKLE,UI,VI,HI,BI)
    deallocate(EVOI,QBI,QSI)
    deallocate(CONC1I,CONC2I,CONC3I,CONC4I)
    deallocate(L2errS)    
!    deallocate(vol,pvalues)
!    deallocate(valu,valua,valua0)

    end 
 
! #####################################################################################################################################
 
   subroutine brierskill(lim2,valun,valanen,valanin,bss)
    
   implicit none
    
   integer, intent(in) :: lim2
   real, intent(in)    :: valun(lim2), valanen(lim2), valanin(lim2)
   real, intent(out)   :: bss
    
   integer             :: i
   real                :: delmo, delbed
   real                :: msemo, msebed


     delmo  = 0.0
     delbed = 0.0
     msemo  = 0.0
     msebed = 0.0
   
     do i = 1,lim2

     delmo    = (valun(i)-valanen(i))*(valun(i)-valanen(i))
     delbed   = (valanen(i)-valanin(i))*(valanen(i)-valanin(i))
     msemo    = msemo + delmo
     msebed   = msebed + delbed

     end do   
   
     bss      = 1.0 - (msemo/msebed)
     
   return

   end   
    
! #####################################################################################################################################

  subroutine suspmass(tn, susmass)    
 
   use m_selinf
 
   implicit none
    
   integer, intent(in)           :: tn 
   real, intent(out)             :: susmass
    
   integer                       :: i, ii
   real                          :: W(3), xc, yc, tmp(4)
   real                          :: HCT(NELEM), SCCT(4,NELEM), areaN(NELEM)
 
    do i=1,NELEM
    
      call tricentroid(XI(IKLE(1,i)),YI(IKLE(1,i)),XI(IKLE(2,i)),YI(IKLE(2,i)),XI(IKLE(3,i)),YI(IKLE(3,i)),xc,yc)
     
      areaN(i) = 0.5*abs(  XI(IKLE(1,i))*YI(IKLE(2,i)) + XI(IKLE(2,i))*YI(IKLE(3,i)) + &
                           XI(IKLE(3,i))*YI(IKLE(1,i)) - XI(IKLE(2,i))*YI(IKLE(1,i)) - &
                           XI(IKLE(3,i))*YI(IKLE(2,i)) - XI(IKLE(1,i))*YI(IKLE(3,i))   )

      W(1)    = 0.5*abs(   xc*YI(IKLE(2,i)) + XI(IKLE(2,i))*YI(IKLE(3,i)) + &
                           XI(IKLE(3,i))*yc     - XI(IKLE(2,i))*yc        - &
                           XI(IKLE(3,i))*YI(IKLE(2,i)) - xc*YI(IKLE(3,i))   ) / areaN(i)

      W(2)     = 0.5*abs(  XI(IKLE(1,i))*yc + xc*YI(IKLE(3,i))            + &
                           XI(IKLE(3,i))*YI(IKLE(1,i)) - xc*YI(IKLE(1,i)) - &
                           XI(IKLE(3,i))*yc - XI(IKLE(1,i))*YI(IKLE(3,i))    ) / areaN(i)

      W(3)     = 0.5*abs(  XI(IKLE(1,i))*YI(IKLE(2,i)) + XI(IKLE(2,i))*yc + &
                           xc*YI(IKLE(1,i)) - XI(IKLE(2,i))*YI(IKLE(1,i)) - &
                           xc*YI(IKLE(2,i)) - XI(IKLE(1,i))*yc              ) / areaN(i)
    

   ! Compute value of the variable at that profile point for a specific time:
     HCT(i)    = W(1)*HI(tn,IKLE(1,i)) + W(2)*HI(tn,IKLE(2,i)) + W(3)*HI(tn,IKLE(3,i))
     SCCT(1,i) = W(1)*CONC1I(tn,IKLE(1,i)) + W(2)*CONC1I(tn,IKLE(2,i)) + W(3)*CONC1I(tn,IKLE(3,i))
     SCCT(2,i) = W(1)*CONC2I(tn,IKLE(1,i)) + W(2)*CONC2I(tn,IKLE(2,i)) + W(3)*CONC2I(tn,IKLE(3,i))
     SCCT(3,i) = W(1)*CONC3I(tn,IKLE(1,i)) + W(2)*CONC3I(tn,IKLE(2,i)) + W(3)*CONC3I(tn,IKLE(3,i))
     SCCT(4,i) = W(1)*CONC4I(tn,IKLE(1,i)) + W(2)*CONC4I(tn,IKLE(2,i)) + W(3)*CONC4I(tn,IKLE(3,i))
   
   end do
 
   tmp = 0.0

     do i =1,NELEM

       do ii=1,4
       !tmp(ii) = areaN(i) * HCT(i) * SCCT(ii,i)	
       tmp(ii) = areaN(i) * HCT(i)	
       end do

     susmass = tmp(1) + tmp(2) + tmp(3) + tmp(4)

     end do
 
   return
   end   
! #####################################################################################################################################    
    
 subroutine bedcon(ntims,npp,hdx,values,vol)
!   ============================= DO VALIDATION CHECKS ===============================================
!   == 1. CONSERVATION OF SEDIMENT IN BED FOR EACH TIME AND INTEGRATE BED AREA IN TRANSECT USE COMPOSITE TRAPEZIUM RULE FOR SIMPLICITY

   implicit none

   integer, intent(in) :: npp, ntims
   real, intent(in)    :: hdx, values(npp)
   real, intent(out)   :: vol

   integer             :: i
   real                :: delvol
 
  vol    = 0.0
  delvol = 0.0
    
  do i=1,(npp-1)
  delvol    = hdx*(values(i)+values(i+1))
  vol       = vol + delvol
  end do
    
  return

  end
 
! ###############################################################################################
 
   subroutine profvals(spt,npp,xpp,ypv,valu,var) 
    
   use m_selinf 
    
   implicit none 
    
   integer, intent(in)           :: npp, spt 
   character*1, intent(in)       :: var
   real, intent(in)  :: xpp(npp), ypv
   real, intent(out) :: valu(npp)
   integer                       :: i, ii
   real                          :: areaT, wA, wB, wC
   logical                       :: INPOLY

   valu = 0.0
   
! Get values at profile points:
    do ii=1,npp
      i = 0
      do 
        i = i + 1

        if (INPOLY(xpp(ii),ypv,XI(IKLE(1:3,i)),YI(IKLE(1:3,i)),3)) then
        ! Compute weights for interpolation using the "shoelace" formula to compute triangle areas:

        areaT = 0.5*abs(   XI(IKLE(1,i))*YI(IKLE(2,i)) + XI(IKLE(2,i))*YI(IKLE(3,i)) + &
                           XI(IKLE(3,i))*YI(IKLE(1,i)) - XI(IKLE(2,i))*YI(IKLE(1,i)) - &
                           XI(IKLE(3,i))*YI(IKLE(2,i)) - XI(IKLE(1,i))*YI(IKLE(3,i))   )

        wA    = 0.5*abs(   xpp(ii)*YI(IKLE(2,i)) + XI(IKLE(2,i))*YI(IKLE(3,i)) + &
                           XI(IKLE(3,i))*ypv     - XI(IKLE(2,i))*ypv           - &
                           XI(IKLE(3,i))*YI(IKLE(2,i)) - xpp(ii)*YI(IKLE(3,i))   ) / areaT

        wB     = 0.5*abs(  XI(IKLE(1,i))*ypv + xpp(ii)*YI(IKLE(3,i))           + &
                           XI(IKLE(3,i))*YI(IKLE(1,i)) - xpp(ii)*YI(IKLE(1,i)) - &
                           XI(IKLE(3,i))*ypv - XI(IKLE(1,i))*YI(IKLE(3,i))       ) / areaT

        wC     = 0.5*abs(  XI(IKLE(1,i))*YI(IKLE(2,i)) + XI(IKLE(2,i))*ypv     + &
                           xpp(ii)*YI(IKLE(1,i)) - XI(IKLE(2,i))*YI(IKLE(1,i)) - &
                           xpp(ii)*YI(IKLE(2,i)) - XI(IKLE(1,i))*ypv             ) / areaT

        exit

        else if (i==NELEM) then
        print *, 'profile points ', xpp(ii), ypv , ' NOT on mesh - TERMINATING'; STOP
        end if

      end do
          
   ! Compute value of the variable at that profile point for a specific time:
     if (var=="B") then
     valu(ii) = wA*BI(spt,IKLE(1,i)) + wB*BI(spt,IKLE(2,i)) + wC*BI(spt,IKLE(3,i))
     else if (var=="Q") then
     valu(ii) = wA*QSI(spt,IKLE(1,i)) + wB*QSI(spt,IKLE(2,i)) + wC*QSI(spt,IKLE(3,i))
     end if
      
    end do
     
    return
    end
! ##############################################################################################################   

subroutine extractts(nt,xpv,ypv,valu,var) 
    
   use m_selinf 
    
   implicit none 
    
   integer, intent(in)           :: nt 
   character*2, intent(in)       :: var
   real, intent(in)  :: xpv, ypv
   real, intent(out) :: valu(nt)

   integer                       :: i, ii
   real                          :: areaT, wA, wB, wC
   logical                       :: INPOLY

   valu = 0.0
   
! Get values at the spatial location:

      do 
        i = i + 1

        if (INPOLY(xpv,ypv,XI(IKLE(1:3,i)),YI(IKLE(1:3,i)),3)) then
        ! Compute weights for interpolation using the "shoelace" formula to compute triangle areas:

        areaT = 0.5*abs(   XI(IKLE(1,i))*YI(IKLE(2,i)) + XI(IKLE(2,i))*YI(IKLE(3,i)) + &
                           XI(IKLE(3,i))*YI(IKLE(1,i)) - XI(IKLE(2,i))*YI(IKLE(1,i)) - &
                           XI(IKLE(3,i))*YI(IKLE(2,i)) - XI(IKLE(1,i))*YI(IKLE(3,i))   )

        wA    = 0.5*abs(   xpv*YI(IKLE(2,i)) + XI(IKLE(2,i))*YI(IKLE(3,i)) + &
                           XI(IKLE(3,i))*ypv     - XI(IKLE(2,i))*ypv           - &
                           XI(IKLE(3,i))*YI(IKLE(2,i)) - xpv*YI(IKLE(3,i))   ) / areaT

        wB     = 0.5*abs(  XI(IKLE(1,i))*ypv + xpv*YI(IKLE(3,i))           + &
                           XI(IKLE(3,i))*YI(IKLE(1,i)) - xpv*YI(IKLE(1,i)) - &
                           XI(IKLE(3,i))*ypv - XI(IKLE(1,i))*YI(IKLE(3,i))       ) / areaT

        wC     = 0.5*abs(  XI(IKLE(1,i))*YI(IKLE(2,i)) + XI(IKLE(2,i))*ypv     + &
                           xpv*YI(IKLE(1,i)) - XI(IKLE(2,i))*YI(IKLE(1,i)) - &
                           xpv*YI(IKLE(2,i)) - XI(IKLE(1,i))*ypv             ) / areaT

        exit
        else if (i==NELEM) then
        print *, 'Time series point ', xpv, ypv , ' NOT on mesh - TERMINATING'; STOP
        end if

      end do
          
   ! Compute value of the variable at that profile point for time series:
     do ii=1,nt

       if (var=="C1") then
       valu(ii) = wA*CONC1I(ii,IKLE(1,i)) + wB*CONC2I(ii,IKLE(2,i)) + wC*CONC3I(ii,IKLE(3,i))
       else if (var=="C2") then
       valu(ii) = wA*CONC1I(ii,IKLE(1,i)) + wB*CONC2I(ii,IKLE(2,i)) + wC*CONC3I(ii,IKLE(3,i))
       else if (var=="C3") then
       valu(ii) = wA*CONC1I(ii,IKLE(1,i)) + wB*CONC2I(ii,IKLE(2,i)) + wC*CONC3I(ii,IKLE(3,i))
       else if (var=="C4") then
       valu(ii) = wA*CONC1I(ii,IKLE(1,i)) + wB*CONC2I(ii,IKLE(2,i)) + wC*CONC3I(ii,IKLE(3,i))
       end if
      
   end do
     
    return

    end
! ############################################################################################################## 

   subroutine L2error(lim2,valun,valan,L2err)
    
   implicit none
    
   integer, intent(in)           :: lim2
   real, intent(in)              :: valun(lim2), valan(lim2)
   real, intent(out)             :: L2err
    
   integer                       :: i
   real                          :: sumnum, num, sumdenom, denom
    
       sumnum     = 0.0
       sumdenom   = 0.0

       do i=1,lim2 
         num      = (valun(i) - valan(i)) * (valun(i)-valan(i))
         denom    = valan(i) * valan(i)
         sumnum   = sumnum + num; sumdenom = sumdenom + denom
       end do

     L2err        = sumnum / sumdenom     
     
     return
     
     end

! ##########################################################################################
    
   subroutine tricentroid(x1,y1,x2,y2,x3,y3,xc,yc)
    
!  Subroutine to compute the centroid of a triangle
    
   implicit none
    
   real, intent(in)  :: x1, y1, x2, y2, x3, y3
   real, intent(out) :: xc, yc
    
   xc = (x1+x2+x3)/3.0; yc = (y1+y2+y3)/3.0
     
   return
     
   end
    
 ! ##########################################################################################
    

! ================================= THIRD PARTY ROUTINES BELOW HERE ==========================================================    
    

LOGICAL FUNCTION INPOLY( X , Y , XSOM , YSOM , NSOM )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INDICATES IF A POINT WITH COORDINATES X AND Y IS
!+                IN A POLYGON WITH GIVEN VERTICES.
!code
!+    PRINCIPLE: TAKES HALF A LINE STARTING FROM THE POINT AND COUNTS THE
!+               NUMBER OF TIMES IT INTERSECTS WITH THE POLYGON
!+
!+    ALSO WORKS IF THE POLYGON IS NOT CONVEX
!+
!+    INTERSECTIONS ARE IDENTIFIED USING THE LINES PARAMETRIC EQUATIONS :
!+
!+
!+    X + A * MU = XDEP + (XARR-XDEP) * LAMBDA
!+    Y + B * MU = YDEP + (YARR-YDEP) * LAMBDA
!+
!+    THE HALF-LINE IS CHARACTERISED BY THE CHOICE OF A AND B, AND THE
!+    SIGN OF MU. THERE IS INTERSECTION IF MU > 0 AND 0 < LAMBDA < 1
!
!warning  THE POLYGON VERTICES MUST BE DISTINCT (NO DUPLICATE NODES)
!
!history  E. DAVID (LHF)
!+
!+
!+   ORIGINAL IDEA AND CODE
!
!history  J.-M. HERVOUET (LNH)
!+        18/06/96
!+        V5P2
!+
!
!history  JEAN-PHILIPPE RENAUD (CSN BRISTOL)
!+        27/07/99
!+
!+   CORRECTION FOR A SPECIAL CASE
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NSOM           |-->| NUMBER OF APICES OF POLYGON
!| X              |-->| ABSCISSA OF POINT
!| Y              |-->| ORDINATE OF POINT
!| XSOM           |-->| ABSCISSAE OF POLYGON APICES
!| YSOM           |-->| ORDINATES OF POLYGON APICES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSOM
      real, INTENT(IN) :: X,Y
      real, INTENT(IN) :: XSOM(NSOM),YSOM(NSOM)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,NSECT
!
      real A,B,ANGLE,XDEP,YDEP,XARR,YARR,DET,MU,LAMBDA,EPS
!
      INTRINSIC COS,SIN,ABS,MOD
!
!-----------------------------------------------------------------------
!
      EPS = 1.D-9
      ANGLE = -1.0
!
! CHOOSES A AND B SUCH AS TO AVOID SPECIAL CASES
!
1000  CONTINUE
      ANGLE = ANGLE + 1.0
      IF(ANGLE.GT.360.0) THEN
!       SPECIAL CASE OF A POINT ON THE CONTOUR
        INPOLY=.TRUE.
        RETURN
      ENDIF
      A = COS(ANGLE*3.141592653/180.0)
      B = SIN(ANGLE*3.141592653/180.0)
      NSECT=0
!
! LOOP ON ALL THE SEGMENTS OF THE POLYGON
!
      DO 10 N=1,NSOM
!
!     DEP : 1ST POINT OF THE SEGMENT    ARR : 2ND POINT
!
      XDEP=XSOM(N)
      YDEP=YSOM(N)
      IF(N.LT.NSOM) THEN
        XARR=XSOM(N+1)
        YARR=YSOM(N+1)
      ELSE
        XARR=XSOM(1)
        YARR=YSOM(1)
      ENDIF
!
!     CASE WHERE TWO SUCCESSIVE POINTS ARE DUPLICATES
!
      IF(ABS(XDEP-XARR)+ABS(YDEP-YARR).LT.EPS) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'INPOLY : POINTS CONFONDUS DANS LE POLYGONE'
          WRITE(LU,*) 'AU POINT DE COORDONNEES : ',XDEP,'  ET  ',YDEP
          WRITE(LU,*) 'DE NUMERO ',N
          IF(N.EQ.NSOM) THEN
            WRITE(LU,*) 'LE DERNIER POINT NE DOIT PAS ETRE EGAL AU'
            WRITE(LU,*) 'PREMIER (POUR UN TRIANGLE, PAR EXEMPLE,'
            WRITE(LU,*) 'IL FAUT DONNER TROIS POINTS ET NON QUATRE)'
          ENDIF
          WRITE(LU,*) 'INPOLY EST PROBABLEMENT CALLED BY FILPOL'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INPOLY: SUPERIMPOSED POINTS IN THE POLYGON'
          WRITE(LU,*) 'AT POINT: ',XDEP,'  AND  ',YDEP,' WITH NUMBER ',N
          IF(N.EQ.NSOM) THEN
            WRITE(LU,*) 'THE LAST POINT MUST NOT BE EQUAL TO THE FIRST'
            WRITE(LU,*) 'FOR EXAMPLE, GIVE 3 POINTS FOR A TRIANGLE'
          ENDIF
          WRITE(LU,*) 'INPOLY IS PROBABLY CALLED BY FILPOL'
        ENDIF
        WRITE(LU,*) ' '
        WRITE(LU,*) ' '
        STOP
      ENDIF
!
!     CASE WHERE THE POINT IS A VERTEX
!     (THE GENERAL ALGORITHM WOULD DEFINE IT AS EXTERNAL WITH 2 INTERSECTIONS)
!
      IF(ABS(X-XDEP).LE.EPS.AND.ABS(Y-YDEP).LE.EPS) THEN
        NSECT=1
        GO TO 2000
      ENDIF
!
!     DETERMINANT OF THE KRAMER SYSTEM
!
      DET = A*(YDEP-YARR)-B*(XDEP-XARR)
      IF(ABS(DET).LT.EPS) GO TO 1000
!
      MU     = ( (XDEP-X)*(YDEP-YARR)-(YDEP-Y)*(XDEP-XARR) ) / DET
      LAMBDA = (    A    *(YDEP-Y   )-    B   *(XDEP-X   ) ) / DET
!
!-------------------------------------------------------
! JP RENAUD (CSN BRISTOL) CORRECTION TO AVOID THAT THE INTERSECTION
! POINT BE ONE OF THE VRTICES
!
! IF THE INTERSECTION POINT IS A VERTEX, INCREASES THE ANGLE
! OTHERWISE THE POINT WOULD BE COUNTED TWICE INSTEAD OF JUST ONCE
!
      IF ((ABS(X+A*MU-XDEP).LE.EPS.AND.ABS(Y+B*MU-YDEP).LE.EPS).OR. &
      (ABS(X+A*MU-XARR).LE.EPS.AND.ABS(Y+B*MU-YARR).LE.EPS)) GOTO 1000
!
! END OF JP RENAUD CORRECTION
!-------------------------------------------------------
!
      IF(MU.GE.-EPS.AND.LAMBDA.GE.-EPS.AND.LAMBDA.LE.1.0+EPS) THEN
        NSECT=NSECT+1
      ENDIF
!
10    CONTINUE
!
2000  CONTINUE
!
      INPOLY=(MOD(NSECT,2).EQ.1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

! ######################################################################################

 SUBROUTINE ratint(xa,ya,n,x,y)

      implicit none

      integer, intent(in)           :: n
      real, intent(in)              :: x, xa(n), ya(n)
      real, intent(out)             :: y
      
      integer                       :: i,m,ns
      real, parameter   :: TINY = 1E-25
      real              :: dy, dd,h,hh,t,w,c(n),d(n)

      ns=1
      hh=abs(x-xa(1))
      do i=1,n
        h=abs(x-xa(i))
        if (h==0.0)then
          y=ya(i)
          dy=0.0
          return
        else if (h<hh) then
          ns=i
          hh=h
        endif
        c(i)=ya(i)
        d(i)=ya(i)+TINY
       end do
      y=ya(ns)
      ns=ns-1
      do m=1,n-1
        do i=1,n-m
          w=c(i+1)-d(i)
          h=xa(i+m)-x
          t=(xa(i)-x)*d(i)/h
          dd=t-c(i+1)
          if(dd==0.0) stop 'failure in ratint'
          dd=w/dd
          d(i)=c(i+1)*dd
          c(i)=t*dd
         end do
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          ns=ns-1
        endif
        y=y+dy
      end do
      return
      END

! #########################################################################################

subroutine polint(xa,ya,n,x,y)

      implicit none

      integer, intent(in) :: n
      real, intent(in)    :: x,xa(n),ya(n)
      real, intent(out)   :: y

      integer             :: i,m,ns
      real                :: den,dif,dift,ho,hp,w,c(n),d(n),dy
      ns=1
      dif=abs(x-xa(1))
      do i=1,n
        dift=abs(x-xa(i))
        if (dift<dif) then
          ns=i
          dif=dift
        end if
        c(i)=ya(i)
        d(i)=ya(i)
      end do
      y=ya(ns)
      ns=ns-1
     
      do m=1,n-1
        do i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den==0.0) stop 'failure in polint'
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
         end do
        if (2*ns<n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
      end do

      return
      END

!! ###################################################################

! finds the index, j, of x in array xx(n). assuming xx is sorted, x lies
! between xx(j) and xx(j+1) -- from NUMERICAL RECIPES IN FORTRAN 77  
      subroutine locate(xx,n,x,j)
      integer, intent(in) :: n
      real, intent(in) :: xx(n), x
      integer, intent(out) :: j
      integer :: jl, ju, jm
      jl=0
      ju=n+1
      do while(ju-jl>1)
         jm=(ju+jl)/2
         if((xx(n)>xx(1)).eqv.(x>xx(jm)))then
            jl=jm
         else
            ju=jm
         end if
      end do
      j=jl
      end subroutine locate



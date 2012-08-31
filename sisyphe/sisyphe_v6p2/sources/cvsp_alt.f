!        ************************************************************
         DOUBLE PRECISION FUNCTION CVSP_ALT
!        ************************************************************
!
     &(J, FORMULA)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/10/2011
!***********************************************************************
!
!brief    CALCULATES A DYNAMIC ACTIVE LAYER THICKNESS
!+        ACCORDING TO 1 OF A COUPLE OF FORMULAS
!
!
!history  UWE MERKEL
!+        2011-07-20
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!| FORMULA        |<--| WHICH FORMULA TO USE TO CALCULATE THE ALT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE BIEF_DEF, ONLY: IPID, NCSIZE
      USE BIEF
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE

      INTEGER,          INTENT(IN)    :: J
      INTEGER,          INTENT(IN)    :: FORMULA

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      integer  I,K
      doubleprecision rho, rho_s, g, d50, d90, dmax, tauC, tauB, D,DSTAR
      doubleprecision A1, A2, A3, A4, A5, A6, RHOCR, pon, summe
      logical db

      doubleprecision AT
      integer LLT, LTT, JG

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !! ATTENTION !!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !! EXPECTS GRAIN CLASSES TO BE SORTED IN ASCENDING ORDER !!!
        !! unlike SISYPHE!!!
        !! Improve it!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      ! check ascending order of classes d50
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


             do I=1,NSICLA-1
                if (FDM(I).ge.FDM(I+1)) then
                    PRINT*, 'STOPPING!!!! GRAIN CLASSES HAVE TO BE',
     &                 ' IN ASCENDING ORDER!!! FOR DYNAMIC ALT'
                    call plante(1)
                endif
             enddo


      ! Basics
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

         !| VCE            |-->| WATER VISCOSITY
         !| XMVE           |-->| FLUID DENSITY
         !| XMVS           |-->| SEDIMENT DENSITY

            g = GRAV
            rho = XMVE
            rho_s = XMVS
            pon = XKV



      ! Characteristic grain diameters for surface
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            !D50 - raw aproximation over Layer 1 (actually it should be solved recurssive)
                d50 = 0.D0
             do I=1,NSICLA
                d50 =  d50 + FDM(I)*AVAIL(J,1,I)
             enddo
                !d50 = ACLADM%R(J)

            !DMAX - First approximation
             do I=1,NSICLA
                if (AVAIL(J,1,I) >= 0.01D0) dmax = FDM(NSICLA)
             enddo

            !D90 - only first approximation! what if FDM not stetig!?
                summe = AVAIL(J,1,1)
                d90 = 0.D0
             do I=2,NSICLA
                summe = AVAIL(J,1,I) + summe
                if ((summe.GE.0.9D0).AND.(d90.EQ.0.D0)) then
                    d90 = (0.9D0 - (summe-AVAIL(J,1,I)))/AVAIL(J,1,I)*
     &                (FDM(I)-FDM(I-1)) + FDM(I-1)
                endif
             enddo


            !????
            d = d50


      ! Shear parameters
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ! Here are enough possibilities for improvement

             ! Non-dimension particle parameter
             ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             DSTAR = d50*((XMVS/XMVE-1.D0)*g/(VCE**2))**(1./3.)

             ! Shields parameter
             ! ~~~~~~~~~~~~~~~~~
             if (DSTAR <= 4.D0) THEN
                RHOCR = 0.24D0*DSTAR**(-1.D0)
             elseif (DSTAR <= 10.D0)THEN
                RHOCR = 0.14D0*DSTAR**(-0.64D0)
             elseif (DSTAR <= 20.D0)THEN
                RHOCR = 0.04D0*DSTAR**(-0.1D0)
             elseif (DSTAR <= 150.D0)THEN
                RHOCR = 0.013D0*DSTAR**(0.29D0)
             else
                RHOCR = 0.055D0
             endif

          tauC = RHOCR*((XMVS-XMVE)*g*d50)
          tauB = TOB%R(J)




      ! New Active layer Thickness
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      !if (Formula == 1) then
         !Hunziker & Günther

            CVSP_ALT = 5.D0 * dmax
            A1 = CVSP_ALT
!            print*,'a1',a1,dmax

      !else if (Formula == 2) then
         !Fredsoe & Deigaard 1992
            CVSP_ALT = 2.D0 * tauB / (g*(rho_s-rho))
     &             / tan(phised/180.0D0*PI) / (1.D0-pon)
            A2 = CVSP_ALT
!         print*,'a2',a2,CVSP_ALT,taub,g,rho_s,rho,phised,pi,pon

      !else if (Formula == 3) then
         !van RIJN 1993

            if(taub.LT.tauc) then
            CVSP_ALT = 0.D0
            else
            CVSP_ALT = 0.3D0*(DSTAR**0.7D0)*((tauB-tauC)/tauC)**0.5*d50
            endif

            A3 = CVSP_ALT
!            print*,'a3',a3,dstar,taub,tauc,d50

      !else if (Formula == 4) then
         !Wong 2006

            if((tauB/(rho_s-rho)/g/d50).LT.0.0549) then
                CVSP_ALT = 0.D0
            else
                CVSP_ALT=5.0D0*d50*((tauB/(rho_s-rho)/g/d50)
     &                   -0.0549D0)**0.56D0
            endif
            A4 = CVSP_ALT
!          print*,'a4',a4,d50,taub,rho_s,rho,g,d50

      !else if (Formula == 5) then
         !Malcherek 2003

            CVSP_ALT = d90 / (1.D0-pon) * max(1.D0,(tauB/tauC))
            A5 = CVSP_ALT
!           print*,'a5',a5,d90,pon,taub,tauc


      !if (Formula == 6) then
         !Sisyphe

            CVSP_ALT = 3.D0 * d50
            A6 = CVSP_ALT
!           print*,'a6',a6,d50

      !else if (Formula == 0) then
         !constant from CAS file

            CVSP_ALT = ELAY0

      !else if (Formula == 6) then
         ! Man könnte hier die maximale Erosions / sedimentationsstärke des vergangenen Zeitschrittes verarbeiten
         ! Todo

            !CVSP_ALT = ????


      !endif


      if (Formula == 0) CVSP_ALT = ELAY0
      if (Formula == 1) CVSP_ALT = A1
      if (Formula == 2) CVSP_ALT = A2
      if (Formula == 3) CVSP_ALT = A3
      if (Formula == 4) CVSP_ALT = A4
      if (Formula == 5) CVSP_ALT = A5
      if (Formula == 6) CVSP_ALT = A6

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !CHECK FOR ERRORS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      if ((CVSP_ALT.le.FDM(1))) then
            CVSP_ALT = FDM(1)
      endif

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !DEBUG PRINTOUT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
           LLT= LT-1+PERCOU
           LTT=(LLT/LEOPR)*LEOPR

             JG = j
             if (ncsize > 1) JG = mesh%knolg%I(J)

        if (db(JG,0).eqv..true.) THEN

             AT = DT*LT/PERCOU

       !   write (88,'(I6,1X,13(G15.8,1X))')
       !&      JG,AT,ES(J,1),D50,D90,DMAX,tauB,tauC,
       !&      A1,A2,A3,A4,A5,A6      !,DSTAR, rho_s, rho, g
       !&      ,pon,DSTAR,RHOCR, phised, tan(phised/180.0D0*PI)

        endif


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!

        RETURN
      END FUNCTION CVSP_ALT


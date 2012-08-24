!                       *****************
                        SUBROUTINE CVSP_WRITE_PROFILE()
!                       *****************
!
!***********************************************************************
! SISYPHE   V6P2                                   30/07/2012
!***********************************************************************
!
!brief    CVSP_WRITE_PROFILE
!
!history  UWE MERKEL
!+        2011-07-20
!
!history  PAT (Pablo Tassi)
!+        2012-08-24
!
! add WRITE(LU,*)
! + secure programming
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| -              |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE CVSP_OUTPUTFILES
      USE DECLARATIONS_SISYPHE

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      Integer I,J,K
      double precision bsum, sumerr, AT

       WRITE(LU,*) 'CVSP_WRITE_PROFILE on CPU ', IPID

    !--------------------------------------------
    !CHOOSE WHAT YOU WANT TO HAVE IN VSPRES
      ! Number of printout variables
      URBLOC%N = 3+NSICLA ! NumVarUR3D2RES
    !--------------------------------------------

        AT = DT*LT/PERCOU

      do I= 0, PRO_MAX_MAX-1
      do J= 1, NPOIN

            !D50
            Bsum = 0.D0
            sumerr = 1.D0
            do k=1,NSICLA
                bsum = FDM(k)*PRO_F(J,I+1,K) + bsum
                sumerr = sumerr - PRO_F(J,I+1,K)
            enddo


        if ((I+1).le.PRO_MAX(J)) then
            VSP_ERROR%R(j+I*NPOIN) = sumerr
            VSP_D50%R(j+I*NPOIN) = bsum
            VSP_D%R(j+I*NPOIN) = PRO_D(J,I+1,1)
        else
            VSP_ERROR%R(j+I*NPOIN) = VSP_ERROR%R(j+(I-1)*NPOIN)
            VSP_D50%R(j+I*NPOIN)   = VSP_D50%R(j+(I-1)*NPOIN)
            VSP_D%R(j+I*NPOIN)     = VSP_D%R(j+(I-1)*NPOIN)
        endif


          do K= 1, NSICLA
            if ((I+1).le.PRO_MAX(J)) then
                VSP_FRA(K)%R(j+I*NPOIN) = PRO_F(J,I+1,K)
            else
                VSP_FRA(K)%R(j+I*NPOIN) = PRO_F(J,PRO_MAX(J),K)
            endif
          enddo

      enddo
      enddo


    !--------------------------------------------
    !CHOOSE WHAT YOU WANT TO HAVE IN 2DHYD_as_3D
      ! Number of printout variables
      URBLOC2dHYD%N = NumVar2dHYD
    !--------------------------------------------
      do J= 1, NPOIN

        Ur2dHYD(1)%R(J) = ZF%R(J)
        Ur2dHYD(1)%R(J+NPOIN) = Z%R(J)

        Ur2dHYD(2)%R(J) = U2D%R(J)
        Ur2dHYD(2)%R(J+NPOIN) = Ur2dHYD(2)%R(J)

        Ur2dHYD(3)%R(J) = V2D%R(J)
        Ur2dHYD(3)%R(J+NPOIN) = Ur2dHYD(3)%R(J)

        Ur2dHYD(4)%R(J) = 0.D0
        Ur2dHYD(4)%R(J+NPOIN) = Ur2dHYD(4)%R(J)

        Ur2dHYD(5)%R(J) = (U2D%R(J)**2.D0 + V2D%R(J)**2.D0)**0.5D0
        Ur2dHYD(5)%R(J+NPOIN) = Ur2dHYD(5)%R(J)

        Ur2dHYD(6)%R(J) = TOB%R(J)
        Ur2dHYD(6)%R(J+NPOIN) = 0.D0

      enddo




    !--------------------------------------------
      ! Point To BIEF Objects that will be printed to
            ! Make your own decission here
            URBLOC3D%ADR(1)%P => VSP_D
            URBLOC3D%ADR(2)%P => VSP_D50
            URBLOC3D%ADR(3)%P => VSP_ERROR
                do k = 1, NSICLA
            URBLOC3D%ADR(3+k)%P => VSP_FRA(k)
                enddo

      ! Point To BIEF Objects that will be printed to 2dHYD_as_3D file
            ! Make your own decission here
            URBLOC2dHYD%ADR(1)%P => Ur2dHYD(1)
            URBLOC2dHYD%ADR(2)%P => Ur2dHYD(2)
            URBLOC2dHYD%ADR(3)%P => Ur2dHYD(3)
            URBLOC2dHYD%ADR(4)%P => Ur2dHYD(4)
            URBLOC2dHYD%ADR(5)%P => Ur2dHYD(5)
            URBLOC2dHYD%ADR(6)%P => Ur2dHYD(6)


      USERPRINTCOUNT = USERPRINTCOUNT + 1  
! Add this to time because TECPLOT doesn't support multiple TIME STAMPS in one file.
      USERTIME= AT + USERPRINTCOUNT / 1.0D5



    !--------------------------------------------
    !WRITE TIME STEP TO    !VSP
      SELECT CASE (CP_FILES(3)%FMT)
        CASE ('SERAFIN ','SERAFIND')

          call WRITE_DATA_SERAFIN(CP_FILES(3)%LU,NumVarUR3D2RES
     &                            ,USERTIME,LT
     &                            ,UR3D_FILES_OUTVAR,URBLOC3D
     &                            ,CP_FILES(3)%FMT,VSP_FRA(1)%DIM1)

        CASE ('MED     ')


        CASE DEFAULT
          if(LNG.EQ.1) THEN
            WRITE(LU,*) 'WRITE_DATA : MAUVAIS FORMAT : ',CP_FILES(3)%FMT
          endif
          if(LNG.EQ.2) THEN
            WRITE(LU,*) 'WRITE_DATA: BAD FILE FORMAT : ',CP_FILES(3)%FMT
          endif
          call PLANTE(1)
      END SELECT


    !--------------------------------------------
    !WRITE TIME STEP TO   !2DHYD_as_3D
      SELECT CASE (CP_FILES(4)%FMT)
        CASE ('SERAFIN ','SERAFIND')
        !HYD
          call WRITE_DATA_SERAFIN(CP_FILES(4)%LU,NumVar2dHYD
     &                            ,USERTIME,LT
     &                            ,UR2dHYD_FILES_OUTVAR,URBLOC2dHYD
     &                            ,CP_FILES(4)%FMT,Ur2dHYD(1)%DIM1)

        CASE ('MED     ')

        CASE DEFAULT
          if(LNG.EQ.1) THEN
            WRITE(LU,*) 'WRITE_DATA : MAUVAIS FORMAT : ',CP_FILES(4)%FMT
          endif
          if(LNG.EQ.2) THEN
            WRITE(LU,*) 'WRITE_DATA: BAD FILE FORMAT : ',CP_FILES(4)%FMT
          endif
          call PLANTE(1)
      END SELECT




      RETURN
      END

      SUBROUTINE  MainNestor                     !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (   ts, dt_ts_sis, time, ELAY0, z_sis
!    &   , AVAIL, KNOLG, KNOGL, dzCL_sis )
     &   , AVAIL, KNOLG, dzCL_sis )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      ________________________________________________________________
!     |                                                                |
!     |        we assume that the sisyphe time step is constant  !     |
!     |_______________ ________________________________________________|
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor

      IMPLICIT NONE

      INTERFACE !------------------------------------------------------+
        SUBROUTINE Dig_by_Time                                         !
     &    (   A, F, dt_ts, z_sis, dzCL_sis                             !
     &      , AVAIL, ELAY0, time, KNOLG, m   )                         !
!    &      , AVAIL, ELAY0, time, KNOLG, KNOGL, m   )                  !
          USE m_TypeDefs_InterFace                                     !
          USE m_TypeDefs_Nestor                                        !
          USE m_Nestor , ONLY :  ParallelComputing, nGrainClass, ipid  !
     &                          , npoinGlobal                          !
                                                                       !
          IMPLICIT NONE                                                !
          TYPE(t_Action),INTENT(INOUT) :: A            ! Action        !
          TYPE(t_Field) ,INTENT(INOUT) :: F            ! Field         !
          REAL (KIND=8) ,INTENT(IN)    :: dt_ts        ! time-step-duration  [ s ]
          REAL (KIND=8) ,INTENT(IN)    :: z_sis(:)     ! bottom [ m+NN ] assumed-shape array
          REAL (KIND=8) ,INTENT(IN)    :: AVAIL(:,:,:) ! assumed-shape array
          TYPE( t_PointerToArrayOfReals )                              !
     &                  ,INTENT(INOUT) :: dzCL_sis(:)  !  bedload evolution per Class  [ m ]
          REAL (KIND=8) ,INTENT(IN)    :: ELAY0        ! activLayerThickness  [ m ]
          REAL (KIND=8) ,INTENT(IN)    :: time         ! time [ s ]    !
          INTEGER       ,INTENT(IN)    :: KNOLG(:)     ! index list: Local to Global node index
!         INTEGER       ,INTENT(IN)    :: KNOGL(:)     ! index list: Global to Local node index
          INTEGER       ,INTENT(IN)    :: m            ! number of Action
        END SUBROUTINE Dig_by_Time                                     !
      END INTERFACE !--------------------------------------------------+

      INTERFACE !------------------------------------------------------+
        SUBROUTINE Dig_by_Criterion                                    !
     &    (   A, F, dt_ts, z_sis, dzCL_sis                             !
!    &      , AVAIL, ELAY0, time, KNOLG, KNOGL, m   )                  !
     &      , AVAIL, ELAY0, time, KNOLG, m          )                  !
          USE m_TypeDefs_InterFace                                     !
          USE m_TypeDefs_Nestor                                        !
          USE m_Nestor , ONLY :  ParallelComputing, nGrainClass, ipid  !
     &                          , npoinGlobal                          !
                                                                       !
          IMPLICIT NONE                                                !
          TYPE(t_Action),INTENT(INOUT) :: A            ! Action        !
          TYPE(t_Field) ,INTENT(INOUT) :: F            ! Field         !
          REAL (KIND=8) ,INTENT(IN)    :: dt_ts        ! time-step-duration  [ s ]
          REAL (KIND=8) ,INTENT(IN)    :: z_sis(:)     ! bottom [ m+NN ] assumed-shape array
          REAL (KIND=8) ,INTENT(IN)    :: AVAIL(:,:,:) ! assumed-shape array
          TYPE( t_PointerToArrayOfReals )                              !
     &                  ,INTENT(INOUT) :: dzCL_sis(:)  !  bedload evolution per Class  [ m ]
          REAL (KIND=8) ,INTENT(IN)    :: ELAY0        ! activLayerThickness  [ m ]
          REAL (KIND=8) ,INTENT(IN)    :: time         ! time [ s ]    !
          INTEGER       ,INTENT(IN)    :: KNOLG(:)     ! index list: Local to Global node index
!         INTEGER       ,INTENT(IN)    :: KNOGL(:)     ! index list: Global to Local node index
          INTEGER       ,INTENT(IN)    :: m            ! number of Action
        END SUBROUTINE Dig_by_Criterion                                !
      END INTERFACE !--------------------------------------------------+


      INTEGER      , INTENT(IN)    :: ts            !  time-step
      REAL (KIND=8) ,INTENT(IN)    :: dt_ts_sis     !  time-step-duration  [ s ]
      REAL (KIND=8) ,INTENT(IN)    :: time          !  time [s]
      REAL (KIND=8) ,INTENT(IN)    :: ELAY0         !  activLayerThickness [ m ]
      REAL (KIND=8), INTENT(IN)    :: z_sis(:)      !  bottom [ m+NN ]  (assumed-shape array)
      REAL (KIND=8) ,INTENT(IN)    :: AVAIL(:,:,:)  !  assumed-shape array
      INTEGER       ,INTENT(IN)    :: KNOLG(:)      ! index list: Local to Global node index
!     INTEGER       ,INTENT(IN)    :: KNOGL(:)      ! index list: Global to Local node index
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_Sis(:)   !  bedload evolution per Class  [ m ]

      TYPE(t_String_Length) :: SRname ! name of current Subroutine
      TYPE(t_String_Length) :: ErrMsge! error message


      DOUBLE PRECISION     P_DSUM
      EXTERNAL             P_DSUM

      !------- local variables ---------------
      INTEGER              :: i, n, m, iMesh, iCL, status
      REAL (KIND=8)        :: dt_ts     ! time-step-duration respecting MorpholFactor   [ s ]
      REAL (KIND=8)        :: max_dz_dt

      !REAL (KIND=8), SAVE  :: mySum    = 0.0D0 ! debug

      !INTEGER, SAVE        :: countDump = 0            ! debug


!
663   FORMAT(' ?>',2(/,' ?>'))            ! 3 lines like "?>         "
      WRITE(6,*)'?>-------  SR MainNestor ------------------'
      SRname%s = "MainNestor"         ! subroutine name
      SRname%i =  11                   ! length of name string


      WRITE(6,*)'?> time_sis = ', time   ! debug test
      !WRITE(6,*)'?>  z_sis(1078) = ',z_sis(1078)
      !WRITE(6,*)'?>  z_sis(1086) = ',z_sis(1086)
      !WRITE(6,*)'?>  z_sis(1089) = ',z_sis(1089)
      !WRITE(6,*)'?>  z_sis(1083) = ',z_sis(1083)
      !WRITE(6,*)'?>  z_sis(1074) = ',z_sis(1074)
      !WRITE(6,*)'?>  z_sis(1057) = ',z_sis(1057)
      !WRITE(6,*)'?>  z_sis(1030) = ',z_sis(1030)

      WRITE(6,*)'?>   Restart in main = ',Restart  !debug

      IF( Restart ) THEN
        CALL ReadWriteRestart( time, 'read' )
        Restart = .FALSE.
      ENDIF



      DO m=1, nActions
        WRITE(6,'(" ?>   state of Action",I3," =",I3)') m, A(m)%State
!
        IF( A(m)%State == 9 ) CYCLE   !> Status of Action:  0 = not jet     active
                                      !                     1 = currently   active
                                      !                     2 = temporary inactive
                                      !                     9 = for ever  inactive

        IF(       time >= A(m)%TimeStart )THEN
!        IF(       time >= A(m)%TimeStart
!     &      .AND. time <= A(m)%TimeEnd   )THEN



          dt_ts = dt_ts_sis / MorpholFactor
!
          SELECT CASE( A(m)%ActionType )
!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   1   )  ! _________Dig_by_time______________________|
              WRITE(6,*)'?> CASE 1    Dig_by_time'

              n = A(m)%FieldDigID
              CALL Dig_by_Time(   A(m), F(n), dt_ts, z_sis
     &                          , dzCL_sis, AVAIL, ELAY0
     &                          , time, KNOLG, m          )
!    &                          , time, KNOLG, KNOGL, m    )

!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   3   )  ! _________Dig_by_criterion_________________|
              WRITE(6,*)'?> CASE 3    Dig_by_criterion'
              n = A(m)%FieldDigID

              CALL Dig_by_Criterion(   A(m), F(n), dt_ts, z_sis
     &                               , dzCL_sis, AVAIL, ELAY0
!    &                               , time, KNOLG, KNOGL, m    )
     &                               , time, KNOLG, m           )


!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   2   )  ! _________Dump_by_time_____________________|
              WRITE(6,*)'?> CASE 2    Dump_by_time'

              n = A(m)%FieldDumpID

              IF( .NOT. A(m)%DumpPlanar ) THEN
                IF( A(m)%FirstTimeActive )  THEN
                  A(m)%State = 1     ! 1 = Action currently active

                  A(m)%nts = INT(   (A(m)%TimeEnd - time) / dt_ts ) ! calculate number
                                           !> of time steps (nts) to fulfil the Action
                  IF( A(m)%nts < 1 ) THEN
                    ErrMsge%s( 1:29) = "period for this action is to "
                    ErrMsge%s(30:53) = "short!   action number: "
                    ErrMsge%i = 53
                    CALL ErrMsgAndStop( ErrMsge, m, SRname, ipid   )
                  ENDIF

                  A(m)%dzTot      = A(m)%DumpVolume / F(n)%Area        ! change of z to fulfil the Action
                  A(m)%dz_ts      = A(m)%dzTot / DBLE( A(m)%nts )      ! change of z per time step
                  A(m)%dzCL_ts(:) = A(m)%dz_ts * A(m)%GrainClass(:)    ! change of z per time step per
                                                                       !>grain CLass
                  A(m)%tsCount         = 1
                  A(m)%FirstTimeActive = .FALSE.
                ENDIF  !( FirstTimeActive )

                IF( A(m)%tsCount <=  A(m)%nts ) THEN
                  !----  dump it -------------------------------
                  !countDump = countDump + 1                            ! debug
                  !WRITE(6,*) 'countDump = ',countDump                  ! debug
                  DO iCL=1, nGrainClass
                    DO i=1, F(n)%nNodes
                      iMesh = F(n)%Node(i)     ! mesh index of field node
                      dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &                                         + A(m)%dzCL_ts(iCL)
                    ENDDO
                  ENDDO !----------------------------------------
                  !mySum = mySum + A(m)%dz_ts                           ! debug
                ENDIF

                !IF( A(m)%tsCount ==  A(m)%nts ) THEN                   ! debug
                !  WRITE(6,*) 'preset dump   volume= ', A(m)%DumpVolume ! debug
                !  WRITE(6,*) '       dumped volume= ', mySum*F(n)%Area ! debug
                !ENDIF                                                  ! debug

                A(m)%tsCount = A(m)%tsCount + 1
                IF( A(m)%tsCount  >  A(m)%nts ) THEN ! The action is over
                  A(m)%State = 9                     ! 9 = Action for ever inactive
                ENDIF

              ENDIF !( .NOT. DumpPlanar )
!--------------------------------------------------------
              IF( A(m)%DumpPlanar ) THEN
                IF( A(m)%FirstTimeActive ) THEN
                  A(m)%State = 1     ! 1 = Action currently active

                  ALLOCATE( F(n)%Z(    F(n)%nNodes ), stat=status)
                  ALLOCATE( F(n)%dZ(   F(n)%nNodes ), stat=status)
                  ALLOCATE( F(n)%refZ( F(n)%nNodes ), stat=status)
                  ALLOCATE( F(n)%km(   F(n)%nNodes ), stat=status)

                  A(m)%sumInput = 0.0_8

                  F(n)%dZ   = 999.9D0
                  DO i=1, F(n)%nNodes     ! set value in the Field-Structur
                    iMesh        = F(n)%Node(i)   ! mesh index of field node
                    F(n)%Z(i)    = z_sis( iMesh )
                  ENDDO
                  CALL Set_RefLevelByProfiles( F(n),A(m)%ReferezLevel) ! the result is F(n)%refZ(:)

                  CALL Calculate_PlanarLevel( F(n), A(m)%DumpVolume, 1 )    ! 1 => dump;  the result is F(n)%dz(:)
                  A(m)%nts = INT( (A(m)%TimeEnd - time) / dt_ts ) ! calculate number
                                         !> of time steps (nts) to fulfil the Action

                  max_dz_dt =  MAXVAL( F(n)%dz ) / DBLE( A(m)%nts )

                  IF( max_dz_dt  >  ELAY0 ) THEN
                    WRITE(6,663)
                    WRITE(6,'(" ?> max_dz = ",(G10.3)
     &                       ," ELAY0 = ",(G10.3))' ) max_dz_dt, ELAY0
                    ErrMsge%s = "dz_dt > ELAY0 "
                    ErrMsge%i = 14
                    CALL ErrMsgAndStop( ErrMsge, 0, SRname, ipid   )
                  ENDIF

                  A(m)%tsCount         = 1
                  A(m)%FirstTimeActive = .FALSE.
                ENDIF  !( FirstTimeActive )

                IF( A(m)%tsCount <=  A(m)%nts ) THEN
                  !  Before dumping we calc. the amount of sediment that was
                  !> transported during the last time step into the field.
                  DO iCL=1, nGrainClass       !> Only nodes below the planar
                    DO i=1, F(n)%nNodes       !> level are included for it.
                      iMesh = F(n)%Node(i)    !   mesh index of field node
                      IF( F(n)%dz(i) > 0.0_8 ) A(m)%sumInput
     &                  =    A(m)%sumInput
     &                     + dzCL_sis(iCL)%R(iMesh) * F(n)%NodeArea(i)
                    ENDDO
                  ENDDO

                  DO iCL=1, nGrainClass  !----- dump one time step -----
                    DO i=1, F(n)%nNodes
                      iMesh = F(n)%Node(i)     ! mesh index of field node
                      dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &                + A(m)%GrainClass(iCL) *F(n)%dz(i) /DBLE(A(m)%nts)
                    ENDDO
                  ENDDO !-----------------------------------------------

!                  DO iCL=1, nGrainClass                                ! debug
!                    DO i=1, F(n)%nNodes                                ! debug
!                      mySum = mySum  +  F(n)%dz(i) / DBLE( A(m)%nts )  ! debug
!     &                               *  A(m)%GrainClass(iCL)           ! debug
!     &                               *  F(n)%NodeArea(i)               ! debug
!                    ENDDO                                              ! debug
!                  ENDDO                                                ! debug

                ENDIF !( A(m)%tsCount <=  A(m)%nts )

                A(m)%tsCount = A(m)%tsCount + 1

                IF( A(m)%tsCount  >  A(m)%nts ) THEN ! The action is over
                  IF(ParallelComputing) THEN
                    !mySum         = P_DSUM(mySum)                      ! debug
                    A(m)%sumInput = P_DSUM(A(m)%sumInput)
                  ENDIF
                  WRITE(6,663)
                  WRITE(6,*)'?> Message to field: ',F(n)%name
                  WRITE(6,*)'?>       time Start: ',A(m)%TimeStart
                  WRITE(6,*)'?>       time      : ',time
                  WRITE(6,*)'?>   preset   dump   volume ='
     &                         ,A(m)%DumpVolume
                  !WRITE(6,*)'?>   de facto dumped volume =',mySum      ! debug
                  WRITE(6,*)'?>   sediment transported into'
                  WRITE(6,*)'?>     the field while dumping:   '
     &                         ,A(m)%sumInput


!                    mySum = 0.0_8                                ! debug
!                    DO i=1, F(n)%nNodes                          ! debug
!                      iMesh = F(n)%Node(i)                       ! debug
!                      IF( F(n)%dz(i) > 0.0_8 ) mySum             ! debug
!     &                     = mySum + F(n)%NodeArea(i)            ! debug
!                    ENDDO                                        ! debug
!                    WRITE(6,*)'?> touched area = ',mySum         ! debug


                  DEALLOCATE( F(n)%Z   , stat=status )
                  DEALLOCATE( F(n)%dZ  , stat=status )
                  DEALLOCATE( F(n)%refZ, stat=status )
                  DEALLOCATE( F(n)%km  , stat=status )
                  A(m)%State = 9                        ! 9 = Action for ever inactive
                ENDIF



              ENDIF !( A(m)%DumpPlanar )
!----------------------------------

              !WRITE(6,*) 'time   = ', time , A(m)%TimeEnd              ! debug

!
            CASE DEFAULT
               WRITE(6,*)'?>  CASE DEFault erreicht '
          END SELECT

        ENDIF !A(m)%TimeStart <= time <= A(m)%TimeEnd
      ENDDO !m=1, nActions

      IF( MOD(ts,GraphicOutputPeriod) == 0 )
     &  CALL ReadWriteRestart( time, 'write' )



      WRITE(6,*)'?>-------  SR MainNestor End --------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE MainNestor                  !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************

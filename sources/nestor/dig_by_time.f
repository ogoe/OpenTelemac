      SUBROUTINE  Dig_by_Time                    !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(   A, F, dt_ts, z_sis, dzCL_sis
     &  , AVAIL, ELAY0, time, KNOLG, m   )
!    &  , AVAIL, ELAY0, time, KNOLG, KNOGL, m   )

      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  ParallelComputing, nGrainClass, ipid
     &                      , npoinGlobal
      IMPLICIT NONE

      INTERFACE !------------------------------------------------------+
        SUBROUTINE Dump_by_Rate                                        !
     &    ( A, dt_ts, dzCL_sis )                                       !
          USE m_TypeDefs_InterFace                                     !
          USE m_TypeDefs_Nestor                                        !
          USE m_Nestor,ONLY : ParallelComputing, nGrainClass, F, ipid  !
                                                                       !
          IMPLICIT NONE                                                !
          TYPE(t_Action),INTENT(INOUT) :: A                            !
          REAL (KIND=8) ,INTENT(IN)    :: dt_ts       ! time-step-duration  [ s ]
          TYPE( t_PointerToArrayOfReals )                              !
     &                  ,INTENT(INOUT) :: dzCL_sis(:)                  !
        END SUBROUTINE Dump_by_Rate                                    !
      END INTERFACE !--------------------------------------------------+

      TYPE(t_Action),INTENT(INOUT) :: A
      TYPE(t_Field) ,INTENT(INOUT) :: F
      REAL (KIND=8), INTENT(IN)    :: dt_ts
      REAL (KIND=8), INTENT(IN)    :: z_sis(:)     ! bottom [m+NN] at time (assumed-shape array)
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_sis(:)
      REAL (KIND=8) ,INTENT(IN)    :: AVAIL(:,:,:) ! debug test!  assumed-shape array
      REAL (KIND=8) ,INTENT(IN)    :: ELAY0        !  activLayerThickness  [ m ]
      REAL (KIND=8) ,INTENT(IN)    :: time         !  time [s]
      INTEGER       ,INTENT(IN)    :: KNOLG(:)     ! index list: Local to Global node index
!      INTEGER       ,INTENT(IN)    :: KNOGL(:)     ! index list: Global to Local node index
      INTEGER       ,INTENT(IN)    :: m            ! number of Action

      DOUBLE PRECISION     P_DSUM
      INTEGER              P_ISUM
      EXTERNAL             P_DSUM, P_ISUM

      !------- local variables ---------------
      INTEGER            :: i, iCL, iMesh, status, nodeIndex
      INTEGER            :: nLessNodesToDig = 0
      REAL (KIND=8)      :: dzDig_ts, dzDig
      REAL (KIND=8)      :: dzEvo_sis
      !REAL (KIND=8)      :: x, y
      REAL (KIND=8)      :: remainingDepthToDig

      REAL (KIND=8)                       :: heap   !> total dug volume of all FieldNodes
      REAL (KIND=8),ALLOCATABLE                     !  during the current timestep
     &             ,SAVE ,DIMENSION   (:) :: heapCL !> total dug volume per grain class of all
      REAL (KIND=8),ALLOCATABLE                     !  FieldNodes during the current timestep
     &             ,SAVE ,DIMENSION   (:) :: layCL


      TYPE(t_String_Length) :: SRname ! name of current Subroutine
      TYPE(t_String_Length) :: ErrMsge

      WRITE(6,*)'?>-------  SR Dig_by_Time ------------------'
      SRname%s = "Dig_by_Time"         ! subroutine name
      SRname%i =  11                   ! length of name string

      IF( .NOT. ALLOCATED(layCL) ) ALLOCATE(  layCL( nGrainClass ))
      IF( .NOT. ALLOCATED(heapCL)) ALLOCATE( heapCL( nGrainClass ))

      !DO i=1, F%nNodes                                                        ! debug
      !    iMesh = F%Node(i)                                                   ! debug
      !    WRITE(*,*)'?>    dzCL_sis(1)%R(i)=', dzCL_sis(1)%R(iMesh)           ! debug
      !    WRITE(*,*)'?>                     ', dzCL_sis(2)%R(iMesh)           ! debug
      !    WRITE(*,*)'?>                     ', dzCL_sis(3)%R(iMesh)           ! debug
      !ENDDO                                                                   ! debug

      IF( .NOT. A%DigPlanar ) THEN
        !_________________________________________________________
        !                                                        /
        IF( A%FirstTimeActive )  THEN  !________________________/
          A%State = 1     ! 1 = Action currently active

          A%nts = INT(   (A%TimeEnd - time) / dt_ts ) ! calculate number
                                   !> of time steps (nts) to fulfill the Action
          IF( A%nts < 1 ) THEN
            ErrMsge%s( 1:29) = "period for this action is to "
            ErrMsge%s(30:53) = "short!   action number: "
            ErrMsge%i = 53
            CALL ErrMsgAndStop( ErrMsge, m, SRname, ipid   )
          ENDIF

          A%dzTot      = A%DigVolume / F%Area         ! change of z to fulfil the Action
          A%dz_ts      = A%dzTot / DBLE( A%nts )      ! change of z per time step

          A%tsCount         = 1
          A%FirstTimeActive = .FALSE.

          A%DumpVolume = 0.0_8                ! debug
                                          !_____________________
        ENDIF  !(IF A%FirstTimeActive )                         \
        !________________________________________________________\


        ! The point where Nestor is linked in the Sisyphe time loop we have
        ! the following situation:
        ! 1.) The active layer respectively the AVAIL array
        !     has the state of the previous time step.
        ! 2.) The bottom level (z_sis) has the state of the previous time step.
        ! 3.) The evolution per class (dzCL_sis) is already calculated
        !     for the current time step. But it's not mixed into the
        !     active layer yet.
        ! To know what sediment mixture the digger will grab, we combine
        ! the active layer and the evolution.

        dzDig = A%dz_ts

        heapCL(:) = 0.0_8 ! new time step => reset heap
        IF( A%tsCount <= A%nts  ) THEN

          DO i=1, F%nNodes  ! loop over FieldNodes
            iMesh = F%Node(i)

            !IF( .NOT. F%NodeToDig(i) ) CYCLE
            dzEvo_sis  =  0.0_8
            DO iCL=1, nGrainClass
              dzEvo_sis  =   dzEvo_sis + dzCL_sis(iCL)%R(iMesh)  !> Sum of evolution of all classes
                                                                 !  at node at current time step
              layCL(iCL) =   dzCL_sis(iCL)%R(iMesh)     !>  Thickness of evolution per class
     &                     + AVAIL(iMesh,1,iCL) * ELAY0 ! + thickness of class in active layer
                             !........................!----- convert fraction to thickness
            ENDDO

            IF( -dzDig + dzEvo_sis <= -ELAY0 ) Call ErrMsgAndStop2(
     &       "while Action  Dig_by_Time                               "
     &      ,56
     &      ,"reason: Digger exceeds active layer thickness           "
     &      ,56
     &      ,"        ==> increase time slot or reduce time step or..."
     &      ,56
     &      ,"occured in Action: ", 19, m, SRname, ipid      )


            layCL(:) = layCL(:) / (dzEvo_sis + ELAY0) !> Convert thickness to fraction




            DO iCL=1, nGrainClass                                    !> Digging happens here
              dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)      !> We calculate the mixture which
     &                                 - layCL(iCL) * dzDig          !  is left in the ground
            ENDDO

            heapCL(:) = heapCL(:) + layCL(:) * dzDig * F%NodeArea(i) !> During the current time step of all nodes
                                                                     !  we collect for each class the dug material
                                                                     !  and put it in heaps per class
          ENDDO  !  loop over FieldNodes

          A%tsCount = A%tsCount + 1

        ENDIF !( A%tsCount <= A%nts  )



        IF( A%FieldDumpID .GT. 0 ) THEN    !> Only if a valid dump field
                                           !  is linked to the action.
          ! Collect the total dug volume since the Action started
          ! and calculate the grain composition

          IF( ParallelComputing ) THEN
            DO iCL=1, nGrainClass
              heapCL(iCL) = P_DSUM( heapCL(iCL) )
            ENDDO
          ENDIF

          heap = SUM( heapCL(:) )  ! Total volume of dug material at current time step

          ! Add per class   volume of dug material and DumpVolume
          heapCL(:) = heapCL(:) + A%GrainClass(:) * A%DumpVolume
                                  !............................!--- convert fraction to volume

          A%DumpVolume = A%DumpVolume + heap

          IF( A%DumpVolume  <=  0.0_8  ) THEN
            A%GrainClass(:) = heapCL(:) / heap
          ELSE                !..............!------------- convert thickness to fraction
            A%GrainClass(:) = heapCL(:) / A%DumpVolume
          ENDIF               !......................!----- convert thickness to fraction

                   !IF(abs(A%DumpVolume - SUM(heapCL(:))) .GE. 0.00000001_8 ) THEN ! debug
                   !  WRITE(*,*)'?>  A%DumpVolume    =', A%DumpVolume              ! debug
                   !  WRITE(*,*)'?>  heap            =', heap                      ! debug
                   !  WRITE(*,*)'?>  A%GrainClass(:) =', A%GrainClass(:)           ! debug
                   !  !STOP                                                        ! debug
                   !ENDIF                                                          ! debug

          IF( A%DumpVolume  >  0.0_8  ) THEN
            CALL Dump_by_Rate( A, dt_ts, dzCL_sis )
          ENDIF

        ENDIF  !( A%FieldDumpID .GT. 0 )

        IF(       A%tsCount    >  A%nts
     &      .AND. A%DumpVolume <= 0.0_8  ) THEN
          A%State = 9         !  9 = for ever  inactive
        ENDIF

      ENDIF !( .NOT. DigPlanar )



      IF(  A%DigPlanar ) THEN   ! noch in Arbeit !!!!
         WRITE(*,*)'?>  sorry  option  planar is not coded jet' ! debug
         STOP
        !_________________________________________________________
        !                                                        /
        IF( A%FirstTimeActive )  THEN  !________________________/
          A%State = 1     ! 1 = Action currently active

          A%nts = INT(   (A%TimeEnd - time) / dt_ts ) ! calculate number
                                   !> of time steps (nts) to fulfil the Action
          IF( A%nts < 1 ) THEN
            ErrMsge%s( 1:29) = "period for this action is to "
            ErrMsge%s(30:53) = "short!   action number: "
            ErrMsge%i = 53
            CALL ErrMsgAndStop( ErrMsge, m, SRname, ipid   )
          ENDIF

          A%dzTot      = A%DigVolume / F%Area         ! change of z to fulfil the Action
          A%dz_ts      = A%dzTot / DBLE( A%nts )      ! change of z per time step

          A%tsCount         = 1
          A%FirstTimeActive = .FALSE.

          A%DumpVolume = 0.0_8                ! debug
                                          !_____________________
        ENDIF  !(IF A%FirstTimeActive )                         \
        !________________________________________________________\

!       :
!       :
!       :
!       :

      ENDIF !( DigPlanar )



      IF( A%State == 2 .OR. A%State == 9  ) THEN   !> 2 = temporary inactive
        DEALLOCATE( layCL )                        !  9 = for ever inactive
        DEALLOCATE( heapCL )
      ENDIF

      WRITE(6,*)'?>-------  SR Dig_by_Time END --------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE Dig_by_Time                 !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************

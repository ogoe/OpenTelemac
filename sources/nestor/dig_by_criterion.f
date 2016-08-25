      SUBROUTINE  Dig_by_Criterion               !********************************************
!***                                              ********************************************
!***                                              ********************************************
!     &(   A, F, dt_ts, z_sis, x_sis, y_sis, dzCL_sis
!     &  , AVAIL, ELAY0, time, KNOLG, KNOGL   )
!     &(   A, F, dt_ts, z_sis, dzCL_sis
!     &  , AVAIL, ELAY0, time, KNOLG, KNOGL, m   )
     &(   A, F, dt_ts, z_sis, dzCL_sis
     &  , AVAIL, ELAY0, time, KNOLG, m   )

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

      INTERFACE !------------------------------------------------------+
        SUBROUTINE CalcDigVolumeInRadius                               !
!     &    (  A, F, z_sis, KNOGL  )                                     !
     &    (  A, F, z_sis, KNOLG  )                                     !
          USE m_TypeDefs_Nestor                                        !
          USE m_Nestor,ONLY :  ParallelComputing, npoinGlobal, ipid    !
     &                        , npoin                                  !
          IMPLICIT NONE                                                !
          TYPE(t_Action),INTENT(INOUT) :: A                            !
          TYPE(t_Field) ,INTENT(INOUT) :: F                            !
          REAL (KIND=8), INTENT(IN)    :: z_sis(:)     ! z- coordinate [ m ]  (assumed-shape array)
!         INTEGER       ,INTENT(IN)    :: KNOGL(:)     ! index list: Global to Local node index
          INTEGER       ,INTENT(IN)    :: KNOLG(:)     ! index list: Local to Global node index
        END SUBROUTINE CalcDigVolumeInRadius                           !
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
!     INTEGER       ,INTENT(IN)    :: KNOGL(:)     ! index list: Global to Local node index
      INTEGER       ,INTENT(IN)    :: m            ! number of Action
      TYPE(t_String_Length) :: SRname ! name of current Subroutine

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



!663   FORMAT(' ?>',2(/,' ?>'))            ! 3 lines like "?>         "
      WRITE(6,*)'?>-------  SR Dig_by_Criterion -------------'
      SRname%s = "Dig_by_Criterion"    ! subroutine name
      SRname%i =  16                   ! length of name string


      IF( .NOT. ALLOCATED(layCL) ) ALLOCATE(  layCL( nGrainClass ))
      IF( .NOT. ALLOCATED(heapCL)) ALLOCATE( heapCL( nGrainClass ))
!      ________________________________________________________________
!     |                                                                |
      IF( A%FirstTimeActive ) THEN !___________________________________|
        A%State = 1     ! 1 = Action currently active
        A%DumpVolume = 0.0_8
        ALLOCATE( F%refZ(      F%nNodes ), stat=status)
        ALLOCATE( F%km(        F%nNodes ), stat=status)
        ALLOCATE( F%NodeToDig( F%nNodes ), stat=status)
        !ALLOCATE( layCL(    nGrainClass ), stat=status)
        !ALLOCATE( heapCL(   nGrainClass ), stat=status)

        !A%sumInput = 0.0_8  ! initialisation
        WRITE(6,*)'?> A%ReferezLevel = ',A%ReferezLevel  ! debug test

        CALL Set_RefLevelByProfiles( F, A%ReferezLevel) ! the result is F%refZ(:)

        A%tsCount = 0            ! counter of time steps while digger is working

        F%NodeToDig(:) = .FALSE. ! initialisation

        F%nNodeToDig   = 0       ! initialisation
        DO i=1, F%nNodes         ! mark nodes to dig
          iMesh = F%Node(i)      ! mesh index of field node
          IF( z_sis(iMesh) > (F%refZ(i) - A%CritDepth) ) THEN
            F%NodeToDig(i) = .TRUE.
            F%nNodeToDig   = F%nNodeToDig + 1
          ENDIF
        ENDDO

        !DO i=1, F%nNodes                                          ! debug test
        ! !IF( F%NodeToDig(i)) THEN                                ! debug test
        !    WRITE(*,*)' ?> F%refZ = ',F%X(i),F%Y(i),F%refZ(i)     ! debug test
        !  !ENDIF                                                  ! debug test
        !ENDDO                                                     ! debug test

        F%refZ(:) =  F%refZ(:) - A%DigDepth !> Set refZ to target digging level
        F%targZ   => F%refZ(:)              !> Because of what we did the line above
                                            !  we use a more meaningful name !

        !DO i=1, F%nNodes                                          ! debug test
        !  !IF( F%NodeToDig(i) ) THEN                              ! debug test
        !    WRITE(*,*)' ?> F%targZ = ',F%X(i),F%Y(i),F%refZ(i)    ! debug test
        !  !ENDIF                                                  ! debug test
        !ENDDO                                                     ! debug test



        IF( A%MinVolume > 0.0_8 ) THEN
!         CALL CalcDigVolumeInRadius( A, F, z_sis, KNOGL )  !> The result is F%NodeToDig(:)
          CALL CalcDigVolumeInRadius( A, F, z_sis, KNOLG )  !> The result is F%NodeToDig(:)
        ENDIF                                               !  and F%nNodeToDig

        IF( ParallelComputing ) THEN
          F%nNodeToDig = P_ISUM( F%nNodeToDig )
          WRITE(6,1234)' ?> DibyCr',ipid,'F%nNodeToDig  =',F%nNodeToDig! debug test
        ENDIF

        A%GrainClass(:) = 0.0_8
        A%DumpVolume    = 0.0_8

        A%FirstTimeActive = .FALSE.    !_______________________________
      ENDIF  ! FirstTimeActive                                         |
!     |________________________________________________________________|



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

      heapCL(:)       = 0.0_8              !> Initialisation must happen here AND again below
      nLessNodesToDig = 0
      IF( F%nNodeToDig > 0 ) THEN
        A%tsCount = A%tsCount + 1          !> Count time steps while digger is working
        dzDig_ts  = dt_ts * A%DigRate      !> Depth to dig during one time step
        heapCL(:) = 0.0_8
        DO i=1, F%nNodes

          IF( .NOT. F%NodeToDig(i) ) CYCLE

          dzDig = dzDig_ts
          iMesh = F%Node(i)

          dzEvo_sis = 0.0_8
          DO iCL=1, nGrainClass
            dzEvo_sis  =   dzEvo_sis + dzCL_sis(iCL)%R(iMesh)  !> Sum of evolution of all classes
                                                               !  at node at current time step
            layCL(iCL) =   dzCL_sis(iCL)%R(iMesh)     !>  Thickness of evolution per class
     &                   + AVAIL(iMesh,1,iCL) * ELAY0 ! + thickness of class in active layer
          ENDDO            !........................!----- convert fraction to thickness

          remainingDepthToDig = z_sis(iMesh) + dzEvo_sis - F%targZ(i)

          IF( remainingDepthToDig < dzDig ) THEN   !> dzDig is to big ==> digger would dig to deep.
            dzDig = remainingDepthToDig            !> Fit digging depth for this node and time step so
                                                   !  that the specified depth will be reached exactly.
            F%NodeToDig(i)  = .FALSE.              !> No more digging for this node.
            nLessNodesToDig = nLessNodesToDig + 1  !> count nodes that are no longer supposed to be dug.
            !---- write output -------------
            IF( ParallelComputing ) THEN
               nodeIndex = KNOLG(iMesh)
            ELSE
               nodeIndex = iMesh
            ENDIF
           !FORMAT (A4,X,2(G15.8,X),F8.3,X,I6,X,3(G15.8,X),A16)
9119        FORMAT (A5,X,3(G15.8,X),I8,X,F8.3,X,2(G15.8,X),A16)
            WRITE(6,9119) 'XdigX ', A%TimeStart, time   !> Write the action concerning data of this node to the log-file
     &        , F%NodeArea(i) * ( dzDig_ts * (A%tsCount-1) + dzDig ) !> For this period calc. dug volume of this node
     &        , nodeIndex, F%km(i), F%X(i), F%Y(i), F%Name           !> Write location of this node.


!     9120   FORMAT (A4,X,2(G15.8,X),F8.3,X,I6,X,3(G15.8,X),A16)  ! output DredgeSim style
!                 WRITE(6 , FMT = 9120)  '$%%$'                   ! output DredgeSim style
!          &         , A%TimeStart                                           !DS: time_to_real_seconds(old_time_to_observe(n)-initial_time)
!          &         , time                                                  !DS: time_to_real_seconds(act_time-initial_time)
!          &         , F%km(i)                                               !DS:
!          &         , nodeIndex                                             !DS: knolg(i)
!          &         , F%X(i)                                                !DS:
!          &         , F%Y(i)                                                !DS:
!          &         , F%NodeArea(i) * ( dzDig_ts * (A%tsCount-1) + dzDig )  !DS: node_total_volume
!          &         , F%Name


          ENDIF

          IF( -dzDig + dzEvo_sis <= -ELAY0 ) Call ErrMsgAndStop2(
     &     "while Action  Dig_by_Criterion                          ",56
     &    ,"reason: Digger exceeds active layer thickness           ",56
     &    ,"        ==> increase DigRate or reduce time step or ... ",56
     &    ,"occured in Action: ", 19, m, SRname, ipid      )


          IF( -dzDig + dzEvo_sis <= -ELAY0 ) CALL ErrMsgAndStop
     &    ( "At this time step Digger exceeds active layer thickness:"
     &     ,"  ==> reduce DigRate or time step!",   i,  56,  34   )

          layCL(:) = layCL(:) / (dzEvo_sis + ELAY0) !> Convert thickness to fraction


          DO iCL=1, nGrainClass   !>  Digging happens here
            dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)      !> We calculate the mixture which
     &                               - layCL(iCL) * dzDig          !  is left in the ground
          ENDDO
                                  !..............................!--- convert fraction to volume
          heapCL(:) = heapCL(:) + layCL(:) * dzDig * F%NodeArea(i) !> During the current time step of all nodes
                                                                   !  we collect for each class the dug material
                                                                   !  and put it in heaps per class
         ENDDO  ! nNodes

      ENDIF  ! F%nNodeToDig > 0


      !=================================================================
      !Organize  the dumping ===========================================
      !each time step we add the dug material per class to A%DumpVolume

      IF( ParallelComputing ) THEN
        DO iCL=1, nGrainClass
          heapCL(iCL) = P_DSUM( heapCL(iCL) )
        ENDDO
        nLessNodesToDig = P_ISUM( nLessNodesToDig )
      ENDIF

      F%nNodeToDig = F%nNodeToDig - nLessNodesToDig
      WRITE(6,1234)' ?> DibyCr',ipid,' F%nNodeToDig  = ', F%nNodeToDig ! debug test

      heap = SUM( heapCL(:) )  ! Total volume of dug material at current time step
      WRITE(6,1234)' ?> DibyCr',ipid,'         heap  = ', heap         ! debug test


      IF( A%DumpVolume < 0.0_8 ) A%DumpVolume = 0.0_8  !  In case in subroutine Dump_by_Rate
                                                       !> A%DumpVolume was set to -1.0

      ! Add per class   volume of dug material and DumpVolume
      heapCL(:) = heapCL(:) + A%GrainClass(:) * A%DumpVolume
                              !............................!--- convert fraction to volume


      A%DumpVolume = A%DumpVolume + heap



1234  FORMAT(A, I2, A, g18.9)
      WRITE(6,1234)' ?> DibyCr',ipid,' A%DumpVolume  = ',A%DumpVolume  ! debug test
      WRITE(6,1234)' ?> DibyCr',ipid,' A%FieldDumpID = ',A%FieldDumpID ! debug test


      IF(       F%nNodeToDig  >  0                 !  Digging not accomplished
     &    .AND. A%DumpVolume  >  0.0_8  ) THEN     !  Dumping not accomplished

        A%GrainClass(:) = heapCL(:) / A%DumpVolume
                          !......................!----- convert thickness to fraction

        IF(abs(A%DumpVolume - SUM(heapCL(:))) .GE. 0.00000001_8 ) THEN ! debug
          WRITE(*,*)'?>  SumheapCL(:)    =', SUM( heapCL(:) )          ! debug
          WRITE(*,*)'?>  A%DumpVolume    =', A%DumpVolume              ! debug
          WRITE(*,*)'?>  A%GrainClass(:) =', A%GrainClass(:)           ! debug
          STOP                                                         ! debug
        ENDIF                                                          ! debug

        IF( A%FieldDumpID > 0 ) THEN               !> Only if a valid dump field
          CALL Dump_by_Rate( A, dt_ts, dzCL_sis )  !  is linked to the action
        ENDIF

      ELSE     !> finalise action temporarily
        WRITE(6,*)'?> finalise action temporarily '  ! debug test
        A%State           = 2     ! 2 = temporary inactive!
        A%TimeStart       = A%TimeStart + A%TimeRepeat
        A%FirstTimeActive = .TRUE.
        A%tsCount         = 0     !  counter of time steps while digger is working

        DEALLOCATE( F%refZ )
        DEALLOCATE( F%km )
        DEALLOCATE( F%NodeToDig )
        !DEALLOCATE( layCL )
        !DEALLOCATE( heapCL )
!        DEALLOCATE( A%GrainClass )
      ENDIF

      WRITE(6,*)'?>-------  SR Dig_by_Criterion END ---------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE Dig_by_Criterion            !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************


!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************

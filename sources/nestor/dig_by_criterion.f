!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dig_by_Criterion               !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(   A, F, dt_ts, z_sis, dzCL_sis
     &  , AVAIL, ELAY0, time, KNOLG, m   )

      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  ParallelComputing, nGrainClass, ipid
     &                     , npoinGlobal
      USE INTERFACE_PARALLEL, ONLY : P_DSUM, P_ISUM   
        
#ifndef NESTOR_INTERFACES                                        
      USE m_Interfaces_Nestor, ONLY : Dump_by_Rate
     &                               ,CalcDigVolumeInRadius
#endif NESTOR_INTERFACES                                        
     
      IMPLICIT NONE

      TYPE(t_Action),INTENT(INOUT)   :: A
      TYPE(t_Field) ,INTENT(INOUT)   :: F
      REAL (KIND=R8),INTENT(IN)      :: dt_ts
      REAL (KIND=R8),INTENT(IN)      :: z_sis(:)     ! bottom [m+NN] at time (assumed-shape array)
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT)   :: dzCL_sis(:)
      REAL (KIND=R8),INTENT(IN)      :: AVAIL(:,:,:) ! debug test!  assumed-shape array
      REAL (KIND=R8),INTENT(IN)      :: ELAY0        !  activLayerThickness  [ m ]
      REAL (KIND=R8),INTENT(IN)      :: time         !  time [s]
      INTEGER       ,INTENT(IN)      :: KNOLG(:)     ! index list: Local to Global node index
      INTEGER       ,INTENT(IN)      :: m            ! number of Action
      
      
#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------





      INTEGER            :: i, iCL, iMesh, status, nodeIndex
      INTEGER            :: nLessNodesToDig = 0
      REAL (KIND=R8)     :: dzDig_ts, dzDig
      REAL (KIND=R8)     :: dzEvo_sis
      !REAL (KIND=R8)     :: x, y
      REAL (KIND=R8)     :: remainingDepthToDig


      REAL (KIND=R8)                       :: heap   !> total dug volume of all FieldNodes
      REAL (KIND=R8),ALLOCATABLE                     !  during the current timestep
     &              ,SAVE ,DIMENSION   (:) :: heapCL !> total dug volume per grain class of all
      REAL (KIND=R8),ALLOCATABLE                     !  FieldNodes during the current timestep
     &              ,SAVE ,DIMENSION   (:) :: layCL

      TYPE(t_String_Length) :: SRname ! name of current Subroutine 
      
      
1234  FORMAT(A, I2, A, g18.9)
!663   FORMAT(' ?>',2(/,' ?>'))            ! 3 lines like "?>         "
!      dbug WRITE(6,*)'?>-------  SR Dig_by_Criterion -------------'
      SRname%s = "Dig_by_Criterion"    ! subroutine name
      SRname%i =  16                   ! length of name string


      IF( .NOT. ALLOCATED(layCL) ) ALLOCATE(  layCL( nGrainClass ))
      IF( .NOT. ALLOCATED(heapCL)) ALLOCATE( heapCL( nGrainClass ))
!      ________________________________________________________________
!     |                                                                |
      IF( A%FirstTimeActive ) THEN !___________________________________|
        A%State = 1     ! 1 = Action currently active
        CALL InfoMessage( A, m, time )
        A%DumpVolume = 0.0D0
        ALLOCATE( F%refZ(      F%nNodes ), stat=status)
        ALLOCATE( F%km(        F%nNodes ), stat=status)
        ALLOCATE( F%NodeToDig( F%nNodes ), stat=status)
        !ALLOCATE( layCL(    nGrainClass ), stat=status)
        !ALLOCATE( heapCL(   nGrainClass ), stat=status)

        !A%sumInput = 0.0D0  ! initialisation
        !WRITE(6,*)'?> A%ReferenceLevel = ',A%ReferenceLevel  ! debug test                

        CALL Set_RefLevelByProfiles( F, A%ReferenceLevel) ! the result is F%refZ(:) 

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



        IF( A%MinVolume > 0.0D0 ) THEN
          CALL CalcDigVolumeInRadius( A, F, z_sis, KNOLG )  !> The result is F%NodeToDig(:)
        ENDIF                                               !  and F%nNodeToDig

        IF( ParallelComputing ) THEN
          F%nNodeToDig = P_ISUM( F%nNodeToDig )
          !WRITE(6,1234)' ?> DibyCr',ipid,'F%nNodeToDig  =',F%nNodeToDig! debug test
        ENDIF

        A%GrainClass(:) = 0.0D0
        A%DumpVolume    = 0.0D0
        A%MovedVolume   = 0.0D0

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

      heapCL(:)       = 0.0D0              !> Initialisation must happen here AND again below
      nLessNodesToDig = 0
      IF( F%nNodeToDig > 0 ) THEN
        A%tsCount = A%tsCount + 1          !> Count time steps while digger is working
        dzDig_ts  = dt_ts * A%DigRate      !> Depth to dig during one time step
        heapCL(:) = 0.0D0
        DO i=1, F%nNodes

          IF( .NOT. F%NodeToDig(i) ) CYCLE

          dzDig = dzDig_ts
          iMesh = F%Node(i)

          dzEvo_sis = 0.0D0
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
9119        FORMAT (A5,1X,3(G15.8,1X),I8,1X,F8.3,1X,2(G15.8,1X),A16)
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

          IF( -dzDig + dzEvo_sis <= -ELAY0 ) Call ErrMsgAndStop(
     &     "while Action  Dig_by_Criterion                          ",56
     &    ,"reason: Digger exceeds active layer thickness           ",56
     &    ,"        ==> reduce DigRate or reduce time step or ...   ",56 
     &    ,"occured in Action: ", 19, m, SRname, ipid      )

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

      
      IF( ParallelComputing ) THEN
        DO iCL=1, nGrainClass
          heapCL(iCL) = P_DSUM( heapCL(iCL) )
        ENDDO
        nLessNodesToDig = P_ISUM( nLessNodesToDig )
      ENDIF
      
      F%nNodeToDig = F%nNodeToDig - nLessNodesToDig
      !WRITE(6,1234)' ?> DibyCr',ipid,' F%nNodeToDig  = ', F%nNodeToDig ! debug test
      
      
      !=================================================================
      !> calc. new DumpVolume and its sediment composition
      !  each time step we add the dug material per class to A%DumpVolume

      heap = SUM( heapCL(:) )  ! Total volume of dug material at current time step
      !WRITE(6,1234)' ?> DibyCr',ipid,'         heap  = ', heap         ! debug test


      !IF( A%DumpVolume < 0.0D0 ) A%DumpVolume = 0.0D0  !  In case in subroutine Dump_by_Rate
      !                                                 !> A%DumpVolume was set to -1.0

      ! Add per class   volume of dug material and old DumpVolume
      heapCL(:) = heapCL(:) + A%GrainClass(:) * A%DumpVolume
                              !............................!--- convert fraction to volume

      ! new DumpVolume 
      A%DumpVolume  = A%DumpVolume  + heap
      
      ! new sediment composition of the DumpVolume 
      IF( A%DumpVolume  > 0.0D0 ) THEN
        A%GrainClass(:) = heapCL(:) / A%DumpVolume
                          !......................!----- convert volume to fraction
      ENDIF                  
      
      A%MovedVolume = A%MovedVolume + heap


      !WRITE(6,1234)' ?> DibyCr',ipid,' A%DumpVolume  = ',A%DumpVolume  ! debug test
      !WRITE(6,1234)' ?> DibyCr',ipid,' A%FieldDumpID = ',A%FieldDumpID ! debug test

!      IF(       F%nNodeToDig  >  0                 !  Digging not accomplished
!     &     .OR. A%DumpVolume  >  0.0D0  ) THEN     !  Dumping not accomplished  



      IF(     F%nNodeToDig  >  0                !> Digging not accomplished
     &   .OR. (       A%FieldDumpID > 0         !> Duming is assigned (a dump field is linked to the action)
     &          .AND. A%DumpVolume  > 0.0D0 )   !  and Dumping is not accomplished
     &   ) THEN                                      


        !IF(abs(A%DumpVolume - SUM(heapCL(:))) .GE. 0.00000001D0 ) THEN ! debug
        !  WRITE(*,*)'?>  SumheapCL(:)    =', SUM( heapCL(:) )          ! debug
        !  WRITE(*,*)'?>  A%DumpVolume    =', A%DumpVolume              ! debug
        !  WRITE(*,*)'?>  A%GrainClass(:) =', A%GrainClass(:)           ! debug
        !  STOP                                                         ! debug
        !ENDIF                                                          ! debug

        IF( A%FieldDumpID > 0 ) THEN    !> Only if a dump field is linked to the action
          IF( A%DumpVolume > 0.0D0 ) THEN          
            CALL Dump_by_Rate( A, dt_ts, dzCL_sis )  
          ENDIF 
        ENDIF

      ELSE     !> finalise action temporarily

        IF( time <= A%TimeEnd ) THEN
          
          IF( A%TimeStart + A%TimeRepeat  >  A%TimeEnd )THEN
            A%State           = 9     ! 9 = for ever inactive
            CALL InfoMessage( A, m, time ) 
          ELSE
            A%State           = 2     ! 2 = temporary inactive
            CALL InfoMessage( A, m, time )
            A%TimeStart       = A%TimeStart + A%TimeRepeat
            A%FirstTimeActive = .TRUE.
            A%tsCount         = 0     !  counter of time steps while digger is working
          ENDIF   
          
        ELSE
          A%State           = 9     ! 9 = for ever inactive
          CALL InfoMessage( A, m, time )
        ENDIF

        DEALLOCATE( F%refZ )
        DEALLOCATE( F%km )
        DEALLOCATE( F%NodeToDig )
        !DEALLOCATE( layCL )
        !DEALLOCATE( heapCL )
        !DEALLOCATE( A%GrainClass )
      ENDIF

!      dbug WRITE(6,*)'?>-------  SR Dig_by_Criterion END ---------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE Dig_by_Criterion            !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

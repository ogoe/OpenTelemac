!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dump_by_Time                   !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(   A, F, dt_ts, z_sis, dzCL_sis
     &  , ELAY0, time, m   )
                                         
      USE m_TypeDefs_InterFace           
      USE m_TypeDefs_Nestor             
      USE m_Nestor , ONLY :  ParallelComputing, nGrainClass, ipid
      USE INTERFACE_PARALLEL, ONLY : P_DSUM   
      
     
                                                                       
      IMPLICIT NONE               
      TYPE(t_Action),INTENT(INOUT) :: A            !> Action  
      TYPE(t_Field) ,INTENT(INOUT) :: F            !> Field   
      REAL (KIND=R8),INTENT(IN)    :: dt_ts        !> time-step-duration  [ s ]
      REAL (KIND=R8),INTENT(IN)    :: z_sis(:)     !> bottom [ m+NN ] assumed-shape array
      TYPE( t_PointerToArrayOfReals )    
     &              ,INTENT(INOUT) :: dzCL_sis(:)  !> bedload evolution per Class  [ m ]  
      REAL (KIND=R8),INTENT(IN)    :: ELAY0        !> activLayerThickness  [ m ]
      REAL (KIND=R8),INTENT(IN)    :: time         !> time [ s ]    
      INTEGER       ,INTENT(IN)    :: m            !> number of Action

#ifndef  NESTOR_INTERFACES                                        

      !------- local variables ---------------
      TYPE(t_String_Length) :: SRname ! name of current Subroutine 
      
         

      
      INTEGER            :: i, iCL, iMesh, status !, nodeIndex
      REAL (KIND=R8)     :: max_dz_dt  
      
      
663   FORMAT(' ?>',2(/,' ?>'))            !> 3 lines like "?>         "
       
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Time -----------------'
      SRname%s = "Dump_by_Time"        !> subroutine name      
      SRname%i =  12                   !> length of name string        

      IF( .NOT. A%DumpPlanar ) THEN
        IF( A%FirstTimeActive )  THEN
          A%State = 1     !> 1 = Action currently active
          CALL InfoMessage( A, m, time ) 
                                                                
          A%nts = INT(   (A%TimeEnd - time) / dt_ts ) !> calculate number of time
                                                      !  steps (nts) to fulfil the Action                                 

          IF( A%nts < 1 ) Call ErrMsgAndStop( " ",            1
     &      ,"reason:  period for this action is to short ", 44
     &      ," ",1,"occured in action number:",25, m, SRname, ipid )
          
          A%dzTot      = A%DumpVolume / F%Area        !> change of z to fulfil the Action
          A%dz_ts      = A%dzTot / DBLE( A%nts )      !> change of z per time step
          A%dzCL_ts(:) = A%dz_ts * A%GrainClass(:)    !> change of z per time step per
                                                      !  Grain CLass  

          !WRITE(6,*) '?> dz_ts pro time step = ',A%dz_ts  ! debug
                                                                
          A%tsCount         = 1
          A%FirstTimeActive = .FALSE.                
        ENDIF  !( FirstTimeActive )
        
        
                                 
        IF( A%tsCount <=  A%nts ) THEN               
          !----  dump it -------------------------------
          !countDump = countDump + 1                            ! debug
          !WRITE(6,*) 'countDump = ',countDump                  ! debug
          DO iCL=1, nGrainClass  
            DO i=1, F%nNodes  
              iMesh = F%Node(i)     ! mesh index of field node
              dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &                                 + A%dzCL_ts(iCL)
            ENDDO                
          ENDDO !----------------------------------------
          !mySum = mySum + A%dz_ts                           ! debug
        ENDIF                    
                                 
        !IF( A%tsCount ==  A%nts ) THEN                      ! debug     
        !  WRITE(6,*) 'preset dump   volume= ', A%DumpVolume ! debug
        !  WRITE(6,*) '       dumped volume= ', mySum*F%Area ! debug
        !ENDIF                                               ! debug
                                 
        A%tsCount = A%tsCount + 1
        IF( A%tsCount  >  A%nts ) THEN !> The action is over 
          A%State = 9                  !> 9 = Action for ever inactive
          CALL InfoMessage( A, m, time ) 
          
        ENDIF                    
                                 
      ENDIF !( .NOT. DumpPlanar )
!--------------------------------------------------------              
      IF( A%DumpPlanar ) THEN 
        IF( A%FirstTimeActive ) THEN
          A%State = 1     ! 1 = Action currently active
          CALL InfoMessage( A, m, time ) 
                                 
          ALLOCATE( F%Z(    F%nNodes ), stat=status)
          ALLOCATE( F%dZ(   F%nNodes ), stat=status)
          ALLOCATE( F%refZ( F%nNodes ), stat=status)
          ALLOCATE( F%km(   F%nNodes ), stat=status)
                                 
          A%sumInput = 0.0D0  
                                 
          F%dZ   = 999.9D0    
          DO i=1, F%nNodes     !> set value in the Field-Structur
            iMesh        = F%Node(i)   !> mesh index of field node 
            F%Z(i)    = z_sis( iMesh )
          ENDDO
          CALL Set_RefLevelByProfiles( F,A%ReferenceLevel) !> the result is F%refZ(:) 
                                 
          CALL Calculate_PlanarLevel( F, A%DumpVolume, 1 )    !> 1 => dump;  the result is F%dz(:) 
          A%nts = INT( (A%TimeEnd - time) / dt_ts ) !> calculate number of time 
                                                    !  steps (nts) to fulfil the Action  
                                 
          max_dz_dt =  MAXVAL( F%dz ) / DBLE( A%nts )
          
          IF( max_dz_dt  >  ELAY0 )Call ErrMsgAndStop( "    ",  4 
     &    ,"reason:  dz_dt > ELAY0                           ",49 
     &    ,"         Change per time step of bottom level    ",49
     &    ,"         is too big                              ",49
     &    , -1, SRname, ipid                                     )
          
                                                                                                         
          A%tsCount         = 1               
          A%FirstTimeActive = .FALSE.  
        ENDIF  !( FirstTimeActive )
                                 
        IF( A%tsCount <=  A%nts ) THEN
          !> Before dumping we calc. the amount of sediment that was
          !  transported by morphodynamic during the last time step 
          !  into the field. 
          !  I case there is a futher action operating at the same time  
          !  on this field and it is carried out allready (depends on the 
          !  internal order of execution), then it will 
          !  appear here as sumInput too.          
          DO iCL=1, nGrainClass    !  Only nodes below the planar
            DO i=1, F%nNodes       !  level are included for it.
              iMesh = F%Node(i)    !> mesh index of field node
              IF( F%dz(i) > 0.0D0 ) A%sumInput 
     &          =    A%sumInput
     &             + dzCL_sis(iCL)%R(iMesh) * F%NodeArea(i)
            ENDDO                
          ENDDO                  
                                 
          DO iCL=1, nGrainClass  !----- dump one time step -----
            DO i=1, F%nNodes  
              iMesh = F%Node(i)     !> mesh index of field node
              dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &        + A%GrainClass(iCL) *F%dz(i) /DBLE(A%nts) 
            ENDDO                
          ENDDO !-----------------------------------------------
                                 
!          DO iCL=1, nGrainClass                             ! debug
!            DO i=1, F%nNodes                                ! debug
!              mySum = mySum  +  F%dz(i) / DBLE( A%nts )     ! debug
!                             *  A%GrainClass(iCL)           ! debug
!                             *  F%NodeArea(i)               ! debug
!            ENDDO                                           ! debug
!          ENDDO                                             ! debug 
                                 
        ENDIF !( A%tsCount <=  A%nts )
                                 
        A%tsCount = A%tsCount + 1
                                 
        IF( A%tsCount  >  A%nts ) THEN !> The action is over 
          IF(ParallelComputing) THEN
            !mySum         = P_DSUM(mySum)                      ! debug
            A%sumInput = P_DSUM(A%sumInput)
          ENDIF                    
!            WRITE(6,663)               
!            WRITE(6,*)'?> Message to field: ',F%name
!            WRITE(6,*)'?>       time Start: ',A%TimeStart 
!            WRITE(6,*)'?>       time      : ',time 
!            WRITE(6,*)'?>   preset   dump   volume ='
!       &                 ,A%DumpVolume
!            !WRITE(6,*)'?>   de facto dumped volume =',mySum      ! debug 
!            WRITE(6,*)'?>   sediment transported into'
!            WRITE(6,*)'?>     the field while dumping:   '
!       &                 ,A%sumInput
                                 
                                                   
!            mySum = 0.0D0                             ! debug  
!            DO i=1, F%nNodes                          ! debug
!              iMesh = F%Node(i)                       ! debug
!              IF( F%dz(i) > 0.0D0 ) mySum             ! debug
!     &              = mySum + F%NodeArea(i)           ! debug 
!            ENDDO                                     ! debug 
!            WRITE(6,*)'?> touched area = ',mySum      ! debug 
                                 
                                 
          DEALLOCATE( F%Z   , stat=status )
          DEALLOCATE( F%dZ  , stat=status )
          DEALLOCATE( F%refZ, stat=status )
          DEALLOCATE( F%km  , stat=status )
          A%State = 9                        !> 9 = Action for ever inactive
          CALL InfoMessage( A, m, time ) 
        ENDIF                    
                                  
      ENDIF !( A%DumpPlanar ) 
!----------------------------------              
              !WRITE(6,*) 'time   = ', time , A%TimeEnd              ! debug

!      dbug WRITE(6,*)'?>-------  SR Dump_by_Time END -------------'
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
#endif   NESTOR_INTERFACES                       !********************************************
      END SUBROUTINE Dump_by_Time                !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

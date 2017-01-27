!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  Dump_by_Rate                   !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A, dt_ts, dzCL_sis )             
!                                         
      USE m_TypeDefs_InterFace           
      USE m_TypeDefs_Nestor             
      USE m_Nestor , ONLY : ParallelComputing, nGrainClass, F, ipid
!                                         
      IMPLICIT NONE                      
      TYPE(t_Action),INTENT(INOUT) :: A  
      REAL (KIND=R8),INTENT(IN)    :: dt_ts      ! time-step-duration  [ s ]
      TYPE( t_PointerToArrayOfReals )    
     &              ,INTENT(INOUT) :: dzCL_sis(:)
     
#ifndef NESTOR_INTERFACES         
!                                        
      !------- local variables ---------------
      INTEGER            :: i, iCL, iMesh ,n  !  , status
      REAL (KIND=R8)     :: dz_ts        
                                         
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Rate -----------------'
!      
!     
             !1234  FORMAT(A, I2, A, g15.6)                                         ! debug test
             !      WRITE(6,1234)' ?>1DubyRa',ipid,' A%DumpVolume = ', A%DumpVolume ! debug test
                                         
      n = A%FieldDumpID                  
                                         
      dz_ts = dt_ts * A%DumpRate                   ! Hight to dump during one time step
        !WRITE(*,*)'?>  0            dz_ts =',dz_ts        ! debug test
                                         
      IF( dz_ts * F(n)%Area  >=  A%DumpVolume ) THEN  !> Dump hight calc. by DumpRate is bigger than DumpVolume 
        dz_ts        = A%DumpVolume / F(n)%Area       !> Reduce dz_ts
        A%dzCL_ts(:) = dz_ts * A%GrainClass(:)     !> Change of z per time step per class 
!       WRITE(*,*)'?>  1 sum_A%dzCL_ts(:) = ', SUM(A%dzCL_ts(:))    ! debug test 
        DO iCL=1, nGrainClass                      !> Dump it
          DO i=1, F(n)%nNodes            
            iMesh = F(n)%Node(i)     ! Mesh index of field node
            dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &                               + A%dzCL_ts(iCL)
          ENDDO                          
          A%DumpVolume =  0.0D0  
        ENDDO                    
      ELSE                               
        A%dzCL_ts(:) = dz_ts * A%GrainClass(:)     !> Change of z per time step per class   
        DO iCL=1, nGrainClass                      !> Dump it
          DO i=1, F(n)%nNodes            
            iMesh = F(n)%Node(i)     ! Mesh index of field node
            dzCL_sis(iCL)%R(iMesh) =   dzCL_sis(iCL)%R(iMesh)
     &                               + A%dzCL_ts(iCL)
          ENDDO
          
        ENDDO                            
        A%DumpVolume = A%DumpVolume - dz_ts * F(n)%Area  !> Because the value of dz_ts is the same for all nodes 
      ENDIF                                              !  and because F(n)%Area is the whole area over all 
                                                         !  partitions there is no need of "P_DSUM" at this point.
                                                         
      !WRITE(*,*)'?>  dump-dz_ts = ',dz_ts
      !WRITE(6,1234)' ?>2DubyRa',ipid,' A%DumpVolume  = ', A%DumpVolume ! debug test
      !WRITE(6,*)' ?> DumpVolume left = ', A%DumpVolume ! debug test
                                          
!      dbug WRITE(6,*)'?>-------  SR Dump_by_Rate END -------------'
!                                        
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !********************************************  
      END SUBROUTINE Dump_by_Rate                !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

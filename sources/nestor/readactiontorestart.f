      SUBROUTINE  ReadActionToRestart            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( fileName )                             
                                         
      USE m_TypeDefs_Nestor             
      USE m_Nestor, ONLY :  A, nActions, nGrainClass, ipid
     &                     , ParallelComputing
      IMPLICIT NONE                      
                                         
      !REAL (KIND=8) ,INTENT(INOUT)    :: time          !  time [s]   
      CHARACTER(128),INTENT(IN)       :: fileName
        
      !------- local variables ---------------
      INTEGER :: i, n, fu  ! fu: file unit
      CHARACTER (128) :: zeile, valuePart 
      REAL (KIND=8)   :: time
        
        
      WRITE(6,*)'?>-------  SR ReadActionToRestart ----------'
        
      WRITE(6,*)'?> ipid = ', ipid
        
      fu       = 594 
      CALL open_File( fileName, fu, 'r' )        ! open File
        
      ! === Read global variables ====              
      READ( fu, '(A)') zeile
      write(6,*) zeile    ! debug                    ! read line  1                  
      READ( fu, '(A)') zeile
      write(6,*) zeile    ! debug                    ! read line  2
      valuePart = zeile(25:)        
      READ(valuePart,*)        nActions      
      READ( fu, '(A)') zeile
      write(6,*) zeile    ! debug                    ! read line  3
      valuePart = zeile(25:)        
      READ(valuePart,*)        nGrainClass      
      READ( fu, '(A)') zeile
      write(6,*) zeile    ! debug                    ! read line  4
      valuePart = zeile(25:)        
      READ(valuePart,*)        time      
      READ( fu, '(A)') zeile                    ! read line  5  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  6  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  7  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  8  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  9  (dummy line )
      READ( fu, '(A)') zeile                    ! read line 10  (dummy line )
        
      DO n=1, nActions    ! === read Actions ====
        READ( fu, '(A)') zeile                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%ActionType
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%ActionTypeStr
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%FieldDig
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%FieldDigID
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%ReferezLevel
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%TimeStart
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%TimeEnd
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%TimeRepeat
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%DigVolume
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%DigRate
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%DigDepth
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%DigPlanar
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%CritDepth
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%MinVolume
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%MinVolumeRadius
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%FieldDump
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%FieldDumpID
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%DumpVolume
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%DumpRate
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%DumpPlanar
        DO i=1, nGrainClass                     
          READ( fu, '(A)') zeile
          write(6,*) zeile    ! debug                 
          valuePart = zeile(25:)        
          READ(valuePart,*)      A(n)%GrainClass(i)
        ENDDO                                     
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%FirstTimeActive
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%State
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%nts
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%tsCount
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%sumInput
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%dt_ts
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%dz_ts
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%dz_dt
        READ( fu, '(A)') zeile
        write(6,*) zeile    ! debug                    
        valuePart = zeile(25:)        
        READ(valuePart,*)       A(n)%dzTot
        DO i=1, nGrainClass                  
          READ( fu, '(A)') zeile
          write(6,*) zeile    ! debug                    
          valuePart = zeile(25:)        
          READ(valuePart,*)     A(n)%dzCL_ts(i)
        ENDDO 
      ENDDO  ! n=1, nActions  
        
      WRITE(6,*)'?>  time = ',time 
!                       
      IF( ParallelComputing ) CALL P_SYNC()
      CLOSE(fu)  
      WRITE(6,*)'?>-------  SR ReadActionToRestart End ------'
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE ReadActionToRestart         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
        
!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************

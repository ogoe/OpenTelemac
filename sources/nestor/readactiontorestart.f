!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ReadActionToRestart            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( fileName )                             
                                         
      USE m_TypeDefs_Nestor             
      USE m_Nestor, ONLY :  A, nActions, nGrainClass, ipid
     &                    , ParallelComputing, ncsize

         
     
#ifndef NESTOR_INTERFACES                                        
      USE m_Interfaces_Nestor, ONLY :  open_File
     &                               , ErrMsgAndStop
#endif NESTOR_INTERFACES                                        
     
     
     
      IMPLICIT NONE                      
                                         
      CHARACTER(128),INTENT(IN)       :: fileName
        
#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------
      

      INTEGER :: i, n, fu      ! fu: file unit
      INTEGER :: prior_ncsize  ! parallel threads of restart file
     
      CHARACTER (128) :: zeile, valuePart 
      REAL (KIND=R8)  :: time
        
      TYPE(t_String_Length) :: SRname     ! subroutine where the error occured 
      
!      dbug WRITE(6,*)'?>-------  SR ReadActionToRestart ----------'
      SRname%s = "ReadActionToRestart"
      SRname%i = 19
        
      !WRITE(6,*)'?> ipid = ', ipid
        
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

      READ( fu, '(A)') zeile
      write(6,*) zeile    ! debug                    ! read line  5
      valuePart = zeile(25:)        
      READ(valuePart,*)        prior_ncsize
 
      READ( fu, '(A)') zeile                    ! read line  6  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  7  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  8  (dummy line )
      READ( fu, '(A)') zeile                    ! read line  9  (dummy line )
      READ( fu, '(A)') zeile                    ! read line 10  (dummy line )

      IF( prior_ncsize /= ncsize) THEN 
        Call ErrMsgAndStop(
     &   "while read the Nestor restart file         ", 43
     &  ,"reason:  number of parallel threads differ ", 43 
     &  ,"                                           ", 43
     &  ,"         cpus used for prior computation:",   41
     &  , prior_ncsize, SRname, ipid     )
      ENDIF


      
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
        READ(valuePart,*)       A(n)%ReferenceLevel
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
!      dbug WRITE(6,*)'?>-------  SR ReadActionToRestart End ------'
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE ReadActionToRestart         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
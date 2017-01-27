!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  WriteFieldToRestart            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( time, fileName )                             
                                         
      USE m_TypeDefs_Nestor             
      USE m_Nestor, ONLY :  F, nFields, ipid
     &                     , ParallelComputing
     
#ifndef NESTOR_INTERFACES                                        
      USE m_Interfaces_Nestor, ONLY :  open_File 
#endif NESTOR_INTERFACES                                        
     
     
      IMPLICIT NONE                      
                                         
      REAL (KIND=R8),INTENT(IN)    :: time          !  time [s]   
      CHARACTER(128),INTENT(IN)    :: fileName
        
#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------
      
      INTEGER :: n, fu  ! fu: file unit
      
!      dbug WRITE(6,*)'?>-------  SR WriteFieldToRestart ----------'
        
      !WRITE(6,*)'ipid = ', ipid
        
      !fileName = "_restart_FieldData.dat"
      fu       = 479
      CALL open_File( fileName, fu, 'w' )        ! open File
      ! 
      ! === Write global variables ====
      !           123456789012345678901234567890         
      WRITE(fu,*)'info text: time      =    ', time              ! 1 
      WRITE(fu,*)'nFields              =    ', nFields           ! 2 
      WRITE(fu,*)'xx                   =    ', -11               ! 3 
      WRITE(fu,*)'xx                   =    ', -11               ! 4 
      WRITE(fu,*)'xx                   =    ', -11               ! 5 
      WRITE(fu,*)'xx                   =    ', -11               ! 6 
                                            
      WRITE(fu,*)'ipid                 =    ', ipid
        
      DO n=1, nFields 
        WRITE(fu,'(" Field name           =    ",A)') F(n)%Name
        
        WRITE(fu,*)'Field index          =    ', n
        WRITE(fu,*)'Field nNodeToDig     =    ', F(n)%nNodeToDig
        IF( ALLOCATED(F(n)%Z) )THEN 
          WRITE(fu,*)"____Z"
          WRITE(fu,*)  F(n)%Z(:)                
        ENDIF
        IF( ALLOCATED(F(n)%dZ) )THEN 
          WRITE(fu,*)"___dZ" 
          WRITE(fu,*) F(n)%dZ(:)               
        ENDIF
        IF( ASSOCIATED(F(n)%refZ) )THEN 
          WRITE(fu,*)"_refZ" 
           !WRITE(fu,*)  F(n)%refZ => null()(i)   
          WRITE(fu,*) F(n)%refZ(:)   
        ENDIF
        IF( ALLOCATED(F(n)%km) )THEN 
          WRITE(fu,*)"___km" 
          WRITE(fu,*) F(n)%km(:)               
        ENDIF
        IF( ALLOCATED(F(n)%NodeToDig) )THEN 
          WRITE(fu,*)"ToDig" 
          WRITE(fu,*) F(n)%NodeToDig(:)        
        ENDIF
        
        WRITE(fu,*)"F-END" 
        
        
      ENDDO  ! n=1, nFields 
        
        
        
      CLOSE(fu)  
!      dbug WRITE(6,*)'?>-------  SR WriteFieldToRestart End ------'
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE WriteFieldToRestart         !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ReadFieldToRestart             !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( fileName )                             
                                         
      USE m_TypeDefs_Nestor             
      USE m_Nestor, ONLY :  F, nFields, ipid, ncsize
     &                     , ParallelComputing
         
         

#ifndef NESTOR_INTERFACES                                        
      USE m_Interfaces_Nestor, ONLY :  open_File 
#endif NESTOR_INTERFACES                                        
     
      IMPLICIT NONE                      
                                         
      CHARACTER(128),INTENT(IN)    :: fileName
        
#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------
      INTEGER :: n, j, fu  ! fu: file unit
      INTEGER :: partition
      CHARACTER (128) :: zeile
      REAL (KIND=R8)  ::  time          !  time [s]   
        
!      dbug WRITE(6,*)'?>-------  SR ReadFieldToRestart -----------'
        
      WRITE(6,*)'ipid     = ', ipid
      WRITE(6,*)'ncsize   = ', ncsize 
      WRITE(6,*)'fileName = ', fileName 
        
      !fileName = "_restart_FieldData.dat"
      fu = 459 
      CALL open_File( fileName, fu, 'r' )     
        
      READ( fu,*) zeile
      DO WHILE( zeile(1:5) /= 'A-END' )  !> run over Action data to the 
        READ( fu,*) zeile                !  position where Field data begins
      ENDDO      
        
      ! 
      ! === Read global variables ====
        
      READ( fu,'(A)') zeile
      READ(zeile(25:),*)        time      
      write(6,*) zeile    ! debug                    ! read line  1                  
        
      READ( fu,'(A)') zeile
      READ(zeile(25:),*)        nFields      
      write(6,*)   nFields    ! debug                    ! read line  2
        
      READ( fu,'(A)') zeile
      write(6,*) zeile    ! debug                    ! read line  2
        
      READ( fu,'(A)') zeile
      write(6,*) zeile    ! debug                    ! read line  2
        
      READ( fu,'(A)') zeile
      write(6,*) zeile    ! debug                    ! read line  2
        
      READ( fu,'(A)') zeile
      write(6,*) zeile    ! debug                    ! read line  2
      !DO j=1, ncsize
      DO      
        READ( fu,'(A)', END=900 ) zeile ! ipid
        IF(     zeile(2:5) == 'ipid' 
     &     .OR. zeile(1:4) == 'ipid'  ) THEN
          write(6,*) zeile                        ! debug                   
          READ(zeile(25:),*)  partition 
          IF( partition == ipid )THEN
            write(6,*)'partition =', partition   ! debug  
            EXIT
          ENDIF !( partition == ipid )         
        ENDIF !( zeile(2:5) == 'ipid' )            !  
      ENDDO  
        
      DO j=1, nFields 
        READ(fu,'(A)') zeile                        ! Field name (and ignore)
        write(6,*) zeile                                         ! debug                    
        READ(fu,'(A)') zeile                     
        write(6,*) zeile                                         ! debug                   
        READ(zeile(25:),*)  n                       ! Field index value                                               
        write(6,*) 'n = ',n                                      ! debug                    
        
        READ(fu,'(A)') zeile                      
        write(6,*) zeile                                         ! debug                   
        READ(zeile(25:),*)  F(n)%nNodeToDig         ! Field nNodeToDig value                                               
        write(6,*) 'F(n)%nNodeToDig = ',F(n)%nNodeToDig          ! debug                    
        
        
        DO                                                  
          READ(fu,*) zeile    ! identifier                                     
          write(6,*) zeile    ! debug                    
          SELECT CASE( zeile(1:5) )                                 
           CASE( '____Z' ) !----------------------------------------
             write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
           
             IF(.NOT. ALLOCATED( F(n)%Z) )                          
     &                 ALLOCATE( F(n)%Z( F(n)%nNodes ))              
             !DO i=1, F(n)%nNodes                                    
               Read(fu,*)  F(n)%Z(:)                                
             !ENDDO
             CYCLE                
           CASE( '___dZ' ) !----------------------------------------
             write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%dZ) )                         
     &                 ALLOCATE( F(n)%dZ( F(n)%nNodes ))             
             !DO i=1, F(n)%nNodes                                    
               Read(fu,*)  F(n)%dZ(:)                               
             !ENDDO                                                  
             CYCLE                
           CASE( '_refZ' ) !----------------------------------------
             write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ASSOCIATED( F(n)%refZ) )                       
     &                 ALLOCATE( F(n)%refZ( F(n)%nNodes ))           
             !DO i=1, F(n)%nNodes                                    
               Read(fu,*)  F(n)%refZ(:)                             
             !ENDDO
             F(n)%targZ => F(n)%refZ(:)                
             CYCLE                
           CASE( '___km' ) !----------------------------------------
             write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%km) )                         
     &                 ALLOCATE( F(n)%km( F(n)%nNodes ))             
             !DO i=1, F(n)%nNodes                                    
               Read(fu,*)  F(n)%km(:)                               
             !ENDDO                                                  
             CYCLE                
           CASE( 'ToDig' ) !----------------------------------------
             write(6,*)'case:', zeile(1:5)    ! debug                    ! read line  2
             IF(.NOT. ALLOCATED( F(n)%NodeToDig) )                   
     &                 ALLOCATE( F(n)%NodeToDig( F(n)%nNodes ))       
             !DO i=1, F(n)%nNodes  
               Read(fu,*)  F(n)%NodeToDig(:)               
             !ENDDO 
             CYCLE                
           CASE DEFAULT    !----------------------------------------                
             write(6,*)'case:   DEFAULT'    ! debug              
             EXIT
          END SELECT
        ENDDO
         
       ENDDO !  j=1, nFields 
          !ENDIF ! partition == ipid
        !ENDIF ! zeile(1:4) == 'ipid' 
        
900     CONTINUE   
         
      !ENDDO !j=1, ncsize 
        
      IF( ParallelComputing ) CALL P_SYNC()
      CLOSE(fu)  
!      dbug WRITE(6,*)'?>-------  SR ReadFieldToRestart End -------'
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE ReadFieldToRestart          !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
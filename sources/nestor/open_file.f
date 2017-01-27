!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  open_File                      !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( fileName, FU, whatToDo )                  
                                         
      USE m_TypeDefs_Nestor             
      USE m_Nestor , ONLY : ParallelComputing, ipid, Path

#ifndef  NESTOR_INTERFACES                                        
      USE m_Interfaces_Nestor, ONLY :  ErrMsgAndStop           
#endif   NESTOR_INTERFACES                                        
                                         
      IMPLICIT NONE                      
      CHARACTER(128), INTENT(IN) :: fileName      
      INTEGER,        INTENT(IN) :: FU         ! file unit
      CHARACTER,      INTENT(IN) :: whatToDo      

#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------
      
      TYPE(t_String_Length) :: SRname ! name of current Subroutine 
      
      CHARACTER (128)     :: fiName, chtmp, part2 
      INTEGER             :: stat
                
      ! 1. if write: create file names dependent on the current number of partition
      !    and open to write
      !     e.g.:  fileName =     _restart_Dig.dat
      !              =>       .../_restart_Dig.dat-0000   partititon 0
      !           or =>       .../_restart_Dig.dat-0001   partititon 1
      !                              :
      !           or =>       .../_restart_Dig.dat-0128   partititon 128
      !                              :
      !               
      ! 2. open file to read or to write
        
        
        
                                         
!      dbug WRITE(6,*)'?>-------  SR open_File --------------------'
      SRname%s = "open_File"         ! subroutine name  
      SRname%i =  9                  ! length of name string     
        
      fiName = fileName              ! make a local copy of fileName 
                                         
      !WRITE(6,*)'?> fiName = ', TRIM(fiName)    ! debug
        
      IF( whatToDo == 'r' ) THEN   ! open to read ===============                                
        !IF(ParallelComputing) THEN                                    
        !  WRITE(chtmp,*) ipid            ! write integer into buffer                               
        !  READ(chtmp,*) part2            ! readout string from buffer                        
        !  IF    ( ipid >= 0   .AND. ipid <= 9   ) THEN
        !    fiName = TRIM(Path)//TRIM(fiName)//"-000"//TRIM(part2)
        !  ELSEIF( ipid >= 10  .AND. ipid <= 99  ) THEN
        !    fiName = TRIM(Path)//TRIM(fiName)//"-00"//TRIM(part2) 
        !  ELSEIF( ipid >= 100 .AND. ipid <= 999 ) THEN
        !    fiName = TRIM(Path)//TRIM(fiName)//"-0"//TRIM(part2) 
        !  ELSE                             
        !    fiName = TRIM(Path)//TRIM(fiName)//"-"//TRIM(part2) 
        !  ENDIF                            
        !ELSE                                                          
        !  fiName = TRIM(Path)//TRIM(fiName)                       
        !ENDIF                              
                                                                      
        !fiName = TRIM(Path)//TRIM(fiName)                       
         fiName =             TRIM(fiName)                       
        !WRITE(6,'("?>             fiName = ",A)') fiName    ! debug
                                                                     
        OPEN( FU, FILE = fiName, STATUS = 'OLD'                         
     &          , IOSTAT = stat, ACTION = 'read' )
          !WRITE(*,*)'?>  while open returned status = ', stat       ! debug                            
     
      ENDIF  ! open to read
        
      IF( whatToDo == 'w' ) THEN   ! open to write ==============                                
        IF(ParallelComputing) THEN                                    
          WRITE(chtmp,*) ipid            ! write integer into buffer                               
          READ(chtmp,*) part2            ! readout string from buffer                        
          IF    ( ipid >= 0   .AND. ipid <= 9   ) THEN
            fiName = TRIM(fiName)//"-000"//TRIM(part2)
          ELSEIF( ipid >= 10  .AND. ipid <= 99  ) THEN
            fiName = TRIM(fiName)//"-00"//TRIM(part2) 
          ELSEIF( ipid >= 100 .AND. ipid <= 999 ) THEN
            fiName = TRIM(fiName)//"-0"//TRIM(part2) 
          ELSE                             
            fiName = TRIM(fiName)//"-"//TRIM(part2) 
          ENDIF                            
        ELSE                                                          
          fiName = TRIM(fiName)                       
        ENDIF                              
                                                                      
        !WRITE(6,'("?>             fiName = ",A)') fiName    ! debug
                                                                             
        OPEN( FU, FILE = fiName, STATUS = 'REPLACE'                     
     &          , IOSTAT = stat, ACTION = 'write' ) 
      ENDIF  ! open to write
        
      IF( stat .NE. 0 ) THEN 
        Call ErrMsgAndStop( "while open file: "//TRIM(fiName),
     &  17+LEN_TRIM(fiName)," ",1," ",1," ",1, -1, SRname, ipid   )
      ENDIF
      
!      dbug WRITE(6,*)'?>-------  SR open_File END ----------------'
!                                        
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE open_File                   !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************